library(dplyr)
library(purrr)
library(cluster)   # daisy
library(ape)        # pcoa
library(vegan)       # procrustes
library(ggplot2)

# =============================================================================
# 1. CORE FUNCTION: given a set of variables, build the Gower/PCoA ordination,
#    align axis 1 to the reference ordination (sign/rotation), attach it to
#    fdata, and fit lm(height_ratio ~ Ax1 + complexity)
# =============================================================================

fit_axis1_model <- function(vars_subset,
                            mat_ord,                 # ordinal-coded matrix, rows = societies
                            ref_coords,               # reference PCoA coords (full-variable solution)
                            fdata,                     # data.frame with height_ratio, complexity, ...
                            id_col   = NULL,            # col in fdata matching rownames(mat_ord); NULL -> use rownames(fdata)
                            formula  = height_ratio ~ Ax1 + complexity) {
  
  out <- list(n_vars = length(vars_subset), r2 = NA_real_, adj_r2 = NA_real_,
              ax1_coef = NA_real_, ax1_se = NA_real_, ax1_p = NA_real_,
              n_obs = NA_integer_, axis_r = NA_real_)
  
  if (length(vars_subset) < 2) return(as.data.frame(out))
  
  mat_sub <- mat_ord[, vars_subset, drop = FALSE]
  
  all_na_rows <- apply(mat_sub, 1, function(x) all(is.na(x)))
  if (all(all_na_rows)) return(as.data.frame(out))
  
  gd_sub <- tryCatch(
    daisy(mat_sub, metric = "gower", type = list(ordratio = 1:ncol(mat_sub))),
    error = function(e) NULL)
  if (is.null(gd_sub) || any(!is.finite(as.matrix(gd_sub)))) return(as.data.frame(out))
  
  pc_sub <- tryCatch(pcoa(gd_sub), error = function(e) NULL)
  if (is.null(pc_sub) || sum(pc_sub$values$Eigenvalues > 0) < 1) return(as.data.frame(out))
  
  # align this axis 1 onto the reference ordination's axis 1 (handles arbitrary
  # sign/rotation of PCoA solutions so Ax1 is comparable across subsamples)
  pro <- tryCatch(procrustes(ref_coords, pc_sub$vectors[, 1, drop = FALSE]),
                  error = function(e) NULL)
  if (is.null(pro)) return(as.data.frame(out))
  
  ax1_aligned <- pro$Yrot[, 1]
  out$axis_r  <- suppressWarnings(cor(ref_coords[, 1], ax1_aligned, method = "spearman"))
  
  # --- attach Ax1 to fdata -------------------------------------------------
  soc_ids <- rownames(mat_sub)
  if (is.null(id_col)) {
    fdata_sub <- fdata[match(soc_ids, rownames(fdata)), , drop = FALSE]
  } else {
    fdata_sub <- fdata[match(soc_ids, fdata[[id_col]]), , drop = FALSE]
  }
  fdata_sub$Ax1 <- ax1_aligned
  
  mod <- tryCatch(lm(formula, data = fdata_sub), error = function(e) NULL)
  if (is.null(mod)) return(as.data.frame(out))
  
  s <- summary(mod)
  out$r2     <- s$r.squared
  out$adj_r2 <- s$adj.r.squared
  out$n_obs  <- stats::nobs(mod)
  if ("Ax1" %in% rownames(s$coefficients)) {
    out$ax1_coef <- s$coefficients["Ax1", "Estimate"]
    out$ax1_se   <- s$coefficients["Ax1", "Std. Error"]
    out$ax1_p    <- s$coefficients["Ax1", "Pr(>|t|)"]
  }
  
  as.data.frame(out)
}

# =============================================================================
# 2. RANDOM-SUBSAMPLE WRAPPER: repeats fit_axis1_model() over n_boot random
#    draws for each proportion in `props`, mirroring your original stability
#    loop, and summarizes R^2 / the Ax1 coefficient at each size
# =============================================================================

boot_axis1_prediction <- function(vars_pool, props, n_boot,
                                  mat_ord, ref_coords, fdata,
                                  id_col  = NULL,
                                  formula = height_ratio ~ Ax1 + complexity,
                                  seed    = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  map_dfr(seq_along(props), function(pi) {
    prop <- props[pi]
    n_v  <- max(2, floor(length(vars_pool) * prop))
    
    res_b <- map_dfr(1:n_boot, function(b) {
      boot_vars <- sample(vars_pool, size = n_v, replace = FALSE)
      fit_axis1_model(boot_vars, mat_ord, ref_coords, fdata, id_col, formula)
    })
    
    data.frame(
      prop          = prop,
      n_vars        = n_v,
      mean_r2       = mean(res_b$r2, na.rm = TRUE),
      ci_low_r2     = quantile(res_b$r2, 0.025, na.rm = TRUE),
      ci_high_r2    = quantile(res_b$r2, 0.975, na.rm = TRUE),
      mean_coef     = mean(res_b$ax1_coef, na.rm = TRUE),
      ci_low_coef   = quantile(res_b$ax1_coef, 0.025, na.rm = TRUE),
      ci_high_coef  = quantile(res_b$ax1_coef, 0.975, na.rm = TRUE),
      n_valid       = sum(!is.na(res_b$r2))
    )
  })
}

# =============================================================================
# 3. FIXED / "GIVEN" SUBSET(S): evaluate one or several specific, named
#    variable subsets (e.g. theoretically chosen variables) the same way,
#    so they can be placed against the random-subsample distribution above
# =============================================================================

fit_named_subsets <- function(subset_list,     # named list: list(my_subset = c("v1","v2",...), ...)
                              mat_ord, ref_coords, fdata,
                              id_col  = NULL,
                              formula = height_ratio ~ Ax1 + complexity) {
  
  map_dfr(names(subset_list), function(nm) {
    res <- fit_axis1_model(subset_list[[nm]], mat_ord, ref_coords, fdata, id_col, formula)
    res$subset_name <- nm
    res
  })
}

# =============================================================================
# 3b. ORDERED / NESTED SEQUENCE: instead of one fixed subset, add variables
#     one at a time in a given order (e.g. names(sort(p_values_loadings)))
#     and refit at every step -> a full curve across n_vars, not one point
# =============================================================================

fit_ordered_sequence <- function(ordered_vars,      # character vector, already in the desired order
                                 mat_ord, ref_coords, fdata,
                                 id_col   = NULL,
                                 formula  = height_ratio ~ Ax1 + complexity,
                                 min_vars = 2) {
  
  map_dfr(min_vars:length(ordered_vars), function(n_v) {
    vars_subset <- ordered_vars[1:n_v]
    fit_axis1_model(vars_subset, mat_ord, ref_coords, fdata, id_col, formula)
  })
}

# =============================================================================
# 4. PLOT: random-subsample distribution (ribbon + mean line) vs the fixed
#    subset(s) (points), as a function of number of variables used
# =============================================================================

plot_axis1_prediction <- function(boot_summary,
                                  specific_results = NULL,   # optional: single fixed subsets, plotted as points
                                  ordered_results   = NULL,   # optional: output of fit_ordered_sequence(), plotted as a red line
                                  metric = c("r2", "coef")) {
  
  metric <- match.arg(metric)
  
  if (metric == "r2") {
    yvar <- "mean_r2"; ylo <- "ci_low_r2"; yhi <- "ci_high_r2"
    spec_y <- "r2"; ylab <- expression(R^2)
  } else {
    yvar <- "mean_coef"; ylo <- "ci_low_coef"; yhi <- "ci_high_coef"
    spec_y <- "ax1_coef"; ylab <- "Ax1 coefficient"
  }
  
  p <- ggplot() +
    geom_ribbon(data = boot_summary,
                aes(x = n_vars, ymin = .data[[ylo]], ymax = .data[[yhi]]),
                fill = "grey70", alpha = 0.4) +
    geom_line(data = boot_summary,
              aes(x = n_vars, y = .data[[yvar]]),
              color = "grey30", linewidth = 0.8)
  
  if (!is.null(ordered_results)) {
    p <- p +
      geom_line(data = ordered_results,
                aes(x = n_vars, y = .data[[spec_y]]),
                color = "red", linewidth = 0.9) +
      geom_point(data = ordered_results,
                 aes(x = n_vars, y = .data[[spec_y]]),
                 color = "red", size = 1.5)
  }
  
  if (!is.null(specific_results)) {
    p <- p +
      geom_point(data = specific_results,
                 aes(x = n_vars, y = .data[[spec_y]], color = subset_name),
                 size = 3)
  }
  
  p +
    labs(x = "Number of variables", y = ylab,
         color = "Specific subset",
         title = "Random-subsample vs targeted prediction of height_ratio",
         subtitle = "Grey ribbon = 95% range of random subsamples; red = variables added by p-value rank; points = named fixed subsets") +
    theme_minimal(base_size = 13)
}

# =============================================================================
# 5. EXAMPLE USAGE (edit to match your objects)
# =============================================================================

ref_coords <- pcoa(daisy(mat_ord_fixed[, vars_gender], metric = "gower",
                         type = list(ordratio = 1:length(vars_gender))))$vectors[, 1, drop = FALSE]

props  <- c(0.025, 0.05, 0.075, 0.1, 0.2, 0.3, 0.4, 0.5, 0.55, 0.6,
            0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1)


boot_summary <- boot_axis1_prediction(
  vars_pool  = vars_gender,
  props      = props,
  n_boot     = 500,
  mat_ord    = mat_ord_fixed,
  ref_coords = ref_coords,
  fdata      = pred_resp_dimorphism,
  id_col     = "soc_id",
  seed       = 1
)

vars_specific <- nvar_significant

specific_results <- fit_named_subsets(
  subset_list = list(theoretical_subset = vars_specific),
  mat_ord     = mat_ord_fixed,
  ref_coords  = ref_coords,
  fdata       = pred_resp_dimorphism,
  id_col      = "soc_id"
)

plot_axis1_prediction(boot_summary, specific_results, metric = "r2")
plot_axis1_prediction(boot_summary, specific_results, metric = "coef")


ordered_vars <- names(sort(p_values_loadings))

ordered_results <- fit_ordered_sequence(
  ordered_vars = ordered_vars,
  mat_ord      = mat_ord_fixed,
  ref_coords   = ref_coords,
  fdata        = pred_resp_dimorphism,
  id_col       = "soc_id"
)

plot_axis1_prediction(boot_summary, ordered_results = ordered_results, metric = "r2")
plot_axis1_prediction(boot_summary, ordered_results = ordered_results, metric = "coef")


# modify boot_axis1_prediction to also return raw r2 draws per n_vars,
# or rerun the inner loop for the relevant n_vars and compare directly:
empirical_p <- function(observed_r2, random_r2_draws) {
  mean(random_r2_draws >= observed_r2, na.rm = TRUE)
}
