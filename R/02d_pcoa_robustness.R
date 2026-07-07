# is the signal stable?

source("R/02a_pcoa_data_prep.R") 
pcoa_result <-  readRDS(here::here("output/pcoa_result"))


# functions ####

# Runs Gower + PCoA on `mat`, then Procrustes-aligns it to `ref_coords`
# ref_coords must already have the same rows as mat (for society-mode resampling)
compare_pcoa_to_ref <- function(mat, ref_coords, n_axes = 5) {
  
  gd <- tryCatch(
    daisy(mat, metric = "gower", type = list(ordratio = 1:ncol(mat))),
    error = function(e) NULL)
  
  if (is.null(gd) || any(!is.finite(as.matrix(gd)))) {
    return(list(r = NA_real_, axis_r = rep(NA_real_, n_axes)))
  }
  
  pc <- tryCatch(pcoa(gd), error = function(e) NULL)
  if (is.null(pc)) return(list(r = NA_real_, axis_r = rep(NA_real_, n_axes)))
  
  k <- min(n_axes, sum(pc$values$Eigenvalues > 0))
  if (k < 1) return(list(r = NA_real_, axis_r = rep(NA_real_, n_axes)))
  
  boot_sub <- pc$vectors[, 1:k, drop = FALSE]
  ref_sub  <- ref_coords[, 1:k, drop = FALSE]
  
  prot <- tryCatch(protest(ref_sub, boot_sub, permutations = 99), error = function(e) NULL)
  pro  <- tryCatch(procrustes(ref_sub, boot_sub),               error = function(e) NULL)
  
  if (is.null(prot) || is.null(pro)) {
    return(list(r = NA_real_, axis_r = rep(NA_real_, n_axes)))
  }
  
  rotated <- pro$Yrot
  axis_r  <- rep(NA_real_, n_axes)
  for (j in 1:k) {
    axis_r[j] <- cor(ref_sub[, j], rotated[, j], method = "spearman")
  }
  
  list(r = prot$t0, axis_r = axis_r)
}


bootstrap_pcoa_stability <- function(mat, ref_coords, n_boot = 500, prop = 0.80,
                                     n_axes = 10, mode = c("society", "variable"), seed = 42) {
  mode <- match.arg(mode)
  set.seed(seed)
  
  boot_r      <- numeric(n_boot)
  boot_axis_r <- matrix(NA_real_, nrow = n_boot, ncol = n_axes)
  
  for (b in 1:n_boot) {
    
    if (mode == "society") {
      idx      <- sample(seq_len(nrow(mat)), size = floor(nrow(mat) * prop), replace = FALSE)
      mat_boot <- mat[idx, , drop = FALSE]
      ref_sub  <- ref_coords[idx, 1:n_axes, drop = FALSE]
    } else {
      idx      <- sample(seq_len(ncol(mat)), size = floor(ncol(mat) * prop), replace = FALSE)
      mat_boot <- mat[, idx, drop = FALSE]
      ref_sub  <- ref_coords[, 1:n_axes, drop = FALSE]
    }
    
    res <- compare_pcoa_to_ref(mat_boot, ref_sub, n_axes = n_axes)
    
    boot_r[b]        <- res$r
    boot_axis_r[b, ] <- res$axis_r
  }
  
  list(
    boot_r         = boot_r,
    axis_stability = colMeans(boot_axis_r, na.rm = TRUE)
  )
}


ref_coords  <- pcoa_result$vectors[, 1, drop = FALSE]
vars_gender <- colnames(mat_ord_fixed)

# 1. Society and variable bootstrap ####
society_stability  <- bootstrap_pcoa_stability(mat_ord_fixed, pcoa_result$vectors, 
                                               n_boot = 500, prop = 0.80, n_axes = 5, 
                                               mode = "society", seed = 42)
society_stability$boot_r %>% summary()
society_stability$axis_stability

variable_stability <- bootstrap_pcoa_stability(mat_ord_fixed, pcoa_result$vectors, 
                                               n_boot = 500, prop = 0.80, n_axes = 5, 
                                               mode = "variable", seed = 123)
variable_stability$boot_r %>% summary()
variable_stability$axis_stability


# 2. Stability by number of variables ####
props <- c(0.025, 0.05, 0.075, 0.1, 0.2, 0.3, 0.4, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95)
n_boot <- 200

stability_by_prop <- map_dfr(seq_along(props), function(pi) {
  prop   <- props[pi]
  axis_r <- numeric(n_boot)
  
  for (b in 1:n_boot) {
    boot_vars <- sample(vars_gender,
                        size    = max(2, floor(length(vars_gender) * prop)),
                        replace = FALSE)
    mat_boot  <- mat_ord_fixed[, boot_vars, drop = FALSE]
    
    all_na_rows <- apply(mat_boot, 1, function(x) all(is.na(x)))
    if (any(all_na_rows)) { axis_r[b] <- NA; next }
    
    gd_boot <- tryCatch(
      daisy(mat_boot, metric = "gower", type = list(ordratio = 1:ncol(mat_boot))),
      error = function(e) NULL)
    if (is.null(gd_boot) || any(!is.finite(as.matrix(gd_boot)))) { axis_r[b] <- NA; next }
    
    pc_boot <- tryCatch(pcoa(gd_boot), error = function(e) NULL)
    if (is.null(pc_boot) || sum(pc_boot$values$Eigenvalues > 0) < 1) { axis_r[b] <- NA; next }
    
    pro <- tryCatch(
      procrustes(ref_coords, pc_boot$vectors[, 1, drop = FALSE]),
      error = function(e) NULL)
    axis_r[b] <- if (!is.null(pro)) cor(ref_coords[, 1], pro$Yrot[, 1], method = "spearman") else NA
  }
  
  data.frame(
    prop        = prop,
    n_vars      = floor(length(vars_gender) * prop),
    mean_r      = mean(axis_r, na.rm = TRUE),
    ci_low      = quantile(axis_r, 0.025, na.rm = TRUE),
    ci_high     = quantile(axis_r, 0.975, na.rm = TRUE),
    pct_above08 = mean(axis_r >= 0.8, na.rm = TRUE),
    n_valid     = sum(!is.na(axis_r))
  )
})



# Most important variables ####
# signif_vars and signif_vars_adjusted are imported from `02c_pcoa_permutation.R`

compare_pcoa_specific_variables <- function(fmat){
  # Pairs of rows must also have at least a non-na value for the same variable!
  mat_iter <- fmat  # start from the original 4-variable matrix
  
  repeat {
    gd_test <- daisy(mat_iter, metric = "gower", type = list(ordratio = 1:ncol(mat_iter)))
    dm <- as.matrix(gd_test)
    if (all(is.finite(dm))) break
    
    bad <- which(!is.finite(dm), arr.ind = TRUE)
    bad <- bad[bad[,1] < bad[,2], ]
    
    offender_counts <- table(c(bad[,1], bad[,2]))
    worst_row <- as.integer(names(which.max(offender_counts)))
    
    mat_iter <- mat_iter[-worst_row, , drop = FALSE]
  }
  
  fmat_clean <- mat_iter
  nrow(fmat_clean)         # how many societies survive
  nrow(fmat) - nrow(fmat_clean)  # how many were dropped
  
  ref_coords_clean <- ref_coords[rownames(fmat_clean), , drop = FALSE]
  compare_pcoa_to_ref(fmat_clean, ref_coords_clean, n_axes = 1)
}

fmat_14 <- mat_ord_fixed[,signif_vars]
r2_14_vars <- compare_pcoa_specific_variables(fmat_14)
correlation_14 <- r2_14_vars$axis_r

fmat_4 <- mat_ord_fixed[,signif_vars_adjusted]
r2_4_vars <- compare_pcoa_specific_variables(fmat_4)
correlation_4 <- r2_4_vars$axis_r


set.seed(42)

get_pval_comparison_pcoa_specific_variables <- function(fmat, correlation, n_rep = 999){
  random_axis_r <- numeric(n_reps)
  for (i in 1:n_reps) {
    rand_vars <- sample(colnames(mat_ord_fixed), size = ncol(fmat), replace = FALSE)
    mat_rand  <- mat_ord_fixed[, rand_vars, drop = FALSE]
    # apply same missingness-cleaning logic as fmat_clean2 here
    res <- compare_pcoa_to_ref(mat_rand, ref_coords, n_axes = 1)
    random_axis_r[i] <- res$axis_r
  }
  # which(is.na(random_axis_r)) %>% length()
  mean(random_axis_r >= correlation, na.rm = TRUE)  # empirical p-value for the observed correlation
}

pval_14 <- get_pval_comparison_pcoa_specific_variables(fmat = mat_ord_fixed[,signif_vars], 
                                                       correlation = correlation_14)

pval_4 <- get_pval_comparison_pcoa_specific_variables(fmat = mat_ord_fixed[,signif_vars_adjusted], 
                                                      correlation = correlation_4)

# Figure ####

# appendix_pcoa_stability_axis1 <- ggplot(stability_by_prop, aes(x = n_vars, y = mean_r)) +
#   geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "grey80") +
#   geom_line(color = "steelblue", linewidth = 0.8) +
#   geom_point(color = "steelblue", size = 2) +
#   geom_hline(yintercept = 0.9, linetype = "dashed", color = "grey40") +
#   labs(x        = "Number of variables",
#        y        = "Axis 1 stability (Spearman r with full ordination)",
#        title    = "How many variables needed to recover axis 1?",
#        subtitle = "Dashed line = r = 0.90 stability threshold") +
#   theme_minimal(base_size = 11)



observed_points <- data.frame(
  n_vars = c(4, 14),
  mean_r = c(correlation_4, correlation_14),   # replace with actual value
  label  = c("4 FDR-significant\nvariables", "14 loading-ranked\nvariables")
)

appendix_pcoa_stability_axis1 <- ggplot(stability_by_prop, aes(x = n_vars, y = mean_r)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "grey80") +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_point(color = "steelblue", size = 2) +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "grey40") +
  geom_point(
    data = observed_points,
    aes(x = n_vars, y = mean_r, color = label, shape = label),
    size = 3.5, inherit.aes = FALSE
  ) +
  ggrepel::geom_label_repel(
    data = observed_points,
    aes(x = n_vars, y = mean_r, label = label, color = label),
    size = 3.2, fill = "white", fontface = "bold",
    nudge_y = 0.08, segment.color = "grey30",
    show.legend = FALSE, inherit.aes = FALSE
  ) +
  scale_color_manual(values = c("4 FDR-significant\nvariables" = "firebrick",
                                "14 loading-ranked\nvariables" = "darkorange")) +
  scale_shape_manual(values = c("4 FDR-significant\nvariables" = 18,
                                "14 loading-ranked\nvariables" = 17)) +
  labs(x        = "Number of variables",
       y        = "Axis 1 stability (Spearman r with full ordination)",
       title    = "How many variables needed to recover axis 1?",
       subtitle = "Dashed line = r = 0.90 stability threshold",
       color    = NULL, shape = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

appendix_pcoa_stability_axis1

ggsave("draft/fig/appendix_pcoa_stability_axis1.png", appendix_pcoa_stability_axis1)
