# is the signal stable?

source("R/02a_pcoa_data_prep.R") 
pcoa_result <-  readRDS(here::here("output/pcoa_result"))


# functions ####

bootstrap_pcoa_stability <- function(mat, ref_coords, n_boot = 500, prop = 0.80, 
                                     n_axes = 10, mode = c("society", "variable"), seed = 42) {
  mode <- match.arg(mode)
  set.seed(seed)
  
  boot_r      <- numeric(n_boot)
  boot_axis_r <- matrix(NA, nrow = n_boot, ncol = n_axes)
  
  for (b in 1:n_boot) {
    
    if (mode == "society") {
      idx      <- sample(1:nrow(mat), size = floor(nrow(mat) * prop), replace = FALSE)
      mat_boot <- mat[idx, ]
      ref_sub  <- ref_coords[idx, 1:n_axes]
    } else {
      idx      <- sample(1:ncol(mat), size = floor(ncol(mat) * prop), replace = FALSE)
      mat_boot <- mat[, idx, drop = FALSE]
      ref_sub  <- ref_coords[, 1:n_axes]
    }
    
    gd_boot <- daisy(mat_boot, metric = "gower", type = list(ordratio = 1:ncol(mat_boot)))
    pc_boot <- pcoa(gd_boot)
    k       <- min(n_axes, sum(pc_boot$values$Eigenvalues > 0))
    
    boot_sub <- pc_boot$vectors[, 1:k, drop = FALSE]
    prot     <- protest(ref_sub[, 1:k], boot_sub, permutations = 99)
    boot_r[b] <- prot$t0
    
    pro     <- procrustes(ref_sub[, 1:k], boot_sub)
    rotated <- pro$Yrot
    for (j in 1:k) {
      boot_axis_r[b, j] <- cor(ref_sub[, j], rotated[, j], method = "spearman")
    }
  }
  
  list(
    boot_r          = boot_r,
    axis_stability  = colMeans(boot_axis_r, na.rm = TRUE)
  )
}


ref_coords  <- pcoa_result$vectors[, 1, drop = FALSE]
vars_gender <- whyte_vars$var_id_whyte

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
props <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95)
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


appendix_pcoa_stability_axis1 <- ggplot(stability_by_prop, aes(x = n_vars, y = mean_r)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "grey80") +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_point(color = "steelblue", size = 2) +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "grey40") +
  labs(x        = "Number of variables",
       y        = "Axis 1 stability (Spearman r with full ordination)",
       title    = "How many variables needed to recover axis 1?",
       subtitle = "Dashed line = r = 0.90 stability threshold") +
  theme_minimal(base_size = 11)

ggsave("draft/fig/appendix_pcoa_stability_axis1.png", appendix_pcoa_stability_axis1)




