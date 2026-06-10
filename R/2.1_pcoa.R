source("R/01_import.R")
source("R/functions.R")


library(cluster)    # for daisy/Gower
library(ape)        # for pcoa
library(vegan) # for robustness

whyte_category_angarika <- read.csv("data/SCCS/gender_response_variables_recode_angarika.csv") %>% 
  select(var_id, category_Angarika)


# pcoa ####
## Matrix on response variables ####

whyte_vars_to_remove <- c("DV34") # locality

whyte_vars <- gender_response %>% 
  filter(column_number_whyte78 %in% rep(1:52)) %>% 
  select(var_id, title,var_id_whyte, column_number_whyte78, category1, category2, domain, domain_name, title_shortened) %>%
  mutate(short_title = paste0(var_id, "_", str_trunc(title, width = 30, ellipsis = "..."))) %>% 
  mutate(title = if_else(var_id == "SCCS625", paste("(No)", title), title)) %>%  # high = no high value on men being agressive
  filter(!var_id_whyte %in% whyte_vars_to_remove) # Remove locality

# Categories of variables
whyte_category <- whyte_vars %>% 
  arrange(category1, category2) %>% 
  select(var_id, var_id_whyte, category1, category2, title, title_shortened) %>% 
  mutate(category = 
           case_when(category1 == "role" & category2 %in% c("rituals", "sexual") ~ "role_symbolic",
                     category1 == "role" & category2 %in% c("subsistence", "warfare") ~ "role_material",
                     category1 == "autonomy" & category2 == "sexual" ~ "sexual_autonomy",
                     TRUE ~ category1)) %>% 
  left_join(whyte_category_angarika)


  
whyte_vars %>% 
  group_by(category1, category2) %>% 
  summarize(n = n())

whyte_category %>% 
  group_by(category) %>% 
  summarize(n = n())

whyte_codes <- get_codes(whyte_vars$title)

whyte_data_wide <- sccs %>% 
  filter(var_id %in% whyte_vars$var_id) %>% 
  # If multiple sub_cases per society/variable, take one (e.g. most recent)
  group_by(soc_id, var_id) %>%
  slice_max(year, na_rm = TRUE, n = 1) %>%
  ungroup() %>%
  dplyr::select(soc_id, var_id, code) %>%
  pivot_wider(names_from = var_id, values_from = code) %>% 
  filter(!if_all(-soc_id, is.na))

mat <- whyte_data_wide %>%
  dplyr::select(-soc_id) %>%       # remove the id column
  as.matrix()

rownames(mat) <- whyte_data_wide$soc_id

## Missingness ####

society_coverage <- rowMeans(!is.na(mat))
data.frame(
  min    = min(society_coverage),
  q1     = quantile(society_coverage, 0.25),
  median = median(society_coverage),
  mean   = mean(society_coverage),
  q3     = quantile(society_coverage, 0.75),
  max    = max(society_coverage)) %>% 
  write_csv("output/02_pcoa_society_coverage_summary.csv")

# Remove rows with > 30% missing (=7 rows)
row_miss <- rowMeans(is.na(mat))
mat_filtered <- mat[row_miss <= 0.30, ]
colnames(mat_filtered) <- label_vars(colnames(mat_filtered))

variable_coverage <- colMeans(!is.na(mat))
data.frame(
  min    = min(variable_coverage),
  q1     = quantile(variable_coverage, 0.25),
  median = median(variable_coverage),
  mean   = mean(variable_coverage),
  q3     = quantile(variable_coverage, 0.75),
  max    = max(variable_coverage)) %>% 
  write_csv("output/02_pcoa_variable_coverage_summary.csv")

col_miss <- colMeans(is.na(mat))
mat[, col_miss >= 0.30]

# Removes: "SCCS583" "SCCS620" "SCCS621" "SCCS622"
# Leadership Posts in Kinship or Extended Family Units
# Physical Punishment of the Spouse Condoned
# (No) Explicit View That Men Should and Do Dominat
# Attendance and Participation in General Community


mat[,"SCCS622"] %>% is.na() %>% mean()

## Ordinal and binary variables ####


# Tell daisy which variables are ordinal and which are binary

# 1. Convert columns to ordered factors (needed for daisy to treat as ordinal)
mat_ord <- data.frame(lapply(as.data.frame(mat_filtered), function(x) {
  factor(x, levels = sort(unique(x[!is.na(x)])), ordered = TRUE)
}))
rownames(mat_ord) <- rownames(mat_filtered)

binary_cols <- c(6, 9, 13, 20, 21, 27, 33, 36, 47, 51)
ordinal_cols <- setdiff(1:ncol(mat_ord), binary_cols)

mat_ord_fixed <- mat_ord

# Recode binary columns to 0/1
mat_ord_fixed[, binary_cols] <- lapply(mat_ord[, binary_cols], function(x) {
  x <- as.numeric(as.character(x))
  vals <- sort(unique(x[!is.na(x)]))
  # maps lowest value -> 0, highest -> 1
  ifelse(x == vals[1], 0, ifelse(x == vals[2], 1, NA))
})

## Distance and pcoa ####
# Gower distance — handles NAs and ordinal variables
gower_ordinal <- cluster::daisy(mat_ord_fixed,
                                metric = "gower",
                                type = list(
                                  ordratio = ordinal_cols,
                                  symm = binary_cols
                                ))

pcoa_result <- ape::pcoa(gower_ordinal)

saveRDS(pcoa_result, here::here("output/pcoa_result"))


# Robustness checks ####

robustness_check <- F

if(robustness_check){
  n_axes <- 10
  ref_coords <- pcoa_result$vectors[, 1:n_axes]

  # ── Summaries ──────────────────────────────────────────────────────────────────
  cat("Variable bootstrap r:  mean =", round(mean(boot_r), 4),
      " 95% CI [", round(quantile(boot_r, 0.025), 4),
      ",", round(quantile(boot_r, 0.975), 4), "]\n")
  
  # per-axis mean stability
  axis_stability_vars <- colMeans(boot_axis_r, na.rm = TRUE)
  names(axis_stability_vars) <- paste0("PCo", 1:n_axes)
  print(round(axis_stability_vars, 3))
  
  
  ## Societies ####
  set.seed(42)
  n_boot      <- 500
  n_axes      <- 12
  n_soc       <- nrow(mat_ord_fixed)
  ref_coords  <- pcoa_result$vectors[, 1:n_axes]
  boot_r      <- numeric(n_boot)
  boot_axis_r <- matrix(NA, nrow = n_boot, ncol = n_axes)
  prop        <- 0.80
  
  for(b in 1:n_boot){
    
    idx      <- sample(1:n_soc, size = floor(n_soc * prop), replace = FALSE)
    mat_boot <- mat_ord_fixed[idx, ]
    
    gd_boot  <- daisy(mat_boot,
                      metric = "gower",
                      type   = list(ordratio = 1:ncol(mat_boot)))
    
    pc_boot  <- pcoa(gd_boot)
    k        <- min(n_axes, sum(pc_boot$values$Eigenvalues > 0))
    
    ref_sub  <- ref_coords[idx, 1:k]
    boot_sub <- pc_boot$vectors[, 1:k, drop = FALSE]
    
    prot        <- protest(ref_sub, boot_sub, permutations = 99)
    boot_r[b]   <- prot$t0
    
    pro     <- procrustes(ref_sub, boot_sub)
    rotated <- pro$Yrot
    for(j in 1:k){
      boot_axis_r[b, j] <- cor(ref_sub[, j], rotated[, j],
                               method = "spearman")
    }
  }
  
  cat("Society bootstrap r:  mean =", round(mean(boot_r), 4),
      " 95% CI [", round(quantile(boot_r, 0.025), 4),
      ",", round(quantile(boot_r, 0.975), 4), "]\n")
  axis_stability_soc <- colMeans(boot_axis_r, na.rm = TRUE)
  names(axis_stability_soc) <- paste0("PCo", 1:n_axes)
  print(round(axis_stability_soc, 3))
  
  
  ## Variables ####
  
  set.seed(123)
  n_boot      <- 500
  n_axes      <- 10
  ref_coords  <- pcoa_result$vectors[, 1:n_axes]
  vars_gender <- whyte_vars$var_id_whyte
  boot_r      <- numeric(n_boot)
  boot_axis_r <- matrix(NA, nrow = n_boot, ncol = n_axes)
  prop        <- 0.80
  
  for(b in 1:n_boot){
    
    boot_vars <- sample(vars_gender, size = floor(length(vars_gender) * prop), replace = FALSE)
    mat_boot  <- mat_ord_fixed[, boot_vars, drop = FALSE]
    
    gd_boot <- daisy(mat_boot,
                     metric = "gower",
                     type   = list(ordratio = 1:ncol(mat_boot)))
    
    pc_boot <- pcoa(gd_boot)
    k       <- min(n_axes, sum(pc_boot$values$Eigenvalues > 0))
    
    prot        <- protest(ref_coords[, 1:k],
                           pc_boot$vectors[, 1:k, drop = FALSE],
                           permutations = 99)
    boot_r[b]   <- prot$t0
    
    pro     <- procrustes(ref_coords[, 1:k],
                          pc_boot$vectors[, 1:k, drop = FALSE])
    rotated <- pro$Yrot
    for(j in 1:k){
      boot_axis_r[b, j] <- cor(ref_coords[, j], rotated[, j],
                               method = "spearman")
    }
  }
  
  cat("Variable bootstrap r:  mean =", round(mean(boot_r), 4),
      " 95% CI [", round(quantile(boot_r, 0.025), 4),
      ",", round(quantile(boot_r, 0.975), 4), "]\n")
  axis_stability_vars <- colMeans(boot_axis_r, na.rm = TRUE)
  names(axis_stability_vars) <- paste0("PCo", 1:n_axes)
  print(round(axis_stability_vars, 3))
}


## Stability depending on the number of variables ####

if (robustness_check){
  props <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95)
  n_boot <- 200  # per proportion
  n_axes <- 1    # focus on axis 1 only
  
  ref_coords <- pcoa_result$vectors[, 1, drop = FALSE]
  
  
  stability_by_prop <- map_dfr(props, function(prop) {
    
    axis_r <- numeric(n_boot)
    
    for (b in 1:n_boot) {
      boot_vars <- sample(vars_gender, 
                          size    = max(2, floor(length(vars_gender) * prop)), 
                          replace = FALSE)
      mat_boot  <- mat_ord_fixed[, boot_vars, drop = FALSE]
      
      # Skip if any society has all NAs across selected variables
      all_na_rows <- apply(mat_boot, 1, function(x) all(is.na(x)))
      if (any(all_na_rows)) {
        axis_r[b] <- NA
        next
      }
      
      gd_boot <- tryCatch(
        daisy(mat_boot, metric = "gower",
              type = list(ordratio = 1:ncol(mat_boot))),
        error = function(e) NULL
      )
      if (is.null(gd_boot)) { axis_r[b] <- NA; next }
      
      # Skip if dissimilarity matrix has infinite or NA values
      if (any(!is.finite(as.matrix(gd_boot)))) { axis_r[b] <- NA; next }
      
      pc_boot <- tryCatch(
        pcoa(gd_boot),
        error = function(e) NULL
      )
      if (is.null(pc_boot)) { axis_r[b] <- NA; next }
      
      if (sum(pc_boot$values$Eigenvalues > 0) >= 1) {
        pro <- tryCatch(
          procrustes(ref_coords, pc_boot$vectors[, 1, drop = FALSE]),
          error = function(e) NULL
        )
        if (!is.null(pro)) {
          axis_r[b] <- cor(ref_coords[, 1], pro$Yrot[, 1], method = "spearman")
        } else {
          axis_r[b] <- NA
        }
      }
    }
    
    data.frame(
      prop     = prop,
      n_vars   = floor(length(vars_gender) * prop),
      mean_r   = mean(axis_r,            na.rm = TRUE),
      ci_low   = quantile(axis_r, 0.025, na.rm = TRUE),
      ci_high  = quantile(axis_r, 0.975, na.rm = TRUE),
      n_valid  = sum(!is.na(axis_r))  # how many bootstraps succeeded
    )
  })
  
  
  ggplot(stability_by_prop, aes(x = n_vars, y = mean_r)) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "grey80") +
    geom_line(color = "steelblue", linewidth = 0.8) +
    geom_point(color = "steelblue", size = 2) +
    geom_hline(yintercept = 0.9, linetype = "dashed", color = "grey40") +
    labs(x        = "Number of variables",
         y        = "Axis 1 stability (Spearman r with full ordination)",
         title    = "How many variables needed to recover axis 1?",
         subtitle = "Dashed line = r = 0.90 stability threshold") +
    theme_minimal(base_size = 11)
}


# Rather than looking for a plateau, ask: at what n do you reliably (say, 80% of the time) get r ≥ 0.8?

if (robustness_check){
  all_axis_r <- list()
  
  stability_by_prop <- map_dfr(seq_along(props), function(pi) {
    prop   <- props[pi]
    axis_r <- numeric(n_boot)
    
    for (b in 1:n_boot) {
      boot_vars <- sample(vars_gender, 
                          size    = max(2, floor(length(vars_gender) * prop)), 
                          replace = FALSE)
      mat_boot  <- mat_ord_fixed[, boot_vars, drop = FALSE]
      
      # Skip if any society has all NAs across selected variables
      all_na_rows <- apply(mat_boot, 1, function(x) all(is.na(x)))
      if (any(all_na_rows)) {
        axis_r[b] <- NA
        next
      }
      
      gd_boot <- tryCatch(
        daisy(mat_boot, metric = "gower",
              type = list(ordratio = 1:ncol(mat_boot))),
        error = function(e) NULL
      )
      if (is.null(gd_boot)) { axis_r[b] <- NA; next }
      
      # Skip if dissimilarity matrix has infinite or NA values
      if (any(!is.finite(as.matrix(gd_boot)))) { axis_r[b] <- NA; next }
      
      pc_boot <- tryCatch(
        pcoa(gd_boot),
        error = function(e) NULL
      )
      if (is.null(pc_boot)) { axis_r[b] <- NA; next }
      
      if (sum(pc_boot$values$Eigenvalues > 0) >= 1) {
        pro <- tryCatch(
          procrustes(ref_coords, pc_boot$vectors[, 1, drop = FALSE]),
          error = function(e) NULL
        )
        if (!is.null(pro)) {
          axis_r[b] <- cor(ref_coords[, 1], pro$Yrot[, 1], method = "spearman")
        } else {
          axis_r[b] <- NA
        }
      }
    }
    
    all_axis_r[[pi]] <<- axis_r  # store raw values
    
    data.frame(
      prop        = prop,
      n_vars      = floor(length(vars_gender) * prop),
      mean_r      = mean(axis_r,            na.rm = TRUE),
      ci_low      = quantile(axis_r, 0.025, na.rm = TRUE),
      ci_high     = quantile(axis_r, 0.975, na.rm = TRUE),
      pct_above08 = mean(axis_r >= 0.8,     na.rm = TRUE),
      n_valid     = sum(!is.na(axis_r))
    )
  })
  
  stability_by_prop %>% 
    ggplot(aes(x = n_vars, y = pct_above08)) +
    geom_point()
  
}



