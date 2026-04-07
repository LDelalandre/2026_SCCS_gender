source("R/01_import.R")
source("R/functions.R")


library(cluster)    # for daisy/Gower
library(ape)        # for pcoa
library(vegan) # for robustness

# pcoa ####
## Matrix on response variables ####

whyte_vars_to_remove <- c("DV34") # locality

whyte_vars <- gender_response %>% 
  filter(column_number_whyte78 %in% rep(1:52)) %>% 
  select(var_id, title,var_id_whyte, column_number_whyte78, domain, domain_name) %>%
  mutate(short_title = paste0(var_id, "_", str_trunc(title, width = 30, ellipsis = "..."))) %>% 
  mutate(title = if_else(var_id == "SCCS625", paste("(No)", title), title)) %>%  # high = no high value on men being agressive
  filter(!var_id_whyte %in% whyte_vars_to_remove) # Remove locality
  
whyte_codes <- get_codes(whyte_vars$title)

whyte_data_wide <- sccs %>% 
  filter(var_id %in% whyte_vars$var_id) %>% 
  # If multiple sub_cases per society/variable, take one (e.g. most recent)
  group_by(soc_id, var_id) %>%
  slice_max(year, na_rm = TRUE, n = 1) %>%
  ungroup() %>%
  select(soc_id, var_id, code) %>%
  pivot_wider(names_from = var_id, values_from = code) %>% 
  filter(!if_all(-soc_id, is.na))

mat <- whyte_data_wide %>%
  select(-soc_id) %>%       # remove the id column
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

n_axes <- 10
ref_coords <- pcoa_result$vectors[, 1:n_axes]

## Variables: subsampling ####

vars_gender <- whyte_vars$var_id_whyte 

n_boot       <- 500
boot_m2      <- numeric(n_boot)
boot_r       <- numeric(n_boot)
boot_axis_r  <- matrix(NA, nrow = n_boot, ncol = n_axes)


prop <- 0.70   # adjust — try 0.70 and 0.80 to see sensitivity

set.seed(123)
prop <- 0.80

for(b in 1:n_boot){
  boot_vars <- sample(vars_gender, size = floor(length(vars_gender) * prop), replace = FALSE)
  mat_boot  <- mat_filtered2[, boot_vars, drop = FALSE]
  
  gd_boot <- daisy(mat_boot,
                   metric = "gower",
                   type   = list(ordratio = 1:ncol(mat_boot)))
  
  pc_boot <- pcoa(gd_boot)
  k       <- min(n_axes, sum(pc_boot$values$Eigenvalues > 0))
  
  pro     <- procrustes(ref_coords[, 1:k],
                        pc_boot$vectors[, 1:k, drop = FALSE])
  boot_m2[b] <- pro$ss
  boot_m2[b] <- 1 - prot$t0^2   # always bounded [0,1], sample-size independent
  
  prot       <- protest(ref_coords[, 1:k],
                        pc_boot$vectors[, 1:k, drop = FALSE],
                        permutations = 99)
  boot_r[b]  <- prot$t0
  
  rotated <- pro$Yrot
  for(j in 1:k){
    boot_axis_r[b, j] <- cor(ref_coords[, j], rotated[, j],
                             method = "spearman")
  }
}

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
n_soc       <- nrow(mat_filtered2)
ref_coords  <- pcoa_result$vectors[, 1:n_axes]
boot_r      <- numeric(n_boot)
boot_axis_r <- matrix(NA, nrow = n_boot, ncol = n_axes)
prop        <- 0.80

for(b in 1:n_boot){
  
  idx      <- sample(1:n_soc, size = floor(n_soc * prop), replace = FALSE)
  mat_boot <- mat_filtered2[idx, ]
  
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
  mat_boot  <- mat_filtered2[, boot_vars, drop = FALSE]
  
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
