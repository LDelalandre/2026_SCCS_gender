library(cluster)
library(ape)

# Shuffle variable values ####

n_perm <- 999

# Store variance explained by each axis for each permutation
perm_variance <- matrix(NA, nrow = n_perm, ncol = 10)

for (i in 1:n_perm) {
  
  # Shuffle each column independently, preserving NAs in place
  mat_perm <- mat_ord_fixed %>%
    mutate(across(everything(), function(col) {
      non_na <- which(!is.na(col))
      col[non_na] <- sample(col[non_na])
      col
    }))
  
  # Recompute Gower dissimilarity
  gower_perm <- cluster::daisy(mat_perm,
                               metric = "gower",
                               type = list(
                                 ordratio = ordinal_cols,
                                 symm    = binary_cols
                               ))
  
  # PCoA
  pcoa_perm <- ape::pcoa(gower_perm)
  
  # Store variance explained (positive eigenvalues only)
  eigenvalues   <- pcoa_perm$values$Eigenvalues
  pos_eigen     <- eigenvalues[eigenvalues > 0]
  var_explained <- pos_eigen / sum(pos_eigen)
  
  perm_variance[i, ] <- var_explained[1:10]
  
  if (i %% 100 == 0) cat("Permutation", i, "of", n_perm, "\n")
}

# Observed variance explained
obs_eigenvalues   <- pcoa_result$values$Eigenvalues
obs_pos           <- obs_eigenvalues[obs_eigenvalues > 0]
obs_variance      <- obs_pos / sum(obs_pos)

# P-value for each axis: proportion of permutations exceeding observed
p_values <- sapply(1:10, function(ax) {
  mean(perm_variance[, ax] >= obs_variance[ax])
})

# Summary table
results <- data.frame(
  axis          = paste0("PCo", 1:10),
  observed      = round(obs_variance[1:10] * 100, 1),
  perm_mean     = round(colMeans(perm_variance) * 100, 1),
  perm_95       = round(apply(perm_variance, 2, quantile, 0.95) * 100, 1),
  p_value       = round(p_values, 3)
)

print(results)

# Visualise
perm_long <- as.data.frame(perm_variance) %>%
  setNames(paste0("PCo", 1:10)) %>%
  pivot_longer(everything(), names_to = "axis", values_to = "variance") %>%
  mutate(axis = factor(axis, levels = paste0("PCo", 1:10)))

obs_df <- data.frame(
  axis     = factor(paste0("PCo", 1:10), levels = paste0("PCo", 1:10)),
  observed = obs_variance[1:10] * 100
)

ggplot(perm_long, aes(x = variance * 100)) +
  geom_histogram(fill = "grey70", color = "white", bins = 40) +
  geom_vline(data = obs_df, aes(xintercept = observed),
             color = "red", linewidth = 0.8) +
  facet_wrap(~ axis, scales = "free_x") +
  labs(x    = "Variance explained (%)",
       y    = "Count",
       title    = "Permutation test on PCoA eigenvalue structure",
       subtitle = "Red line = observed; grey = null distribution (999 permutations)") +
  theme_minimal(base_size = 11)


# See the loadings ####

# Store loadings of all variables on axis 1 for each permutation
perm_loadings <- matrix(NA, nrow = n_perm, ncol = ncol(mat_ord_fixed))
colnames(perm_loadings) <- colnames(mat_ord_fixed)

for (i in 1:n_perm) {
  
  mat_perm <- mat_ord_fixed %>%
    mutate(across(everything(), function(col) {
      non_na <- which(!is.na(col))
      col[non_na] <- sample(col[non_na])
      col
    }))
  
  gower_perm <- cluster::daisy(mat_perm,
                               metric = "gower",
                               type = list(
                                 ordratio = ordinal_cols,
                                 symm     = binary_cols
                               ))
  
  pcoa_perm <- ape::pcoa(gower_perm)
  
  # Get site scores on axis 1
  site_scores_perm <- pcoa_perm$vectors[, 1]
  
  # Correlate each variable with axis 1
  perm_loadings[i, ] <- sapply(colnames(mat_ord_fixed), function(v) {
    vals <- as.numeric(mat_perm[[v]])
    cor(site_scores_perm, vals, method = "spearman", use = "complete.obs")
  })
  
  if (i %% 100 == 0) cat("Permutation", i, "of", n_perm, "\n")
}

# Observed loadings on axis 1
obs_site_scores <- pcoa_result$vectors[, 1]
obs_loadings_ax1 <- sapply(colnames(mat_ord_fixed), function(v) {
  vals <- as.numeric(mat_ord_fixed[[v]])
  cor(obs_site_scores, vals, method = "spearman", use = "complete.obs")
})

# Visualise distribution of permuted loadings vs observed
perm_loadings_long <- as.data.frame(perm_loadings) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "loading")

obs_loadings_df <- data.frame(
  variable = names(obs_loadings_ax1),
  observed = obs_loadings_ax1
)

ggplot() +
  geom_histogram(data = perm_loadings_long,
                 aes(x = loading),
                 fill = "grey70", color = "white", bins = 40) +
  geom_vline(data = obs_loadings_df,
             aes(xintercept = observed),
             color = "red", linewidth = 0.6) +
  facet_wrap(~ variable, scales = "free_x") +
  labs(x        = "Loading on axis 1 (Spearman r)",
       y        = "Count",
       title    = "Permuted vs observed loadings on PCo1",
       subtitle = "Red line = observed loading; grey = null distribution") +
  theme_minimal(base_size = 9)


## Stats on this ####

### 1. Compare the mean loading to the null distribution ####

# Observed mean loading across all variables on axis 1
obs_mean_loading <- mean(obs_loadings_ax1, na.rm = TRUE)

# Null distribution of mean loadings
perm_mean_loadings <- rowMeans(perm_loadings, na.rm = TRUE)

# P-value: how often does the null produce a mean as extreme as observed?
p_mean <- mean(abs(perm_mean_loadings) >= abs(obs_mean_loading))

cat("Observed mean loading on axis 1:", round(obs_mean_loading, 3), "\n")
cat("Permutation p-value:", round(p_mean, 3), "\n")

# Visualise
ggplot(data.frame(x = perm_mean_loadings), aes(x = x)) +
  geom_histogram(fill = "grey70", color = "white", bins = 40) +
  geom_vline(xintercept = obs_mean_loading, color = "red", linewidth = 0.8) +
  labs(x     = "Mean loading across all variables on PCo1",
       y     = "Count",
       title = "Is the positive shift in axis 1 loadings beyond chance?",
       subtitle = paste0("Observed mean = ", round(obs_mean_loading, 3),
                         " — permutation p = ", round(p_mean, 3))) +
  theme_minimal(base_size = 11)

### 2. Count how many variables load positively ####

# Observed: number of variables with positive loading
obs_n_positive <- sum(obs_loadings_ax1 > 0, na.rm = TRUE)
obs_n_total    <- sum(!is.na(obs_loadings_ax1))

# Null distribution: number of positive loadings per permutation
perm_n_positive <- rowSums(perm_loadings > 0, na.rm = TRUE)

p_positive <- mean(perm_n_positive >= obs_n_positive)

cat("Observed positive loadings:", obs_n_positive, "/", obs_n_total, "\n")
cat("Permutation p-value:", round(p_positive, 3), "\n") # never do permutations generate as many positive loadings as observed on axis 1


