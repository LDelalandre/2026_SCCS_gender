# is the signal real? (vs random)

source("R/02b_pcoa_core.R")  # provides: pcoa_result, mat_ord_fixed, ordinal_cols, binary_cols

library(cluster)
library(ape)

n_perm <- 999

# 1. Permutation loop ####
# Shuffles variable values independently, preserving NAs in place
# Stores variance explained (10 axes) and loadings on PCo1 for each permutation

perm_variance <- matrix(NA, nrow = n_perm, ncol = 10)

# One matrix per axis, same structure as before, stored in a named list
n_axes <- 5
perm_loadings_list <- lapply(1:n_axes, function(x) {
  m <- matrix(NA, nrow = n_perm, ncol = ncol(mat_ord_fixed))
  colnames(m) <- colnames(mat_ord_fixed)
  m
})
names(perm_loadings_list) <- paste0("ax", 1:n_axes)

for (i in 1:n_perm) {
  
  mat_perm <- mat_ord_fixed %>%
    mutate(across(everything(), function(col) {
      non_na <- which(!is.na(col))
      col[non_na] <- sample(col[non_na])
      col
    }))
  
  gower_perm <- cluster::daisy(mat_perm,
                               metric = "gower",
                               type = list(ordratio = ordinal_cols, symm = binary_cols))
  
  pcoa_perm <- ape::pcoa(gower_perm)
  
  # Variance explained
  eigenvalues    <- pcoa_perm$values$Eigenvalues
  pos_eigen      <- eigenvalues[eigenvalues > 0]
  var_explained  <- pos_eigen / sum(pos_eigen)
  perm_variance[i, ] <- var_explained[1:10]
  
  # Loadings on PCo1 to PCo5
  for (ax in 1:n_axes) {
    site_scores_perm <- pcoa_perm$vectors[, ax]
    perm_loadings_list[[ax]][i, ] <- sapply(colnames(mat_ord_fixed), function(v) {
      vals <- as.numeric(mat_perm[[v]])
      cor(site_scores_perm, vals, method = "spearman", use = "complete.obs")
    })
  }
  
  if (i %% 100 == 0) cat("Permutation", i, "of", n_perm, "\n")
}

# If you want them as separate named objects (perm_loadings_ax1 ... ax5) instead of a list:
list2env(setNames(perm_loadings_list, paste0("perm_loadings_", names(perm_loadings_list))), envir = .GlobalEnv)


# 2. Variance explained ####

obs_eigenvalues  <- pcoa_result$values$Eigenvalues
obs_pos          <- obs_eigenvalues[obs_eigenvalues > 0]
obs_variance     <- obs_pos / sum(obs_pos)

p_values <- sapply(1:10, function(ax) mean(perm_variance[, ax] >= obs_variance[ax]))

results_eigenvalues <- data.frame(
  axis      = paste0("PCo", 1:10),
  observed  = round(obs_variance[1:10] * 100, 1),
  perm_mean = round(colMeans(perm_variance) * 100, 1),
  perm_95   = round(apply(perm_variance, 2, quantile, 0.95) * 100, 1),
  p_value   = round(p_values, 3)
)
print(results_eigenvalues)

perm_long <- as.data.frame(perm_variance) %>%
  setNames(paste0("PCo", 1:10)) %>%
  pivot_longer(everything(), names_to = "axis", values_to = "variance") %>%
  mutate(axis = factor(axis, levels = paste0("PCo", 1:10)))

write.csv(perm_long, "output/02c_perm_long.csv", row.names=F)

obs_df <- data.frame(
  axis     = factor(paste0("PCo", 1:10), levels = paste0("PCo", 1:10)),
  observed = obs_variance[1:10] * 100
)

appendix_pcoa_permutation_variance <- ggplot(perm_long, aes(x = variance * 100)) +
  geom_histogram(fill = "grey70", color = "white", bins = 40) +
  geom_vline(data = obs_df, aes(xintercept = observed), color = "red", linewidth = 0.8) +
  facet_wrap(~ axis, scales = "free_x") +
  labs(x        = "Variance explained (%)",
       y        = "Count",
       title    = "Permutation test on PCoA eigenvalue structure",
       subtitle = "Red line = observed; grey = null distribution (999 permutations)") +
  theme_minimal(base_size = 11)

ggsave("draft/fig/appendix_pcoa_permutation_variance.png", appendix_pcoa_permutation_variance)



# 3. Variable loadings on PCo1 ####

n_axes <- 5
obs_loadings_list <- lapply(1:n_axes, function(ax) {
  obs_site_scores <- pcoa_result$vectors[, ax]
  sapply(colnames(mat_ord_fixed), function(v) {
    vals <- as.numeric(mat_ord_fixed[[v]])
    cor(obs_site_scores, vals, method = "spearman", use = "complete.obs")
  })
})
names(obs_loadings_list) <- paste0("ax", 1:n_axes)

# If you want separate named objects obs_loadings_ax1 ... ax5:
list2env(setNames(obs_loadings_list, paste0("obs_loadings_", names(obs_loadings_list))), envir = .GlobalEnv)
perm_loadings_ax1_long <- as.data.frame(perm_loadings_ax1) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "loading")

obs_loadings_df <- data.frame(
  variable = names(obs_loadings_ax1),
  observed = obs_loadings_ax1
)

# NB: SOME VARIABLES HAVE BEEN RECODED IN DIRECTION SINCE (for the figures)
# Maybe they should be here too??
pcoa_permutation_loading_ax1 <- ggplot() +
  geom_histogram(data = df_perm_mean_loadings, aes(x = Ax1),
                 fill = "grey70", color = "white", bins = 40) +
  geom_vline(data = obs_loadings_df, aes(xintercept = observed),
             color = "red", linewidth = 0.6) +
  facet_wrap(~ variable, scales = "free_x") +
  labs(x        = "Loading on PCo1 (Spearman r)",
       y        = "Count",
       title    = "Permuted vs observed loadings on PCo1",
       subtitle = "Red line = observed; grey = null distribution (999 permutations)") +
  theme_minimal(base_size = 9)


# 4. Summary statistics on loadings ####

## 4a. Mean loading across all variables ####
obs_mean_loading   <- mean(obs_loadings_ax1, na.rm = TRUE)


perm_mean_loadings_ax1 <- rowMeans(perm_loadings_ax1, na.rm = TRUE)
perm_mean_loadings_ax2 <- rowMeans(perm_loadings_ax2, na.rm = TRUE)
perm_mean_loadings_ax3 <- rowMeans(perm_loadings_ax3, na.rm = TRUE)
perm_mean_loadings_ax4 <- rowMeans(perm_loadings_ax4, na.rm = TRUE)
perm_mean_loadings_ax5 <- rowMeans(perm_loadings_ax5, na.rm = TRUE)

p_mean_abs             <- mean(abs(perm_mean_loadings_ax1) >= abs(obs_mean_loading))
p_mean             <- mean(perm_mean_loadings_ax1 >= obs_mean_loading)

cat("Observed mean loading on PCo1:", round(obs_mean_loading, 3), "\n")
cat("Permutation p-value:", round(p_mean, 3), "\n")

df_perm_mean_loadings <- data.frame(Ax1 = perm_mean_loadings_ax1,
                                    Ax2 = perm_mean_loadings_ax2,
                                    Ax3 = perm_mean_loadings_ax3,
                                    Ax4 = perm_mean_loadings_ax4,
                                    Ax5 = perm_mean_loadings_ax5
                                    )
write.csv(df_perm_mean_loadings, "output/02C_df_perm_mean_loadings.csv")

# Observed mean loading per axis (from obs_loadings_list created earlier)
obs_mean_loading_df <- tibble(
  axis = paste0("Ax", 1:5),
  obs_mean_loading = sapply(obs_loadings_list, mean, na.rm = TRUE)
)

appendix_mean_loading <- df_perm_mean_loadings %>% 
  pivot_longer(cols = everything(), 
               names_to = "axis", 
               values_to = "perm_loading") %>% 
  ggplot(aes(x = perm_loading)) +
  geom_histogram(fill = "grey70", color = "white", bins = 40) +
  geom_vline(data = obs_mean_loading_df,
             aes(xintercept = obs_mean_loading),
             color = "red", linewidth = 0.8) +
  labs(x = "Mean variable loading",
       y = "Count") +
  theme_classic(base_size = 11) +
  facet_wrap(~axis)

appendix_mean_loading
ggsave("draft/fig/appendix_mean_loading.png", appendix_mean_loading)

# pvalue for other axes 
length(which(perm_mean_loadings_ax3 < obs_mean_loading_df %>% 
               filter(axis == "Ax3") %>% pull(obs_mean_loading)))/length(perm_mean_loadings_ax3)

length(which(perm_mean_loadings_ax5 > obs_mean_loading_df %>% 
               filter(axis == "Ax5") %>% pull(obs_mean_loading)))/length(perm_mean_loadings_ax3)

## 4b. Number of variables with positive loading ####
obs_n_positive  <- sum(obs_loadings_ax1 > 0, na.rm = TRUE)
obs_n_total     <- sum(!is.na(obs_loadings_ax1))
perm_n_positive <- rowSums(perm_loadings_ax1 > 0, na.rm = TRUE)
p_positive      <- mean(perm_n_positive >= obs_n_positive)

cat("Observed positive loadings:", obs_n_positive, "/", obs_n_total, "\n")
cat("Permutation p-value:", round(p_positive, 3), "\n")

## 4.c significant loadings for individual variables ####

# One-sided p-value for each variable
# "Is the observed loading more extreme than the null distribution?"

p_values_loadings <- sapply(colnames(mat_ord_fixed), function(v) {
  obs  <- obs_loadings_ax1[v]
  null <- perm_loadings_ax1[, v]
  if (obs >= 0) {
    mean(null >= obs)   # one-sided: how often does null exceed observed?
  } else {
    mean(null <= obs)   # one-sided: how often does null go as negative?
  }
})

whyte_vars_signif_loading <- p_values_loadings %>% 
  as.data.frame() %>% 
  rename(p_value = ".") %>% 
  rownames_to_column("var_id") %>% 
  arrange(p_value) %>% 
  left_join(whyte_vars)

signif_vars <- p_values_loadings[p_values_loadings < 0.05] %>% names()
p_values_loadings[p_values_loadings < 0.05/51]

# Benjamini-Hochberg: controls false discovery rate rather than family-wise error
p_adjusted <- p.adjust(p_values_loadings, method = "BH")
signif_vars_adjusted <- p_adjusted[p_adjusted < 0.05] %>% names()

whyte_vars %>% 
  filter(var_id %in% signif_vars) %>% pull(title)
whyte_vars %>% 
  filter(var_id %in% signif_vars_adjusted) %>% pull(title)

p_values_loadings_sorted <- p_values_loadings %>% 
  sort()
nvar_significant <- p_values_loadings_sorted[1:12] %>%  # 12 to cross the 90% threshold
  names()



whyte_category_angarika %>% 
  filter(var_id %in% nvar_significant) %>% 
  select(var_id, title, category_Angarika) %>% 
  arrange(category_Angarika)
