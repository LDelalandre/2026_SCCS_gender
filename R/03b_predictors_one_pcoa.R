
run_one_pcoa <- function(var_ids, label,
                              correction = "lingoes",
                              min_observed = 0.50) {
  
  mat <- predictors_wide[, var_ids, drop = FALSE]
  
  observed_prop <- colMeans(!is.na(mat))
  mat <- mat[, observed_prop >= min_observed, drop = FALSE]
  
  if (ncol(mat) < 2) {
    message(label, ": too few variables after missing-data filter")
    return(NULL)
  }
  
  ordinal_idx <- seq_len(ncol(mat))
  
  gd <- cluster::daisy(
    mat,
    metric = "gower",
    type = list(ordratio = ordinal_idx)
  )
  
  pc <- ape::pcoa(gd, correction = correction)
  
  eigenvalues <- pc$values$Eigenvalues
  positive_eigenvalues <- eigenvalues[eigenvalues > 0]
  
  var_exp_positive <- positive_eigenvalues /
    sum(positive_eigenvalues)
  
  list(
    pcoa = pc,
    distance = gd,
    var_exp = var_exp_positive,
    label = label,
    vars = colnames(mat),
    societies = rownames(mat),
    observed_prop = observed_prop
  )
}



# One PCoA on all predictors ####

predictors_detailed <- read.csv("output/predictors_detailed.csv")

predictors_detailed_nomethod <- predictors_detailed %>% 
  filter(!category=="method") 

pcoa_all_pred <- run_one_pcoa(predictors_detailed_nomethod$var_id, "all variables together")

cumvar <- cumsum(pcoa_all_pred$var_exp)
round(data.frame(
  axis    = seq_along(pcoa_all_pred$var_exp),
  var_exp = round(pcoa_all_pred$var_exp, 3),
  cumvar  = round(cumvar, 3)
), 3)


df <- data.frame(
  axis    = seq_along(pcoa_all_pred$var_exp),
  var_exp = pcoa_all_pred$var_exp
)

ggplot(df, aes(x = axis, y = var_exp)) +
  geom_line(color = "grey50") +
  geom_point(size = 2.5, color = "#2166ac") +
  geom_hline(yintercept = 0.1, linetype = "dashed", 
             color = "grey60", linewidth = 0.4) +
  scale_x_continuous(breaks = df$axis) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "All variables", x = "Axis", y = "Variance explained") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))


naxes <- 4
soc_ids    <- rownames(pcoa_all_pred$pcoa$vectors)
axis_names <- paste0("Axis.", 1:naxes)

coords_all_pred <- pcoa_all_pred$pcoa$vectors %>%
  as.data.frame() %>%
  dplyr::select(all_of(axis_names)) %>%
  tibble::rownames_to_column("soc_id") %>% 
  left_join(societies %>% dplyr::select(soc_id, pref_name_for_society, 
                                        origLat, origLong),
            by = "soc_id") %>% 
  rename(pred1 = Axis.1, "pred2" = "Axis.2", "pred3" = "Axis.3", "pred4" = "Axis.4")

used_vars <- pcoa_all_pred$vars

loadings_all_predictors <- predictors_data %>%
  filter(var_id %in% used_vars) %>%
  filter(soc_id %in% pcoa_all_pred$societies) %>%
  left_join(
    predictors_detailed_nomethod %>%
      select(var_id, title),
    by = "var_id"
  ) %>%
  left_join(coords_all_pred, by = "soc_id") %>%
  group_by(var_id, title) %>%
  summarise(
    across(
      all_of(c("pred1", "pred2", "pred3", "pred4")),
      ~ cor(.x, code, method = "spearman",
            use = "pairwise.complete.obs")
    ),
    .groups = "drop"
  )

# Model ####

pred_resp_one_pcoa <- 
  coords_all_pred %>% 
  left_join(coords_df)


mod <- lm(Ax1 ~ pred1 + pred2 + pred3 + pred4, data = pred_resp_one_pcoa)
summary(mod)


