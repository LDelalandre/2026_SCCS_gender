mat <- predictors_wide

# drop variables that are entirely NA
mat <- mat[, colSums(!is.na(mat)) > nrow(mat) * 0.5]

if(ncol(mat) < 2) {
  message(label, ": too few variables after NA filter")
  return(NULL)
}

gd <- daisy(mat, metric = "gower",
            type = list(ordratio = 1:ncol(mat)))

pc <- pcoa(gd)

eigenvalues <- pc$values$Eigenvalues
pos_eigen   <- eigenvalues[eigenvalues > 0]
var_exp     <- pos_eigen / sum(pos_eigen)



library(ggplot2)
library(vegan)

# ── 1. Scree plot ──────────────────────────────────────────────────────────────
scree_df <- data.frame(
  axis    = 1:length(var_exp),
  var_exp = var_exp * 100
)

ggplot(scree_df[1:min(15, nrow(scree_df)), ],
       aes(x = axis, y = var_exp)) +
  geom_col(fill = "steelblue", width = 0.6) +
  geom_line(colour = "grey40") +
  geom_point(colour = "grey40") +
  scale_x_continuous(breaks = 1:15) +
  labs(x = "Axis", y = "Variance explained (%)",
       title = "Scree plot") +
  theme_minimal()

# ── 2. Variable loadings (correlations with axes) ─────────────────────────────
# PCoA does not give loadings directly — standard approach is to
# correlate each variable with the PCoA scores

n_axes_to_show <- 5  # adjust
scores         <- pc$vectors[, 1:n_axes_to_show]

# For ordinal variables, use Spearman
loadings_mat <- matrix(NA,
                       nrow = ncol(mat),
                       ncol = n_axes_to_show,
                       dimnames = list(colnames(mat),
                                       paste0("PCo", 1:n_axes_to_show)))

for(v in colnames(mat)){
  for(j in 1:n_axes_to_show){
    loadings_mat[v, j] <- cor(mat[[v]], scores[, j],
                              method   = "spearman",
                              use      = "pairwise.complete.obs")
  }
}

# ── 3. Plot loadings as heatmap ───────────────────────────────────────────────
loadings_df <- as.data.frame(loadings_mat) %>% 
  rownames_to_column("var_id") %>% 
  left_join(predictors_detailed) 

loadings_long <- tidyr::pivot_longer(loadings_df %>% 
                                       select(-c(var_id, category)),
                                     cols      = -title,
                                     names_to  = "axis",
                                     values_to = "r") %>% 
  left_join(predictors_detailed, by = "title")

ggplot(loadings_long, aes(x = axis, y = title, fill = r)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(r, 2)), size = 3) +
  scale_fill_gradient2(low  = "firebrick", mid = "white", high = "steelblue",
                       midpoint = 0, limits = c(-1, 1),
                       name = "Spearman r") +
  labs(x = NULL, y = NULL, title = "Variable correlations with PCoA axes") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9))


# In your heatmap or a biplot, colour by category
# You already have the category column in loadings_df — use it

ggplot(loadings_long, aes(x = axis, y = title, fill = r)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(r, 2)), size = 3) +
  scale_fill_gradient2(low = "firebrick", mid = "white", high = "steelblue",
                       midpoint = 0, limits = c(-1, 1)) +
  # Add category as y-axis text colour or strip
  facet_grid(category ~ ., scales = "free_y", space = "free_y") +
  theme_minimal()


# Extract society positions on global PCoA axes ####

# Assign interpretive names based on your loadings
axis_names <- c(
  "PCo1_complexity_intensification",
  "PCo2_hunting_vs_roots",
  "PCo3_male_solidarity",
  "PCo4_kinship_matrilineal",
  "PCo5_secondary"
)

n_axes_extract <- length(axis_names)

global_axes <- pc$vectors[, 1:n_axes_extract] %>%
  as.data.frame() %>%
  setNames(axis_names) %>%
  tibble::rownames_to_column("soc_id")

# ── Variance explained per axis ───────────────────────────────────────────────
var_exp_df <- data.frame(
  axis    = axis_names,
  var_exp = var_exp[1:n_axes_extract] * 100
)


# ── Join with gender PCoA axes for regression ─────────────────────────────────
# Assuming you have gender_axes with soc_id + gender PCo columns

response_axes <- pcoa_result$vectors %>%
  as.data.frame() %>%
  rownames_to_column("soc_id")

analysis_df <- global_axes %>%
  left_join(response_axes, by = "soc_id")

# ── Quick correlation check between predictor axes ───────────────────────────
# Should be near zero since PCoA axes are orthogonal
cor(global_axes[, -1], use = "pairwise.complete.obs") %>%
  round(3)



# Models ####


# Should all be roughly mean 0, sd in similar range
analysis_df %>%
  select(Axis.1, starts_with("PCo")) %>%
  summarise(across(everything(), list(mean = mean, sd = sd),
                   na.rm = TRUE))

# Standardize predictors and response (for interpretability)
analysis_df <- analysis_df %>%
  mutate(across(c(PCo1_complexity_intensification,
                  PCo2_hunting_vs_roots,
                  PCo3_male_solidarity,
                  PCo4_kinship_matrilineal),
                scale)) %>%
  mutate(
    Axis.1_z = as.numeric(scale(Axis.1)),
    Axis.2_z = as.numeric(scale(Axis.2))
  )


library("brms")
m_gender_PCo1 <- brm(
  Axis.1_z ~ 
    PCo1_complexity_intensification +
    PCo2_hunting_vs_roots +
    PCo3_male_solidarity +
    PCo4_kinship_matrilineal,
  
  data   = analysis_df,
  family = gaussian(),
  
  prior = c(
    # Global PCoA axes are standardised (~mean 0, comparable scale)
    # so normal(0, 1) is reasonable — effect rarely exceeds 1 SD
    prior(normal(0, 1), class = b),
    prior(normal(0, 1), class = Intercept),
    prior(exponential(1), class = sigma)
    # No dirichlet needed — dropped mo()
  ),
  
  cores = 4, iter = 4000, seed = 42
)


m_gender_PCo2 <- brm(
  Axis.2 ~ 
    PCo1_complexity_intensification +
    PCo2_hunting_vs_roots +
    PCo3_male_solidarity +
    PCo4_kinship_matrilineal,
  
  data   = analysis_df,
  family = gaussian(),
  
  prior = c(
    # Global PCoA axes are standardised (~mean 0, comparable scale)
    # so normal(0, 1) is reasonable — effect rarely exceeds 1 SD
    prior(normal(0, 1), class = b),
    prior(normal(0, 1), class = Intercept),
    prior(exponential(1), class = sigma)
    # No dirichlet needed — dropped mo()
  ),
  
  cores = 4, iter = 4000, seed = 42
)




summary(m_gender_PCo1)
bayestestR::pd(m_gender_PCo1)


summary(m_gender_PCo2)
bayestestR::pd(m_gender_PCo2)



## Plough ####

 

analysis_df_more_vars <- predictors_wide %>% 
  select(SCCS680, SCCS704, SCCS728) %>% 
  rownames_to_column("soc_id") %>% 
  right_join(analysis_df) %>% 
  mutate(SCCS680_z = as.numeric(scale(SCCS680)),  # plow
         SCCS704_z = as.numeric(scale(SCCS704)),  # private property
         SCCS728_z = as.numeric(scale(SCCS728)),  # animal husbandry
         Axis1_z   = as.numeric(scale(Axis.1)),   # standardise response too
         Axis2_z   = as.numeric(scale(Axis.2)))

# ── Residualise each variable against PCo1 ────────────────────────────────────
residualise <- function(var_z, data) {
  m <- lm(as.formula(paste(var_z, "~ PCo1_complexity_intensification")),
          data = data, na.action = na.exclude)  # na.exclude preserves row positions
  residuals(m)  # returns NA for missing rows automatically
}

analysis_df_more_vars <- analysis_df_more_vars %>%
  mutate(
    plow_residual     = residualise("SCCS680_z", cur_data()),
    property_residual = residualise("SCCS704_z", cur_data()),
    herding_residual  = residualise("SCCS728_z", cur_data())
  ) %>%
  mutate(
    plow_residual_z     = as.numeric(scale(plow_residual)),
    property_residual_z = as.numeric(scale(property_residual)),
    herding_residual_z  = as.numeric(scale(herding_residual))
  )


m_PCo1_additional_pred <- brm(
  Axis1_z ~ PCo1_complexity_intensification +
    plow_residual_z +
    property_residual_z +
    herding_residual_z +
    PCo4_kinship_matrilineal,
  data   = analysis_df_more_vars,
  family = gaussian(),
  prior  = c(prior(normal(0, 1), class = b),
             prior(normal(0, 1), class = Intercept),
             prior(exponential(1), class = sigma)),
  cores  = 4, iter = 4000, seed = 42
)

summary(m_PCo1_additional_pred)
bayestestR::pd(m_PCo1_additional_pred)
