library(vegan)
library(cluster)  # for daisy/gower
library(tidyverse)

# ============================================================
# 1. Recompute distance matrix
# (use same method as your original PCoA)
# ============================================================

dist_gower <- gower_ordinal

# ============================================================
# 2. Prepare predictor matrix
# ============================================================

predictors <- pred_resp %>%
  select(complexity, agriculture_cereals, agriculture_importance,
         herding_importance, herding_milk, hunting,
         warfare_intensity, warfare_location, matri) %>%
  as.data.frame()

# db-RDA requires complete cases in predictors
# identify which rows have complete predictor data
complete_idx <- complete.cases(predictors)
cat("Complete cases:", sum(complete_idx), "out of", nrow(predictors), "\n")

# Subset distance matrix and predictors to complete cases
dist_complete <- as.dist(as.matrix(dist_gower)[complete_idx, complete_idx])
pred_complete  <- predictors[complete_idx, ]

# ============================================================
# 3. Full db-RDA
# ============================================================

dbrda_full <- capscale(
  dist_complete ~ 
    complexity + agriculture_cereals + agriculture_importance +
    herding_importance + herding_milk + hunting +
    warfare_intensity + warfare_location + matri,
  data = pred_complete,
  sqrt.dist = TRUE  # recommended for Gower to avoid negative eigenvalues
)

# Summary
summary(dbrda_full)

# Overall model significance
anova(dbrda_full, permutations = 9999)

# Each predictor controlled for all others (your key test)
anova_margin <- anova(dbrda_full, by = "margin", permutations = 9999)
print(anova_margin)

# Sequential (order matters - put theory-driven predictors first)
anova_term <- anova(dbrda_full, by = "term", permutations = 9999)
print(anova_term)

# Adjusted R2 for full model
RsquareAdj(dbrda_full)

# ============================================================
# 4. Variation partitioning: complexity vs matri
# your core independence question
# ============================================================

vp <- varpart(dist_complete, 
              ~ complexity, 
              ~ matri,
              data = pred_complete,
              sqrt.dist = TRUE)

plot(vp, 
     Xnames = c("Complexity", "Matrilineality"),
     bg = c("steelblue", "tomato"),
     alpha = 80,
     digits = 3,
     main = "Variation partitioning:\nComplexity vs Matrilineality")

# The Venn diagram shows:
# [a] = variance explained uniquely by complexity
# [b] = shared variance (their collinearity)
# [c] = variance explained uniquely by matrilineality
# [d] = unexplained

# Test each fraction
# Unique effect of complexity (controlling for matri)
rda_complexity <- capscale(dist_complete ~ complexity + 
                             Condition(matri),
                           data = pred_complete,
                           sqrt.dist = TRUE)
anova(rda_complexity, permutations = 9999)

# Unique effect of matri (controlling for complexity)
rda_matri <- capscale(dist_complete ~ matri + 
                        Condition(complexity),
                      data = pred_complete,
                      sqrt.dist = TRUE)
anova(rda_matri, permutations = 9999)



# Part 2 ####

# Run for all predictors including complexity and hunting
# even non-significant ones, for comparison

predictors_to_profile <- c("complexity", 
                           "agriculture_cereals",
                           "herding_importance",
                           "hunting",
                           "matri")

# For each, control for all others
profiles_all <- map(predictors_to_profile, function(pred) {
  others <- setdiff(predictors_to_profile, pred)
  condition_str <- paste(others, collapse = " + ")
  formula_str <- paste0("dist_complete ~ ", pred,
                        " + Condition(", condition_str, ")")
  
  dbrda_focal <- capscale(as.formula(formula_str),
                          data = pred_complete,
                          sqrt.dist = TRUE)
  
  cap_scores <- scores(dbrda_focal, display = "sites", choices = 1)
  
  cors <- cor(mat_filtered2[complete_idx, ],
              cap_scores,
              use = "pairwise.complete.obs")[, 1]
  
  cors
})

names(profiles_all) <- predictors_to_profile

# Build wide dataframe: rows = variables, columns = predictors
# values = correlation score
scores_wide <- profiles_all %>%
  as.data.frame() %>%
  rownames_to_column("var_id_whyte") %>%
  left_join(whyte_vars, by = "var_id_whyte") %>%
  select(var_id_whyte, title, 
         complexity, agriculture_cereals, 
         herding_importance, hunting, matri) %>%
  # add rank columns for each predictor
  mutate(
    rank_complexity        = rank(-abs(complexity)),
    rank_agriculture       = rank(-abs(agriculture_cereals)),
    rank_herding           = rank(-abs(herding_importance)),
    rank_hunting           = rank(-abs(hunting)),
    rank_matri             = rank(-abs(matri))
  ) %>%
  arrange(rank_agriculture)  # default sort by agriculture

# Round score columns
scores_wide <- scores_wide %>%
  mutate(across(c(complexity, agriculture_cereals, 
                  herding_importance, hunting, matri), 
                ~ round(.x, 2)))



# PLot ####

scores_wide %>%
  ggplot(aes(x = agriculture_cereals, y = complexity, label = title)) +
  geom_point() +
  geom_text(size = 2.5, hjust = 0, nudge_x = 0.01,
            check_overlap = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  labs(x = "Association with cereal agriculture",
       y = "Association with complexity",
       title = "Gender variables by predictor association") +
  theme_bw() +
  geom_smooth(method = "lm")