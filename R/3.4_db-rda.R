library(vegan)
library(cluster)  # for daisy/gower
library(tidyverse)

dist_gower <- gower_ordinal

predictors <- pred_resp %>%
  select(complexity, agriculture_cereals, agriculture_importance,
         herding_importance, herding_milk, hunting,
         warfare_intensity, warfare_location, matri) %>%
  as.data.frame()

direction_table <- gender_response_recode %>% 
  select(var_id, women_favorable)

complete_idx <- complete.cases(predictors) # db-RDA requires complete cases in predictors
cat("Complete cases:", sum(complete_idx), "out of", nrow(predictors), "\n")

# Subset distance matrix and predictors to complete cases
dist_complete <- as.dist(as.matrix(dist_gower)[complete_idx, complete_idx])
pred_complete  <- predictors[complete_idx, ]

# Models ####

## Full db-rda ####

dbrda_full <- capscale(
  dist_complete ~ 
    complexity + agriculture_cereals + agriculture_importance +
    herding_importance + herding_milk + hunting +
    warfare_intensity + warfare_location + matri,
  data = pred_complete,
  sqrt.dist = TRUE  # recommended for Gower to avoid negative eigenvalues
)


summary(dbrda_full)
anova(dbrda_full, permutations = 9999)

# Each predictor controlled for all others
anova_margin <- anova(dbrda_full, by = "margin", permutations = 9999)
print(anova_margin)

RsquareAdj(dbrda_full) # Adjusted R2 for full model


## Model selection ####

# Vif
vif.cca(dbrda_full) # No collinearity problem (VIF < 5 everywhere)

m0 <- capscale(dist_complete ~ 1, data = pred_complete, sqrt.dist = TRUE)
m_sel <- ordiR2step(m0, scope = formula(dbrda_full), permutations = 9999)


dbrda_final <- capscale(
  dist_complete ~ complexity + herding_importance + matri,
  data = pred_complete,
  sqrt.dist = TRUE
)

anova(dbrda_final, permutations = 9999)
anova(dbrda_final, by = "margin", permutations = 9999)


## Biplot ####

# Extract scores
site_scores <- as.data.frame(scores(dbrda_final, display = "sites"))
bp_scores   <- as.data.frame(scores(dbrda_final, display = "bp"))  # predictor arrows

# Optional: attach metadata to sites
site_scores$society <- rownames(site_scores)
# site_scores <- left_join(site_scores, your_metadata, by = "society")

# Plot
db_rda_biplot <- ggplot() +
  # societies
  geom_point(data = site_scores,
             aes(x = CAP1, y = CAP2), alpha = 0.6, size = 2) +
  # predictor arrows
  geom_segment(data = bp_scores,
               aes(x = 0, y = 0, xend = CAP1, yend = CAP2),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "firebrick") +
  geom_text(data = bp_scores,
            aes(x = CAP1 * 1.1, y = CAP2 * 1.1,
                label = rownames(bp_scores)),
            color = "firebrick", size = 3.5) +
  theme_bw() +
  labs(title = "db-RDA biplot",
       x = "CAP1", y = "CAP2")

ggsave("output/3.4_db-rda_db_rda_biplot.png", db_rda_biplot)


# Profiles ####

predictors_to_profile <- c("complexity", 
                           "agriculture_cereals",
                           "herding_importance",
                           "hunting",
                           "matri")

# For each predictor, control for all others
profiles_all <- map(predictors_to_profile, function(pred) {
  others <- setdiff(predictors_to_profile, pred)
  condition_str <- paste(others, collapse = " + ")
  formula_str <- paste0("dist_complete ~ ", pred,
                        " + Condition(", condition_str, ")")
  
  dbrda_focal <- capscale(as.formula(formula_str),
                          data = pred_complete,
                          sqrt.dist = TRUE)
  
  cap_scores <- scores(dbrda_focal, display = "sites", choices = 1)
  
  cors <- cor(mat_filtered[complete_idx, ],
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
  left_join(direction_table, by = "var_id") %>%          # <-- join direction table
  select(var_id_whyte, var_id, title,
         complexity, agriculture_cereals,
         herding_importance, hunting, matri,
         women_favorable) %>%                             # keep for multiplication
  mutate(
    across(
      c(complexity, agriculture_cereals,
        herding_importance, hunting, matri),
      ~ . * women_favorable                               # <-- flip sign
    )
  ) %>%
  mutate(
    rank_complexity        = rank(-abs(complexity)),
    rank_agriculture       = rank(-abs(agriculture_cereals)),
    rank_herding           = rank(-abs(herding_importance)),
    rank_hunting           = rank(-abs(hunting)),
    rank_matri             = rank(-abs(matri))
  ) %>%
  select(-women_favorable) %>%                           # <-- drop helper column
  arrange(rank_agriculture)

# Round score columns
scores_wide <- scores_wide %>%
  mutate(across(c(complexity, agriculture_cereals, 
                  herding_importance, hunting, matri), 
                ~ round(.x, 2)))

f_predictor <- "agriculture_cereals"

scores_wide %>% 
  filter(abs(get(f_predictor)) > 0.2) %>% 
  arrange(get(f_predictor)) %>% 
  select(c("var_id_whyte", "title", all_of(f_predictor))) 


# Plots ####


# Using a priori categories ####

# Add your categories to scores_wide
scores_wide <- scores_wide %>%
  left_join(
    whyte_category %>% select(var_id_whyte, category, category2),
    by = "var_id_whyte"
  )

## Boxplot categories ~ predictors ####

## Boxplot categories ~ predictors colored ####
category_order <- c("domination", "power", "role_material", "role_symbolic", "sexual_autonomy")

category_colors <- c(
  "domination"      = "#66C2A5",
  "power"           = "#FC8D62", 
  "sexual_autonomy" = "#8DA0CB",
  "role_material"   = "#E78AC3",
  "role_symbolic"   = "#A6D854"
)

db_rda_boxplot <- scores_wide %>%
  filter(!is.na(category)) %>%
  mutate(category = factor(category, levels = rev(category_order))) %>%
  pivot_longer(cols = c(complexity, matri, herding_importance),
               names_to = "predictor",
               values_to = "correlation") %>%
  mutate(predictor = factor(predictor, 
                            levels = c("complexity", "herding_importance", "matri"),
                            labels = c("Complexity", "Herding importance", "Matrilineality"))) %>%
  ggplot(aes(x = category, y = correlation, fill = category)) +
  geom_boxplot(alpha = 0.7) +
  geom_point( size = 1.5, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_fill_manual(values  = category_colors) +
  scale_color_manual(values = category_colors) +
  facet_wrap(~ predictor) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = NULL,
       y = "Correlation with predictor-constrained axis",
       title = "Which domains does each predictor affect?")

ggsave("output/3.4_db-rda_db_rda_boxplot.png", db_rda_boxplot)

# Loadings of variables ####

# x-axis is partial correlation, not a PCoA loading 
# worth flagging in the caption since the scale is comparable but the meaning differs slightly

# Step 1: reshape scores_wide into long format, like vars_plot
vars_plot_dbrda <- scores_wide %>%
  filter(!is.na(category)) %>%
  left_join(whyte_category %>% select(var_id_whyte, title_shortened),
            by = "var_id_whyte") %>%
  pivot_longer(
    cols = c(complexity, herding_importance, matri),
    names_to  = "predictor",
    values_to = "correlation"
  ) %>%
  mutate(
    predictor = factor(predictor,
                       levels = c("complexity", "herding_importance", "matri"),
                       labels = c("Complexity", "Herding importance", "Matrilineality"))
  )

# Step 2: define y axis order (e.g. grouped by category, then alphabetical)
var_order <- vars_plot_dbrda %>%
  distinct(title_shortened, category) %>%
  arrange(desc(category), desc(title_shortened)) %>%   # <-- desc()
  pull(title_shortened)

vars_plot_dbrda <- vars_plot_dbrda %>%
  mutate(title_shortened = factor(title_shortened, levels = var_order))

# Step 3: y axis colors (same logic as your original)
y_axis_colors_dbrda <- vars_plot_dbrda %>%
  distinct(title_shortened, category) %>%
  mutate(title_shortened = factor(title_shortened, levels = var_order)) %>%
  arrange(title_shortened) %>%
  mutate(color = category_colors[as.character(category)]) %>%
  pull(color)

# Step 4: plot — same structure as loadings_plot
dbrda_loadings_plot <- ggplot(vars_plot_dbrda,
                              aes(x = correlation,
                                  y = title_shortened,
                                  fill = category)) +
  geom_col() +
  geom_vline(xintercept = c(-0.30, 0.30),
             linetype = "dotted", color = "grey40", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
  facet_wrap(~ predictor, nrow = 1) +
  scale_fill_manual(values = category_colors, name = "Domain") +
  theme_bw(base_size = 9) +
  theme(
    axis.text.y  = element_text(size = 7, color = y_axis_colors_dbrda),
    strip.background = element_blank(),
    strip.text   = element_text(face = "bold", size = 10),
    legend.position = "none",
    panel.spacing = unit(1, "lines")
  ) +
  labs(x = "Partial correlation with predictor axis (sign-corrected)",
       y = NULL)

# Step 5: combine with legend
dbrda_loadings_plot2 <- cowplot::plot_grid(
  standalone_legend,
  dbrda_loadings_plot,
  rel_widths = c(0.2, 1)
)

ggsave("output/3.4_db-rda_predictor_loadings_plot.png",
       dbrda_loadings_plot2,
       width = 9, height = 6)


# REMOVE? What differentiates complexity and herding ####

# scores_wide already has: complexity = correlation with complexity-CAP1
# treat complexity vs herding as a 2D space
ggplot(scores_wide %>% filter(!is.na(category)),
       aes(x = complexity, y = herding_importance, 
           label = title, color = category)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(size = 2, alpha = 0.7) +
  ggrepel::geom_text_repel(size = 2.5, max.overlaps = 15) +
  theme_bw() +
  labs(x = "Correlation with complexity axis (partial)",
       y = "Correlation with herding axis (partial)",
       title = "Response variables in predictor space") +
  geom_smooth(method = "lm")


ggplot(scores_wide %>% filter(!is.na(category)),
       aes(x = complexity, y = matri, 
           label = title, color = category)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(size = 2, alpha = 0.7) +
  ggrepel::geom_text_repel(size = 2.5, max.overlaps = 15) +
  theme_bw() +
  labs(x = "Correlation with complexity axis (partial)",
       y = "Correlation with matri axis (partial)",
       title = "Complexity vs matrilineality: opposing effects on cultural variables")


# And variables in that space

library(ggrepel)

# Compute category centroids in the complexity x matri space
category_centroids <- scores_wide %>%
  filter(!is.na(category)) %>%
  group_by(category) %>%
  summarise(
    complexity = mean(complexity, na.rm = TRUE),
    matri      = mean(matri,      na.rm = TRUE),
    herding    = mean(herding_importance, na.rm = TRUE),
    .groups    = "drop"
  )

# Individual variables as points, category arrows as overlays
ggplot() +
  # individual variables
  geom_point(data = scores_wide %>% filter(!is.na(category)),
             aes(x = complexity, y = matri, color = category),
             size = 2, alpha = 0.6) +
  # category centroid arrows from origin
  geom_segment(data = category_centroids,
               aes(x = 0, y = 0,
                   xend = complexity, yend = matri,
                   color = category),
               arrow = arrow(length = unit(0.3, "cm")),
               linewidth = 1.2) +
  geom_label_repel(data = category_centroids,
                   aes(x = complexity, y = matri,
                       label = category, color = category),
                   size = 3.5, show.legend = FALSE) +
  scale_color_manual(values = category_colors) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Partial correlation with complexity axis",
       y = "Partial correlation with matrilineality axis",
       title = "Cultural domains in predictor space")
