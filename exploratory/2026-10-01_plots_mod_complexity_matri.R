## 0. Packages ####

library(brms)
library(tidybayes)
library(tidyverse)
library(posterior)
library(patchwork)
library(ggsignif)   # brackets with contrasts in panel B


## 1. Prepare the plotting data ####

plot_data <- pred_resp_phylo %>%
  drop_na(Axis.1, complexity, matri_cat) %>%
  mutate(
    matri_cat = factor(
      matri_cat,
      levels = levels(pred_resp_phylo$matri_cat)
    )
  )

n_obs <- nrow(plot_data)
lev   <- levels(plot_data$matri_cat)   # e.g. "non-matri", "intermediate", "matri"

# Switch: TRUE  = full stats box on panel A (as in your original)
#         FALSE = only beta on the plot (R2, lambda, n go in the legend)
annotate_full <- FALSE


## 2. Helper function ####

# x = c(lower, estimate, upper)  ->  "estimate [lower, upper]"
fmt_q <- function(x, digits = 2) {
  paste0(
    round(x[2], digits),
    " [", round(x[1], digits), ", ", round(x[3], digits), "]"
  )
}


## 3. Bayesian R2 ####

# Marginal R2: excludes the phylogenetic random effect
r2_marg <- bayes_R2(
  mod_axis1_phylo,
  re_formula = NA,
  probs = c(0.025, 0.975)
)

# Conditional R2: includes the phylogenetic random effect
r2_cond <- bayes_R2(
  mod_axis1_phylo,
  probs = c(0.025, 0.975)
)


## 4. Posterior draws: beta and phylogenetic signal ####

draws <- as_draws_df(mod_axis1_phylo)

# Coefficient for complexity
beta_q <- quantile(
  draws$b_complexity,
  probs = c(0.025, 0.50, 0.975),
  na.rm = TRUE
)

# Phylogenetic signal:
# lambda = phylogenetic variance / (phylogenetic variance + residual variance)
lambda_draws <- draws %>%
  transmute(
    lambda = sd_phylo__Intercept^2 / (sd_phylo__Intercept^2 + sigma^2)
  ) %>%
  pull(lambda)

lambda_q <- quantile(
  lambda_draws,
  probs = c(0.025, 0.50, 0.975),
  na.rm = TRUE
)


## 5. Annotation text ####

r2_marg_txt <- paste0(
  "R² marginal = ",
  fmt_q(unlist(r2_marg[1, c("Estimate", "Q2.5", "Q97.5")]), digits = 2)
)
r2_cond_txt <- paste0(
  "R² conditional = ",
  fmt_q(unlist(r2_cond[1, c("Estimate", "Q2.5", "Q97.5")]), digits = 2)
)
beta_txt   <- paste0("β complexity = ", fmt_q(beta_q, digits = 2))
lambda_txt <- paste0("λ phylogenetic = ", fmt_q(lambda_q, digits = 2))

# Text drawn on panel A
label_text <- if (annotate_full) {
  paste(r2_marg_txt, r2_cond_txt, beta_txt, lambda_txt,
        paste0("n = ", n_obs), sep = "\n")
} else {
  beta_txt
}

# Text for the figure legend (printed to the console, copy-paste it)
legend_stats <- paste(
  r2_marg_txt, r2_cond_txt, lambda_txt, paste0("n = ", n_obs),
  sep = "; "
)
legend_stats


## 6. Panel A: Axis 1 ~ complexity ####

# Most frequent category of matri_cat (reference for the prediction line)
modal_matri <- plot_data %>%
  count(matri_cat) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  pull(matri_cat)

# Prediction grid
complexity_grid <- tibble(
  complexity = seq(
    min(plot_data$complexity, na.rm = TRUE),
    max(plot_data$complexity, na.rm = TRUE),
    length.out = 100
  ),
  matri_cat = factor(modal_matri, levels = lev)
)

# Population-level posterior predictions
# re_formula = NA excludes taxon-specific phylogenetic random effects
pred_complexity <- complexity_grid %>%
  add_epred_draws(
    object     = mod_axis1_phylo,
    newdata    = .,
    re_formula = NA,
    ndraws     = 1000
  ) %>%
  group_by(complexity) %>%
  summarise(
    mean  = mean(.epred),
    lower = quantile(.epred, 0.025),
    upper = quantile(.epred, 0.975),
    .groups = "drop"
  )

p_a <- ggplot() +
  
  # 95% credible interval for the expected mean
  geom_ribbon(
    data = pred_complexity,
    aes(x = complexity, ymin = lower, ymax = upper),
    fill = "#2196F3", alpha = 0.20
  ) +
  
  # Posterior mean relationship
  geom_line(
    data = pred_complexity,
    aes(x = complexity, y = mean),
    colour = "#2196F3", linewidth = 0.9
  ) +
  
  # Observed data
  geom_point(
    data = plot_data,
    aes(x = complexity, y = Axis.1),
    alpha = 0.5, size = 1.8
  ) +
  
  # Statistical annotation
  annotate(
    "label",
    x = Inf, y = Inf,
    label = label_text,
    hjust = 1.05, vjust = 1.1,
    size = 3.0, lineheight = 1.05,
    fill = "white", alpha = 0.88, colour = "black",
    label.size = 0.25
  ) +
  
  labs(x = "Social complexity", y = "Axis 1", title = "A") +
  
  theme_classic() +
  theme(
    plot.title  = element_text(face = "bold"),
    plot.margin = margin(5.5, 8, 5.5, 5.5)
  )


## 7. Panel B: posterior means per category ####

matri_grid <- tibble(
  matri_cat  = factor(lev, levels = lev),
  complexity = mean(plot_data$complexity, na.rm = TRUE)
)

# ONE set of posterior draws, used for both the means and the contrasts
# (all draws; contrasts are more stable than with 1000)
matri_draws <- matri_grid %>%
  add_epred_draws(
    object     = mod_axis1_phylo,
    newdata    = .,
    re_formula = NA
  ) %>%
  ungroup()

# Posterior mean + 95% CrI per category
pred_matri <- matri_draws %>%
  group_by(matri_cat) %>%
  summarise(
    mean  = mean(.epred),
    lower = quantile(.epred, 0.025),
    upper = quantile(.epred, 0.975),
    .groups = "drop"
  )


## 8. Panel B: pairwise contrasts ####

# Contrasts built from the factor levels (nothing hard-coded):
#   lev[2] - lev[1], lev[3] - lev[1], lev[3] - lev[2]
contrast_defs <- tibble(
  a = c(lev[2], lev[3], lev[3]),
  b = c(lev[1], lev[1], lev[2])
) %>%
  mutate(contrast = paste(a, "-", b))

# One row per draw, one column per category
draws_wide <- matri_draws %>%
  select(.draw, matri_cat, .epred) %>%
  pivot_wider(names_from = matri_cat, values_from = .epred)

# Contrast computed once per draw, then summarised
contrast_results <- pmap_dfr(contrast_defs, function(a, b, contrast) {
  d <- draws_wide[[a]] - draws_wide[[b]]
  tibble(
    contrast   = contrast,
    group1     = b,
    group2     = a,
    estimate   = mean(d),
    lower      = quantile(d, 0.025),
    upper      = quantile(d, 0.975),
    p_positive = mean(d > 0),
    pd         = max(mean(d > 0), mean(d < 0))   # probability of direction
  )
}) %>%
  mutate(
    label = sprintf("Δ = %.2f [%.2f, %.2f]\npd = %.2f",
                    estimate, lower, upper, pd)
  )

contrast_results   # report this as a table too

# Order rows as (1-2), (2-3), (1-3) so they match the bracket heights
contrast_plot <- contrast_results %>%
  mutate(order = case_when(
    group1 == lev[1] & group2 == lev[2] ~ 1,
    group1 == lev[2] & group2 == lev[3] ~ 2,
    TRUE                                ~ 3
  )) %>%
  arrange(order)

# Bracket heights: adjacent pairs low, extreme pair on top
y_max <- max(plot_data$Axis.1, na.rm = TRUE)
y_rng <- diff(range(plot_data$Axis.1, na.rm = TRUE))
y_pos <- y_max + y_rng * c(0.06, 0.06, 0.20)


## 9. Panel B: plot ####

p_b <- ggplot() +
  
  # Observed distributions
  geom_boxplot(
    data = plot_data,
    aes(x = matri_cat, y = Axis.1, group = matri_cat),
    outlier.shape = NA, width = 0.4, colour = "grey40"
  ) +
  
  # Observed points
  geom_jitter(
    data = plot_data,
    aes(x = matri_cat, y = Axis.1),
    width = 0.08, alpha = 0.5, size = 1.8
  ) +
  
  # Posterior expected means and 95% credible intervals
  geom_pointrange(
    data = pred_matri,
    aes(x = matri_cat, y = mean, ymin = lower, ymax = upper),
    colour = "#2196F3", linewidth = 0.8, fatten = 2
  ) +
  
  # Contrast brackets (Δ = difference in posterior means, 95% CrI, pd)
  geom_signif(
    comparisons = map2(contrast_plot$group1, contrast_plot$group2, c),
    annotations = contrast_plot$label,
    y_position  = y_pos,
    tip_length  = 0.01,
    textsize    = 2.8,
    vjust       = -0.2
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.18))) +
  
  labs(x = "Kinship", y = "Axis 1", title = "B") +
  
  theme_classic() +
  theme(
    plot.title  = element_text(face = "bold"),
    plot.margin = margin(5.5, 5.5, 5.5, 8)
  )


## 10. Combine the panels ####

plot_complexity_matri_axis1 <- p_a + p_b

plot_complexity_matri_axis1