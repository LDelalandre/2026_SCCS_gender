library(brms)
library(tidybayes)
library(ggplot2)
library(patchwork)

m_axis1 <- readRDS(here::here("output/models/m_axis1"))

# 1. POSTERIOR DISTRIBUTIONS

plot_mod_axis1 <- mod_axis1_phylo %>%
  gather_draws(b_complexity, bsp_momatri_cat) %>%
  mutate(.variable = recode(.variable,
                            b_complexity      = "Complexity",
                            bsp_momatri_cat  = "Kinship")) %>%
  ggplot(aes(x = .value, y = .variable)) +
  stat_halfeye(.width = c(0.89, 0.95),
               fill = "#7F77DD", alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "gray40") +
  labs(x = "Posterior estimate", y = NULL,
       subtitle = paste0("n = ", nrow(pred_resp %>% drop_na(Axis.1, complexity, matri_cat))),
       title = "Predictors of women's status (PCo1)") +
  theme_minimal(base_size = 13)

ggsave("draft/fig/plot_mod_axis1.png", plot_mod_axis1)


# 3. POSTERIOR PREDICTIVE CHECK
# does the model reproduce the distribution of Axis.1?
brms::pp_check(mod_axis1_phylo, ndraws = 100) +
  labs(title = "Posterior predictive check — Axis 1") +
  theme_minimal(base_size = 13)

# 4. R²
brms::bayes_R2(mod_axis1_phylo)



# Complexity and Matri ####


library(tidybayes)
library(ggplot2)
library(dplyr)
library(patchwork)

# ── Panel A: Axis.1 ~ complexity ──────────────────────────────────────────────
# Most frequent level of matri_cat
modal_matri <- pred_resp %>%
  count(matri_cat) %>%
  slice_max(n, n = 1) %>%
  pull(matri_cat)

complexity_grid <- tibble(
  complexity = seq(
    min(pred_resp$complexity, na.rm = TRUE),
    max(pred_resp$complexity, na.rm = TRUE),
    length.out = 100
  ),
  matri_cat = modal_matri
)

pred_complexity <- complexity_grid %>%
  add_epred_draws(mod_axis1_phylo, ndraws = 500) %>%
  group_by(complexity) %>%
  summarise(
    mean  = mean(.epred),
    lower = quantile(.epred, 0.025),
    upper = quantile(.epred, 0.975),
    .groups = "drop"
  )

p_a <- ggplot() +
  geom_ribbon(data = pred_complexity,
              aes(x = complexity, ymin = lower, ymax = upper),
              alpha = 0.2, fill = "#2196F3") +
  geom_line(data = pred_complexity,
            aes(x = complexity, y = mean),
            colour = "#2196F3", linewidth = 0.9) +
  geom_point(data = pred_resp %>% drop_na(Axis.1, complexity),
             aes(x = complexity, y = Axis.1),
             alpha = 0.5, size = 1.8) +
  labs(x = "Complexity", y = "Axis 1", title = "A") +
  theme_classic()

# ── Panel B: Axis.1 ~ matri_cat ───────────────────────────────────────────────
matri_grid <- tibble(
  matri_cat = levels(pred_resp$matri_cat) %>% 
    factor(levels = levels(pred_resp$matri_cat)), # preserve factor levels
  complexity = mean(pred_resp$complexity, na.rm = TRUE)
)

pred_matri <- matri_grid %>%
  add_epred_draws(m_axis1, ndraws = 500) %>%
  group_by(matri_cat) %>%
  summarise(
    mean  = mean(.epred),
    lower = quantile(.epred, 0.025),
    upper = quantile(.epred, 0.975),
    .groups = "drop"
  )

p_b <- ggplot() +
  geom_boxplot(data = pred_resp %>% drop_na(Axis.1, matri_cat),
               aes(x = matri_cat, y = Axis.1, group = matri_cat),
               outlier.shape = NA, width = 0.4, colour = "grey40") +
  geom_jitter(data = pred_resp %>% drop_na(Axis.1, matri_cat),
              aes(x = matri_cat, y = Axis.1),
              width = 0.08, alpha = 0.5, size = 1.8) +
  geom_pointrange(data = pred_matri,
                  aes(x = matri_cat, y = mean, ymin = lower, ymax = upper),
                  colour = "#2196F3", size = 0.7, linewidth = 1.1) +
  labs(x = "Kinship", y = "Axis 1", title = "B") +
  theme_classic()

# ── Combine ────────────────────────────────────────────────────────────────────
plot_complexity_matri_axis1 <- p_a + p_b

ggsave("draft/fig/plot_complexity_matri_axis1.png", plot_complexity_matri_axis1)





