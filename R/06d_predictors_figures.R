library(brms)
library(tidybayes)
library(ggplot2)
library(patchwork)

m_axis1 <- readRDS(here::here("output/models/m_axis1"))

# 1. POSTERIOR DISTRIBUTIONS

plot_mod_axis1 <- m_axis1 %>%
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
brms::pp_check(m_axis1, ndraws = 100) +
  labs(title = "Posterior predictive check — Axis 1") +
  theme_minimal(base_size = 13)

# 4. R²
brms::bayes_R2(m_axis1)


