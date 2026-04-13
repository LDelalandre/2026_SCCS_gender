library(tidybayes)
library(ggplot2)
library(patchwork)

m_axis1 <- readRDS(here::here("output/models/m_axis1"))
m_axis2 <- readRDS(here::here("output/models/m_axis2"))

# 1. POSTERIOR DISTRIBUTIONS
# clean coefficient plot with tidybayes
m_axis1 %>%
  gather_draws(b_complexity, b_herding_importance, bsp_momatri_cat  , b_hunting) %>%
  mutate(.variable = recode(.variable,
                            b_complexity         = "Complexity",
                            b_herding_importance = "Herding importance",
                            bsp_momatri_cat               = "Matrilineal/local",
                            b_hunting            = "Hunting")) %>%
  ggplot(aes(x = .value, y = .variable)) +
  stat_halfeye(.width = c(0.89, 0.95), 
               fill = "#7F77DD", alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", 
             color = "gray40") +
  labs(x = "Posterior estimate", y = NULL,
       title = "Axis 1- Posterior distributions") +
  theme_minimal(base_size = 19)

m_axis2 %>%
  gather_draws(b_hunting, b_agriculture_importance) %>%
  mutate(.variable = recode(.variable,
                            b_hunting                = "Hunting",
                            b_agriculture_importance = "Agriculture importance")) %>%
  ggplot(aes(x = .value, y = .variable)) +
  stat_halfeye(.width = c(0.89, 0.95),
               fill = "#7F77DD", alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "gray40") +
  labs(x = "Posterior estimate", y = NULL,
       title = "Axis 2 - Posterior distributions") +
  theme_minimal(base_size = 19) +
  coord_flip()

# 2. CONDITIONAL EFFECTS (predictor-response bivariate)

# 3. POSTERIOR PREDICTIVE CHECK
# does the model reproduce the distribution of Axis.1?
pp_check(m_axis1, ndraws = 100) +
  labs(title = "Posterior predictive check — Axis 1") +
  theme_minimal(base_size = 13)

pp_check(m_axis2, ndraws = 100) +
  labs(title = "Posterior predictive check — Axis 2") +
  theme_minimal(base_size = 13)

# 4. R²
bayes_R2(m_axis1)
bayes_R2(m_axis2)


## Additional tests ####
# check exact parameter names first
names(as_draws_df(m_creation)) %>% grep("mo", ., value = TRUE)
names(as_draws_df(m_weaning))  %>% grep("mo", ., value = TRUE)
names(as_draws_df(m_taboo))    %>% grep("mo", ., value = TRUE)

# extract draws from each model
draws_creation <- m_creation %>%
  gather_draws(bsp_mocreation_stories_ord) %>%
  mutate(.variable = "Creation stories\n(N=53)")

draws_weaning <- m_weaning %>%
  gather_draws(bsp_moweaning_ord) %>%
  mutate(.variable = "Weaning age\n(N=72)")

draws_taboo <- m_taboo %>%
  gather_draws(bsp_motaboo_ord) %>%
  mutate(.variable = "Post-partum taboo\n(N=50)")

bind_rows(draws_creation, draws_weaning, draws_taboo) %>%
  mutate(.variable = factor(.variable, 
                            levels = c("Creation stories\n(N=53)",
                                       "Weaning age\n(N=72)",
                                       "Post-partum taboo\n(N=50)"))) %>%
  ggplot(aes(x = .value, y = .variable)) +
  stat_halfeye(.width = c(0.89, 0.95),
               fill = "#7F77DD", alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "gray40") +
  labs(x = "Posterior estimate", y = NULL,
       title = "Fertility and symbolic culture — \n univariate models") +
  theme_minimal(base_size = 13)
