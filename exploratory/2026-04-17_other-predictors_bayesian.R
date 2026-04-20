library(brms)

# Standardise first
d <- model_data_all %>%
  dplyr::select(Ax1, guttman_3, complexity, matri) %>%
  drop_na() %>%
  mutate(across(everything(), ~ as.vector(scale(.))))

# Main model
m_main <- brm(
  Ax1 ~ guttman_3,
  data = d,
  family = gaussian(),
  prior = c(
    prior(normal(0, 0.2), class = Intercept),
    prior(normal(0, 0.5), class = b),
    prior(exponential(1), class = sigma)
  ),
  chains = 4,
  iter = 2000,
  seed = 42
)

# Complexity model
m_cmpl <- brm(
  Ax1 ~ guttman_3 + complexity,
  data = d,
  family = gaussian(),
  prior = c(
    prior(normal(0, 0.2), class = Intercept),
    prior(normal(0, 0.5), class = b),
    prior(exponential(1), class = sigma)
  ),
  chains = 4,
  iter = 2000,
  seed = 42
)

# Matri model
m_matri <- brm(
  Ax1 ~ guttman_3 + matri,
  data = d,
  family = gaussian(),
  prior = c(
    prior(normal(0, 0.2), class = Intercept),
    prior(normal(0, 0.5), class = b),
    prior(exponential(1), class = sigma)
  ),
  chains = 4,
  iter = 2000,
  seed = 42
)



# Model comparison with LOO
m_main <- add_criterion(m_main, "loo")
m_cmpl <- add_criterion(m_cmpl, "loo")
m_matri <- add_criterion(m_matri, "loo")

loo_compare(m_main, m_cmpl, m_matri)
# “If I leave out one observation, fit the model on the rest, 
# and try to predict the left-out point—how well does the model perform?”

# here, matri a bit better, but no really better model (low differences)

# Posterior summaries
summary(m_main)
summary(m_matri)

# Plot posteriors — much nicer than rethinking
library(tidybayes)

m_cmpl %>%
  gather_draws(b_guttman_3, b_complexity) %>%
  ggplot(aes(x = .value, y = .variable)) +
  stat_halfeye() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Standardised coefficient",
       y = NULL)

m_matri %>%
  gather_draws(b_guttman_3, b_matri) %>%
  ggplot(aes(x = .value, y = .variable)) +
  stat_halfeye() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Standardised coefficient",
       y = NULL)

# Bayesian mediation as multivariate model
m_mediation <- brm(
  bf(Ax1 ~ guttman_3 + matri) +
    bf(matri ~ guttman_3),
  data = d,
  family = gaussian(),
  prior = c(
    prior(normal(0, 0.2), class = Intercept, resp = Ax1),
    prior(normal(0, 0.2), class = Intercept, resp = matri),
    prior(normal(0, 0.5), class = b),
    prior(exponential(1), class = sigma)
  ),
  chains = 4,
  iter = 2000,
  seed = 42
)

# Extract indirect effect (a*b path)
post <- as_draws_df(m_mediation)

# Indirect effect = guttman->matri * matri->Ax1
post <- post %>%
  mutate(
    indirect = b_matri_guttman_3 * b_Ax1_matri,
    direct   = b_Ax1_guttman_3,
    total    = direct + indirect,
    prop_med = indirect / total
  )

# Summarise
post %>%
  select(indirect, direct, total, prop_med) %>%
  summarise(across(everything(), 
                   list(mean = mean, 
                        lower = ~ quantile(., 0.025),
                        upper = ~ quantile(., 0.975))))