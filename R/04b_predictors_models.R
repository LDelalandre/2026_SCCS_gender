

# Models ####
library("brms")
run_models <- F

pred_resp_phylo <- pred_resp %>% 
  left_join(coords_matched_86 %>% select(soc_id, phylo))

A <- ape::vcv(tree_86, corr = TRUE) # covariance matrix of societies

if (run_models){
  mod_axis1_phylo <- 
    brm(Axis.1 ~ complexity + mo(matri_cat) + (1 | gr(phylo, cov = A)),
        data = pred_resp_phylo %>% drop_na(Axis.1, complexity, matri_cat),
        data2 = list(A = A),
        family = gaussian(),
        prior = c(prior(normal(0, 1), class = b),
                  prior(normal(0, 1), class = Intercept),
                  prior(exponential(1), class = sigma),
                  prior(exponential(1), class = sd),
                  prior(dirichlet(1), class = simo,
                        coef = "momatri_cat1")),
        cores = 4, iter = 4000, seed = 42,
        control = list(adapt_delta = 0.99) )

  mod_axis1_nophylo <- brm(Axis.1 ~ complexity + mo(matri_cat),
                 data = pred_resp %>% drop_na(Axis.1, complexity, matri_cat),
                 family = gaussian(),
                 prior = c(prior(normal(0, 1), class = b),
                           prior(normal(0, 1), class = Intercept),
                           prior(exponential(1), class = sigma),
                           prior(dirichlet(1), class = simo,
                                 coef = "momatri_cat1")),
                 cores = 4, iter = 4000, seed = 42)
  
  saveRDS(mod_axis1_phylo, here::here("output/models/mod_axis1_phylo"))
  saveRDS(mod_axis1_nophylo, here::here("output/models/mod_axis1_nophylo"))
} else{
  mod_axis1_phylo <- readRDS(here::here("output/models/mod_axis1_phylo"))
  mod_axis1_nophylo <- readRDS(here::here("output/models/mod_axis1_nophylo"))
}

summary(mod_axis1_phylo)
bayestestR::pd(mod_axis1_phylo)


plot(mod_axis1_phylo)
pp_check(mod_axis1_phylo)
pp_check(mod_axis1_phylo, type = "scatter_avg")
bayes_R2(mod_axis1_phylo)

post <- as_draws_df(mod_axis1_phylo)

# R²: marginal = Ax1 only; conditional = Ax1 + phylogeny
r2_marg <- bayes_R2(mod_axis1_phylo, re_formula = NA)
r2_cond <- bayes_R2(mod_axis1_phylo)

# Phylogenetic signal
lambda_draws <- post$sd_phylo__Intercept^2 /
  (post$sd_phylo__Intercept^2 + post$sigma^2)
lam_q <- quantile(lambda_draws, c(0.5, 0.025, 0.975))


W_sp <- compute_spatial_weight_matrix(coords_extended)

resid_vec <- residuals(mod_axis1_phylo)[, "Estimate"]
ape::Moran.I(resid_vec, W_sp)     # spatial autocorrelation in residuals


# Model one pcoa ####

mod <- lm(Ax1 ~ pred1 + pred2 + pred3 + pred4, data = pred_resp_one_pcoa)
summary(mod)

# Technology scale ####


model_techno_complete <- pred_resp %>%
  select(Axis.1, weaving, irrigation, plow, complexity, technology) %>%
  drop_na()

m0 <- lm(Axis.1 ~ 1,                           data = model_techno_complete)
m1 <- lm(Axis.1 ~ weaving,                     data = model_techno_complete)
m2 <- lm(Axis.1 ~ weaving + irrigation,        data = model_techno_complete)
m3 <- lm(Axis.1 ~ weaving + irrigation + plow, data = model_techno_complete)
anova(m0, m1, m2, m3)



# Among societies WITH weaving, does having a plow add anything?
model_techno_complete %>%
  filter(weaving == 1) %>%
  lm(Axis.1 ~ plow, data = .) %>%
  summary()

# Among societies WITHOUT plow, does weaving still predict?
model_techno_complete %>%
  filter(plow == 0) %>%
  lm(Axis.1 ~ weaving, data = .) %>%
  summary()

model_techno_complete %>%
  filter(plow == 0) %>% 
  group_by(weaving) %>% 
  summarize(n = n())

# weaving-irrigation
model_techno_complete %>%
  filter(weaving == 1) %>%
  lm(Axis.1 ~ irrigation, data = .) %>%
  summary()
model_techno_complete %>%
  filter(irrigation == 0) %>%
  lm(Axis.1 ~ weaving, data = .) %>%
  summary()

# irrigation-plow
model_techno_complete %>%
  filter(irrigation == 1) %>%
  lm(Axis.1 ~ plow, data = .) %>%
  summary()
model_techno_complete %>%
  filter(plow == 0) %>%
  lm(Axis.1 ~ irrigation, data = .) %>%
  summary()


# Does plow predict Ax1 completely independently of weaving and irrigation?
model_techno_complete %>%
  filter(weaving == 0 & plow ==1)



summary(lm(Axis.1 ~ complexity + weaving, data = model_techno_complete))
summary(lm(Axis.1 ~ complexity + irrigation, data = model_techno_complete))
summary(lm(Axis.1 ~ complexity + plow, data = model_techno_complete))
summary(lm(Axis.1 ~ complexity + technology, data = model_techno_complete))






# TO ERASE? ####

# Model complexity size ####

# TODO: compute complexity for all of Gleeson's data points!!
# TODO: add control on region6?

pred_resp_dimorphism <- coords_extended %>% 
  left_join(pred_resp, by = "soc_id") %>% 
  filter(!is.na(complexity)) 

pred_resp_dimorphism %>% 
  ggplot(aes(x = complexity, y = height_ratio)) +
  geom_point() +
  geom_smooth(method = "lm")


run_model <- F

if (run_model == T){
  mod_complexity_size <- brm(
    height_ratio ~ complexity + (1|region6),
    family = gaussian(),
    prior = c(prior(normal(0, 0.5), class = b),
              prior(normal(0, 0.5), class = Intercept),
              prior(exponential(1), class = sd),
              prior(exponential(1), class = sigma)),
    data = pred_resp_dimorphism %>% arrange(complexity),
    cores = 4, iter = 4000, seed = 42,
    control = list(adapt_delta = 0.99)  # accept 99% of the steps during warmup, and not only 80%. Forces more cautions, smaller steps. I do it because variance in size dimorphism among region6s is not substantive, - or I don't have enough measurements per region6 - and this caused some problems.
  )
  saveRDS(mod_complexity_size, here::here("output/2026-03-19_whyte_mod_complexity_size.rds"))
}


mod_complexity_size <- readRDS(here::here("output/2026-03-19_whyte_mod_complexity_size.rds"))


mod_complexity_size %>% summary()

pred_resp_dimorphism %>%
  tidybayes::add_epred_draws(mod_complexity_size, ndraws = 100) %>%
  ggplot(aes(x = complexity, y = height_ratio)) +
  tidybayes::stat_lineribbon(aes(y = .epred), 
                             .width = c(0.5, 0.95), 
                             alpha = 0.3, 
                             fill = "steelblue") +
  geom_point() +
  labs(x = "Complexity", y = "Height ratio") +
  theme_classic()


# Basic PPC - most informative first look
pp_check(mod_complexity_size, ndraws = 100)

# Check the mean
pp_check(mod_complexity_size, type = "stat", stat = "mean", ndraws = 1000)

# Check mean AND sd jointly
pp_check(mod_complexity_size, type = "stat_2d", ndraws = 1000)

# Check for outliers / tail behaviour
pp_check(mod_complexity_size, type = "scatter_avg")



# mod_no_region6 <- brm(
#   height_ratio ~ complexity,
#   family = gaussian(),
#   prior = c(prior(normal(0, 0.5), class = b),
#             prior(normal(0, 0.5), class = Intercept),
#             prior(exponential(1), class = sigma)),
#   data = pred_resp_dimorphism,
#   cores = 4, iter = 4000, seed = 42
# )
# 
# loo_region6    <- loo(mod_complexity_size)
# loo_no_region6 <- loo(mod_no_region6)
# 
# loo_compare(loo_region6, loo_no_region6)
# The LOO favors mod_no_region6 marginally, but the difference (−1.0 ± 0.8) is well within 1 SE — so the models are indistinguishable predictively.



# Model mediation ####

pred_resp_dimorphism %>% 
  select(complexity, height_ratio) %>% 
  na.omit() %>% 
  dim()

run_model <- F

if (run_model == T){
  mod_mediation <- brm(
    bf(height_ratio ~ Ax1 + complexity + (1 | region6)) +
      bf(Ax1 ~ complexity + herding_importance + mo(matri_cat) + hunting) +
      set_rescor(FALSE),
    family = gaussian(),
    prior = c(
      # height_ratio equation
      prior(normal(0, 0.5), class = b, resp = heightratio),
      prior(normal(0, 0.5), class = Intercept, resp = heightratio),
      prior(exponential(1), class = sigma, resp = heightratio),
      prior(exponential(1), class = sd, resp = heightratio),
      # Ax1 equation
      prior(normal(0, 0.5), class = b, resp = Ax1),
      prior(normal(0, 0.5), class = Intercept, resp = Ax1),
      prior(exponential(1), class = sigma, resp = Ax1)
    ),
    data = pred_resp_dimorphism,  # needs all variables in one dataframe
    cores = 4, iter = 4000, seed = 42,
    control = list(adapt_delta = 0.99)
  )
  saveRDS(mod_mediation, here::here("output/2026-03-19_whyte_mod_mediation.rds"))
}


mod_mediation <- readRDS(here::here("output/2026-03-19_whyte_mod_mediation.rds"))

summary(mod_mediation)
post <- as_draws_df(mod_mediation) # these are the posterior distributions

# indirect effect = a path × b path
post$indirect <- post$b_heightratio_Ax1 * post$b_Ax1_complexity

# direct effect
post$direct <- post$b_heightratio_complexity

# total effect
post$total <- post$indirect + post$direct

# summarise: Bayesian credible interval (CI) table
posterior::summarise_draws(post[, c("indirect", "direct", "total")],
                           mean, sd, ~quantile(.x, probs = c(0.025, 0.975)))
