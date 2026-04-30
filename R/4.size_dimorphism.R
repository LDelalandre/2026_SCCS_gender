# Match Gleeson's height data and the SCCS ####

# Expand societies into one row per alternative name
societies_long <- societies %>%
  mutate(alt_name = str_split(alt_names_by_society, ",")) %>%
  unnest(alt_name) %>%
  mutate(alt_name = trimws(alt_name)) %>% 
  select(soc_id, xd_id, pref_name_for_society, alt_name, everything())

matched_pref_name <- inner_join(gleeson, societies, 
                                by = c("society_name" = "pref_name_for_society"))
matched_alt_name <- inner_join(gleeson, societies_long, 
                               by = c("society_name" = "alt_name")) %>% 
  select(-pref_name_for_society)

intersect(matched_pref_name$soc_id, matched_alt_name$soc_id)

matched <- matched_pref_name %>% 
  bind_rows(matched_alt_name) %>% 
  unique() 

unmatched <- gleeson %>% 
  filter(!society_name %in% matched$society_name)

# For each unmatched, find the closest name in societies_long
library(stringdist)
fuzzy_match <- unmatched %>%
  rowwise() %>%
  mutate(
    best_match = societies_long$alt_name[
      which.min(stringdist(society_name, 
                           societies_long$alt_name, 
                           method = "jw"))  # Jaro-Winkler distance
    ],
    match_dist = min(stringdist(society_name, 
                                societies_long$alt_name, 
                                method = "jw"))
  ) %>% 
  select(society_name, best_match, everything()) %>% 
  arrange(match_dist)

fuzzy_match_good <- fuzzy_match %>% 
  filter(!society_name %in% c("Somali", "Basques")) %>% 
  left_join(societies_long, by = c("best_match" = "alt_name")) 

fuzzy_match_corrected <- fuzzy_match %>% 
  filter(society_name %in% c("Somali", "Basques")) %>% 
  mutate(final_match = case_when(society_name == "Somali" ~ "Somali (Dolbahanta)",
                                 society_name == "Basques" ~ "Spanish Basques",
                                 TRUE ~ best_match)) %>% 
  select(society_name, final_match) %>% 
  left_join(societies, by = c("final_match" = "pref_name_for_society")) 

corresp_gleeson_id <- fuzzy_match_good %>% 
  select(society_name, soc_id) %>% 
  bind_rows(fuzzy_match_corrected %>% select(society_name, soc_id)) %>% 
  bind_rows(matched %>% select(society_name, soc_id))

gleeson_id <- gleeson %>% 
  merge(corresp_gleeson_id, by = "society_name") %>% 
  mutate(height_ratio = female_stature/male_stature)

write.csv(gleeson_id,"output/2026-03_SCCS_gleeson_id.csv", row.names = F)



# Analyse ####

gleeson_id <- read.csv(here::here("output/2026-03_SCCS_gleeson_id.csv"))

dimorphism_vals <- gleeson_id %>%
  select(soc_id, height_ratio, male_stature, female_stature)

dimorphism_vals %>% 
  mutate(ratioMtF <- 1/height_ratio) 

# Join both to coords_df
coords_extended <- coords_df %>%
  left_join(dimorphism_vals,  by = "soc_id")

coords_extended %>% 
  filter(!is.na(height_ratio)) %>% 
  dim()
# Are societies with height ratio data representative of the full PCoA space?
has_size <- !is.na(coords_extended$height_ratio)
plot(pcoa_result$vectors[, 1], pcoa_result$vectors[, 2],
     col = ifelse(has_size, "steelblue", "grey80"), pch = 19,
     xlab = "PCo1", ylab = "PCo2")
legend("topright", c("has size data", "missing"), 
       col = c("steelblue", "grey80"), pch = 19)


coords_extended %>% 
  ggplot(aes(x = Ax1, y = height_ratio)) +
  geom_point() +
  theme_classic() +
  ggtitle("All the regions grouped")

coords_extended %>% 
  ggplot(aes(x = Ax1, y = male_stature)) +
  geom_point() +
  theme_classic() +
  ggtitle("All the regions grouped")

coords_extended %>% 
  ggplot(aes(x = Ax1, y = female_stature)) +
  geom_point() +
  theme_classic() +
  ggtitle("All the regions grouped")


coords_extended %>% 
  ggplot(aes(x = Ax2, y = height_ratio)) +
  geom_point() +
  theme_classic() +
  ggtitle("All the regions grouped")



# Model domination size ####

library(brms)

run_model <- F

if (run_model == T){
  mod_domination_size <- brm(
    height_ratio ~ Ax1 + (1|region),
    family = gaussian(),
    prior = c(prior(normal(0, 0.5), class = b),
              prior(normal(0, 0.5), class = Intercept),
              prior(exponential(1), class = sd),
              prior(exponential(1), class = sigma)),
    data = coords_extended,
    cores = 4, iter = 4000, seed = 42,
    control = list(adapt_delta = 0.99)  # accept 99% of the steps during warmup, and not only 80%. Forces more cautions, smaller steps. I do it because variance in size dimorphism among regions is not substantive, - or I don't have enough measurements per region - and this caused some problems.
  )
  saveRDS(mod_domination_size, here::here("output/2026-03-19_whyte_mod_domination_size.rds"))
}


mod_domination_size <- readRDS(here::here("output/2026-03-19_whyte_mod_domination_size.rds"))


mod_domination_size %>% summary()

mod_domination_size %>% bayes_R2()

coords_extended %>%
  tidybayes::add_epred_draws(mod_domination_size, ndraws = 100) %>%
  ggplot(aes(x = Ax1, y = height_ratio)) +
  tidybayes::stat_lineribbon(aes(y = .epred), 
                             .width = c(0.5, 0.95), 
                             alpha = 0.3, 
                             fill = "steelblue") +
  geom_point() +
  labs(x = "Axis 1", y = "Height ratio") +
  theme_classic()


coords_extended %>% 
  ggplot(aes(x = Ax1, y = height_ratio)) +
  geom_point() +
  theme_classic() +
  facet_wrap(~region)


# Model complexity size ####

# TODO: compute complexity for all of Gleeson's data points!!
# TODO: add control on region?

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
    height_ratio ~ complexity + (1|region),
    family = gaussian(),
    prior = c(prior(normal(0, 0.5), class = b),
              prior(normal(0, 0.5), class = Intercept),
              prior(exponential(1), class = sd),
              prior(exponential(1), class = sigma)),
    data = pred_resp_dimorphism,
    cores = 4, iter = 4000, seed = 42,
    control = list(adapt_delta = 0.99)  # accept 99% of the steps during warmup, and not only 80%. Forces more cautions, smaller steps. I do it because variance in size dimorphism among regions is not substantive, - or I don't have enough measurements per region - and this caused some problems.
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



# mod_no_region <- brm(
#   height_ratio ~ complexity,
#   family = gaussian(),
#   prior = c(prior(normal(0, 0.5), class = b),
#             prior(normal(0, 0.5), class = Intercept),
#             prior(exponential(1), class = sigma)),
#   data = pred_resp_dimorphism,
#   cores = 4, iter = 4000, seed = 42
# )
# 
# loo_region    <- loo(mod_complexity_size)
# loo_no_region <- loo(mod_no_region)
# 
# loo_compare(loo_region, loo_no_region)
# The LOO favors mod_no_region marginally, but the difference (−1.0 ± 0.8) is well within 1 SE — so the models are indistinguishable predictively.



# Model mediation ####

run_model <- F

if (run_model == T){
  mod_mediation <- brm(
    bf(height_ratio ~ Ax1 + complexity + (1 | region)) +
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
