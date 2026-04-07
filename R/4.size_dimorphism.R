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
  select(soc_id, height_ratio)

# Join both to coords_df
coords_extended <- coords_df %>%
  left_join(dimorphism_vals,  by = "soc_id")

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
  ggplot(aes(x = Ax2, y = height_ratio)) +
  geom_point() +
  theme_classic() +
  ggtitle("All the regions grouped")



# Model ####

library(brms)

run_model <- F

if (run_model == T){
  mod_sc_size <- brm(
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
  saveRDS(mod_sc_size, here::here("output/2026-03-19_whyte_mod_sc_size.rds"))
}


mod_sc_size <- readRDS(here::here("output/2026-03-19_whyte_mod_sc_size.rds"))


mod_sc_size %>% summary()

coords_extended %>%
  tidybayes::add_epred_draws(mod_sc_size, ndraws = 100) %>%
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