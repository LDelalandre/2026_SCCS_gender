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


# Galton' problem ####

# glottocode ####

library(lingtypology)

glottolog_families <- lingtypology::glottolog %>%
  select(glottocode, affiliation) %>%
  mutate(
    language_family = if_else(
      affiliation == "" | is.na(affiliation),
      "isolate",                          # language isolates have no affiliation
      sub(",.*", "", affiliation)         # take everything before the first comma
    )
  )

societies_glottolog <- societies %>% 
  left_join(glottolog_families)

# Analyse ####

gleeson_id <- read.csv(here::here("output/2026-03_SCCS_gleeson_id.csv"))

dimorphism_vals <- gleeson_id %>%
  select(soc_id, height_ratio, male_stature, female_stature)



dimorphism_vals %>% 
  mutate(ratioMtF <- 1/height_ratio) 

# Join both to coords_df
coords_extended <- coords_df %>%
  left_join(dimorphism_vals,  by = "soc_id") %>% 
  left_join(societies_glottolog)

coords_extended %>% 
  count(language_family) %>% 
  arrange(desc(n))

coords_extended %>% 
  filter(!is.na(height_ratio)) %>% 
  dim()
# Are societies with height ratio data representative of the full PCoA space?

plot_df <- data.frame(
  PCo1 = pcoa_result$vectors[, 1],
  PCo2 = pcoa_result$vectors[, 2],
  has_size = ifelse(!is.na(coords_extended$height_ratio), "has size data", "missing")
)

pcoa_dimorphism <- ggplot(plot_df, aes(x = PCo1, y = PCo2, color = has_size)) +
  geom_point(size = 1) +
  scale_color_manual(values = c("has size data" = "steelblue", "missing" = "grey80")) +
  labs(x = "PCo1", y = "PCo2", color = NULL) +
  theme_classic(base_size = 11) +
  theme(legend.position = "top")

ggsave("draft/fig/pcoa_dimorphism.png", pcoa_dimorphism, height = 8, width = 8, units = "cm")

coords_extended %>% 
  ggplot(aes(x = Ax1, y = height_ratio)) +
  geom_point() +
  theme_classic() +
  ggtitle("All the regions grouped")

pcoa_dimorphism_region <- coords_extended %>% 
  ggplot(aes(x = Ax1, y = height_ratio)) +
  geom_point() +
  theme_classic() +
  # ggtitle("All the regions grouped") +
  facet_wrap(~ region6)

ggsave("draft/fig/pcoa_dimorphism_region.png", pcoa_dimorphism_region, height = 15, width = 15, units = "cm")

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


boxplot_dimorphism_region <- coords_extended %>% 
  ggplot(aes(x = region6, y = height_ratio)) +
  geom_boxplot() + 
  geom_point()

# Model domination size ####

library(brms)

run_model <- F

if (run_model == T){
  mod_domination_size <- brm(
    height_ratio ~ Ax1 + (1|region6), # + (1|language_family) # not enough data points to do so
    family = gaussian(),
    prior = c(prior(normal(0, 0.5), class = b),
              prior(normal(0, 0.5), class = Intercept),
              prior(exponential(1), class = sd),
              prior(exponential(1), class = sigma)),
    data = coords_extended,
    cores = 4, iter = 4000, seed = 42,
    control = list(adapt_delta = 0.99)  # accept 99% of the steps during warmup, and not only 80%. Forces more cautions, smaller steps. I do it because variance in size dimorphism among region6s is not substantive, - or I don't have enough measurements per region6 - and this caused some problems.
  )
  saveRDS(mod_domination_size, here::here("output/2026-03-19_whyte_mod_domination_size.rds"))
}


mod_domination_size <- readRDS(here::here("output/2026-03-19_whyte_mod_domination_size.rds"))


r2 <- mod_domination_size %>% bayes_R2() 
beta <- fixef(mod_domination_size)["Ax1", ]
n <- nrow(drop_na(coords_extended, Ax1, height_ratio))

label_text <- paste0(
  "R² = ", round(r2[1, "Estimate"], 2),
  " [", round(r2[1, "Q2.5"], 2), ", ", round(r2[1, "Q97.5"], 2), "]\n",
  "β = ", round(beta["Estimate"], 3),
  " [", round(beta["Q2.5"], 2), ", ", round(beta["Q97.5"], 2), "]\n",
  "n = ", n
)

biplot_dimorphism <- coords_extended %>%
  tidybayes::add_epred_draws(mod_domination_size, ndraws = 100, re_formula = NA) %>%
  ggplot(aes(x = Ax1, y = height_ratio)) +
  tidybayes::stat_lineribbon(aes(y = .epred),
                             .width = c(0.5, 0.95),
                             alpha = 0.3,
                             fill = "steelblue") +
  geom_point() +
  annotate("text", 
           x = min(coords_extended$Ax1, na.rm = TRUE), 
           y = max(coords_extended$height_ratio, na.rm = TRUE),
           label = label_text,
           hjust = 0, vjust = 1, size = 3.2, family = "mono") +
  labs(x = "Axis 1", y = "Height ratio") +
  theme_classic()

ggsave("draft/fig/biplot_dimorphism.png", biplot_dimorphism)

coords_extended %>% 
  ggplot(aes(x = Ax1, y = height_ratio)) +
  geom_point() +
  theme_classic() +
  facet_wrap(~region6)


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



# SMA rather than ratio ####
# Following Holden and Mace 1999

dimorphism_vals %>% 
  ggplot(aes(x = male_stature, y = female_stature)) +
  geom_point()

library(smatr)

mod_dimorphism <- sma(female_stature ~ male_stature, data = coords_extended, log = "") # to log-transform: log = "xy". But seems unnecessary 
summary(mod_dimorphism)

coords_extended$dimorphism_resid <- NA
non_na <- !is.na(coords_extended$male_stature) & !is.na(coords_extended$female_stature)
coords_extended$dimorphism_resid[non_na] <- residuals(mod_dimorphism)

coords_extended %>% 
  ggplot(aes(x = Ax1, y = dimorphism_resid)) +
  geom_point()

# Phylogenetic supertree (Minocher2019) ####

library(ape)

# the tree itself
minocher_tree <- ape::read.nexus("data/minocher2019/Time-calibrated SCCS supertree ultrametric.nex")
class(minocher_tree)      # should be "phylo"
length(minocher_tree$tip.label)
head(minocher_tree$tip.label)



## Match societies names ####

# the name-matching table -- inspect columns before assuming names
lookup <- read.csv("data/minocher2019/socnamesphylo.csv")
str(lookup)
head(lookup)

library(dplyr)
library(tidyr)
library(stringr)
library(stringi)

# normalize: strip accents, lowercase, drop punctuation, squeeze whitespace
norm <- function(x) {
  x |>
    stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_replace_all("\\s*\\([^)]*\\)", "") |>   # drop parenthetical bits like "(SCCS2)" or "(Was Nyae)"
    str_replace_all("[^a-z0-9 ]", " ") |>
    str_squish()
}

# 1. Long table of all names per society, with a priority (lower = better)
names_long <- bind_rows(
  societies |> transmute(soc_id, name = pref_name_for_society, priority = 1),
  societies |> transmute(soc_id, name = ORIG_name_and_ID_in_this_dataset, priority = 2),
  societies |>
    select(soc_id, alt_names_by_society) |>
    separate_rows(alt_names_by_society, sep = ",\\s*") |>
    transmute(soc_id, name = alt_names_by_society, priority = 3)
) |>
  mutate(key = norm(name)) |>
  filter(key != "") |>
  distinct(soc_id, key, .keep_all = TRUE)

# 2. Join the lookup
lookup$key <- norm(lookup$socname)

matched <- lookup |>
  left_join(names_long, by = "key", relationship = "many-to-many")

# 3. Diagnostics
unmatched <- matched |> filter(is.na(soc_id))            # no society found
ambiguous <- matched |>                                   # one socname -> several societies
  filter(!is.na(soc_id)) |>
  group_by(socname) |> filter(n_distinct(soc_id) > 1) |> ungroup()

# 4. Resolve: keep best-priority match per socname
lookup_matched <- matched |>
  filter(!is.na(soc_id)) |>
  group_by(socname) |>
  slice_min(priority, n = 1, with_ties = TRUE) |>
  ungroup()

# also check the reverse: SCCS societies with no phylo entry, or duplicated
setdiff(societies$soc_id, lookup_matched$soc_id)
lookup_matched |> count(soc_id) |> filter(n > 1)


## Prune the tree to my sample ####

coords_matched <- coords_extended %>%
  left_join(lookup_matched, by = "soc_id") %>% 
  filter(!is.na(height_ratio))

# check for anything that didn't match before pruning
coords_matched %>% filter(is.na(phylo)) %>% select(soc_id, pref_name_for_society)

# prune the tree to just your sample (plain ape, no glottoTrees needed --
# this tree is already society-level, not language-classification-level)
my_tree <- ape::keep.tip(minocher_tree, tip = coords_matched$phylo)


png("draft/fig/tree_plot.png", width = 20, height = 25, units = "cm", res = 300)
plot.phylo(my_tree)
dev.off()


## Model ####

library(ape)
library(brms)

tree <- my_tree
dat  <- coords_matched
# rescale the variables
dat <- dat |> mutate(
  height_z = as.numeric(scale(height_ratio)),
  Ax1_z    = as.numeric(scale(Ax1))
)
# refit with height_z ~ Ax1_z + (1 | gr(phylo, cov = A)) + (1 | region6)

# 2. Phylogenetic correlation matrix
A <- ape::vcv(tree, corr = TRUE) # covariance matrix of societies
A <- A[dat$phylo, dat$phylo]      # not required, but confirms names line up
stopifnot(all(dat$phylo %in% rownames(A)))

# 3. Model
run_models <- F
if (run_models){
  mod_domination_phylo <- brm(
    height_z ~ Ax1_z + (1 | gr(phylo, cov = A)),
    data  = dat,
    data2 = list(A = A),
    family = gaussian(),
    prior = c(prior(normal(0, 0.5), class = b),
              prior(normal(0, 0.5), class = Intercept),
              prior(exponential(1), class = sd),
              prior(exponential(1), class = sigma)),
    cores = 4, iter = 4000, seed = 42,
    control = list(adapt_delta = 0.99)
  )
  
  mod_domination_nophylo <- brm(
    height_z ~ Ax1_z ,
    data  = dat,
    family = gaussian(),
    prior = c(prior(normal(0, 0.5), class = b),
              prior(normal(0, 0.5), class = Intercept),
              prior(exponential(1), class = sigma)),
    cores = 4, iter = 4000, seed = 42,
    control = list(adapt_delta = 0.99)
  )
  
  saveRDS(mod_domination_phylo, here::here("output/2026-03-19_whyte_mod_domination_phylo.rds"))
  saveRDS(mod_domination_nophylo, here::here("output/2026-03-19_whyte_mod_domination_nophylo.rds"))
}

mod_domination_phylo <- readRDS(here::here("output/2026-03-19_whyte_mod_domination_phylo.rds"))
mod_domination_nophylo <- readRDS(here::here("output/2026-03-19_whyte_mod_domination_nophylo.rds"))

mod_domination_phylo
r2 <- mod_domination_phylo %>% bayes_R2() 

plot(mod_domination_phylo)
plot(conditional_effects(mod_domination_phylo), points = TRUE)

# phylogenetic signal (lambda) = ICC intraclass correlation coefficient
hyp <- "sd_phylo__Intercept^2 / (sd_phylo__Intercept^2 + sigma^2) = 0"
(hyp <- hypothesis(mod_domination_phylo, hyp, class = NULL))
plot(hyp)

# More digits
print(mod_domination_phylo, digits = 4)
fixef(mod_domination_phylo, probs = c(0.025, 0.05, 0.95, 0.975))

# Effect of Ax1 only: probability of direction
post <- as_draws_df(mod_domination_phylo)
mean(post$b_Ax1 > 0)

# Phylogenetic signal (lambda) with its uncertainty
lambda <- post$sd_phylo__Intercept^2 /
  (post$sd_phylo__Intercept^2 + post$sd_region6__Intercept^2 + post$sigma^2)
quantile(lambda, c(0.025, 0.5, 0.975))

# Variance explained by Ax1 alone vs. Ax1 + random effects
bayes_R2(mod_domination_phylo, re_formula = NA)   # fixed effects only (marginal)
bayes_R2(mod_domination_phylo)                    # conditional (what you ran)


# Compare the two 
mod_phylo   <- add_criterion(mod_domination_phylo,   "loo")
mod_nophylo <- add_criterion(mod_domination_nophylo, "loo")
loo_compare(mod_phylo, mod_nophylo)

loo(mod_domination_phylo)
loo(mod_domination_nophylo)


## Spatial autocorrelation in the residuals? ####

library(Matrix)      # nearPD(), to force a valid correlation matrix

# Dependency-free replacement for geosphere::distm(fun = distHaversine).
# Takes a matrix/data.frame with columns (lon, lat) in decimal degrees,
# returns an n x n matrix of great-circle distances in km.
haversine_dist_matrix <- function(lonlat, R = 6371) {
  lon <- lonlat[, 1] * pi / 180
  lat <- lonlat[, 2] * pi / 180
  n <- length(lon)
  D <- matrix(0, n, n)
  for (i in seq_len(n - 1)) {
    dlon <- lon[(i + 1):n] - lon[i]
    dlat <- lat[(i + 1):n] - lat[i]
    a <- sin(dlat / 2)^2 + cos(lat[i]) * cos(lat[(i + 1):n]) * sin(dlon / 2)^2
    a <- pmin(pmax(a, 0), 1)          # guard against tiny floating-point overshoot
    d <- 2 * R * asin(sqrt(a))
    D[i, (i + 1):n] <- d
    D[(i + 1):n, i] <- d
  }
  D
}


stopifnot(!anyNA(coords_extended$Long), !anyNA(coords_extended$Lat))

coords_extended_nogap <- coords_extended %>% filter(!is.na(height_ratio))
n   <- nrow(coords_extended_nogap)
ids <- coords_extended_nogap$soc_id

# SPATIAL matrix
coords_mat <- as.matrix(coords_extended_nogap[, c("Long", "Lat")])
D_sp <- haversine_dist_matrix(coords_mat)   # km
dimnames(D_sp) <- list(ids, ids)

# 1a. Row-standardized inverse-distance weights (for LAG covariates, step 4)
W_sp <- 1 / (D_sp + 1)     # +1km guards the diagonal before we zero it
diag(W_sp) <- 0
W_sp <- W_sp / rowSums(W_sp)   # rows sum to 1

# 1b. Exponential-decay CORRELATION matrix (for the brms gr(cov=) approach, step 5)
# phi = distance scale over which similarity decays. Median pairwise
# distance is a reasonable data-driven default -- try a couple of
# alternatives (e.g. 1000km, or the 25th percentile) as a sensitivity check.
phi_sp <- median(D_sp[upper.tri(D_sp)])
A_sp <- exp(-D_sp / phi_sp)
diag(A_sp) <- 1
A_sp <- as.matrix(Matrix::nearPD(A_sp, corr = TRUE)$mat)  # force valid PD correlation matrix
dimnames(A_sp) <- list(ids, ids)

resid_vec <- residuals(mod_domination_phylo)[, "Estimate"]
ape::Moran.I(resid_vec, W_sp)     # spatial autocorrelation in residuals

## Plot ####

### Annotation ####

post <- as_draws_df(mod_domination_phylo)

# Slope on the original scale
# per SD of Ax1 (keeps numbers readable): b_z * sy
beta_draws <- post$b_Ax1_z * sy
beta_q <- quantile(beta_draws, c(0.5, 0.025, 0.975))

# R²: marginal = Ax1 only; conditional = Ax1 + phylogeny
r2_marg <- bayes_R2(mod_domination_phylo, re_formula = NA)
r2_cond <- bayes_R2(mod_domination_phylo)

# Phylogenetic signal
lambda_draws <- post$sd_phylo__Intercept^2 /
  (post$sd_phylo__Intercept^2 + post$sigma^2)
lam_q <- quantile(lambda_draws, c(0.5, 0.025, 0.975))

n <- nrow(dat)   # the 46 societies actually in the model

fmt <- function(q, d) sprintf(paste0("%.", d, "f [%.", d, "f, %.", d, "f]"), q[1], q[2], q[3])

label_text <- paste0(
  "R² marginal = ",    fmt(unlist(r2_marg[1, c("Estimate", "Q2.5", "Q97.5")]), 2), "\n",
  "R² conditional = ", fmt(unlist(r2_cond[1, c("Estimate", "Q2.5", "Q97.5")]), 2), "\n",
  "β = ",              fmt(beta_q, 3), " per SD of Ax1\n",
  "λ = ",              fmt(lam_q, 2), "\n",
  "n = ", n
)

### Plot ####

# Constants used when standardizing (compute from the same data the model was fit on)
mx <- mean(dat$Ax1);           sx <- sd(dat$Ax1)
my <- mean(dat$height_ratio);  sy <- sd(dat$height_ratio)

# Prediction grid on the ORIGINAL Ax1 scale, converted to z for the model
grid <- tibble(Ax1 = seq(min(dat$Ax1), max(dat$Ax1), length.out = 100)) |>
  mutate(Ax1_z = (Ax1 - mx) / sx)

pred <- grid |>
  tidybayes::add_epred_draws(mod_domination_phylo, re_formula = NA) |>   # population-level line
  mutate(height_pred = .epred * sy + my)                      # back-transform y

biplot_dimorphism_phylo <- ggplot(pred, aes(x = Ax1, y = height_pred)) +
  tidybayes::stat_lineribbon(.width = c(0.5, 0.95), alpha = 0.3, fill = "steelblue") +
  geom_point(data = dat, aes(y = height_ratio)) +
  annotate("text",
           x = min(dat$Ax1), y = max(dat$height_ratio),
           label = label_text, hjust = 0, vjust = 1, size = 3.2, family = "mono") +
  labs(x = "Axis 1", y = "Height ratio") +
  theme_classic()

ggsave("draft/fig/biplot_dimorphism_phylo.png", biplot_dimorphism_phylo)
