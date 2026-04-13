

# Correlations ####

# define your predictors and axes
predictors <- c("complexity", "agriculture_cereals", "agriculture_importance",
                "herding_importance", "herding_milk", "hunting",
                "warfare_intensity", "warfare_location", "matri",
                "creation_stories", "weaning", "taboo")

axes <- paste0("Axis.", 1:8)

# compute spearman correlations with p-values
cor_table <- map_dfr(predictors, function(pred) {
  map_dfc(axes, function(ax) {
    if (ax %in% names(pred_resp) & pred %in% names(pred_resp)) {
      test <- cor.test(pred_resp[[pred]], pred_resp[[ax]],
                       method = "spearman",
                       use = "complete.obs")
      tibble(!!ax := sprintf("%.2f%s",
                             test$estimate,
                             case_when(test$p.value < 0.001 ~ "***",
                                       test$p.value < 0.01  ~ "**",
                                       test$p.value < 0.05  ~ "*",
                                       test$p.value < 0.10  ~ "†",
                                       TRUE                 ~ "")))
    } else {
      tibble(!!ax := NA_character_)
    }
  }) %>%
    mutate(predictor = pred, .before = 1)
})

print(cor_table)

library(knitr)
library(kableExtra)

cor_table %>%
  kable(caption = "Spearman correlations between predictors and response PCoA axes",
        col.names = c("Predictor", paste0("Axis ", 1:8)),
        align = c("l", rep("c", 8))) %>%
  kable_styling(bootstrap_options = c("striped", "condensed"),
                full_width = FALSE) %>%
  footnote(general = "† p<0.10, * p<0.05, ** p<0.01, *** p<0.001")


# Models ####
library("brms")
run_models <- F

if (run_models){
  m_axis1 <- brm(Axis.1 ~ complexity + 
                   herding_importance + 
                   mo(matri_cat) + 
                   hunting,
                 data = pred_resp,
                 family = gaussian(),
                 prior = c(prior(normal(0, 1), class = b),
                           prior(normal(0, 1), class = Intercept),
                           prior(exponential(1), class = sigma),
                           prior(dirichlet(1), class = simo,
                                 coef = "momatri_cat1")),
                 cores = 4, iter = 4000, seed = 42)
  
  saveRDS(m_axis1, here::here("output/models/m_axis1"))
} else{
  m_axis1 <- readRDS(here::here("output/models/m_axis1"))
}

summary(m_axis1)
bayestestR::pd(m_axis1)

if (run_models){
  m_axis2 <- brm(Axis.2 ~ hunting + 
                   agriculture_importance,
                 data = pred_resp,
                 family = gaussian(),
                 prior = c(prior(normal(0, 1), class = b),
                           prior(normal(0, 1), class = Intercept),
                           prior(exponential(1), class = sigma)),
                 cores = 4, iter = 4000, seed = 42)
  
  saveRDS(m_axis2, here::here("output/models/m_axis2"))
} else{
  m_axis2 <- readRDS(here::here("output/models/m_axis2"))
}



# convert to ordered factors first — brms requires this for mo()
pred_resp <- pred_resp %>%
  mutate(
    creation_stories_ord = ordered(creation_stories),
    weaning_ord          = ordered(weaning),
    taboo_ord            = ordered(taboo)
  )



if (run_models){
  # creation stories (N=53)
  m_creation <- brm(Axis.1 ~ mo(creation_stories_ord),
                    data = pred_resp,
                    family = gaussian(),
                    prior = c(prior(normal(0, 1), class = b),
                              prior(normal(0, 1), class = Intercept),
                              prior(dirichlet(1), class = simo, 
                                    coef = "mocreation_stories_ord1")),
                    cores = 4, iter = 4000, seed = 42)
  
  
  m_creation_controlled <- brm(
    Axis.1 ~ mo(creation_stories_ord) + complexity,
    data = pred_resp,
    family = gaussian(),
    prior = c(prior(normal(0, 1), class = b),
              prior(normal(0, 1), class = Intercept),
              prior(exponential(1), class = sigma),
              prior(dirichlet(1), class = simo,
                    coef = "mocreation_stories_ord1")),
    cores = 4, iter = 4000, seed = 42)
  
  m_creation_full <- brm(
    Axis.1 ~ mo(creation_stories_ord) + complexity + mo(matri_cat),
    data = pred_resp,
    family = gaussian(),
    prior = c(prior(normal(0, 1), class = b),
              prior(normal(0, 1), class = Intercept),
              prior(exponential(1), class = sigma),
              prior(dirichlet(1), class = simo,
                    coef = "mocreation_stories_ord1"),
              prior(dirichlet(1), class = simo,
                    coef = "momatri_cat1")),
    cores = 4, iter = 4000, seed = 42)
  
  
  # matri alone on N=53
  m_matri_53 <- brm(
    Axis.1 ~ mo(matri_cat),
    data = pred_resp %>% filter(!is.na(creation_stories)),
    family = gaussian(),
    prior = c(prior(normal(0, 1), class = b),
              prior(normal(0, 1), class = Intercept),
              prior(exponential(1), class = sigma),
              prior(dirichlet(1), class = simo,
                    coef = "momatri_cat1")),
    cores = 4, iter = 4000, seed = 42)
  
  
  
  # weaning (N=72)
  m_weaning <- brm(Axis.1 ~ mo(weaning_ord),
                   data = pred_resp,
                   family = gaussian(),
                   prior = c(prior(normal(0, 1), class = b),
                             prior(normal(0, 1), class = Intercept),
                             prior(dirichlet(1), class = simo,
                                   coef = "moweaning_ord1")),
                   cores = 4, iter = 4000, seed = 42)
  
  # taboo (N=50)
  m_taboo <- brm(Axis.1 ~ mo(taboo_ord),
                 data = pred_resp,
                 family = gaussian(),
                 prior = c(prior(normal(0, 1), class = b),
                           prior(normal(0, 1), class = Intercept),
                           prior(dirichlet(1), class = simo,
                                 coef = "motaboo_ord1")),
                 cores = 4, iter = 4000, seed = 42)
  
  saveRDS(m_creation, here::here("output/models/m_creation"))
  saveRDS(m_creation_controlled, here::here("output/models/m_creation_controlled"))
  saveRDS(m_creation_full, here::here("output/models/m_creation_full"))
  saveRDS(m_matri_53, here::here("output/models/m_matri_53"))
  
  saveRDS(m_weaning, here::here("output/models/m_weaning"))
  saveRDS(m_taboo, here::here("output/models/m_taboo"))
} else{
  m_creation <-  readRDS(here::here("output/models/m_creation"))
  m_creation_controlled <-  readRDS(here::here("output/models/m_creation_controlled"))
  m_creation_full <-  readRDS(here::here("output/models/m_creation_full"))
  m_creation_53 <-  readRDS(here::here("output/models/m_matri_53"))
  
  m_weaning <-  readRDS(here::here("output/models/m_weaning"))
  m_taboo <-  readRDS(here::here("output/models/m_taboo"))
}

# quick summary of all three
map(list(creation = m_creation, 
         weaning  = m_weaning, 
         taboo    = m_taboo), 
    ~ posterior_summary(.x) %>% 
      as.data.frame() %>% 
      rownames_to_column("parameter") %>%
      filter(str_detect(parameter, "^b_")))

# visualize the monotonic effect
conditional_effects(m_creation)
conditional_effects(m_weaning)
conditional_effects(m_taboo)

summary(m_creation_controlled)
bayestestR::pd(m_creation_controlled)

pp_check(m_creation_full)
bayes_R2(m_creation_full)

summary(m_creation_full)
bayestestR::pd(m_creation_full)

cor.test(as.numeric(pred_resp$matri_cat), 
         as.numeric(pred_resp$creation_stories_ord),
         method = "spearman")