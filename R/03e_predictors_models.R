




# Models ####
library("brms")
run_models <- F

if (run_models){
  m_axis1 <- brm(Axis.1 ~ complexity + mo(matri_cat),
                 data = pred_resp %>% drop_na(Axis.1, complexity, matri_cat),
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

# if (run_models){
#   m_axis2 <- brm(Axis.2 ~ hunting + 
#                    agriculture_importance,
#                  data = pred_resp,
#                  family = gaussian(),
#                  prior = c(prior(normal(0, 1), class = b),
#                            prior(normal(0, 1), class = Intercept),
#                            prior(exponential(1), class = sigma)),
#                  cores = 4, iter = 4000, seed = 42)
#   
#   saveRDS(m_axis2, here::here("output/models/m_axis2"))
# } else{
#   m_axis2 <- readRDS(here::here("output/models/m_axis2"))
# }



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