

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