pred_resp %>% 
  ggplot(aes(x = complexity, y = weaning)) +
  geom_point() +
  geom_smooth()

pred_resp %>% 
  select(complexity, weaning, taboo) %>% 
  str()

cor.test(pred_resp$complexity, pred_resp$weaning, method = "spearman")

table(pred_resp$weaning, useNA = "always")
table(pred_resp$taboo, useNA = "always")

mod_weaning <- brm(
  weaning ~ complexity,
  family = cumulative("logit"),
  prior = c(prior(normal(0, 1.5), class = Intercept),  # threshold priors
            prior(normal(0, 0.5), class = b)),
  data = pred_resp,
  cores = 4, iter = 4000, seed = 42
)

summary(mod_weaning)
exp(fixef(mod_weaning)["complexity", "Estimate"])


mod_taboo <- brm(
  taboo ~ complexity,
  family = cumulative("logit"),
  prior = c(prior(normal(0, 1.5), class = Intercept),
            prior(normal(0, 0.5), class = b)),
  data = pred_resp,
  cores = 4, iter = 4000, seed = 42
)
