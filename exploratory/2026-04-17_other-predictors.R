pcoa_result <-  readRDS(here::here("output/pcoa_result"))

coords_df <- as.data.frame(pcoa_result$vectors[, 1:10])
colnames(coords_df) <- paste0("Ax", 1:10)
coords_df$soc_id <- rownames(coords_df)

coords_df <- coords_df %>%
  left_join(societies %>% select(soc_id, pref_name_for_society, 
                                 origLat, origLong),
            by = "soc_id")


whyte_societies <- sccs %>% 
  filter(var_id %in% whyte_vars$var_id) %>% 
  filter(!is.na(code)) %>% 
  pull(soc_id) %>% 
  unique()

class_variables <- c(
  "SCCS272", # caste differentiation NOT INFORMATIVE, most data points = absence of distinction
  "SCCS270", # class diffrentiation
  "SCCS711", # social complexity
  "SCCS707", # pottery
  "SCCS706", # metalworking
  "SCCS708", # weaving
  "SCCS681", # irrigation
  "SCCS680" # plow
)

class_data <- sccs %>% 
  filter(var_id %in% class_variables) 




library(tidyverse)

# Pivot the SCCS variables wide and join to coords_df
model_data <- class_data %>%
  select(soc_id, var_id, code) %>%
  pivot_wider(names_from = var_id, values_from = code) %>%
  right_join(coords_df, by = "soc_id")

model_data %>% 
  ggplot(aes(x = SCCS711, y = SCCS270)) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  ggtitle("social classes as a function of social complexity")


model_data %>% 
  ggplot(aes(x = as.factor(SCCS707), y = Ax1)) +
  geom_boxplot() +
  geom_point() +
  xlab("Pottery")

model_data %>% 
  ggplot(aes(x = as.factor(SCCS706), y = Ax1)) +
  geom_boxplot() +
  geom_point() +
  xlab("Metalworking")

model_data %>% 
  ggplot(aes(x = as.factor(SCCS708), y = Ax1)) +
  geom_boxplot() +
  geom_point() +
  xlab("Weaving")

fit <- lm(Ax1 ~ SCCS708, data = model_data)
summary(fit)

model_data %>% 
  ggplot(aes(x = as.factor(SCCS681), y = Ax1)) +
  geom_boxplot() +
  geom_point() +
  xlab("Irrigation")

fit <- lm(Ax1 ~ SCCS681, data = model_data)
summary(fit)

model_data %>% 
  ggplot(aes(x = as.factor(SCCS680), y = Ax1)) +
  geom_boxplot() +
  geom_point() +
  xlab("Plow")

fit <- lm(Ax1 ~ SCCS680, data = model_data)
summary(fit)

model_data %>% 
  ggplot(aes(x = SCCS270, y = Ax1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Social classes")

model_data %>% 
  ggplot(aes(x = SCCS680, y = SCCS708)) +
  geom_jitter() +
  xlab("Plough") +
  ylab("weaving")

# Treat as numeric (continuous) — change to factor() if you want categorical
model_data <- model_data %>%
  mutate(across(all_of(class_variables), as.numeric))


# test the sequantial addition of technologies ####

library(tidyverse)

# The Guttman sequence from simplest to most complex
# pottery -> metalworking -> weaving -> irrigation -> plow
guttman_seq <- c("SCCS707", "SCCS706", "SCCS708", "SCCS681", "SCCS680")

model_data_complete <- model_data %>% 
  select(Ax1, all_of(guttman_seq)) %>% 
  drop_na()

# Then refit m0-m5 using data = model_data_complete

# Fit cumulative models, each adding one technology
m0 <- lm(Ax1 ~ 1,                                                        data = model_data_complete)
m1 <- lm(Ax1 ~ SCCS707,                                                   data = model_data_complete)
m2 <- lm(Ax1 ~ SCCS707 + SCCS706,                                         data = model_data_complete)
m3 <- lm(Ax1 ~ SCCS707 + SCCS706 + SCCS708,                               data = model_data_complete)
m4 <- lm(Ax1 ~ SCCS707 + SCCS706 + SCCS708 + SCCS681,                     data = model_data_complete)
m5 <- lm(Ax1 ~ SCCS707 + SCCS706 + SCCS708 + SCCS681 + SCCS680,           data = model_data_complete)

# Compare with ANOVA — tests whether each addition is significant
anova(m0, m1, m2, m3, m4, m5)


models <- list(m0, m1, m2, m3, m4, m5)
labels <- c("null", "pottery", "+ metalworking", "+ weaving", "+ irrigation", "+ plow")

map2_dfr(models, labels, ~ tibble(
  model    = .y,
  r2       = summary(.x)$r.squared,
  adj_r2   = summary(.x)$adj.r.squared,
  n        = nobs(.x)
))


library(broom)

map2_dfr(models, labels, ~ tidy(.x) %>% mutate(model = .y)) %>%
  filter(term != "(Intercept)") %>%
  select(model, term, estimate, std.error, p.value) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))
# How to READ IT
# Row 1 = model 1 (pottery)
# Row 2: coefficient for pottery in model 2
# Row 3 = coefficient for metalworking in model 2
# Row 4 = coefficient for pottery in model 3


# In the pottery-only model, pottery has estimate -0.025, p = 0.27 — weak and non-significant
# Once weaving enters (model 4), pottery's coefficient collapses to -0.003 and becomes completely noise (p = 0.91)
# 
# This means pottery was acting as a weak proxy for weaving — it had a faint negative relationship with Axis 1 only because pottery-present societies are more likely to also have weaving. Once you control for weaving directly, pottery has zero independent effect.

