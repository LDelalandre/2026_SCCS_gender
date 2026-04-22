pcoa_result <-  readRDS(here::here("output/pcoa_result"))

coords_df <- as.data.frame(pcoa_result$vectors[, 1:10])
colnames(coords_df) <- paste0("Ax", 1:10)
coords_df$soc_id <- rownames(coords_df)

coords_df <- coords_df %>%
  left_join(societies %>% dplyr::select(soc_id, pref_name_for_society, 
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
  "SCCS680", # plow
  "SCCS128", # loom weaving, Murdock: who does it
  "SCCS711 # complexity scale"
)

class_data <- sccs %>% 
  filter(var_id %in% class_variables) 




library(tidyverse)

# Pivot the SCCS variables wide and join to coords_df
model_data <- class_data %>%
  dplyr::select(soc_id, var_id, code) %>%
  pivot_wider(names_from = var_id, values_from = code) %>%
  right_join(coords_df, by = "soc_id") %>%
  right_join(pred_resp, by = "soc_id")

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
# model_data <- model_data %>%
#   mutate(across(all_of(class_variables), as.numeric))


# test the sequantial addition of technologies ####

library(tidyverse)

# The Guttman sequence from simplest to most complex
# pottery -> metalworking -> weaving -> irrigation -> plow
guttman_seq <- c("SCCS707", "SCCS706", "SCCS708", "SCCS681", "SCCS680")

model_data_complete <- model_data %>% 
  dplyr::select(Ax1, all_of(guttman_seq)) %>% 
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


# Links between complexity and sexual division of labor in different domains ####


technology_variables <- c(
  "SCCS128" # loom weaving
)

technology_data <- sccs %>% 
  filter(var_id %in% technology_variables) 

technology_data <- technology_data %>%
  select(soc_id, var_id, code) %>%
  pivot_wider(names_from = var_id, values_from = code) %>%
  right_join(pred_resp, by = "soc_id")

technology_data %>% 
  ggplot(aes(x = complexity, y = SCCS128)) +
  geom_point()


technology_data <- technology_data %>%
  mutate(SCCS128 = factor(SCCS128,
                          levels = c(-1, 0, 1, 3, 4, 5),
                          labels = c("present?", "absent", "male", "equal", "female mostly", "female only")
  ))

ggplot(technology_data, aes(x = complexity, y = SCCS128)) +
  geom_jitter(height = 0.2, width = 0)


# Weaning presence or who does it ####

# Add SCCS128 to your model_data from the sccs data
# Then test whether who-does-weaving predicts Ax1
# better than whether-weaving-is-present

fit_presence <- lm(Ax1 ~ SCCS708, data = model_data)
fit_who      <- lm(Ax1 ~ SCCS128, data = model_data)

# Or jointly
fit_both     <- lm(Ax1 ~ SCCS708 + SCCS128, data = model_data)

# And the interaction: does the effect of weaving presence
# depend on who does it?
fit_interact <- lm(Ax1 ~ SCCS708 * SCCS128, data = model_data)

summary(fit_both)


# Among societies that have loom weaving, does male appropriation of it predict higher male domination?
model_data %>%
  filter(SCCS708 == 2, !is.na(SCCS128)) %>%
  lm(Ax1 ~ SCCS128, data = .) %>%
  summary()


# Techn innovation Guttman scale ####

library(tidyverse)

# The five items in Guttman order
guttman_items <- c("SCCS707", "SCCS706", "SCCS708", "SCCS681", "SCCS680")
# pottery, metalworking, weaving, irrigation, plow

# First, recode to binary presence/absence (assuming 2 = present, 1 = absent)
guttman_data <- model_data %>%
  dplyr::select(soc_id, all_of(guttman_items)) %>%
  mutate(across(all_of(guttman_items), ~ case_when(
    . == 2 ~ 1,
    . == 1 ~ 0,
    TRUE   ~ NA_real_
  ))) %>%
  rename(pottery = SCCS707, metalworking = SCCS706, 
         weaving = SCCS708, irrigation = SCCS681, plow = SCCS680)

# Frequency of each item — should decrease in order
guttman_data %>%
  summarise(across(pottery:plow, ~ sum(., na.rm = TRUE)))

# Cross-tabulate adjacent pairs to see if the order holds
# e.g. can you have metalworking without pottery?
guttman_data %>% count(pottery, metalworking)
guttman_data %>% count(metalworking, weaving)
guttman_data %>% count(weaving, irrigation)
guttman_data %>% count(irrigation, plow)



# Scale score = sum of present items
guttman_data <- guttman_data %>%
  mutate(guttman_score = rowSums(across(pottery:plow), na.rm = FALSE))

# Check the pattern of responses per score
# In a perfect scale, score 1 = pottery only, score 2 = pottery+metalworking, etc.
guttman_data %>%
  dplyr::select(pottery:plow, guttman_score) %>%
  arrange(guttman_score) %>%
  print(n = Inf)

# Coefficient of scalability (Menzel's formula)
# CS = 1 - (observed errors / expected errors)
# An error = having a harder item without an easier one

compute_cs <- function(df, items) {
  df_complete <- df %>% dplyr::select(all_of(items)) %>% drop_na()
  n <- nrow(df_complete)
  k <- length(items)
  
  # Item marginals
  p <- colMeans(df_complete)
  
  # Expected errors under marginal independence
  expected_errors <- sum(p * (1 - p)) * n  # simplified version
  
  # Observed errors: for each pair in order, count violations
  observed_errors <- 0
  for (i in 1:(k-1)) {
    for (j in (i+1):k) {
      # Error: item j present but item i absent (harder item without easier)
      violations <- sum(df_complete[[i]] == 0 & df_complete[[j]] == 1, na.rm = TRUE)
      observed_errors <- observed_errors + violations
    }
  }
  
  cs <- 1 - (observed_errors / expected_errors)
  
  list(
    observed_errors = observed_errors,
    expected_errors = expected_errors,
    cs = cs
  )
}

compute_cs(guttman_data, c("pottery", "metalworking", "weaving", "irrigation", "plow"))



guttman_data %>%
  mutate(
    perfect_pattern = case_when(
      is.na(pottery) | is.na(metalworking) | 
        is.na(weaving) | is.na(irrigation) | is.na(plow) ~ "missing",
      # perfect patterns only
      pottery==0 & metalworking==0 & weaving==0 & irrigation==0 & plow==0 ~ "0: none",
      pottery==1 & metalworking==0 & weaving==0 & irrigation==0 & plow==0 ~ "1: pottery",
      pottery==1 & metalworking==1 & weaving==0 & irrigation==0 & plow==0 ~ "2: +metal",
      pottery==1 & metalworking==1 & weaving==1 & irrigation==0 & plow==0 ~ "3: +weaving",
      pottery==1 & metalworking==1 & weaving==1 & irrigation==1 & plow==0 ~ "4: +irrigation",
      pottery==1 & metalworking==1 & weaving==1 & irrigation==1 & plow==1 ~ "5: +plow",
      TRUE ~ "error pattern"
    )
  ) %>%
  count(perfect_pattern)


## SUbscale without pottery and metalworking ####
# Test the three-item subscale
compute_cs(guttman_data, c("weaving", "irrigation", "plow"))

# And crosstabs are already suggestive:
# weaving -> irrigation: 5 violations
# irrigation -> plow: 3 violations
# weaving -> plow directly:
guttman_data %>% count(weaving, plow)



guttman_data <- guttman_data %>%
  mutate(
    guttman_3 = rowSums(across(c(weaving, irrigation, plow)), na.rm = FALSE)
  )

model_data <- model_data %>%
  left_join(guttman_data %>% dplyr::select(soc_id, guttman_3), by = "soc_id")

model_data_all <- model_data %>%
  left_join(predictors_wide %>% rownames_to_column("soc_id"), by = "soc_id") %>% 
  left_join(whyte_data_wide, by = "soc_id")

summary(lm(Ax1 ~ guttman_3, data = model_data))

summary(lm(Ax1 ~ SCCS711 + guttman_3, data = model_data))

summary(lm(Ax1 ~ complexity + SCCS708, data = model_data)) # weaving
summary(lm(Ax1 ~ complexity + SCCS680, data = model_data)) # plow
summary(lm(Ax1 ~ complexity + SCCS681, data = model_data)) # irrigation

# Does hunting or agriculture_importance confound the guttman_3 -> Ax1 relationship?
summary(lm(Ax1 ~ guttman_3 + hunting + agriculture_importance, 
           data = model_data))


summary(lm(Ax2 ~ complexity + guttman_3, data = model_data))

summary(lm(Ax2 ~ hunting , data = model_data))
summary(lm(Ax2 ~ hunting + agriculture_importance, data = model_data))
summary(lm(Ax2 ~ hunting + agriculture_importance + agriculture_cereals, data = model_data))

model_data %>% 
  ggplot(aes(x = hunting, y = agriculture_importance)) +
  geom_point() +
  geom_smooth(method = "lm")

# Correlation between complexity and guttman_3
cor(model_data$complexity, model_data$guttman_3, use = "complete.obs")


summary(lm(Ax2 ~ guttman_3 , data = model_data))

# And variance inflation factors in the joint model
library(car)
vif(lm(Ax1 ~ complexity + guttman_3, data = model_data))




### Bigger DAG

dag_full <- dagitty('dag {
  
  # Subsistence variables
  hunting -> Ax1
  hunting -> Ax2
  hunting -> guttman_3
  
  agriculture_importance -> Ax1
  agriculture_importance -> Ax2
  agriculture_importance -> guttman_3
  
  herding_importance -> Ax1
  herding_importance -> Ax2
  herding_importance -> guttman_3
  
  # Technology
  guttman_3 -> Ax1
  guttman_3 -> Ax2
  
  # Complexity
  complexity -> Ax1
  guttman_3 -> complexity
  agriculture_importance -> complexity
  herding_importance -> complexity
  
  # Warfare
  warfare_intensity -> Ax1
  warfare_location -> Ax1
  warfare_intensity -> warfare_location
  
  # Kinship
  kinship -> Ax1
  kinship -> Ax2
  guttman_3 -> kinship
  agriculture_importance -> kinship
  herding_importance -> kinship
  
  # Axis relationship
  Ax2 -> Ax1
}')

# Basic plot
ggdag(dag_full) + theme_dag()


## Herding ####


model_data_all %>% 
  group_by(herding_importance) %>%
  summarise(
    mean_settlement = mean(SCCS705, na.rm = TRUE),
    n = n()
  ) %>% print(n = 48)

# Does herding predict Ax1 independently of guttman_3?
summary(lm(Ax1 ~ <- + herding_importance, data = model_data))

# Are herding and guttman_3 correlated?
cor(model_data$herding_importance, model_data$guttman_3, 
    use = "complete.obs")


## Boserup mediation ####
# Proper Boserupian mediation test using raw variables
library(mediation)

# Does labour segregation (DV14) mediate between
# guttman_3 and belief in female inferiority (DV51)?
fit_med <- lm(SCCS589 ~ guttman_3, data = model_data_all) # segregation in subsistence
fit_out <- lm(SCCS626 ~ guttman_3 + SCCS589, data = model_data_all) # women inferior to men

med_result <- mediate(fit_med, fit_out,
                      treat = "guttman_3",
                      mediator = "SCCS589",
                      boot = TRUE, sims = 1000)
summary(med_result)




boserup_vars <- c("SCCS585", "SCCS586", "SCCS589", "SCCS595", "SCCS592","SCCS593","SCCS594")
# SCCS585 — Proportional Contribution of Women to Overall Subsistence
# SCCS586 — Relative Time and Effort on Subsistence Activities
# SCCS589 — Degree of Segregation in Subsistence Activities
# SCCS595 — Men's Domestic Work
# SCCS592 — Control of fruits of male labour
# SCCS593 — Control of fruits of joint labour
# SCCS594 — Control of fruits of female labour

# Pull from sccs data if not already in model_data
boserup_data <- sccs %>%
  filter(var_id %in% boserup_vars) %>%
  dplyr::select(soc_id, var_id, code) %>%
  pivot_wider(names_from = var_id, values_from = code)

model_data <- model_data %>%
  left_join(boserup_data, by = "soc_id")

# Test each directly
map_dfr(boserup_vars, ~ {
  formula <- as.formula(paste(.x, "~ guttman_3"))
  fit <- lm(formula, data = model_data)
  s <- summary(fit)
  tibble(
    outcome = .x,
    estimate = coef(s)["guttman_3", "Estimate"],
    p_value = coef(s)["guttman_3", "Pr(>|t|)"],
    r2 = s$r.squared
  )
})

### Property inheritance ####
# Is property inheritance a mediator between guttman_3 and ideology?
# Pull SCCS590 (inheritance of property)
summary(lm(SCCS590 ~ guttman_3, data = model_data_all))

# Does it mediate guttman_3 -> DV51?
fit_med <- lm(SCCS590 ~ guttman_3, data = model_data_all)
fit_out <- lm(SCCS626 ~ guttman_3 + SCCS590, data = model_data_all)


med_result <- mediate(fit_med, fit_out,
                      treat = "guttman_3",
                      mediator = "SCCS590",
                      boot = TRUE, sims = 1000)
summary(med_result)

### maybe more the domination variables

summary(lm(SCCS615 ~ guttman_3, data = model_data_all)) # wife institutional deference
summary(lm(SCCS621 ~ guttman_3, data = model_data_all)) # men should dominate
summary(lm(SCCS626 ~ guttman_3, data = model_data_all)) # belief inferior
summary(lm(SCCS611 ~ guttman_3, data = model_data_all)) # esae of remarriage

# BUT non-significant have missing data points! Which can be non-random

# Who is missing on SCCS621?
model_data_all %>%
  mutate(missing_621 = is.na(SCCS621)) %>%
  group_by(missing_621) %>%
  summarise(
    mean_guttman = mean(guttman_3, na.rm = TRUE),
    mean_ax1 = mean(Ax1, na.rm = TRUE),
    n = n()
  )

# Engels

summary(lm(SCCS605 ~ guttman_3, data = model_data_all))

summary(lm(matri ~ guttman_3, data = model_data_all))
summary(lm(matri ~ herding_importance, data = model_data_all))


# Herding, complexity, and technologies
summary(lm(Ax1 ~ complexity , data = model_data_all))
summary(lm(Ax1 ~ herding_importance , data = model_data_all))
summary(lm(Ax1 ~ complexity + herding_importance, data = model_data_all))

summary(lm(Ax1 ~ complexity + guttman_3, data = model_data_all))
summary(lm(Ax1 ~ herding_importance + guttman_3, data = model_data_all))

summary(lm(Ax1 ~ agriculture_cereals , data = model_data_all))
summary(lm(Ax1 ~ agriculture_cereals + guttman_3, data = model_data_all))

## More axes ####
# Test guttman_3 against all 10 axes
axes <- paste0("Ax", 1:10)

map_dfr(axes, ~ {
  formula <- as.formula(paste(.x, "~ guttman_3"))
  fit <- lm(formula, data = model_data)
  s <- summary(fit)
  tibble(
    axis = .x,
    estimate = coef(s)["guttman_3", "Estimate"],
    p_value = coef(s)["guttman_3", "Pr(>|t|)"],
    r2 = s$r.squared
  )
}) %>%
  mutate(significant = p_value < 0.05)

## DAGs ####
library(dagitty)

library(dagitty)
library(ggdag)
library(ggplot2)


### Simple DAGs ####

# 1) Boserup
dag_boserup <- dagitty('dag {
  complexity -> guttman_3
  guttman_3 -> Ax2
  Ax2 -> Ax1
  complexity -> Ax1
}')

# 2) complexity
dag_complexity <- dagitty('dag {
  complexity -> guttman_3
  guttman_3 -> Ax1
  complexity -> Ax1
}')


fdag <- dag_boserup

plot(fdag)  
ggdag(fdag) + theme_dag()



# Are these Markov equivalent?
# i.e. do they imply the same conditional independencies?
impliedConditionalIndependencies(dag_tech_first)
impliedConditionalIndependencies(dag_complexity_first)


## DAG Axis 1 ####

library(dagitty)

# I put ecology upstream because herding and agri_cereals are negatively correlated

# DAG A: technology drives complexity
dag_A_revised <- dagitty('dag {
  ecology -> agri_cereals
  ecology -> herding_importance
  agri_cereals -> guttman_3
  herding_importance -> guttman_3
  guttman_3 -> complexity
  guttman_3 -> Ax1
  complexity -> Ax1
}')

dag_B_revised <- dagitty('dag {
  ecology -> agri_cereals
  ecology -> herding_importance
  agri_cereals -> complexity
  herding_importance -> complexity
  complexity -> guttman_3
  guttman_3 -> Ax1
}')

dag_C_revised <- dagitty('dag {
  ecology -> agri_cereals
  ecology -> herding_importance
  agri_cereals -> complexity
  agri_cereals -> guttman_3
  herding_importance -> complexity
  herding_importance -> guttman_3
  complexity -> Ax1
  guttman_3 -> Ax1
}')

plot(dag_A_revised)
plot(dag_B_revised)
plot(dag_C_revised)

  impliedConditionalIndependencies(dag_A_revised)
impliedConditionalIndependencies(dag_B_revised)
impliedConditionalIndependencies(dag_C_revised)

# First, I cannot test ecology

# then how to discriminate among dags?
# TEST 1: Ax1 _||_ agr_ | gt_3
# Appears in DAG A and B but NOT C
# DAG C predicts agri_cereals STILL affects Ax1 after guttman_3
summary(lm(Ax1 ~ guttman_3 + agriculture_cereals, 
           data = model_data_all))
# If agri_cereals non-significant -> consistent with A and B, against C

# TEST 2: Ax1 _||_ cmpl | gt_3  
# Appears in DAG B only
# Already tested — YOU KNOW THIS IS SATISFIED
# This is evidence FOR DAG B

# TEST 3: agr_ _||_ cmpl | gt_3
# Appears in DAG A only
# DAG A predicts agriculture and complexity are independent once guttman_3 controlled
summary(lm(complexity ~ guttman_3 + agriculture_cereals,
           data = model_data_all))
# If agri_cereals non-significant -> consistent with DAG A

# TEST 4: agr_ _||_ gt_3 | cmpl
# Appears in DAG B only
# DAG B predicts agriculture drops out once complexity controlled
summary(lm(guttman_3 ~ complexity + agriculture_cereals,
           data = model_data_all))
# If agri_cereals non-significant -> consistent with DAG B

# TEST 5: cmpl _||_ gt_3 | agr_, hrd_
# Appears in DAG C only
# DAG C predicts complexity and guttman_3 independent given subsistence
summary(lm(guttman_3 ~ agriculture_cereals + herding_importance + complexity,
           data = model_data_all))
# If complexity_clean non-significant -> consistent with DAG C



model_data_all %>% 
  filter(!is.na(guttman_3)) %>% 
  ggplot(aes(x = as.factor(guttman_3), y = Ax1)) +
  geom_boxplot() +
  geom_point()

### With matri ####
dag_B_final <- dagitty('dag {
  ecology -> agriculture_cereals
  ecology -> herding_importance
  agriculture_cereals -> complexity
  herding_importance -> complexity
  complexity -> guttman_3
  guttman_3 -> matri
  guttman_3 -> Ax1
  matri -> Ax1
}')

plot(dag_B_final)
impliedConditionalIndependencies(dag_B_final)

# Run all seven
t1 <- summary(lm(Ax1 ~ complexity + agriculture_cereals, 
                 data = model_data_all))

t2 <- summary(lm(Ax1 ~ guttman_3 + agriculture_cereals,
                 data = model_data_all))

t3 <- summary(lm(Ax1 ~ guttman_3 + complexity,
                 data = model_data_all))  # already done

t4 <- summary(lm(Ax1 ~ complexity + herding_importance,
                 data = model_data_all))

t5 <- summary(lm(Ax1 ~ guttman_3 + herding_importance,
                 data = model_data_all))  # already done

t6 <- summary(lm(guttman_3 ~ complexity + agriculture_cereals,
                 data = model_data_all))

t7 <- summary(lm(guttman_3 ~ complexity + herding_importance,
                 data = model_data_all))

# Fits all the preditctions


# And considering matri
# Does agriculture predict matri once guttman_3 controlled?
summary(lm(matri ~ guttman_3 + agriculture_cereals, data = model_data_all))

# Does agriculture predict matri once complexity controlled?
summary(lm(matri ~ complexity + agriculture_cereals, data = model_data_all))

# Does complexity predict matri once guttman_3 controlled?
summary(lm(matri ~ guttman_3 + complexity, data = model_data_all))

# Does herding predict matri once guttman_3 controlled?
summary(lm(matri ~ guttman_3 + herding_importance, data = model_data_all))

# Does herding predict matri once complexity controlled?
summary(lm(matri ~ complexity + herding_importance, data = model_data_all))

# And the key causal question — does matri predict Ax1 after guttman_3?
# This tests whether matri is a mediator
summary(lm(Ax1 ~ guttman_3 + matri, data = model_data_all))
# DAG violation, matri was just a mediator

# Test mediation
library(mediation)

fit_med <- lm(matri ~ guttman_3, data = model_data_all)
fit_out <- lm(Ax1 ~ guttman_3 + matri, data = model_data_all)

med_result <- mediate(fit_med, fit_out,
                      treat = "guttman_3",
                      mediator = "matri",
                      boot = TRUE, sims = 1000)
summary(med_result)
# ACME = indirect, mediated effect. Exists if significant.
# ADE = direct effect. Here, most of the effect is direct
# Suggests that most of the effect is not mediated


### Relation between complexity and guttman ####
# Does complexity predict guttman_3 after subsistence variables?
summary(lm(guttman_3 ~ complexity + agriculture_cereals + 
             herding_importance, data = model_data_all))

# Does guttman_3 predict complexity after subsistence variables?
summary(lm(complexity ~ guttman_3 + agriculture_cereals + 
             herding_importance, data = model_data_all))

# As the agriculture and herding effect drop when complexity is a predictor,
# I can put complexity upstream of guttman3

### With complexity downstream ####


# Does Ax1 predict complexity?
summary(lm(complexity ~ Ax1, data = model_data_all))

# Does Ax1 predict complexity after guttman_3?
summary(lm(complexity ~ guttman_3 + Ax1, data = model_data_all))


# Are these equivalent?
dag_complexity_cause <- dagitty('dag {
  guttman_3 -> complexity
  guttman_3 -> Ax1
}')

dag_complexity_effect <- dagitty('dag {
  guttman_3 -> Ax1
  Ax1 -> complexity
}')

impliedConditionalIndependencies(dag_complexity_cause)
impliedConditionalIndependencies(dag_complexity_effect)



## Connect axis 1 and 2 ####

#### From graph 1 to hunting ####
summary(lm(hunting ~ agriculture_cereals, 
           data = model_data_all))

summary(lm(hunting ~ herding_importance, 
           data = model_data_all))

summary(lm(hunting ~ complexity, 
           data = model_data_all))

summary(lm(hunting ~ guttman_3, 
           data = model_data_all))

summary(lm(hunting ~ matri, 
           data = model_data_all))

summary(lm(hunting ~ Ax1, 
           data = model_data_all))

#### From graph 1 to agriculture_importance ####
summary(lm(agriculture_importance ~ guttman_3, 
           data = model_data_all))

summary(lm(agriculture_importance ~ complexity, 
           data = model_data_all))
# Here, complexity is the best predictor!

summary(lm(agriculture_importance ~ herding_importance, 
           data = model_data_all))

# Show that complexity predicts Ax2 once you control
# for both hunting and agriculture_importance simultaneously
summary(lm(Ax2 ~ complexity + hunting + agriculture_importance,
           data = model_data_all))

# And show the two pathways separately
summary(lm(Ax2 ~ hunting, data = model_data_all))
summary(lm(Ax2 ~ agriculture_importance, data = model_data_all))
summary(lm(Ax2 ~ complexity, data = model_data_all))

dag_Ax2_correct <- dagitty('dag {
  ecology -> hunting
  ecology -> agriculture_importance
  ecology -> herding_importance
  herding_importance -> hunting
  agriculture_cereals -> hunting
  complexity -> hunting
  complexity -> agriculture_importance
  hunting -> Ax2
  agriculture_importance -> Ax2
}')

plot(dag_Ax2_correct)

# From graph 1 to axis 2
summary(lm(Ax2 ~ herding_importance, 
           data = model_data_all))

# Markov equivalent graphs ####
equiv <- equivalentDAGs(dag_B_final)
for (i in length(equiv)){
  plot(equiv[[i]])
}
i <- 6
plot(equiv[[i]])


# Creation stories, warfare ####

summary(lm(Ax1 ~ creation_stories + guttman_3, 
           data = model_data_all))


# Is creation_stories predicted by guttman_3?
summary(lm(creation_stories ~ guttman_3, data = model_data_all))

# Is it predicted by Ax1?
summary(lm(creation_stories ~ Ax1, data = model_data_all))

# Does it add to Ax1 prediction after guttman_3 and matri?
summary(lm(Ax1 ~ guttman_3 + matri + creation_stories,
           data = model_data_all))


# Other axes ####

summary(lm(Ax3 ~ matri + guttman_3, 
           data = model_data_all))


