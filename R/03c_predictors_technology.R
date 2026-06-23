technology_variables <- predictors_detailed %>% 
  filter(category == "technology") %>% 
  pull(var_id)


# Pivot the SCCS variables wide and join to coords_df
model_data <- sccs %>% 
  filter(var_id %in% technology_variables)  %>%
  dplyr::select(soc_id, var_id, code) %>%
  pivot_wider(names_from = var_id, values_from = code) %>%
  right_join(coords_df, by = "soc_id") %>%
  right_join(pred_resp, by = "soc_id") 

technology_data <- model_data %>%
  dplyr::select(soc_id, all_of(technology_variables)) %>%
  mutate(across(all_of(technology_variables), ~ case_when(
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


## Subscale without pottery and metalworking ####
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


pred_resp %>% 
  left_join(guttman_data %>% select(soc_id, guttman_3))
