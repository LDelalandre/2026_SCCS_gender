

technology_variables <- predictors_detailed %>% 
  filter(category == "technology") %>% 
  pull(var_id)

technology_data <- sccs %>% 
  filter(var_id %in% technology_variables)  %>%
  dplyr::select(soc_id, var_id, code) %>%
  pivot_wider(names_from = var_id, values_from = code) %>%
  right_join(coords_df, by = "soc_id") %>%
  right_join(pred_resp, by = "soc_id")  %>%
  dplyr::select(soc_id, all_of(technology_variables)) %>%
  mutate(across(all_of(technology_variables), ~ case_when(
    . == 2 ~ 1,
    . == 1 ~ 0,
    TRUE   ~ NA_real_
  ))) %>%
  rename(pottery = SCCS707, metalworking = SCCS706, 
         weaving = SCCS708, irrigation = SCCS681, plow = SCCS680)

# Frequency of each item (should decrease in order)
technology_data %>%
  summarise(across(pottery:plow, ~ sum(., na.rm = TRUE)))

# Cross-tabulate adjacent pairs to see if the order holds
# e.g. can I have metalworking without pottery?
technology_data %>% count(pottery, metalworking)
technology_data %>% count(metalworking, weaving)
technology_data %>% count(weaving, irrigation)
technology_data %>% count(irrigation, plow)

# Scale score = sum of present items
technology_data <- technology_data %>%
  mutate(guttman_score = rowSums(across(pottery:plow), na.rm = FALSE))

# Check the pattern of responses per score
# In a perfect scale, score 1 = pottery only, score 2 = pottery+metalworking, etc.
technology_data %>%
  dplyr::select(pottery:plow, guttman_score) %>%
  arrange(guttman_score) %>%
  print(n = Inf)

# Coefficient of scalability (Menzel's formula)
# CS = 1 - (observed errors / expected errors)
# An error = having a harder item without an easier one


# Compare 5 to 3 items ####

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

compute_cs(technology_data, c("pottery", "metalworking", "weaving", "irrigation", "plow"))
compute_cs(technology_data, c("weaving", "irrigation", "plow"))
# If CS improves or stays high → dropping items improves scale quality


technology_data <- technology_data %>%
  mutate(
    technology = rowSums(across(c(weaving, irrigation, plow)), na.rm = FALSE)
  )

pred_resp <- pred_resp %>% 
  left_join(technology_data, by = "soc_id")




