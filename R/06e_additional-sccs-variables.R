vars_violence_politics <- c("SCCS793", "SCCS794", "SCCS795", # (Ross: political participation)
                              "SCCS667", # Sanday, rape
                              "SCCS754") # Wife-beating (Broude)

vars_bradley <- c("SCCS1271", "SCCS1272", "SCCS1273","SCCS1274", "SCCS1275")

vars_additional <- c(vars_violence_politics, vars_bradley)

# recode (high = good for women)
sccs_violence_politics <- sccs %>%
  filter(var_id %in% vars_violence_politics) %>%
  select(soc_id, var_id, code) %>%
  group_by(var_id) %>% 
  mutate(max_value = max(code, na.rm = T)) %>% 
  mutate(code = max_value + 1 - code) %>% 
  select(-max_value)

sccs_bradley <- sccs %>%
  filter(var_id %in% vars_bradley) %>%
  select(soc_id, var_id, code)

sccs_additional <- rbind(sccs_violence_politics, sccs_bradley)


sccs_additional %>% 
  mutate(title_short = case_when(
    var_id == "SCCS793" ~ "Female public political participation",
    var_id == "SCCS794" ~ "Female private political participation",
    var_id == "SCCS795" ~ "Gender diff. in political authority",
    var_id == "SCCS667" ~ "Rape",
    var_id == "SCCS754" ~ "Wife-beating",
    var_id == "SCCS1271" ~ "Land clearance (who does it)",
    var_id == "SCCS1272" ~ "Soil preparation (who does it)",
    var_id == "SCCS1273" ~ "Planting (who does it)",
    var_id == "SCCS1274" ~ "Crop tending (who does it)",
    var_id == "SCCS1275" ~ "Harvesting (who does it)",
    TRUE ~ var_id
  )) %>% 
  mutate(group = case_when(
    var_id %in% c("SCCS793", "SCCS794", "SCCS795") ~ "Political participation",
    var_id %in% c("SCCS667", "SCCS754")             ~ "Violence",
    var_id %in% paste0("SCCS", 1271:1275)           ~ "Agricultural tasks"
  ))





additional_max_values <- sccs_additional %>% 
  select(var_id, max_value) %>% 
  unique()

codes %>% 
  filter(var_id %in% additional_response_vars) %>% 
  left_join(additional_max_values) %>% 
  filter(code == max_value) %>% View
  

sccs_additional %>% 
  filter(unique())
