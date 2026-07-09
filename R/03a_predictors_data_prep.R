library(cluster)
library(ape)
library(patchwork)

whyte_societies <- sccs %>% 
  filter(var_id %in% whyte_vars$var_id) %>% 
  filter(!is.na(code)) %>% 
  pull(soc_id) %>% 
  unique()

# Define predictors ####
predictors_putative <- read.csv(here::here("data/gender_predictors_putative.csv")) %>%
  filter(category != "") %>%
  filter(predictor !="no") %>%
  dplyr::select(var_id, category)

# predictors_additional_id <- c(
#   # WARFARE
#   "SCCS1649", "SCCS1650" # warfare from Ember - this is overall: "SCCS1648"
# )
# 
# predictors_additional_category <- c(rep("warfare", 2))
# 
# predictors_additional <- data.frame(var_id = predictors_additional_id,
#                                     category = predictors_additional_category)
# 
# predictors <- bind_rows(predictors_putative, predictors_additional) %>% 
#   filter(!category == "method")

predictors <- predictors_putative

predictors_detailed <- predictors %>% 
  left_join(variables %>% dplyr::select(var_id, title)) %>% 
  arrange(category) %>% 
  # filter(!var_id == "SCCS693") %>%  # remove Whyte's binary code
  filter(!var_id %in% c("SCCS688", "SCCS689")) %>%  # REMOVED post-european contact husbandry
  filter(!var_id == "SCCS713") %>% # REMOVED pre-classical religion. Or should be in complexity?
  filter(!var_id == "SCCS714") %>% #sex ratio
  filter(!var_id == "SCCS698") %>% # extended family
  filter(!var_id == "SCCS715") %>% # systematic absence of married males
  filter(!var_id %in% c("SCCS695", "SCCS694")) %>%  # male solidarity, male initiation ceremonies
  filter(!var_id %in% c("SCCS690", "SCCS691", "SCCS692")) %>%  # hunting variables that correlated with agriculture and complexity. Not capturing subsistence.
  filter(!var_id %in% c("SCCS729")) # fishing, also remove, I just want a simple hunting-gathering variable

write.csv(predictors_detailed, here::here("output/predictors_detailed.csv"), row.names = F) 
