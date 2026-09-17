library(tidyverse)

# Need to load 
# ~/Documents/Research/Projects/2026_SCCS_gender/R/04_dimorphism.R
# And
# ~/Documents/Research/Projects/2026_SCCS_gender/exploratory/2026-06-16_ethnographic-atlas.R

holden <- read.csv("data/holden1999/holden1999_table1.csv") %>% 
  mutate(height_ratio = Females_Height_cm/Males_Height_cm) %>% 
  rename(soc_id_ea = EA_Code)

holden %>% dim()

holden %>% 
  ggplot(aes(x = Males_Height_cm)) +
  geom_histogram()

holden %>% 
  ggplot(aes(x = Females_Height_cm)) +
  geom_histogram()

holden %>% 
  ggplot(aes(x = height_ratio)) +
  geom_histogram()


ea_holden <- holden %>% 
  left_join(coords_df_ea) 
  
ea_holden %>% 
  ggplot(aes(x = Ax1_ea, y = height_ratio)) +
  geom_point() +
  geom_smooth(method = "lm")

lm(height_ratio ~ Ax1_ea, data = ea_holden) %>% summary()


A <- coords_extended %>% # From dimorphism.R
  left_join(societies_mapping_parsed_whyte) %>% 
  select(soc_id_ea, height_ratio) %>% 
  na.omit() %>% 
  rename(hr_gleeson = height_ratio)

B <- holden %>%  
  # select(soc_id_ea, height_ratio) %>% 
  rename(hr_holden = height_ratio)

full_join(A, B) %>% 
  ggplot(aes(x=hr_gleeson, y=hr_holden)) +
  geom_point()
# diverging values for Javanese. Gleeson got his data not from Holden, but Hastuti, 2013
  

full_hr <- full_join(A, B) %>% 
  mutate(height_ratio = if_else(is.na(hr_holden), hr_gleeson, hr_holden))
  

ea_hr <- full_hr %>% 
  left_join(coords_df_ea) 

ea_hr %>% 
  ggplot(aes(x = Ax1_ea, y = height_ratio)) +
  geom_point() +
  # facet_wrap(~Region)
  geom_smooth(method = "lm")

lm(height_ratio ~ Ax1_ea, data = ea_hr) %>% summary()

lm(height_ratio ~ Ax1_ea, data = ea_hr %>% 
     filter(soc_id_ea %in% societies_mapping_parsed_whyte$soc_id_ea)) %>% summary()

cor.test(ea_hr$Ax1_ea, ea_hr$height_ratio, use = "complete.obs")


