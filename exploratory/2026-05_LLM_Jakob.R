source("R/01_import.R")

variables_whyte <- variables %>% 
  filter(grepl("Whyte", definition))

codes_whyte <- codes %>% 
  filter(var_id %in% variables_whyte$var_id)

sccs_whyte <- sccs %>% 
  filter(var_id %in% variables_whyte$var_id) %>% 
  filter(!is.na(code))
  

write.csv(variables_whyte, "output/recoding/variables_whyte.csv", row.names = F)
write.csv(codes_whyte, "output/recoding/codes_whyte.csv", row.names = F)
write.csv(sccs_whyte, "output/recoding/sccs_whyte.csv", row.names = F)

# Even-numbered societies: nothing
sccs %>% 
  filter(var_id %in% variables_whyte$var_id) %>%
  filter(as.numeric(str_extract(soc_id, "\\d+")) %% 2 == 0) %>% 
  filter(!is.na(code))

sccs %>% 
  filter(var_id %in% variables_whyte$var_id) %>%
  filter(as.numeric(str_extract(soc_id, "\\d+")) %% 2 == 0) %>%
  pull(source_coded_data) %>% 
  unique()


