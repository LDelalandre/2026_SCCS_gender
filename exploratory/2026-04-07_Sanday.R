var_id_sanday <- variables %>% 
  filter(grepl("Sanday",definition)) %>% 
  pull(var_id)

soc_id_sanday <- sccs %>% 
  filter(var_id %in% var_id_sanday) %>% 
  filter(!is.na(code)) %>% 
  pull(soc_id) %>% 
  unique()

sccs %>% 
  filter(soc_id %in% soc_id_sanday) %>% 
  filter(var_id == "SCCS711") %>%  #Societal Complexity (Guttman Scale - Freeman and Winch 1957)
  pull(code) %>% 
  hist()

# There are complex societies in her sample. But not many intermediate ones, and many small-scales ones too.

sccs %>% 
  filter(soc_id %in% soc_id_sanday) %>% 
  filter(var_id == "SCCS711") %>%  #Societal Complexity (Guttman Scale - Freeman and Winch 1957)
  filter(code == 6)

