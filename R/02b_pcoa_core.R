# Runs PCoA on prepared ordinal matrix

gower_ordinal <- cluster::daisy(
  mat_ord_fixed,
  metric = "gower",
  type = list(ordratio = ordinal_cols, symm = binary_cols)
)

pcoa_result <- ape::pcoa(gower_ordinal, correction = "none")

saveRDS(pcoa_result, here::here("output/pcoa_result"))

# Data manip ####

coords_df <- as.data.frame(pcoa_result$vectors[, 1:5])
colnames(coords_df) <- paste0("Ax", 1:5)
coords_df$soc_id <- rownames(coords_df)

coords_df[, paste0("Ax", 1:5)] <- scale(coords_df[, paste0("Ax", 1:5)]) # center reduce

coords_df <- coords_df %>%
  left_join(societies %>% dplyr::select(soc_id, pref_name_for_society, 
                                        origLat, origLong),
            by = "soc_id")


# Associate Murdock's world region ####

societies_mapping_parsed <- societies_mapping %>%
  mutate(soc_id_ea = str_extract_all(related, "[A-Z][a-z]\\d+[a-z]?")) %>%
  unnest(soc_id_ea) %>% 
  select(soc_id, soc_id_ea) %>% 
  unique()

societies_region <- societies_mapping_parsed %>%
  mutate(region6 = case_when(
    startsWith(soc_id_ea, "A") ~ "Sub-Saharan Africa",
    startsWith(soc_id_ea, "C") ~ "Circum-Mediterranean",
    startsWith(soc_id_ea, "E") ~ "East Eurasia",
    startsWith(soc_id_ea, "I") ~ "Insular Pacific",
    startsWith(soc_id_ea, "N") ~ "North America",
    startsWith(soc_id_ea, "S") ~ "South and Central America"
  ))

coords_df <- coords_df %>%
  left_join(societies_region %>% 
              select(soc_id, region6), 
            by = "soc_id")

# Check the distribution
table(coords_df$region6)
