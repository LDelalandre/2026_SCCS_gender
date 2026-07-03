# Runs PCoA on prepared ordinal matrix

source("R/02a_pcoa_data_prep.R")  # provides: mat_ord_fixed, ordinal_cols, binary_cols

gower_ordinal <- cluster::daisy(
  mat_ord_fixed,
  metric = "gower",
  type = list(ordratio = ordinal_cols, symm = binary_cols)
)

pcoa_result <- ape::pcoa(gower_ordinal)

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


# library(sf)
# library(rnaturalearth)
# library(dplyr)
# 
# world <- ne_countries(scale = "medium", returnclass = "sf") %>%
#   select(continent)
# 
# pts <- coords_df %>%
#   st_as_sf(coords = c("origLong", "origLat"), crs = 4326, remove = FALSE)
# 
# pts <- st_join(pts, world, join = st_intersects)
# 
# missing <- is.na(pts$continent)
# if (any(missing)) {
#   nearest_idx <- st_nearest_feature(pts[missing, ], world)
#   pts$continent[missing] <- world$continent[nearest_idx]
# }
# 
# coords_df$continent <- pts$continent
# 
# 
# coords_df <- coords_df %>%
#   mutate(continent = case_when(
#     pref_name_for_society %in% c("Semang", "Iban", "Toradja", "Javanese", "Alorese", "Kimam") ~ "Oceania",
#     pref_name_for_society %in% c("Chukchi", "Nivkh", "Irish", "Ancient Romans") ~ "Eurasia",
#     continent == "Asia" ~ "Eurasia",
#     TRUE ~ continent
#   )) %>% 
#   rename(region = continent)

# Check the distribution
table(coords_df$region6)
