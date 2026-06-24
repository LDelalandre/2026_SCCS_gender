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

coords_df <- as.data.frame(pcoa_result$vectors[, 1:10])
colnames(coords_df) <- paste0("Ax", 1:10)
coords_df$soc_id <- rownames(coords_df)

coords_df <- coords_df %>%
  left_join(societies %>% dplyr::select(soc_id, pref_name_for_society, 
                                        origLat, origLong),
            by = "soc_id")

# ── Derive broad region from coordinates ─────────────────────────────────────

coords_df <- coords_df %>%
  mutate(region = case_when(
    # Africa
    origLong >= -20  & origLong <= 55  & origLat >= -35 & origLat <= 38  ~ "Africa",
    # Europe
    origLong >= -12  & origLong <= 45  & origLat >  38  & origLat <= 72  ~ "Europe",
    # Middle East
    origLong >= 25   & origLong <= 65  & origLat >= 10  & origLat <= 43  ~ "Middle East",
    # South & SE Asia
    origLong >= 65   & origLong <= 145 & origLat >= -12 & origLat <= 38  ~ "Asia",
    # East Asia
    origLong >= 100  & origLong <= 145 & origLat >= 38  & origLat <= 55  ~ "East Asia",
    # Pacific & Oceania
    origLong >= 130  & origLat  < 0                                       ~ "Pacific",
    origLong >= 145  & origLat  >= 0  & origLat <= 30                    ~ "Pacific",
    # North America
    origLong <= -50  & origLat  >= 15                                     ~ "N. America",
    # South America
    origLong <= -35  & origLat  < 15                                      ~ "S. America",
    TRUE ~ "Other"
  ))


# Check the distribution
table(coords_df$region)
