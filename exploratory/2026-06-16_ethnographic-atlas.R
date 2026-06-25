library(tidyverse)

ea <- read.csv(here::here("data/EA/data.csv"))

societies_ea <- read.csv(here::here("data/EA/societies.csv")) %>% 
  rename(soc_id = id)
societies_mapping_ea <- read.csv(here::here("data/EA/societies_mapping.csv")) %>% 
  rename(soc_id = id)

variables_ea <- read.csv(here::here("data/EA/variables.csv")) %>% 
  rename(var_id = id)

codes_ea <- read.csv(here::here("data/EA/codes.csv")) %>% 
  left_join(variables_ea %>% select(var_id, title))


# Step 1: parse the EA society ID from the mapping
# "EA: Barama Carib (Sc3) [Sc3]" -> "Sc3"

societies_mapping_parsed <- societies_mapping %>%
  mutate(soc_id = str_extract_all(related, "[A-Z][a-z]\\d+[a-z]?")) %>%
  unnest(soc_id) %>% 
  select(soc_id, soc_id_ea) %>% 
  unique()


# Load EA variables on SCCS ####

# Step 2: extract EA variables for your societies of interest
ea_vars_of_interest <- c("EA078", "EA043", "EA011", "EA044", "EA045", "EA046", "EA047", "EA048", "EA049",
                         "EA050", "EA051", "EA052", "EA053", "EA054",
                         "EA006")
variables_ea %>% 
  filter(var_id %in% ea_vars_of_interest) %>% 
  View


ea_subset <- ea %>%
  filter(var_id %in% ea_vars_of_interest) %>%
  filter(!is.na(code)) %>%
  rename(ea_soc_id = soc_id)

# Step 3: bridge EA society IDs -> SCCS society IDs -> PCoA coordinates
ea_with_sccs <- ea_subset %>%
  inner_join(societies_mapping_parsed, by = "ea_soc_id") %>%
  # soc_id is now the SCCS id, which matches coords_df
  inner_join(coords_df, by = "soc_id")


ea_loadings <- map_dfr(1:10, function(ax) {
  ax_col <- paste0("Ax", ax)
  
  map_dfr(ea_vars_of_interest, function(v) {
    
    joined <- ea_with_sccs %>%
      filter(var_id == v) %>%
      filter(!is.na(code), !is.na(.data[[ax_col]]))
    
    if (nrow(joined) < 5) return(NULL)   # skip if too few societies
    
    r <- cor(joined[[ax_col]], joined$code,
             method = "spearman", use = "complete.obs")
    
    data.frame(
      var_id       = v,
      pco          = paste0("PCo", ax),
      loading      = r,
      r2_this_axis = r^2,
      n_societies  = nrow(joined)
    )
  })
}) %>%
  group_by(var_id) %>%
  mutate(r2_total = sum(r2_this_axis, na.rm = TRUE)) %>%
  ungroup()

ea_loadings <- ea_loadings %>%
  mutate(
    title_short = case_when(
      var_id == "EA078" ~ "premarital_sex",   # fill in from variables_ea$title
      var_id == "EA043" ~ "descent",
      var_id == "EA011" ~ "residence_transfer",
      TRUE ~ var_id
    ),
    group = "EA variables"   # or split by theme if you want facets
  )

societies_mapping_parsed %>% count(soc_id) %>% filter(n > 1)




## plot ####

# ── Labels ────────────────────────────────────────────────────────────────────

sccs_loadings <- var_loadings %>%
  mutate(source = "SCCS")

all_loadings <- bind_rows(sccs_loadings, ea_loadings)

ea_loadings %>%
  filter(pco %in% c("PCo1", "PCo2")) %>%
  select(var_id, title_short, group, pco, loading) %>%
  pivot_wider(names_from = pco, values_from = loading) %>%
  ggplot(aes(x = PCo1, y = PCo2,
             color = group,
             label = var_id)) +
  geom_segment(aes(xend = 0, yend = 0),
               arrow     = arrow(length = unit(0.2, "cm"), ends = "first"),
               linewidth = 0.8) +
  geom_point(size = 2.5) +
  ggrepel::geom_text_repel(size = 3, max.overlaps = 20) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.4) +
  scale_color_brewer(palette = "Set2", name = "Group") +
  theme_bw(base_size = 10) +
  labs(x     = "PCo1 (formal domination)",
       y     = "PCo2 (work segregation)",
       title = "EA variables projected on main PCoA axes")


# PCoA on EA data ####

ea_vars_of_interest2 <- c("EA078", "EA043", "EA011", "EA044", "EA045", "EA046", "EA048",
                         "EA050", "EA051", "EA052", "EA053", "EA054")

ea_subset_fvar <- ea %>%
  filter(var_id %in% ea_vars_of_interest2) %>%
  filter(!is.na(code)) %>%
  group_by(soc_id, var_id) %>%
  slice(1) %>%   # or slice_max(year) if year is available
  ungroup() %>%
  rename(ea_soc_id = soc_id)


ea_subset_fvar_fsoc <- ea_subset_fvar %>% 
  filter(ea_soc_id %in% societies_mapping_clean$code)
  