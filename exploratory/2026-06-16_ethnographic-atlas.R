library(tidyverse)

ea <- read.csv(here::here("data/EA/data.csv")) %>% 
  rename(soc_id_ea = soc_id, var_id_ea = var_id)

societies_ea <- read.csv(here::here("data/EA/societies.csv")) %>% 
  rename(soc_id_ea = id)
societies_mapping_ea <- read.csv(here::here("data/EA/societies_mapping.csv")) %>% 
  rename(soc_id_ea = id)

variables_ea <- read.csv(here::here("data/EA/variables.csv")) %>% 
  rename(var_id_ea = id)

codes_ea <- read.csv(here::here("data/EA/codes.csv")) %>% 
  rename(var_id_ea = var_id) %>% 
  left_join(variables_ea %>% select(var_id_ea, title))


# Data preparation ####

# Step 1: parse the EA society ID from the mapping
# "EA: Barama Carib (Sc3) [Sc3]" -> "Sc3"

societies_mapping_parsed <- societies_mapping %>%
  mutate(soc_id_ea = str_extract_all(related, "[A-Z][a-z]\\d+[a-z]?")) %>%
  unnest(soc_id_ea) %>% 
  select(soc_id, soc_id_ea) %>% 
  unique()


ea_vars_of_interest <- c("EA078", "EA043", "EA011", "EA044", "EA045", "EA046", "EA047", "EA048", "EA049",
                         # "EA006", # removed: dowry vs. bride price, did not at all on Whyte data's axis 1.
                         "EA050", "EA051", "EA052", "EA053", "EA054"
                         )
variable_definitions <- variables_ea %>% 
  filter(var_id_ea %in% ea_vars_of_interest) %>% 
  select(var_id_ea, title)

ea_subset_fvar <- ea %>%
  filter(var_id_ea %in% ea_vars_of_interest) %>%
  filter(!is.na(code)) %>%
  group_by(soc_id_ea, var_id_ea) %>%
  slice(1) %>%   # or slice_max(year) if year is available
  ungroup()

societies_mapping_parsed_whyte <- societies_mapping_parsed %>% 
  filter(soc_id %in% whyte_societies)

ea_subset_fvar_fsoc <- ea_subset_fvar %>% 
  filter(soc_id_ea %in% societies_mapping_parsed_whyte$soc_id_ea)




## Recode variables ####
codes_inspection <- codes_ea %>% 
  filter(var_id_ea %in% ea_vars_of_interest)

library(dplyr)

ea_fdata_long <- ea_subset_fvar_fsoc %>%
  mutate(
    code_r = case_when(
      
      # ── Division of labor (EA044–EA054): 1=males only → 5=females only, 3=ambiguous ──
      var_id_ea %in% c("EA044","EA045","EA046","EA047","EA048",
                       "EA049","EA050","EA051","EA052","EA053","EA054") &
        code == 1 ~ 1,  # males alone
      var_id_ea %in% c("EA044","EA045","EA046","EA047","EA048",
                       "EA049","EA050","EA051","EA052","EA053","EA054") &
        code == 2 ~ 2,  # both, males more
      var_id_ea %in% c("EA044","EA045","EA046","EA047","EA048",
                       "EA049","EA050","EA051","EA052","EA053","EA054") &
        code == 3 ~ 3,  # differentiated but equal
      var_id_ea %in% c("EA044","EA045","EA046","EA047","EA048",
                       "EA049","EA050","EA051","EA052","EA053","EA054") &
        code == 4 ~ 3,  # equal participation → midpoint (same as differentiated)
      var_id_ea %in% c("EA044","EA045","EA046","EA047","EA048",
                       "EA049","EA050","EA051","EA052","EA053","EA054") &
        code == 5 ~ 4,  # both, females more
      var_id_ea %in% c("EA044","EA045","EA046","EA047","EA048",
                       "EA049","EA050","EA051","EA052","EA053","EA054") &
        code == 6 ~ 5,  # females alone
      var_id_ea %in% c("EA044","EA045","EA046","EA047","EA048",
                       "EA049","EA050","EA051","EA052","EA053","EA054") &
        code %in% c(7, 8, 9) ~ 3,  # irrelevant / unspecified / absent → midpoint
      
      # ── EA006: marriage transactions ──
      var_id_ea == "EA006" & code == 1 ~ 1,  # bride-wealth
      var_id_ea == "EA006" & code == 5 ~ 1,  # woman exchange (equally bad)
      var_id_ea == "EA006" & code == 2 ~ 2,  # bride-service
      var_id_ea == "EA006" & code == 3 ~ 3,  # token bride-wealth
      var_id_ea == "EA006" & code == 7 ~ 3,  # dowry (debatable — resources go to bride but burden on her family)
      var_id_ea == "EA006" & code == 4 ~ 4,  # gift exchange
      var_id_ea == "EA006" & code == 6 ~ 4,  # insignificant/absent
      
      # ── EA011: post-marital residence ──
      var_id_ea == "EA011" & code == 1 ~ 1,  # patrilocal
      var_id_ea == "EA011" & code == 2 ~ 2,  # ambi/neolocal
      var_id_ea == "EA011" & code == 9 ~ 2,  # duolocal → midpoint (debatable: could be 4)
      var_id_ea == "EA011" & code == 3 ~ 3,  # matrilocal
      
      # ── EA043: descent ──
      var_id_ea == "EA043" & code == 1 ~ 1,  # patrilineal
      var_id_ea == "EA043" & code == 4 ~ 2,  # quasi-lineages (patrilineal leaning)
      var_id_ea == "EA043" & code %in% c(5, 6, 7) ~ 3,  # ambilineal / bilateral / mixed → midpoint
      var_id_ea == "EA043" & code == 2 ~ 4,  # duolateral (both lines recognized)
      var_id_ea == "EA043" & code == 3 ~ 5,  # matrilineal
      
      # ── EA078: premarital sex norms for girls ──
      var_id_ea == "EA078" & code == 1 ~ 1,  # precluded by early marriage
      var_id_ea == "EA078" & code == 2 ~ 2,  # prohibited, strongly sanctioned
      var_id_ea == "EA078" & code == 3 ~ 3,  # prohibited, weakly sanctioned
      var_id_ea == "EA078" & code %in% c(4, 5) ~ 4,  # permitted (with or without pregnancy caveat)
      var_id_ea == "EA078" & code == 6 ~ 5,  # freely permitted
      
      # ── Anything else (other variables or unhandled codes) → NA ──
      TRUE ~ NA_real_
    )
  )


# check: see original vs recoded for each variable
ea_fdata_long %>%
  filter(!is.na(code_r)) %>%
  count(var_id_ea, code, code_r) %>%
  arrange(var_id_ea, code) %>% View


# Load EA variables on SCCS ####

ea_wide <- ea_fdata_long %>%
  select(soc_id_ea, var_id_ea, code_r) %>%
  pivot_wider(names_from = var_id_ea, values_from = code_r)

ea_wide_coords <- ea_wide %>%
  inner_join(societies_mapping_parsed, by = "soc_id_ea") %>%
  inner_join(coords_df, by = "soc_id")

# Correlate each EA variable with each PCoA axis
ea_loadings <- map_dfr(ea_vars_of_interest, function(v) {
  map_dfr(1:10, function(ax) {
    ax_col <- paste0("Ax", ax)
    
    dat <- ea_wide_coords %>%
      select(all_of(c(v, ax_col))) %>%
      filter(!is.na(.data[[v]]), !is.na(.data[[ax_col]]))
    
    if (nrow(dat) < 5) return(NULL)
    
    r <- cor(dat[[v]], dat[[ax_col]], method = "spearman")
    
    data.frame(
      var_id_ea    = v,
      pco          = paste0("PCo", ax),
      loading      = r,
      r2_this_axis = r^2,
      n_societies  = nrow(dat)
    )
  })
}) %>%
  group_by(var_id_ea) %>%
  mutate(r2_total = sum(r2_this_axis)) %>%
  ungroup()

societies_mapping_parsed %>% count(soc_id) %>% filter(n > 1)




## plot ####

all_loadings <- bind_rows(
  var_loadings %>% mutate(source = "SCCS"),
  ea_loadings_plot
)

# -- plot
all_loadings %>%
  filter(pco %in% c("PCo1", "PCo2")) %>%
  select(var_id_ea, source, pco, loading) %>%
  pivot_wider(names_from = pco, values_from = loading) %>%
  ggplot(aes(x = PCo1, y = PCo2,
             color = source,
             label = var_id_ea)) +
  geom_segment(aes(xend = 0, yend = 0),
               arrow     = arrow(length = unit(0.2, "cm"), ends = "first"),
               linewidth = 0.8) +
  geom_point(size = 2.5) +
  ggrepel::geom_text_repel(size = 3, max.overlaps = 20) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.4) +
  scale_color_brewer(palette = "Set2", name = "Source") +
  theme_bw(base_size = 10) +
  labs(x     = "PCo1 (formal domination)",
       y     = "PCo2 (work segregation)",
       title = "EA variables projected on SCCS PCoA axes")


variable_definitions

# Keep only variables with |loading| > 0.3 on PCo1, for example
strong_vars <- ea_loadings %>%
  filter(pco == "PCo1", abs(loading) > 0.2) %>%
  pull(var_id_ea)
strong_vars


# PCoA on EA data ####

## Data preparation ####

mat_ea <- ea_wide %>%
  dplyr::select(-soc_id_ea) %>% 
  # select(all_of(strong_vars)) %>% 
  as.matrix()
rownames(mat_ea) <- ea_wide$soc_id_ea

## Missingness ####

society_coverage <- rowMeans(!is.na(mat_ea))
data.frame(
  min    = min(society_coverage),
  q1     = quantile(society_coverage, 0.25),
  median = median(society_coverage),
  mean   = mean(society_coverage),
  q3     = quantile(society_coverage, 0.75),
  max    = max(society_coverage))

# Remove rows with > 40% missing (=2 rows)
row_miss <- rowMeans(is.na(mat_ea))
mat_ea_filtered <- mat_ea[row_miss <= 0.40, ]

societies_removed <- mat_ea[row_miss > 0.40, ] %>% rownames()
societies_removed

variable_coverage <- colMeans(!is.na(mat_ea))
data.frame(
  min    = min(variable_coverage),
  q1     = quantile(variable_coverage, 0.25),
  median = median(variable_coverage),
  mean   = mean(variable_coverage),
  q3     = quantile(variable_coverage, 0.75),
  max    = max(variable_coverage))

## Prepare ordinal variables ####

# 1. Convert columns to ordered factors (needed for daisy to treat as ordinal)
mat_ea_ord <- data.frame(lapply(as.data.frame(mat_ea_filtered), function(x) {
  factor(x, levels = sort(unique(x[!is.na(x)])), ordered = TRUE)
}))
rownames(mat_ea_ord) <- rownames(mat_ea_filtered)

## PCoA ####

gower_ordinal_ea <- cluster::daisy(
  mat_ea_ord,
  metric = "gower",
  type = list(ordratio = 1:ncol(mat_ea_ord))
)

pcoa_result_ea <- ape::pcoa(gower_ordinal_ea)

coords_df_ea <- as.data.frame(pcoa_result_ea$vectors[, 1:10])
colnames(coords_df_ea) <- paste0("Ax", 1:10, "_ea")
coords_df_ea$soc_id_ea <- rownames(coords_df_ea)
coords_df_ea <- coords_df_ea %>% 
  mutate(Ax1_ea = -Ax1_ea)  # flip sign



## Plots ####

scree_plot <- data.frame(
  axis = 1:length(pcoa_result_ea$values$Relative_eig),
  variance = pcoa_result_ea$values$Relative_eig
) %>%
  filter(axis < 12) %>% 
  ggplot(aes(x = axis, y = variance)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_line(color = "darkblue") +
  geom_point(color = "darkblue", size = 2) +
  scale_x_continuous(breaks = 1:length(pcoa_result_ea$values$Relative_eig)) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw(base_size = 10) +
  labs(x = "PCoA axis", y = "Relative eigenvalue",
       title = "Scree plot — EA PCoA")
scree_plot


# Correlation
coords_df_merged <- coords_df_ea %>% 
  left_join(societies_mapping_parsed) %>% 
  left_join(coords_df)

coords_df_merged %>% 
  ggplot(aes(x = Ax1_ea, y = Ax1)) +
  geom_point() +
  geom_smooth(method = "lm")

cor.test(coords_df_merged$Ax1, coords_df_merged$Ax1_ea, use = "complete.obs")
lm(Ax1 ~ Ax1_ea, data = coords_df_merged) %>% summary() 
# 20% of the variance of SCCS Axis1 is captured in EA. This is not much, but this gives an idea?

coords_df_merged_height <- coords_df_merged %>%
  left_join(dimorphism_vals,  by = "soc_id") %>% 
  left_join(societies_glottolog) 

coords_df_merged_height %>% 
  ggplot(aes(x = Ax1_ea, y = height_ratio)) +
  geom_point() +
  geom_smooth(method = "lm")


lm(height_ratio ~ Ax1, data = coords_df_merged_height) %>% summary()
lm(height_ratio ~ Ax1_ea, data = coords_df_merged_height) %>% summary()

library(brms)
bayes_ea <- brm(height_ratio ~ Ax1_ea, 
                data = coords_df_merged_height,
                family = gaussian(),
                prior = c(prior(normal(0, 0.1), class = b)),
                cores = 4)
summary(bayes_ea)



ea_wide_coords_merged_height <- ea_wide_coords %>%
  left_join(dimorphism_vals,  by = "soc_id") %>% 
  left_join(societies_glottolog) 

lm(height_ratio ~ EA011, data = ea_wide_coords_merged_height) %>% summary()
lm(height_ratio ~ EA078, data = ea_wide_coords_merged_height) %>% summary()


# Rather than PCoA anew, prediction ####

# Societies in both datasets
train_data <- ea_wide_coords %>%
  filter(!is.na(Ax1)) %>%   # has SCCS score
  select(Ax1, EA011, EA043,  EA044, EA045, 
         EA046, EA047, EA048, EA049, EA050, 
         EA051, EA052, EA053, EA054, EA078)

# Fit model
model_ea_to_sccs <- lm(Ax1 ~ EA011 + EA043 + EA078, data = train_data)
summary(model_ea_to_sccs)

lm(height_ratio ~ EA078 + EA053 + EA054 + EA011, 
   data = ea_wide_coords_merged_height) %>% summary()

# Predict on full EA
coords_df_ea <- ea_wide_coords %>%
  mutate(Ax1_predicted = predict(model_ea_to_sccs, newdata = .))
