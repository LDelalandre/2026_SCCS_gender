
# VARIABLE LOADING AND DEFINITION ####

pcoa_result <-  readRDS(here::here("output/pcoa_result"))

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



## Table variables per axes ####

# Society coordinates on PCoA axes
coords <- pcoa_result$vectors  # societies × axes

# Correlate each variable with each axis (Spearman, handles ordinal)
n_axes <- 10  # however many you decided to keep

variable_loadings <- matrix(NA, 
                            nrow = ncol(mat_filtered), 
                            ncol = n_axes,
                            dimnames = list(colnames(mat_filtered),
                                            paste0("PCo", 1:n_axes)))

for(i in 1:n_axes){
  variable_loadings[, i] <- apply(mat_filtered, 2, function(x){
    cor(x, coords[, i],
        method = "spearman",
        use    = "pairwise.complete.obs")
  })
}

# R-squared of each variable across all axes
r2_total <- apply(variable_loadings[, 1:n_axes], 1, function(x){
  sum(x^2, na.rm = TRUE)
})

# How much of each variable is captured by your 10 axes?
r2_distrib <- data.frame(
  variable  = names(r2_total),
  r2_total  = round(r2_total, 3)
) %>%
  left_join(whyte_vars %>% dplyr::select(var_id_whyte, short_title, domain),
            by = c("variable" = "var_id_whyte")) %>%
  arrange(desc(r2_total))
# Variables of axis 1
# filter(variable %in% c("DV5", "DV15", "DV20", "DV34", "DV36", "DV40", "DV50", "DV51")) %>% 


ggplot(r2_distrib, aes(x = r2_total)) + 
  geom_histogram() +
  ggtitle("r2 in the variable explained by all the axes")

# Specificity of each variable to each axis
r2_matrix <- variable_loadings[, 1:10]^2

# For each variable, what proportion of its explained variance is on each axis?
r2_proportion <- sweep(r2_matrix, 1, rowSums(r2_matrix, na.rm = TRUE), "/")

# Variables most specific to axis 1
r2_total_axis <- data.frame(
  variable     = rownames(r2_proportion),
  r2_axis1     = round(r2_matrix[, 1], 3),
  r2_total     = round(rowSums(r2_matrix, na.rm = TRUE), 3),
  pct_on_axis1 = round(r2_proportion[, 1] * 100, 1)
) %>%
  left_join(whyte_vars %>% dplyr::select(var_id_whyte, short_title),
            by = c("variable" = "var_id_whyte")) %>%
  arrange(desc(pct_on_axis1)) %>%
  head(15) 



# For each axis, show variables where:
# 1. r2_total >= 0.3 (variable is reasonably captured overall)
# 2. pct_on_this_axis >= 30% (this axis is its primary home)
# 3. Sorted by absolute loading (effect size)

library(knitr)
library(kableExtra)

# ── Tune these ────────────────────────────────────────────────────────────────
r2_threshold  <- 0.3
pct_threshold <- 30

# ── Function ──────────────────────────────────────────────────────────────────
interpret_axis <- function(ax, r2_min = r2_threshold, pct_min = pct_threshold){
  data.frame(
    variable     = rownames(r2_matrix),
    loading      = variable_loadings[, ax],
    r2_total     = rowSums(r2_matrix, na.rm = TRUE),
    r2_this_axis = r2_matrix[, ax]
  ) %>%
    mutate(pct_this_axis = r2_this_axis / r2_total * 100) %>%
    filter(r2_total >= r2_min, pct_this_axis >= pct_min) %>%
    left_join(whyte_vars %>% dplyr::select(var_id_whyte, title, category1, category2),
              by = c("variable" = "var_id_whyte")) %>%
    arrange(desc(abs(loading))) %>%
    dplyr::select(variable, title, category1, category2, loading, r2_total, pct_this_axis) %>%
    mutate(across(where(is.numeric), round, 2))
}

# ── Build table ───────────────────────────────────────────────────────────────
all_axes_table <- map_dfr(1:10, function(ax){
  interpret_axis(ax) %>%
    mutate(pco = ax, .before = everything())
}) %>% 
  left_join(whyte_category %>% select(variable = var_id_whyte, category, category_Angarika)) %>% 
  select(-c(category1,category2))

# ── Render ────────────────────────────────────────────────────────────────────
all_axes_table %>%
  kable(
    caption   = "Variable loadings across PCoA axes",
    col.names = c("PCo", "Variable", "Title" , 
                  "Loading", "R² total", "% this axis", "Category", "category_Angarika"),
    align     = c("c", "l", "l", "l", "l", "r", "r", "r")
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width        = FALSE,
    position          = "left"
  ) %>%
  column_spec(3, width = "8cm") %>%
  column_spec(6, 
              color = ifelse(all_axes_table$loading > 0, 
                             "#1a7a1a", "#cc0000"), 
              bold  = TRUE) %>%
  pack_rows(
    index = setNames(
      as.vector(table(all_axes_table$pco)),
      paste0("PCo ", names(table(all_axes_table$pco)))
    )
  ) %>%
  row_spec(0, bold = TRUE, background = "#f0f0f0")


## Heatmap loading ####
loading_long_unfiltered <- map_dfr(1:10, function(ax) {
  data.frame(
    variable     = rownames(r2_matrix),
    loading      = variable_loadings[, ax],
    r2_total     = rowSums(r2_matrix, na.rm = TRUE),
    r2_this_axis = r2_matrix[, ax]
  ) %>%
    mutate(pco = paste0("PCo", ax))
}) %>%
  left_join(whyte_vars %>% select(var_id_whyte, title, domain),
            by = c("variable" = "var_id_whyte")) %>%
  left_join(
    data.frame(
      pco = paste0("PCo", 1:10),
      pct = paste0("PCo", 1:10, "\n(", round(variance_explained[1:10]*100, 1), "%)")
    ),
    by = "pco"
  ) %>%
  mutate(pct = factor(pct, levels = paste0("PCo", 1:10, "\n(", round(variance_explained[1:10]*100, 1), "%)"))) %>%
  mutate(title = reorder(title, as.numeric(factor(variable)))) %>%
  filter(!is.na(variable), !is.na(title)) 



# Create ordered y-axis labels
loading_long_unfiltered <- loading_long_unfiltered %>%
  arrange(variable) %>%
  mutate(
    var_label = paste0(variable, " – ", title),
    var_label = factor(var_label, levels = unique(var_label)),
    low_r2    = r2_total < 0.3 | r2_this_axis < 0.3
  )

loading_long_unfiltered <- loading_long_unfiltered %>%
  arrange(as.numeric(str_extract(variable, "[0-9]+"))) %>%
  mutate(
    var_label = paste0(variable, " – ", title),
    var_label = factor(var_label, levels = unique(var_label)),
    low_r2    = r2_total < 0.3 | r2_this_axis < 0.3
  )

loading_long_unfiltered <- loading_long_unfiltered %>%
  mutate(
    pct_this_axis = r2_this_axis / r2_total * 100,
    low_r2        = r2_total < r2_threshold | pct_this_axis < pct_threshold
  ) %>% 
  mutate(weighted_loading = loading * sqrt(r2_total))

loadings_axes_unfiltered <- ggplot(loading_long_unfiltered, aes(x = pct, y = var_label, fill = loading)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = round(loading, 2),
                color = abs(loading) > 0.4,
                alpha = ifelse(low_r2, 0.4, 1)),
            size = 2.8) +
  scale_fill_gradient2(
    low      = "#cc0000",
    mid      = "white",
    high     = "#1a7a1a",
    midpoint = 0,
    name     = "Loading\n(Spearman r)"
  ) +
  scale_color_manual(values = c("TRUE" = "white", "FALSE" = "grey30"), guide = "none") +
  scale_alpha_identity() +
  labs(
    x        = NULL,
    y        = NULL,
    title    = "Variable loadings on PCoA axes",
    subtitle = "Faded tiles: r² < 0.3 and < 30% variance on this axis" 
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x     = element_text(size = 9),
    axis.text.y     = element_text(size = 9),
    panel.grid      = element_blank(),
    legend.position = "right"
  ) +
  scale_y_discrete(limits = rev)

ggsave(here::here("output/2.2_pcoa_figures_loadings_axes_unfiltered.png"), loadings_axes_unfiltered, width = 13, height = 10)
loadings_axes_unfiltered




# Correct direction: positive = good for women ####

# For each variable, find the description of the maximum code
# This tells you what "high score" means

max_code_descriptions <- codes %>%
  filter(var_id %in% whyte_category$var_id) %>%
  filter(!is.na(code)) %>%
  group_by(var_id, title) %>%
  slice_max(code, n = 1) %>%
  ungroup() %>%
  select(var_id, title, max_code = code, max_description = description) %>%
  left_join(whyte_category %>% select(var_id, category),
            by = "var_id") %>%
  arrange(category) %>%
  select(var_id, category, title, max_code, max_description) 

write.csv(max_code_descriptions, "output/2026-04-08_pcoa_category_max_code_descriptions.csv", row.names = F)

# direction_table <- read.csv("output/2026-04-08_pcoa_category_max_code_descriptions_edited.csv") %>% 
#   select(var_id, women_favorable)

direction_table <- read.csv("data/SCCS/gender_response_variables_recode.csv") %>%
  select(var_id, title, women_favorable)

# Correlate original variables with society PCoA scores
# This gives you loadings: how much each variable contributes to each axis
var_loadings <- cor(
  mat_filtered,
  pred_resp %>% select(Axis.1:Axis.10),
  use = "pairwise.complete.obs"
) %>%
  as.data.frame() %>%
  rownames_to_column("var_id_whyte") %>%
  left_join(whyte_category %>% select(var_id, var_id_whyte, title, title_shortened, category),
            by = "var_id_whyte")


# Change the sign of the loading if it was coded as high = bad for women
var_loadings_corrected <- var_loadings %>%
  left_join(direction_table, by = "var_id") %>%
  mutate(across(
    starts_with("Axis."),
    ~ . * women_favorable
  ))


var_loadings_corrected %>%
  pivot_longer(cols = starts_with("Axis."),
               names_to = "axis",
               values_to = "loading") %>%
  mutate(axis = factor(axis, 
                       levels = paste0("Axis.", 1:10))) %>%
  filter(axis %in% paste0("Axis.", 1:5)) %>%  # first 5 axes
  ggplot(aes(x = category, y = loading, fill = category)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ axis, nrow = 1) +
  coord_flip() +
  theme_bw() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = NULL, y = "Loading on axis",
       title = "Variable categories by PCoA axis") +
  theme(legend.position = "none")

# Loading per category (léo's) ####

axis_category_summary <- var_loadings_corrected %>%
  pivot_longer(cols = starts_with("Axis."),
               names_to = "axis",
               values_to = "loading") %>%
  filter(axis %in% paste0("Axis.", 1:6)) %>%
  group_by(axis, category) %>%
  summarise(
    mean_loading = mean(loading, na.rm = TRUE),
    mean_abs_loading = mean(abs(loading), na.rm = TRUE),
    sd_loading = sd(loading, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

axis_category_summary %>% 
  select(-c(mean_abs_loading, sd_loading, n)) %>%  
  pivot_wider(names_from = axis, values_from = mean_loading)

corplot_axes_category <- axis_category_summary %>%
  filter(axis %in% paste0("Axis.", 1:6)) %>%
  ggplot(aes(x = axis, y = category, fill = mean_loading)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(mean_loading, 2),
                color = abs(mean_loading) > 0.1),
            size = 3.5) +
  scale_color_manual(values = c("grey60", "black"), guide = "none") +
  scale_fill_gradient2(
    low = "tomato", mid = "white", high = "steelblue",
    midpoint = 0,
    limits = c(-0.3, 0.3)
  ) +
  theme_bw() +
  labs(x = NULL, y = NULL,
       fill = "Mean loading",
       title = "Mean signed loading by category and axis",
       subtitle = "Positive = female-favorable pole; Negative = male-dominant pole")

ggsave("output/2026-04-08_pcoa_category_corplot_axes_category.png", corplot_axes_category,
       width = 6, height = 4)



# Biplots ####

## Social stratification ####

codes_SCCS1751 <- codes %>% 
  filter(var_id == "SCCS1751") %>% 
  select(code, description) %>% 
  rename(SCCS1751 = code) %>% 
  mutate(SCCS1751 = as.factor(SCCS1751))

society_stratification <- sccs %>% 
  filter(soc_id %in% pred_resp$soc_id) %>% 
  filter(var_id %in% c("SCCS1751")) %>%
  select(soc_id, var_id, code) %>% 
  pivot_wider(names_from = var_id, values_from = code) %>% 
  filter(!is.na(SCCS1751)) %>% 
  mutate(SCCS1751 = as.factor(SCCS1751)) %>% 
  left_join(societies %>% select(soc_id, pref_name_for_society)) %>% 
  left_join(codes_SCCS1751) %>% 
  mutate(label = paste(SCCS1751, description, sep = "_")) 


society_stratification %>%
  left_join(pred_resp, by = "soc_id") %>% 
  left_join(coords_df) %>% 
  ggplot(aes(x = SCCS1751, y = Axis.1)) +
  geom_boxplot() +
  geom_point() 


society_stratification %>%
  left_join(pred_resp, by = "soc_id") %>% 
  left_join(coords_df) %>% 
  ggplot(aes(x = Axis.1, y = Axis.2, label = pref_name_for_society)) +
  geom_point() +
  facet_wrap(~ SCCS1751) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  theme_minimal(base_size = 13) +
  theme(
    panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.5),  # or fill = "gray97"
    panel.spacing = unit(1.5, "lines")
  ) +
  theme(
    panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.5),
    panel.spacing = unit(1.5, "lines"),
    # strip.background = element_rect(fill = "gray20", color = NA),
    # strip.text = element_text(color = "white", face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  ggrepel::geom_text_repel(aes(label = pref_name_for_society))

strat_subsistence <- society_stratification %>%
  select(-label) %>% 
  left_join(pred_resp, by = "soc_id") %>% 
  left_join(society_subsistence_type, by = "soc_id", "label") %>% 
  left_join(coords_df) 

strat_subsistence %>% 
  ggplot(aes(x = SCCS820, y = SCCS1751, label = pref_name_for_society)) +
  # geom_boxplot() +
  geom_jitter()

mod <- lm(Axis.1 ~ SCCS1751 + SCCS820, data = strat_subsistence %>% 
            mutate(SCCS820 = as.numeric(SCCS820)) %>% 
            mutate(SCCS1751 = as.numeric(SCCS1751)) )
summary(mod)
# the the subsistence mode is more determinant than the presence of social classes





codes_SCCS270 <- codes %>% 
  filter(var_id == "SCCS270") %>% 
  select(code, description) %>% 
  rename(SCCS270 = code) %>% 
  mutate(SCCS270 = as.factor(SCCS270))

society_stratification <- sccs %>% 
  filter(soc_id %in% pred_resp$soc_id) %>% 
  filter(var_id %in% c("SCCS270")) %>%
  select(soc_id, var_id, code) %>% 
  pivot_wider(names_from = var_id, values_from = code) %>% 
  filter(!is.na(SCCS270)) %>% 
  mutate(SCCS270 = as.factor(SCCS270)) %>% 
  left_join(societies %>% select(soc_id, pref_name_for_society)) %>% 
  left_join(codes_SCCS270) %>% 
  mutate(label = paste(SCCS270, description, sep = "_")) 


society_stratification %>%
  left_join(pred_resp, by = "soc_id") %>% 
  left_join(society_subsistence_type %>% select(soc_id, SCCS820)) %>% 
  left_join(coords_df) %>% 
  ggplot(aes(x = Axis.1, y = Axis.2, label = pref_name_for_society, color = SCCS820)) +
  geom_point() +
  facet_wrap(~ SCCS270) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  theme_minimal(base_size = 13) +
  theme(
    panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.5),  # or fill = "gray97"
    panel.spacing = unit(1.5, "lines")
  ) +
  theme(
    panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.5),
    panel.spacing = unit(1.5, "lines"),
    # strip.background = element_rect(fill = "gray20", color = NA),
    # strip.text = element_text(color = "white", face = "bold"),
    panel.grid.minor = element_blank()
  )  +
  ggrepel::geom_text_repel(aes(label = pref_name_for_society))

society_stratification %>%
  select(-label) %>% 
  left_join(pred_resp, by = "soc_id") %>% 
  left_join(society_subsistence_type, by = "soc_id", "label") %>% 
  left_join(coords_df) %>% 
  ggplot(aes(x = SCCS820, y = Axis.1, label = pref_name_for_society)) +
  # geom_boxplot() +
  geom_point()

society_stratification %>%
  select(-label) %>% 
  left_join(pred_resp, by = "soc_id") %>% 
  left_join(society_subsistence_type, by = "soc_id", "label") %>% 
  left_join(coords_df) %>% 
  ggplot(aes(x = SCCS270, y = Axis.1, label = pref_name_for_society)) +
  geom_boxplot() +
  geom_point() +
  facet_wrap(~label) 