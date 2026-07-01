
# source("R/00_runall.R")

# Version 1 ####

library(patchwork)

# --- shared axis limits, so the marginal panels line up with the biplot ---
xr <- range(pred_resp$Axis.1, na.rm = TRUE)
yr <- range(pred_resp$Axis.2, na.rm = TRUE)
xlims <- xr + c(-1, 1) * diff(xr) * 0.05
ylims <- yr + c(-1, 1) * diff(yr) * 0.05

# --- main biplot ---
main_plot <- pred_resp %>%
  ggplot(aes(x = Axis.1, y = Axis.2)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  coord_cartesian(xlim = xlims, ylim = ylims) +
  labs(x = "Axis 1", y = "Axis 2") +
  theme_minimal(base_size = 13)


# --- compute a scaling factor to bring loadings into the same visual range as site scores ---
site_range_x <- diff(range(pred_resp$Axis.1, na.rm = TRUE))
site_range_y <- diff(range(pred_resp$Axis.2, na.rm = TRUE))

loading_range_x <- diff(range(vars_plot$loading[vars_plot$axis == "Axis.1"], na.rm = TRUE))
loading_range_y <- diff(range(vars_plot$loading[vars_plot$axis == "Axis.2"], na.rm = TRUE))

# scale factor: how much bigger the site range is than the loading range
scale_factor_x <- site_range_x / loading_range_x
scale_factor_y <- site_range_y / loading_range_y

# use a single shared factor so the two axes stay comparable (typical biplot convention)
scale_factor <- min(scale_factor_x, scale_factor_y) * 0.8  # 0.8 = a bit of headroom, tweak as needed

# --- rescaled loadings, used ONLY for the marginal plots ---
vars_plot_scaled <- vars_plot %>%
  mutate(loading_scaled = loading * scale_factor)

top_box <- vars_plot_scaled %>%
  filter(axis == "Axis.1") %>%
  ggplot(aes(x = loading_scaled, y = category_Angarika, fill = category_Angarika)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, width = 0.7) +
  geom_jitter(aes(alpha = significant), height = 0.15, width = 0,
              shape = 21, size = 1.3, color = "black", stroke = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  scale_fill_manual(values = category_colors, guide = "none") +
  scale_alpha_manual(values = c("FALSE" = 0.35, "TRUE" = 1), guide = "none") +
  scale_y_discrete(limits = rev(names(category_colors))) +
  coord_cartesian(xlim = xlims) +
  theme_minimal(base_size = 9) +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.minor = element_blank())

right_box <- vars_plot_scaled %>%
  filter(axis == "Axis.2") %>%
  ggplot(aes(x = category_Angarika, y = loading_scaled, fill = category_Angarika)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, width = 0.7) +
  geom_jitter(aes(alpha = significant), width = 0.15, height = 0,
              shape = 21, size = 1.3, color = "black", stroke = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  scale_fill_manual(values = category_colors, guide = "none") +
  scale_alpha_manual(values = c("FALSE" = 0.35, "TRUE" = 1), guide = "none") +
  scale_x_discrete(limits = names(category_colors)) +
  coord_cartesian(ylim = ylims) +
  theme_minimal(base_size = 9) +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 50))


# --- assemble: top box over main, right box beside main, empty corner ---
design <- "
A#
BC
"

combined <- top_box + main_plot + right_box +
  plot_layout(design = design, widths = c(4, 1), heights = c(1, 4))

combined


# Version 2 ####

library(patchwork)
library(ggrepel)

# ============================================================
# 0. Scaling factor: bring loadings onto the same visual range as site scores
# ============================================================

site_range_x <- diff(range(pred_resp$Axis.1, na.rm = TRUE))
site_range_y <- diff(range(pred_resp$Axis.2, na.rm = TRUE))

loading_range_x <- diff(range(vars_plot$loading[vars_plot$axis == "Axis.1"], na.rm = TRUE))
loading_range_y <- diff(range(vars_plot$loading[vars_plot$axis == "Axis.2"], na.rm = TRUE))

scale_factor <- min(site_range_x / loading_range_x,
                    site_range_y / loading_range_y) * 0.8

vars_plot_scaled <- vars_plot %>%
  mutate(loading_scaled = loading * scale_factor)

xr <- range(pred_resp$Axis.1, na.rm = TRUE)
yr <- range(pred_resp$Axis.2, na.rm = TRUE)
xlims <- xr + c(-1, 1) * diff(xr) * 0.05
ylims <- yr + c(-1, 1) * diff(yr) * 0.05

n_top <- 5  # how many top-loading variables to show per axis

# ============================================================
# 1. Main biplot
# ============================================================

n_label <- 12

labels_df2 <- pred_resp %>%
  left_join(societies) %>% 
  filter(soc_id %in% c("SCCS133", "SCCS69"))

labels_df <- pred_resp %>%
  left_join(societies) %>%
  mutate(dist_from_origin = sqrt(Axis.1^2 + Axis.2^2))  %>%
  slice_max(dist_from_origin, n = n_label)

main_plot <- pred_resp %>%
  ggplot(aes(x = Axis.1, y = Axis.2)) +
  geom_point(size = 2, alpha = 1) + # color = "#2C3E50"
  geom_point(data = labels_df, size = 2, 
             # color = "#B03A2E"
             ) +
  ggrepel::geom_label_repel(
    data = labels_df, aes(label = pref_name_for_society),   # <-- swap "society" for your actual name column
    size = 3, 
    # color = "#B03A2E", 
    fontface = "italic",
    max.overlaps = 20, segment.size = 0.3, segment.color = "grey50"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  coord_cartesian(xlim = xlims, ylim = ylims) +
  # labs(
  #   x = "Axis 1  \u2192  higher status of women",   # <-- fill in
  #   y = "Axis 2  \u2192  more work autonomy, less sexual freedom"
  # ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.x = element_text(hjust = 0.5, size = 10, color = "grey30"),
    axis.title.y = element_text(hjust = 0.5, size = 10, color = "grey30"),
    panel.grid.minor = element_blank()
  )

# ============================================================
# 2. Top marginal: top-loading variables on Axis 1, horizontal bars
#    (continuous axis = x, aligned with Axis 1 on the biplot)
# ============================================================

top_vars_1 <- vars_plot_scaled %>%
  filter(axis == "Axis.1") %>%
  slice_max(abs(loading), n = n_top) %>%
  mutate(title_shortened = fct_reorder(title_shortened, loading))

top_box <- top_vars_1 %>%
  ggplot(aes(x = loading_scaled, y = title_shortened, fill = category_Angarika)) +
  geom_col(alpha = 0.85, width = 0.7) +
  geom_text(aes(x = 0, label = title_shortened),
            hjust = 1, nudge_x = -0.01 * diff(xlims),  # small gap from zero line
            size = 2.5, color = "grey20") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  scale_fill_manual(values = category_colors, guide = "none") +
  coord_cartesian(xlim = xlims, clip = "off") +
  theme_minimal(base_size = 8) +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),   # turn off default labels, replaced by geom_text
        axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank())




# ============================================================
# 3. Right marginal: top-loading variables on Axis 2, vertical bars
#    (continuous axis = y, aligned with Axis 2 on the biplot)
# ============================================================

top_vars_2 <- vars_plot_scaled %>%
  filter(axis == "Axis.2") %>%
  slice_max(abs(loading), n = n_top) %>%
  mutate(title_shortened = fct_reorder(title_shortened, loading))

right_box <- top_vars_2 %>%
  ggplot(aes(x = title_shortened, y = loading_scaled, fill = category_Angarika)) +
  geom_col(alpha = 0.85, width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  scale_fill_manual(values = category_colors, guide = "none") +
  coord_cartesian(ylim = ylims) +
  theme_minimal(base_size = 8) +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 7, angle = 90, hjust = 1, vjust = 0.5),
        panel.grid.minor = element_blank()) 
  # theme(axis.text.x = element_text(angle = 50))

right_box

# ============================================================
# 4. Legend (colors = domain)
# ============================================================

legend_plot <- ggplot(
  data.frame(category = factor(names(category_colors), levels = names(category_colors))),
  aes(x = 1, y = category, fill = category)
) +
  geom_tile() +
  scale_fill_manual(values = category_colors, name = "Domain") +
  theme(legend.position = "right")

standalone_legend <- cowplot::get_legend(legend_plot)

# ============================================================
# 5. Assemble
# ============================================================

design <- "
#A
CB
"

substack_plot <- top_box + main_plot + right_box +
  plot_layout(design = design, widths = c(1, 4), heights = c(1, 4, 0.4)) +
  plot_annotation(
    title = "Variation across societies",
    subtitle = "Each point is one society; closer points are more similar overall in women's status variables",
    # caption = "Bars show the variables that load most strongly on each axis, colored by domain.",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.caption = element_text(size = 8, color = "grey50", hjust = 0)
    )
  )

substack_plot



# Simple one 

top_box2 <- top_vars_1 %>%
  ggplot(aes(x = loading_scaled, y = title_shortened, fill = category_Angarika)) +
  geom_col(alpha = 0.85, width = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  scale_fill_manual(values = category_colors, guide = "none") +
  coord_cartesian(xlim = c(-0.05, 0.25)) +
  theme_minimal(base_size = 8) +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 7),
        panel.grid.minor = element_blank())

design <- "
ABC
"

substack_plot_loadings <- wrap_elements(standalone_legend) + top_box2 + right_box +
  plot_layout(design = design, widths = c(0.2,0.4,0.4), heights = c(0.5,1, 1)) +
  plot_annotation(
    title = "Variation across societies",
    subtitle = "Each point is one society; closer points are more similar overall in women's status variables",
    # caption = "Bars show the variables that load most strongly on each axis, colored by domain.",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.caption = element_text(size = 8, color = "grey50", hjust = 0)
    )
  )

substack_plot_loadings


ggsave("output/substack/main_plot.png", main_plot)



# Biplot arrows ####
library(ggrepel)

# ============================================================
# 0. Combine top variables from both axes into one arrow dataset
#    Each variable needs BOTH its Axis.1 and Axis.2 loading to draw an arrow
# ============================================================

# pick which variables to show as arrows — e.g. union of top loaders on each axis
arrow_vars <- union(top_vars_1$title_shortened, top_vars_2$title_shortened)

arrows_df <- vars_plot %>%
  filter(title_shortened %in% arrow_vars, axis %in% c("Axis.1", "Axis.2")) %>%
  select(title_shortened, axis, loading, category_Angarika) %>%
  pivot_wider(names_from = axis, values_from = loading, names_prefix = "") %>%
  rename(loading_x = Axis.1, loading_y = Axis.2)

# ============================================================
# 1. Scaling factor: bring arrow lengths onto the same visual range as site scores
#    (arrows use the full 2D vector, so scale by the joint magnitude)
# ============================================================

site_range <- max(diff(range(pred_resp$Axis.1, na.rm = TRUE)),
                  diff(range(pred_resp$Axis.2, na.rm = TRUE)))

loading_range <- max(sqrt(arrows_df$loading_x^2 + arrows_df$loading_y^2), na.rm = TRUE)

arrow_scale <- (site_range / loading_range) * 0.4  # 0.4 = headroom so arrows don't overwhelm points; tweak to taste

arrows_df <- arrows_df %>%
  mutate(
    x_end = loading_x * arrow_scale,
    y_end = loading_y * arrow_scale
  )

# ============================================================
# 2. Biplot with arrows
# ============================================================

biplot_arrows <- pred_resp %>%
  ggplot(aes(x = Axis.1, y = Axis.2)) +
  # geom_point(size = 2.2, alpha = 0.5, color = "#2C3E50") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  geom_segment(
    data = arrows_df,
    aes(x = 0, y = 0, xend = x_end, yend = y_end, color = category_Angarika),
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    linewidth = 0.7, inherit.aes = FALSE
  ) +
  ggrepel::geom_label_repel(
    data = arrows_df,
    aes(x = x_end, y = y_end, label = title_shortened, color = category_Angarika),
    size = 3, fontface = "italic", inherit.aes = FALSE,
    max.overlaps = 20, segment.size = 0.2, segment.color = "grey60",
    box.padding = 0.4
  ) +
  scale_color_manual(values = category_colors, name = "Domain") +
  coord_cartesian(xlim = xlims, ylim = ylims) +
  labs(
    x = "Axis 1: Higher women's status \u2192",   # <-- fill in
    y = "Axis 2"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.x = element_text(hjust = 0.5, size = 10, color = "grey30"),
    axis.title.y = element_text(hjust = 0.5, size = 10, color = "grey30"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))


ggsave("output/substack/biplot_arrows.png", biplot_arrows)



# Twana and Garo ####

labels_df2 <- pred_resp %>%
  left_join(societies) %>% 
  filter(soc_id %in% c("SCCS133", "SCCS69"))

twana <- pred_resp %>%
  ggplot(aes(x = Axis.1, y = Axis.2)) +
  geom_point(size = 2, alpha = 1) + # color = "#2C3E50"
  geom_point(data = labels_df2, size = 2, 
             color = "#B03A2E"
  ) +
  ggrepel::geom_label_repel(
    data = labels_df2, aes(label = pref_name_for_society),   # <-- swap "society" for your actual name column
    size = 4, 
    color = "#B03A2E",
    fontface = "italic",
    max.overlaps = 20, segment.size = 0.3, segment.color = "grey50"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  coord_cartesian(xlim = xlims, ylim = ylims) +
  labs(
    x = "Axis 1: increasing women's status \u2192",   # <-- fill in
    # y = "Axis 2  \u2192  more work autonomy, less sexual freedom"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.x = element_text(hjust = 0.5, size = 10, color = "grey30"),
    axis.title.y = element_text(hjust = 0.5, size = 10, color = "grey30"),
    panel.grid.minor = element_blank()
  )

twana

ggsave("output/substack/twana_garo_plot.png", twana)
