# =============================================================================
# Schematic figures for PCoA slides
# Fake data structured to tell the story:
#   - Variables belong to theoretical groups (colors)
#   - Variables within groups do NOT correlate
#   - But all variables load on PC1 in the same direction
# =============================================================================

library(ggplot2)
library(reshape2)
library(patchwork)
library(ggrepel)

set.seed(42)

# --- Parameters ---------------------------------------------------------------

n_soc <- 40   # number of societies
n_var <- 15   # number of variables

# Theoretical variable groups (colors)
groups <- c(
  rep("Subsistence",   5),
  rep("Social org.",   5),
  rep("Ritual",        5)
)
var_names <- paste0(
  c(rep("S", 5), rep("O", 5), rep("R", 5)),
  rep(1:5, 3)
)

group_colors <- c(
  "Subsistence" = "#E07B39",
  "Social org."  = "#3A7DB5",
  "Ritual"       = "#6AAB6E"
)

# --- Simulate data ------------------------------------------------------------
# One latent dimension (PC1 gradient) + noise
# No within-group correlation structure

latent <- seq(-2, 2, length.out = n_soc)  # the true gradient

# Each variable = loading on latent + noise (no group structure in correlations)
loadings <- runif(n_var, 0.4, 0.9)  # all positive, varying strength
noise_sd <- 0.8

X <- matrix(NA, nrow = n_soc, ncol = n_var)
for (j in 1:n_var) {
  X[, j] <- loadings[j] * latent + rnorm(n_soc, 0, noise_sd)
}
colnames(X) <- var_names
rownames(X) <- paste0("Soc", 1:n_soc)

# Standardize
X_scaled <- scale(X)

# =============================================================================
# Figure 1: Societies x Variables matrix
# =============================================================================

df1 <- melt(X_scaled)
colnames(df1) <- c("Society", "Variable", "Value")
df1$Group <- groups[match(df1$Variable, var_names)]

# Order societies by latent gradient
df1$Society <- factor(df1$Society, levels = paste0("Soc", order(latent)))
df1$Variable <- factor(df1$Variable, levels = var_names)

fig1 <- ggplot(df1, aes(x = Variable, y = Society, fill = Value)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient2(
    low = "#4575b4", mid = "white", high = "#d73027",
    midpoint = 0, name = "Value\n(scaled)"
  ) +
  # Color variable names by group
  scale_x_discrete(position = "top") +
  annotate("rect",
           xmin = 0.5, xmax = 5.5, ymin = -Inf, ymax = Inf,
           fill = group_colors["Subsistence"], alpha = 0.08
  ) +
  annotate("rect",
           xmin = 5.5, xmax = 10.5, ymin = -Inf, ymax = Inf,
           fill = group_colors["Social org."], alpha = 0.08
  ) +
  annotate("rect",
           xmin = 10.5, xmax = 15.5, ymin = -Inf, ymax = Inf,
           fill = group_colors["Ritual"], alpha = 0.08
  ) +
  labs(
    title = "1. Societies × Variables",
    subtitle = "Rows = societies (ordered by PC1), columns = variables (colored by theoretical group)",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0, size = 8),
    axis.text.y = element_text(size = 6),
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

# =============================================================================
# Figure 2: Variable correlation matrix
# =============================================================================

cor_mat <- cor(X_scaled)
df2 <- melt(cor_mat)
colnames(df2) <- c("Var1", "Var2", "Correlation")
df2$Group1 <- groups[match(df2$Var1, var_names)]
df2$Group2 <- groups[match(df2$Var2, var_names)]

df2$Var1 <- factor(df2$Var1, levels = var_names)
df2$Var2 <- factor(df2$Var2, levels = var_names)

fig2 <- ggplot(df2, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_fill_gradient2(
    low = "#4575b4", mid = "white", high = "#d73027",
    midpoint = 0, limits = c(-1, 1), name = "r"
  ) +
  # Group background bands
  annotate("rect",
           xmin = 0.5, xmax = 5.5, ymin = 0.5, ymax = 5.5,
           fill = NA, color = group_colors["Subsistence"], linewidth = 1.2
  ) +
  annotate("rect",
           xmin = 5.5, xmax = 10.5, ymin = 5.5, ymax = 10.5,
           fill = NA, color = group_colors["Social org."], linewidth = 1.2
  ) +
  annotate("rect",
           xmin = 10.5, xmax = 15.5, ymin = 10.5, ymax = 15.5,
           fill = NA, color = group_colors["Ritual"], linewidth = 1.2
  ) +
  labs(
    title = "2. Variable correlation matrix",
    subtitle = "Boxes = theoretical groups. No block structure emerges.",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(face = "bold")
  )

# =============================================================================
# Figure 3: Society distance matrix
# =============================================================================

dist_mat <- as.matrix(dist(X_scaled, method = "euclidean"))

# Order by latent gradient
ord <- order(latent)
dist_mat_ord <- dist_mat[ord, ord]

df3 <- melt(dist_mat_ord)
colnames(df3) <- c("Soc1", "Soc2", "Distance")

fig3 <- ggplot(df3, aes(x = Soc1, y = Soc2, fill = Distance)) +
  geom_tile() +
  scale_fill_gradient(low = "#ffffcc", high = "#800026", name = "Distance") +
  labs(
    title = "3. Society distance matrix",
    subtitle = "Societies ordered by PC1 gradient — structure becomes visible",
    x = "Society", y = "Society"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold")
  )

# =============================================================================
# Figure 4: PCoA — points only
# =============================================================================

pca_res <- prcomp(X_scaled, center = TRUE, scale. = FALSE)
pca_df <- as.data.frame(pca_res$x[, 1:2])
pca_df$Society <- rownames(pca_df)

var_exp <- round(100 * pca_res$sdev^2 / sum(pca_res$sdev^2), 1)

fig4 <- ggplot(pca_df, aes(x = PC1, y = PC2)) +
  geom_point(size = 3, color = "grey30", alpha = 0.8) +
  labs(
    title = "4. PCoA — societies",
    subtitle = "Each point = one society",
    x = paste0("PC1 (", var_exp[1], "% variance)"),
    y = paste0("PC2 (", var_exp[2], "% variance)")
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

# =============================================================================
# Figure 5: PCoA — points + variable loadings (biplot)
# =============================================================================

# Variable arrows (scaled for visibility)
arrow_scale <- 3.5
loadings_df <- as.data.frame(pca_res$rotation[, 1:2])
loadings_df$Variable <- rownames(loadings_df)
loadings_df$Group <- groups[match(loadings_df$Variable, var_names)]
loadings_df$xend <- loadings_df$PC1 * arrow_scale
loadings_df$yend <- loadings_df$PC2 * arrow_scale

fig5 <- ggplot(pca_df, aes(x = PC1, y = PC2)) +
  geom_point(size = 3, color = "grey30", alpha = 0.7) +
  geom_segment(
    data = loadings_df,
    aes(x = 0, y = 0, xend = xend, yend = yend, color = Group),
    arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
    linewidth = 0.9
  ) +
  geom_text_repel(
    data = loadings_df,
    aes(x = xend, y = yend, label = Variable, color = Group),
    size = 3, show.legend = FALSE, max.overlaps = 20
  ) +
  scale_color_manual(values = group_colors, name = "Theoretical\ngroup") +
  labs(
    title = "5. PCoA — societies + variable loadings",
    subtitle = "All variables load in the same direction on PC1, regardless of group",
    x = paste0("PC1 (", var_exp[1], "% variance)"),
    y = paste0("PC2 (", var_exp[2], "% variance)")
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))


fig1
fig2
fig3
fig4
fig5
