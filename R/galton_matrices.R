# ============================================================
# Building spatial and linguistic matrices to address Galton's
# problem in the height_ratio ~ Ax1 analysis
#
# Produces, for your 46 societies:
#   W_sp,   W_ling   -- row-standardized weight matrices
#                       (used for spatial/linguistic "lag" covariates)
#   A_sp,   A_ling    -- valid correlation matrices
#                       (used as `cov=` in brms's phylogenetic-style
#                        gr() random effects)
# ============================================================

library(dplyr)
library(Matrix)      # nearPD(), to force a valid correlation matrix

# Dependency-free replacement for geosphere::distm(fun = distHaversine).
# Takes a matrix/data.frame with columns (lon, lat) in decimal degrees,
# returns an n x n matrix of great-circle distances in km.
haversine_dist_matrix <- function(lonlat, R = 6371) {
  lon <- lonlat[, 1] * pi / 180
  lat <- lonlat[, 2] * pi / 180
  n <- length(lon)
  D <- matrix(0, n, n)
  for (i in seq_len(n - 1)) {
    dlon <- lon[(i + 1):n] - lon[i]
    dlat <- lat[(i + 1):n] - lat[i]
    a <- sin(dlat / 2)^2 + cos(lat[i]) * cos(lat[(i + 1):n]) * sin(dlon / 2)^2
    a <- pmin(pmax(a, 0), 1)          # guard against tiny floating-point overshoot
    d <- 2 * R * asin(sqrt(a))
    D[i, (i + 1):n] <- d
    D[(i + 1):n, i] <- d
  }
  D
}

# ------------------------------------------------------------
# 0. Prep: attach glottolog classification info to your 46 rows
# ------------------------------------------------------------
stopifnot(!anyNA(coords_extended$Long), !anyNA(coords_extended$Lat))

coords_extended_nogap <- coords_extended %>% filter(!is.na(height_ratio))
n   <- nrow(coords_extended_nogap)
ids <- coords_extended_nogap$soc_id

# ------------------------------------------------------------
# 1. SPATIAL matrix
# ------------------------------------------------------------
coords_mat <- as.matrix(coords_extended_nogap[, c("Long", "Lat")])
D_sp <- haversine_dist_matrix(coords_mat)   # km
dimnames(D_sp) <- list(ids, ids)

# 1a. Row-standardized inverse-distance weights (for LAG covariates, step 4)
W_sp <- 1 / (D_sp + 1)     # +1km guards the diagonal before we zero it
diag(W_sp) <- 0
W_sp <- W_sp / rowSums(W_sp)   # rows sum to 1

# 1b. Exponential-decay CORRELATION matrix (for the brms gr(cov=) approach, step 5)
# phi = distance scale over which similarity decays. Median pairwise
# distance is a reasonable data-driven default -- try a couple of
# alternatives (e.g. 1000km, or the 25th percentile) as a sensitivity check.
phi_sp <- median(D_sp[upper.tri(D_sp)])
A_sp <- exp(-D_sp / phi_sp)
diag(A_sp) <- 1
A_sp <- as.matrix(Matrix::nearPD(A_sp, corr = TRUE)$mat)  # force valid PD correlation matrix
dimnames(A_sp) <- list(ids, ids)

# ------------------------------------------------------------
# 2. LINGUISTIC matrix, from Glottolog classification strings
# ------------------------------------------------------------
# `affiliation` is a comma-separated path from the top-level family
# down to the most specific node, e.g.
#   "Atlantic-Congo, Volta-Congo, Benue-Congo, Bantoid, ..."
#
# Isolates need special handling: an empty/blank affiliation must NOT
# be treated as "shares everything with every other isolate." Instead
# each isolate gets its own unique pseudo-top-level node (its
# glottocode), so two different isolates are exactly as unrelated as
# two different top-level families.

parse_taxonomy <- function(affiliation, family, glottocode) {
  if (is.na(family) || family == "isolate" ||
      is.na(affiliation) || trimws(affiliation) == "") {
    return(paste0("ISOLATE_", glottocode))
  }
  trimws(strsplit(affiliation, ",")[[1]])
}

taxo <- Map(parse_taxonomy,
            coords_extended_nogap$affiliation,
            coords_extended_nogap$language_family,
            coords_extended_nogap$glottocode)
names(taxo) <- ids

# longest shared prefix between two classification paths
lcp_length <- function(a, b) {
  m <- min(length(a), length(b))
  if (m == 0) return(0)
  k <- 0
  for (i in seq_len(m)) {
    if (identical(a[i], b[i])) k <- i else break
  }
  k
}

# similarity_ij = (shared taxonomic depth) / (deeper of the two paths)
#   1   = identical language
#   0   = nothing shared (different families, or two different isolates)
S_ling <- matrix(0, n, n, dimnames = list(ids, ids))
for (i in seq_len(n)) {
  for (j in seq_len(n)) {
    shared <- lcp_length(taxo[[i]], taxo[[j]])
    S_ling[i, j] <- shared / max(length(taxo[[i]]), length(taxo[[j]]))
  }
}
diag(S_ling) <- 1

# 2a. Row-standardized weights (for LAG covariates, step 4)
W_ling <- S_ling
diag(W_ling) <- 0
row_sums <- rowSums(W_ling)
row_sums[row_sums == 0] <- 1   # guards societies with nothing in common with anyone
W_ling <- W_ling / row_sums

# 2b. CORRELATION matrix for brms gr(cov=), step 5
A_ling <- as.matrix(Matrix::nearPD(S_ling, corr = TRUE)$mat)
dimnames(A_ling) <- list(ids, ids)


# OR using the method of Dow 2007

# Eff's scheme: inverse distance squared, truncated beyond a cutoff, row-standardized
W_sp_eff <- 1 / D_sp^2
diag(W_sp_eff) <- 0
W_sp_eff <- W_sp_eff / rowSums(W_sp_eff)

# ------------------------------------------------------------
# 3. DIAGNOSTIC FIRST: is Galton's problem actually live in your
#    current model's residuals? Do this before building anything else.
# ------------------------------------------------------------
# library(ape)
resid_vec <- residuals(mod_domination_phylo)[, "Estimate"]
ape::Moran.I(resid_vec, W_sp)     # spatial autocorrelation in residuals
ape::Moran.I(resid_vec, W_ling)   # linguistic autocorrelation in residuals
ape::Moran.I(resid_vec, W_sp_eff)   # linguistic autocorrelation in residuals

# A significant Moran's I on either matrix means there's still
# structured non-independence left over after Ax1 + region6 -- i.e.
# Galton's problem is live for THIS outcome, and correcting for it
# is worth doing, not just a formality.

# ------------------------------------------------------------
# 4. OPTION A (cheap, robust at n=46): lagged covariates
# ------------------------------------------------------------
coords_extended_nogap$Ax1_splag     <- as.numeric(W_sp   %*% coords_extended_nogap$Ax1)
coords_extended_nogap$Ax1_linlag    <- as.numeric(W_ling %*% coords_extended_nogap$Ax1)
coords_extended_nogap$height_splag  <- as.numeric(W_sp   %*% coords_extended_nogap$height_ratio)
coords_extended_nogap$height_linlag <- as.numeric(W_ling %*% coords_extended_nogap$height_ratio)

mod_lag <- brm(
  height_ratio ~ Ax1 + height_splag + height_linlag + (1|region6),
  family = gaussian(),
  prior = c(prior(normal(0, 0.5), class = b),
            prior(normal(0, 0.5), class = Intercept),
            prior(exponential(1), class = sd),
            prior(exponential(1), class = sigma)),
  data = coords_extended_nogap,
  cores = 4, iter = 4000, seed = 42,
  control = list(adapt_delta = 0.99)
)
# If Ax1's coefficient and CI barely move once height_splag/height_linlag
# are added, that's good evidence Galton's problem isn't driving the result.

summary(mod_lag)
mod_lag %>% bayes_R2() 

# ------------------------------------------------------------
# 5. OPTION B (fuller): correlated random effects in brms
#    (same machinery as phylogenetic mixed models)
# ------------------------------------------------------------
# coords_extended_nogap$soc_sp <- coords_extended_nogap$soc_id  # spatial grouping factor
# coords_extended_nogap$soc_ln <- coords_extended_nogap$soc_id  # linguistic grouping factor
#
# mod_galton <- brm(
#   bf(height_ratio ~ Ax1 +
#        (1 | gr(soc_sp, cov = A_sp)) +
#        (1 | gr(soc_ln, cov = A_ling))),
#   data = coords_extended,
#   data2 = list(A_sp = A_sp, A_ling = A_ling),
#   family = gaussian(),
#   prior = c(prior(normal(0, 0.5), class = b),
#             prior(normal(0, 0.5), class = Intercept),
#             prior(exponential(1), class = sd),
#             prior(exponential(1), class = sigma)),
#   cores = 4, iter = 4000, seed = 42,
#   control = list(adapt_delta = 0.99)
# )
# With n=46 and one predictor, expect these two variance components to be
# weakly identified (wide posteriors) -- report as a sensitivity/robustness
# analysis, not as a replacement for the diagnostic in step 3.



# Dow's model ####

library(ivreg)

# endogenous regressors: network lags of the OUTCOME
coords_extended_nogap$Wy_sp   <- as.numeric(W_sp   %*% coords_extended_nogap$height_ratio)
coords_extended_nogap$Wy_ling <- as.numeric(W_ling %*% coords_extended_nogap$height_ratio)

# structural equation | instrument set
# (Ax1_splag, Ax1_linlag = lags of the EXOGENOUS predictor, already in your script)
mod_dow_2sls <- ivreg(
  height_ratio ~ Ax1 + Wy_sp + Wy_ling |
    Ax1 + Ax1_splag + Ax1_linlag,
  data = coords_extended_nogap
)

summary(mod_dow_2sls, diagnostics = TRUE)



mod_dow_sp_only <- ivreg(
  height_ratio ~ Ax1 + Wy_sp | Ax1 + Ax1_splag + Ax1_linlag,
  data = coords_extended_nogap
)
summary(mod_dow_sp_only, diagnostics = TRUE)

mod_dow_ling_only <- ivreg(
  height_ratio ~ Ax1 + Wy_ling | Ax1 + Ax1_splag + Ax1_linlag,
  data = coords_extended_nogap
)
summary(mod_dow_ling_only, diagnostics = TRUE)

## NEGATIVE R square. The model does not work. Not enough data.

# Phylogenetic model ####

coords_extended_nogap$soc_sp <- coords_extended_nogap$soc_id
coords_extended_nogap$soc_ln <- coords_extended_nogap$soc_id

mod_galton <- brm(
  bf(height_ratio ~ Ax1 +
       (1 | gr(soc_sp, cov = A_sp)) +
       (1 | gr(soc_ln, cov = A_ling))),
  data = coords_extended_nogap,
  data2 = list(A_sp = A_sp, A_ling = A_ling),
  family = gaussian(),
  prior = c(prior(normal(0, 0.5), class = b),
            prior(normal(0, 0.5), class = Intercept),
            prior(exponential(1), class = sd),
            prior(exponential(1), class = sigma)),
  cores = 4, iter = 4000, seed = 42,
  control = list(adapt_delta = 0.99)
)
summary(mod_galton)



mod_galton_sp <- brm(
  bf(height_ratio ~ Ax1 + (1 | gr(soc_sp, cov = A_sp))),
  data = coords_extended_nogap, data2 = list(A_sp = A_sp),
  family = gaussian(),
  prior = c(prior(normal(0, 0.5), class = b),
            prior(normal(0, 0.5), class = Intercept),
            prior(exponential(1), class = sd),
            prior(exponential(1), class = sigma)),
  cores = 4, iter = 4000, seed = 42, control = list(adapt_delta = 0.99)
)
summary(mod_galton_sp)
mod_galton_sp %>%  bayes_R2()

mod_galton_ling <- update(mod_galton_sp,
                          formula. = height_ratio ~ Ax1 + (1 | gr(soc_ln, cov = A_ling)),
                          newdata = coords_extended_nogap, data2 = list(A_ling = A_ling)
)
mod_galton_ling_adapt <- update(
  mod_galton_ling,
  iter = 8000, warmup = 4000,
  control = list(adapt_delta = 0.999, max_treedepth = 15)
)
summary(mod_galton_ling)
mod_galton_ling %>%  bayes_R2()

summary(mod_galton_ling_adapt)
mod_galton_ling_adapt %>%  bayes_R2()


lambda <- 0.8
A_ling_reg <- lambda * A_ling + (1 - lambda) * diag(nrow(A_ling))
mod_galton_ling_reg <- update(mod_galton_ling, newdata = coords_extended_nogap,
                              data2 = list(A_ling = A_ling_reg))

summary(mod_galton_ling_reg)
mod_galton_ling_reg %>%  bayes_R2()
