# Increase bottom margin BEFORE plotting
par(mar = c(14, 4, 2, 1))

species <- c(
  "Australopithecus afarensis",
  "Australopithecus africanus",
  "Paranthropus boisei",
  "Paranthropus robustus",
  "Homo habilis",
  "Homo rudolfensis",
  "Homo ergaster",
  "early archaic Homo sapiens",
  "late archaic Homo sapiens",
  "Skhul-Qafzeh",
  "Early Upper Paleolithic",
  "Late Upper Paleolithic",
  "Living Humans"
)

# Original M/F values
mf <- c(
  1.55, 1.36, 1.44, 1.25, 1.16,
  1.18, 1.18, 1.07, 1.12, 1.17,
  1.11, 1.16, 1.15
)

# Convert to F/M
fm <- 1 / mf

plot(
  seq_along(species),
  fm,
  pch = 3,
  xaxt = "n",
  xlab = "",
  ylab = "Female weight / Male weight",
  ylim = c(min(fm) - 0.02, max(fm) + 0.02)
)

# Add rotated labels
text(
  x = seq_along(species),
  y = par("usr")[3] - 0.03,
  labels = species,
  srt = 55,
  adj = 1,
  xpd = TRUE,
  cex = 0.8
)