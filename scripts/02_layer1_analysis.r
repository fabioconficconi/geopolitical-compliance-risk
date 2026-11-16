# ============================================================
# LAYER 1 - Country Risk PCA & Clustering
# Applied Statistics Thesis - Politecnico di Milano
# ============================================================

# Libraries
library(tidyverse)
library(cluster)

# ── 0. Paths relative to the repository root ─────────────────
# Detect the script folder (this file lives in scripts/), then go up
# one level to the project root. Works both with Rscript and RStudio.
get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  m <- grep("--file=", args)
  if (length(m) > 0) {
    return(dirname(normalizePath(sub("--file=", "", args[m]))))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    return(dirname(rstudioapi::getSourceEditorContext()$path))
  }
  # Last resort: assume the working directory is the repo root
  return(file.path(getwd(), "scripts"))
}
ROOT <- normalizePath(file.path(get_script_dir(), ".."))
RAW  <- file.path(ROOT, "data", "raw")
PROC <- file.path(ROOT, "data", "processed")

# ── 1. Load data ─────────────────────────────────────────────
layer1 <- read.csv(file.path(PROC, "layer1_input.csv"))

# Quick check
dim(layer1)
head(layer1)
summary(layer1)

# ── 2. Handle missing values ──────────────────────────────────
# How many countries have at least one NA in WGI or LPI?
na_countries <- layer1[!complete.cases(layer1), "iso3"]
cat("Countries with missing values:", length(na_countries), "\n")
cat(na_countries, "\n")

# Strategy: impute with column median
# (removing them would lose ~25% of countries - too many)
vars_to_impute <- c("wgi_voice_accountability", "wgi_gov_effectiveness",
                    "wgi_regulatory_quality", "wgi_rule_of_law",
                    "wgi_control_corruption", "lpi_score")

for (v in vars_to_impute) {
  layer1[[v]][is.na(layer1[[v]])] <- median(layer1[[v]], na.rm = TRUE)
}

cat("Missing after imputation:", sum(is.na(layer1)), "\n")

# ── 2b. Remove World Bank regional aggregates ─────────────────
# These are 3-letter codes that do not correspond to real countries.
# They are identified because they are not in the standard iso3_reference.

iso_ref <- read.csv(file.path(RAW, "iso3_reference.csv"))
valid_iso3 <- iso_ref$iso3

# Keep only real countries
layer1 <- layer1[layer1$iso3 %in% valid_iso3, ]

cat("Real countries after cleaning:", nrow(layer1), "\n")

# ── 3. Build the variable matrix for PCA ─────────────────────
# Select only numeric risk variables (exclude iso3)
risk_vars <- c("wgi_voice_accountability", "wgi_gov_effectiveness",
               "wgi_regulatory_quality", "wgi_rule_of_law",
               "wgi_control_corruption", "lpi_score",
               "crc_rating", "n_sanctioned_entities",
               "sanctioned_flag", "circumvention_flag",
               "circumvention_level")

X <- layer1[, risk_vars]
rownames(X) <- layer1$iso3

# Check
dim(X)
summary(X)

# ── 4. Prepare the final matrix for PCA ───────────────────────

# Remove redundant binary variables
# sanctioned_flag is redundant with n_sanctioned_entities
# circumvention_flag is redundant with circumvention_level
risk_vars_pca <- c("wgi_voice_accountability", "wgi_gov_effectiveness",
                   "wgi_regulatory_quality", "wgi_rule_of_law",
                   "wgi_control_corruption", "lpi_score",
                   "crc_rating", "n_sanctioned_entities",
                   "circumvention_level")

X <- layer1[, risk_vars_pca]
rownames(X) <- layer1$iso3

# Log-transform n_sanctioned_entities (strongly right-skewed)
X$n_sanctioned_entities <- log1p(X$n_sanctioned_entities)

# Standardize: mean 0, variance 1
X_scaled <- scale(X)

# Check
cat("PCA matrix dimensions:", dim(X_scaled), "\n")
cat("Means (should be ~0):\n")
round(colMeans(X_scaled), 3)
cat("Variances (should be 1):\n")
round(apply(X_scaled, 2, var), 3)

# ── 5. PCA ────────────────────────────────────────────────────

pc <- prcomp(X_scaled, scores = TRUE)
summary(pc)

# Scree plot
par(mfrow = c(1, 2))

# Variance explained by each PC
barplot(pc$sdev^2, 
        names.arg = paste0("PC", 1:length(pc$sdev)),
        main = "Variance per Component",
        ylab = "Variance", col = "steelblue", las = 2)

# Cumulative variance
plot(cumsum(pc$sdev^2) / sum(pc$sdev^2), 
     type = "b", pch = 19, col = "steelblue",
     xlab = "Number of Components", 
     ylab = "Cumulative Variance Explained",
     main = "Cumulative Variance",
     ylim = c(0, 1))
abline(h = 0.8, col = "red", lty = 2)
abline(h = 0.9, col = "orange", lty = 2)

par(mfrow = c(1, 1))

# Loadings of the first 3 PCs
cat("\nLoadings PC1, PC2, PC3:\n")
print(round(pc$loadings[, 1:3], 3))

# How many PCs explain >80% of variance?
cum_var <- cumsum(pc$sdev^2) / sum(pc$sdev^2)
cat("\nCumulative variance per PC:\n")
print(round(cum_var, 3))
cat("\nPCs needed for >80% variance:", which(cum_var >= 0.8)[1], "\n")
cat("PCs needed for >90% variance:", which(cum_var >= 0.9)[1], "\n")

# ── 6. Interpret the loadings ─────────────────────────────────

par(mfrow = c(1, 2), mar = c(8, 4, 4, 2))

short_names <- c("voice", "gov_eff", "reg_qual", "rule_law", 
                 "corruption", "lpi", "crc", "sanctions", "circumvention")

barplot(pc$loadings[, 1], 
        names.arg = short_names,
        main = "PC1 (63% variance)",
        col = ifelse(pc$loadings[, 1] > 0, "tomato", "steelblue"),
        las = 2, ylim = c(-1, 1), cex.names = 0.85)
abline(h = 0)

barplot(pc$loadings[, 2], 
        names.arg = short_names,
        main = "PC2 (17% variance)",
        col = ifelse(pc$loadings[, 2] > 0, "tomato", "steelblue"),
        las = 2, ylim = c(-1, 1), cex.names = 0.85)
abline(h = 0)

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))

# Biplot PC1 vs PC2
biplot(pc, choices = c(1, 2), cex = 0.5,
       main = "Biplot PC1 vs PC2")
# ── PCA RESULTS INTERPRETATION ────────────────────────────────
#
# VARIABLES USED:
# WGI indicators (scale -2.5 to +2.5): five dimensions of
#   institutional quality measured by the World Bank.
#   Positive = well-governed, Negative = weak institutions.
#   - voice_accountability: freedom of citizens and media
#   - gov_effectiveness: quality of public administration
#   - regulatory_quality: clarity and stability of market rules
#   - rule_of_law: respect for laws and contracts
#   - control_corruption: absence of corruption
#
# lpi_score (scale 1-5): logistics efficiency - customs,
#   infrastructure, delivery reliability. High = reliable
#   supply chain. Low = operational risk.
#
# crc_rating (scale 0-7): OECD sovereign credit risk.
#   Probability that the country cannot service its external
#   debt. 0 = minimum risk (Germany, Italy). 7 = maximum
#   risk (Russia, Iran, Yemen).
#
# n_sanctioned_entities: number of individuals, companies
#   or vessels from that country listed on international
#   sanctions lists (EU, OFAC, UK HMT). Log-transformed
#   due to strong right skew (Russia ~21,000 vs median ~35).
#
# circumvention_level (0/1/2): documented involvement in
#   sanctions evasion routing toward Russia post-2022.
#   Based on trade anomaly reports (Kyiv School of Economics).
#   0 = no role, 1 = moderate, 2 = systematic (UAE, Turkey,
#   Armenia, Kazakhstan).
#
# ─────────────────────────────────────────────────────────────
#
# PC1 — "Institutional Quality" (63% of variance)
#
# All five WGI indicators and lpi_score load positively
# (~0.33 to 0.41), while crc_rating loads negatively (-0.35).
# PC1 is an axis running from well-governed, logistically
# efficient, low-sovereign-risk countries (high scores:
# DEU, CHE, FIN, NOR, SGP) to poorly governed, logistically
# weak, high-sovereign-risk countries (low scores: SYR, PRK,
# YEM, SOM, VEN). This is the "structural country risk"
# dimension — the classic geopolitical risk axis.
#
# PC2 — "Sanctions and Evasion Risk" (17% of variance)
#
# n_sanctioned_entities (-0.68) and circumvention_level
# (-0.60) dominate with strong negative loadings.
# voice_accountability loads positively (0.24).
# PC2 separates countries with many sanctioned entities OR
# documented evasion hub behavior (low scores: RUS, UZB,
# SRB, TUR, KAZ, ARE, CHN, ARM) from democratic countries
# with no sanctions exposure (high scores: small island
# states, minor democracies).
#
# BIPLOT INTERPRETATION:
#
# Top-right:  good institutions + no sanctions → safe markets
#             (DEU, CHE, FIN, NOR, NZL, IRL)
#
# Bottom-left: weak institutions + many sanctions → high risk
#              (SYR, PRK, IRN, RUS)
#
# Bottom-right: decent institutions but high sanctions/evasion
#               → "grey zone" hubs
#               (ARE, TUR, KAZ, ARM, CHN, SGP)
#
# Left, middle: fragile states with weak governance but few
#               sanctions → operational risk without legal risk
#               (many Sub-Saharan African countries)
#
# The two-component structure already reveals three natural
# groupings, which will be formalized in the clustering step.
# ─────────────────────────────────────────────────────────────
# ── 7. CLUSTERING ─────────────────────────────────────────────

# Extract PC1 and PC2 scores
scores_2pc <- pc$scores[, 1:2]

# Compute Euclidean distance matrix on PC scores
dist_matrix <- dist(scores_2pc, method = "euclidean")

# Ward linkage hierarchical clustering
hc_ward <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc_ward, 
     main = "Dendrogram - Ward Linkage (PC1 + PC2)",
     xlab = "", sub = "", 
     labels = FALSE, hang = -1)

# ── 8. Choose number of clusters ──────────────────────────────

# Silhouette analysis for k = 2 to 6
sil_scores <- sapply(2:6, function(k) {
  cl <- cutree(hc_ward, k = k)
  sil <- silhouette(cl, dist_matrix)
  mean(sil[, 3])
})

par(mfrow = c(1, 2))

plot(2:6, sil_scores, type = "b", pch = 19, col = "steelblue",
     xlab = "Number of clusters k",
     ylab = "Average Silhouette Width",
     main = "Silhouette Analysis")
abline(v = which.max(sil_scores) + 1, col = "red", lty = 2)

# Add rectangles on dendrogram for optimal k
plot(hc_ward, main = "Dendrogram", 
     xlab = "", sub = "", labels = FALSE, hang = -1)
rect.hclust(hc_ward, k = which.max(sil_scores) + 1, border = "red")

par(mfrow = c(1, 1))

cat("Silhouette scores:\n")
print(round(sil_scores, 3))
cat("Optimal k:", which.max(sil_scores) + 1, "\n")

# ── 9. Cut tree at k=3 and characterize clusters ──────────────

k <- 3
clusters <- cutree(hc_ward, k = k)
layer1$cluster <- clusters

# How many countries per cluster?
table(clusters)

# Plot countries in PC space colored by cluster
plot(scores_2pc, 
     col = c("steelblue", "tomato", "darkgreen")[clusters],
     pch = 19, cex = 0.8,
     xlab = "PC1 — Institutional Quality",
     ylab = "PC2 — Sanctions & Evasion Risk",
     main = "Country Clusters (k=3) on PC1-PC2")

text(scores_2pc, labels = rownames(scores_2pc), 
     col = c("steelblue", "tomato", "darkgreen")[clusters],
     cex = 0.5, pos = 3)

legend("topright", legend = paste("Cluster", 1:3),
       col = c("steelblue", "tomato", "darkgreen"),
       pch = 19)

# Mean of each variable by cluster
aggregate(layer1[, risk_vars_pca], 
          by = list(cluster = layer1$cluster), 
          FUN = mean)
# ── 10. Assign cluster risk score and save ────────────────────

# Assign risk score based on cluster profile
# Cluster 1 = stable markets → risk 0.0
# Cluster 2 = high structural risk → risk 1.0  
# Cluster 3 = grey hubs → risk 0.5
risk_scores <- c("1" = 0.0, "2" = 1.0, "3" = 0.5)
layer1$cluster_risk_score <- risk_scores[as.character(layer1$cluster)]

# Summary
cat("Countries per cluster:\n")
print(table(layer1$cluster))

cat("\nCluster profiles:\n")
print(aggregate(cluster_risk_score ~ cluster, data = layer1, FUN = mean))

# Save output
write.csv(layer1[, c("iso3", "cluster", "cluster_risk_score")],
          file.path(PROC, "layer1_countries.csv"),
          row.names = FALSE)

cat("\n✓ Saved: layer1_countries.csv\n")
cat("This is the input for Layer 2 firm exposure.\n")

# ── 10c. Combined risk score ───────────────────────────────────
# Score = distance from safe centroid - distance from high-risk centroid
# Normalized to [0,1]
# High score = far from safe + close to high-risk = risky
# Low score  = close to safe + far from high-risk = safe

# Cluster centroids in PC1-PC2 space (needed for the distances)
centroids <- aggregate(as.data.frame(scores_2pc),
                       by = list(cluster = clusters), FUN = mean)

# Data-driven identification of the reference clusters.
# We do not rely on cutree's numeric label (arbitrary) nor on the sign
# of the PCs: the cluster with the best average institutional quality
# (highest rule of law) is "safe", the one with the worst is "high-risk".
gov_by_cluster   <- tapply(layer1$wgi_rule_of_law, clusters, mean)
safe_cluster     <- as.integer(names(which.max(gov_by_cluster)))
highrisk_cluster <- as.integer(names(which.min(gov_by_cluster)))
cat("Safe cluster:", safe_cluster, "| High-risk cluster:", highrisk_cluster, "\n")

centroid_safe     <- as.numeric(centroids[centroids$cluster == safe_cluster,     c("Comp.1", "Comp.2")])
centroid_highrisk <- as.numeric(centroids[centroids$cluster == highrisk_cluster, c("Comp.1", "Comp.2")])

dist_to_safe     <- apply(scores_2pc, 1, function(x) sqrt(sum((x - centroid_safe)^2)))
dist_to_highrisk <- apply(scores_2pc, 1, function(x) sqrt(sum((x - centroid_highrisk)^2)))

# Raw combined score: far from safe, close to risky
raw_score <- dist_to_safe - dist_to_highrisk

# Normalize to [0, 1]
layer1$cluster_risk_score <- (raw_score - min(raw_score)) / 
  (max(raw_score) - min(raw_score))

# Check key countries
key_countries <- c("DEU", "FIN", "ITA", "RUS", "IRN", "PRK",
                   "ARE", "TUR", "CHN", "KAZ", "IND", "USA", "GBR")
check <- layer1[layer1$iso3 %in% key_countries, 
                c("iso3", "cluster", "cluster_risk_score")]
check <- check[order(check$cluster_risk_score, decreasing = TRUE), ]
check$cluster_risk_score <- round(check$cluster_risk_score, 3)
print(check)

# Distribution
hist(layer1$cluster_risk_score, 
     breaks = 20, col = "steelblue",
     main = "Distribution of Country Risk Scores (combined)",
     xlab = "Risk Score (0 = safe, 1 = high risk)")

# Save
write.csv(layer1[, c("iso3", "cluster", "cluster_risk_score")],
          file.path(PROC, "layer1_countries.csv"),
          row.names = FALSE)

cat("\n✓ Saved with combined risk scores.\n")

# ── CLUSTERING RESULTS INTERPRETATION ─────────────────────────
#
# METHOD: Ward hierarchical clustering on the first two
# principal components (PC1 + PC2, explaining 80% of variance).
# Number of clusters chosen as k=3 based on substantive
# interpretation. Although k=2 maximizes the average silhouette
# width (0.44), k=3 (silhouette = 0.367) produces three
# interpretable clusters aligned with the theoretical framework
# of the project. The silhouette score for k=3 remains in the
# acceptable range (> 0.25).
#
# ─────────────────────────────────────────────────────────────
# CLUSTER 1 — Stable Markets (n = 64)
#
# High institutional quality (WGI all positive, ~+1.0),
# high logistics performance (LPI = 3.29), low sovereign
# credit risk (CRC = 0.28), few sanctioned entities (139),
# no circumvention involvement.
# Examples: DEU, FIN, FRA, NOR, CHE, NZL, IRL, AUS, USA, GBR
# Risk score range: ~0.01 to ~0.25
#
# ─────────────────────────────────────────────────────────────
# CLUSTER 2 — High Structural Risk (n = 90)
#
# Weak institutions (WGI all negative, ~-0.76), low logistics
# performance (LPI = 2.49), high sovereign credit risk
# (CRC = 6.33), moderate sanctioned entities (441),
# minimal circumvention involvement.
# These are countries with poor governance and high financial
# risk — not necessarily sanctioned hubs, but structurally
# fragile and operationally risky.
# Examples: SYR, PRK, IRN, YEM, AFG, SOM, VEN, BLR, RUS
# Risk score range: ~0.70 to ~1.00
#
# ─────────────────────────────────────────────────────────────
# CLUSTER 3 — Grey Zone / Evasion Hubs (n = 61)
#
# Mixed institutional quality (WGI near zero), medium
# logistics (LPI = 2.96), medium sovereign risk (CRC = 2.6),
# but high circumvention level (mean = 0.245) — the defining
# characteristic of this cluster.
# These countries are not directly sanctioned and may appear
# low-risk by standard governance metrics, but are documented
# as transit hubs for sanctions evasion post-2022.
# This is the most compliance-relevant cluster for PwC:
# commercial relationships with these countries require
# enhanced due diligence even in the absence of direct
# sanctions exposure.
# Examples: ARE, TUR, KAZ, ARM, CHN, SGP, HKG, MYS, IND, SRB
# Risk score range: ~0.35 to ~0.61
#
# ─────────────────────────────────────────────────────────────
# CONTINUOUS RISK SCORE METHODOLOGY
#
# Instead of assigning discrete values (0, 0.5, 1.0) to each
# cluster, each country receives a continuous score in [0,1]
# based on its position in PC space relative to the two
# anchor centroids:
#
#   raw_score = dist(country, safe_centroid)
#             - dist(country, highrisk_centroid)
#
# A high raw score means the country is far from the safe
# cluster AND close to the high-risk cluster.
# The score is then min-max normalized to [0,1].
#
# This approach has three advantages over discrete scoring:
# 1. Preserves within-cluster heterogeneity (e.g. Russia and
#    Belarus are both in cluster 2 but have different scores)
# 2. Grey hubs are naturally placed at intermediate values
#    (~0.35-0.61) without arbitrary assignment
# 3. The score is fully data-driven and reproducible
#
# Key country scores (ordered by risk):
# PRK 0.972 | IRN 0.940 | RUS 0.818  (cluster 2)
# KAZ 0.606 | TUR 0.572 | CHN 0.519 | IND 0.486 | ARE 0.356
#                                                  (cluster 3)
# ITA 0.194 | USA 0.119 | GBR 0.043 | DEU 0.032 | FIN 0.011
#                                                  (cluster 1)
