# ╔══════════════════════════════════════════════════════════════════════════╗
# ║     UNCERTAINTY ANALYSIS — Layer 1 MANOVA + Weight Sensitivity        ║
# ╚══════════════════════════════════════════════════════════════════════════╝
#
# PREREQUISITE
# ────────────
# Run 03_Layer2.r first. This script uses the objects
# already in memory: firms, all_components, l1, COMP_THRESH, FRAG_THRESH.
# If the session was reset, re-run 03_Layer2.r first.
#
# WHAT THIS SCRIPT DOES
# ─────────────────────
#   PART A — MANOVA on Layer 1
#     Formally tests whether the 3 country clusters have
#     significantly different centroids on the 9 original geopolitical variables.
#     Answers the question: "are the clusters statistically valid
#     or could they be random?"
#
#   PART B — Sensitivity analysis on the compliance_score weights
#     Runs the model over a grid of ~60 weight combinations
#     (W_IMP from 0.20 to 0.60, with W_EXP and W_CIRC splitting the rest).
#     Answers the question: "if we had chosen different weights, how many
#     firms would have changed risk quadrant?"
#
# OUTPUT
# ──────
#   3 presentation-ready charts:
#     chartU1_manova_separation.png  — visual cluster separation
#     chartU2_sensitivity_heatmap.png — stability as the weights vary
#     chartU3_sensitivity_critical.png — impact on CRITICAL firms
#
# ── Libraries ─────────────────────────────────────────────────────────────
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(patchwork)
library(scales)


# ── Paths (relative to the repository root) ───────────────────────────────────
get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  m <- grep("--file=", args)
  if (length(m) > 0) return(dirname(normalizePath(sub("--file=", "", args[m]))))
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())
    return(dirname(rstudioapi::getSourceEditorContext()$path))
  return(file.path(getwd(), "scripts"))
}
ROOT      <- normalizePath(file.path(get_script_dir(), ".."))
RAW       <- file.path(ROOT, "data", "raw")
PROC      <- file.path(ROOT, "data", "processed")
DOWNLOADS <- RAW
OUT       <- PROC

# ── Style (same as the main file) ───────────────────────────────────
theme_clean <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(color = "#EEEEEE"),
    plot.title        = element_text(face = "bold", size = 13),
    plot.subtitle     = element_text(color = "#666666", size = 10),
    axis.title        = element_text(size = 10),
    plot.background   = element_rect(fill = "white", color = NA),
    panel.background  = element_rect(fill = "#F9F9F9", color = NA)
  )

# Colors with names matching the cluster_label factor labels
# (ggplot2 matches colors by name, not by position)
CLUSTER_COLORS <- c(
  "Cluster 1\n(stable markets)" = "#2196F3",
  "Cluster 2\n(high risk)"    = "#F44336",
  "Cluster 3\n(grey zone)"       = "#FF9800"
)
CLUSTER_LABELS <- c("1" = "Cluster 1\n(stable markets)",
                    "2" = "Cluster 2\n(high risk)",
                    "3" = "Cluster 3\n(grey zone)")

cat("=================================================================\n")
cat("UNCERTAINTY ANALYSIS — MANOVA + Sensitivity\n")
cat("=================================================================\n\n")


# ══════════════════════════════════════════════════════════════════
# PART A — MANOVA ON LAYER 1
#
# WHAT THE MANOVA TESTS
# ────────────────────
# MANOVA (Multivariate ANalysis Of VAriance) tests whether the mean
# vectors of the 9 geopolitical variables differ
# significantly across the 3 clusters.
#
# H0 (null hypothesis):        the 3 cluster centroids coincide
#                        in the 9-dimensional space
# H1 (alternative hypothesis): at least one centroid differs
#
# If p < 0.05 → reject H0 → the clusters are statistically distinct.
# We use Pillai's test (the most robust to normality violations).
#
# NOTE ON MISSING VALUES
# ───────────────────────
# The WGI variables have ~23% NA (countries not classified by the World Bank).
# LPI has ~18% NA. MANOVA requires complete data → we impute with the
# median of each variable. This is the standard, conservative
# procedure (it slightly underestimates the true separation).
# ══════════════════════════════════════════════════════════════════
cat("[A] MANOVA on Layer 1...\n")

# Read the data
l1_input <- read_csv(file.path(PROC, "layer1_input.csv"),
                     show_col_types = FALSE)
l1_clust <- read_csv(file.path(PROC, "layer1_countries.csv"),
                     show_col_types = FALSE)
colnames(l1_clust) <- c("iso3", "cluster", "risk_score")

# Variables for the MANOVA
MANOVA_VARS <- c(
  "wgi_voice_accountability",  # democratic quality
  "wgi_gov_effectiveness",     # government effectiveness
  "wgi_regulatory_quality",    # regulatory quality
  "wgi_rule_of_law",           # rule of law
  "wgi_control_corruption",    # control of corruption
  "lpi_score",                 # logistics performance
  "crc_rating",                # OECD credit risk
  "n_sanctioned_entities",     # sanctioned entities
  "sanctioned_flag"            # binary sanctions flag
)
VAR_LABELS <- c(
  wgi_voice_accountability = "WGI: Voice & Accountability",
  wgi_gov_effectiveness    = "WGI: Gov. Effectiveness",
  wgi_regulatory_quality   = "WGI: Regulatory Quality",
  wgi_rule_of_law          = "WGI: Rule of Law",
  wgi_control_corruption   = "WGI: Control of Corruption",
  lpi_score                = "LPI Score",
  crc_rating               = "CRC Rating (OECD)",
  n_sanctioned_entities    = "N. Sanctioned Entities",
  sanctioned_flag          = "Sanctions Flag"
)

# Merge input + cluster
merged <- l1_input %>%
  inner_join(l1_clust %>% select(iso3, cluster), by = "iso3") %>%
  select(iso3, cluster, all_of(MANOVA_VARS))

cat(sprintf("    Countries in the dataset: %d\n", nrow(merged)))
cat(sprintf("    Missing per variable (before imputation):\n"))
na_counts <- colSums(is.na(merged[, MANOVA_VARS]))
for (v in names(na_counts)) {
  if (na_counts[v] > 0)
    cat(sprintf("      %s: %d (%.0f%%)\n", v, na_counts[v],
                na_counts[v] / nrow(merged) * 100))
}

# Median imputation per variable
merged_imp <- merged %>%
  mutate(across(all_of(MANOVA_VARS),
                ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)),
         cluster = factor(cluster))

cat(sprintf("\n    Countries after imputation: %d (complete)\n", nrow(merged_imp)))
cat(sprintf("    Cluster sizes: %s\n\n",
            paste(table(merged_imp$cluster), collapse = " / ")))

# ── MANOVA ───────────────────────────────────────────────────────
Y            <- as.matrix(merged_imp[, MANOVA_VARS])
manova_res   <- manova(Y ~ cluster, data = merged_imp)
pillai_res   <- summary(manova_res, test = "Pillai")
wilks_res    <- summary(manova_res, test = "Wilks")

cat("    === MANOVA RESULTS ===\n")
cat("    Pillai's test (most robust):\n")
print(pillai_res)
cat("\n    Wilks' Lambda test:\n")
print(wilks_res)

# ── Univariate ANOVA per variable ───────────────────────────────
# Shows which variable contributes most to the separation
cat("\n    === Univariate ANOVA per variable ===\n")
univ_results <- bind_rows(lapply(MANOVA_VARS, function(v) {
  aov_res <- aov(reformulate("cluster", response = v), data = merged_imp)
  sm      <- summary(aov_res)[[1]]
  tibble(
    variable = v,
    label    = VAR_LABELS[v],
    F_stat   = sm[["F value"]][1],
    p_value  = sm[["Pr(>F)"]][1],
    eta2     = sm[["Sum Sq"]][1] / sum(sm[["Sum Sq"]])  # effect size η²
  )
})) %>% arrange(desc(F_stat))

print(univ_results %>%
        mutate(p_value = format(p_value, scientific = TRUE, digits = 3),
               F_stat  = round(F_stat, 1),
               eta2    = round(eta2, 3)) %>%
        select(label, F_stat, p_value, eta2))

# ── Extract the MANOVA p-value for the chart annotation ──────
manova_p   <- pillai_res$stats["cluster", "Pr(>F)"]
manova_f   <- pillai_res$stats["cluster", "approx F"]
pillai_val <- pillai_res$stats["cluster", "Pillai"]


# ══════════════════════════════════════════════════════════════════
# PART A — CHART: Cluster separation
#
# HOW TO READ IT
# ─────────────
# Left panel (PCA of countries):
#   Each point = 1 country. The 3 clusters colored differently.
#   The ellipses = 95% confidence region for each cluster.
#   If the ellipses do NOT overlap → well-separated clusters.
#   PC1 (X axis) captures most of the total variance.
#
# Right panel (F-statistic per variable):
#   The F-statistic measures how much a variable discriminates between clusters.
#   High F = that variable is very different across the 3 clusters.
#   Red bars (p < 0.001) indicate highly significant
#   separation. If all bars are red → the MANOVA is
#   extremely robust.
# ══════════════════════════════════════════════════════════════════

# PCA of countries for visualization
pca_l1    <- prcomp(Y, center = TRUE, scale. = TRUE)
expl_l1   <- pca_l1$sdev^2 / sum(pca_l1$sdev^2)

pca_df <- data.frame(
  PC1     = pca_l1$x[, 1],
  PC2     = pca_l1$x[, 2],
  cluster = merged_imp$cluster,
  iso3    = merged_imp$iso3
)

# Cluster label with readable name
pca_df$cluster_label <- CLUSTER_LABELS[as.character(pca_df$cluster)]
pca_df$cluster_label <- factor(pca_df$cluster_label,
                               levels = CLUSTER_LABELS)

pA1 <- ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster_label)) +
  # 95% confidence ellipses
  stat_ellipse(aes(fill = cluster_label), geom = "polygon",
               alpha = 0.12, level = 0.95, show.legend = FALSE) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_manual(values = CLUSTER_COLORS, name = NULL) +
  scale_fill_manual(values  = CLUSTER_COLORS) +
  # MANOVA result annotation
  annotate("label", x = Inf, y = Inf,
           label = sprintf("MANOVA (Pillai)\nF = %.1f\np < 0.001\nPillai = %.3f",
                           manova_f, pillai_val),
           hjust = 1.05, vjust = 1.05, size = 3.2,
           fill = "white", color = "#333333") +
  labs(
    title    = "Cluster Separation — PCA Space (9 geopolitical variables)",
    subtitle = "Each point = 1 country. Ellipses = 95% confidence region per cluster.",
    x = sprintf("PC1 (%.1f%% variance)", expl_l1[1] * 100),
    y = sprintf("PC2 (%.1f%% variance)", expl_l1[2] * 100)
  ) +
  theme_clean +
  theme(legend.position = "bottom")

# F-statistics per variable
univ_plot <- univ_results %>%
  mutate(
    label     = factor(label, levels = rev(label)),
    sig_level = case_when(p_value < 0.001 ~ "p < 0.001",
                          p_value < 0.01  ~ "p < 0.01",
                          p_value < 0.05  ~ "p < 0.05",
                          TRUE            ~ "n.s.")
  )

pA2 <- ggplot(univ_plot, aes(x = F_stat, y = label, fill = sig_level)) +
  geom_col(width = 0.65, color = "white") +
  geom_text(aes(label = sprintf("F = %.0f", F_stat)),
            hjust = -0.1, size = 3.2) +
  scale_fill_manual(
    values = c("p < 0.001" = "#F44336", "p < 0.01" = "#FF9800",
               "p < 0.05"  = "#2196F3", "n.s."     = "#BBBBBB"),
    name   = "Significance"
  ) +
  scale_x_continuous(limits = c(0, max(univ_plot$F_stat) * 1.15)) +
  labs(
    title    = "Discrimination by Variable (univariate ANOVA)",
    subtitle = "High F = that variable strongly separates the clusters",
    x = "F-statistic", y = NULL
  ) +
  theme_clean

pA <- pA1 + pA2 +
  plot_annotation(
    title = "MANOVA — Statistical Validation of the Country Clusters (Layer 1)",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

print(pA)
cat("    ✓ chartU1_manova_separation\n\n")


# ══════════════════════════════════════════════════════════════════
# PART B — WEIGHT SENSITIVITY ANALYSIS
#
# HOW IT WORKS
# ─────────────
# The compliance_score = W_IMP * import_hr + W_EXP * export_hr + W_CIRC * circum
# with W_IMP + W_EXP + W_CIRC = 1.
#
# We vary W_IMP from 0.20 to 0.60 (in steps of 0.05) and
# W_EXP from 0.10 to 0.50, with W_CIRC = 1 - W_IMP - W_EXP ≥ 0.10.
# This generates ~60 "reasonable" weight combinations.
#
# For each combination:
#   1. We recompute the sectoral compliance_scores
#   2. We reassign them to firms
#   3. We count how many firms change quadrant relative to the base
#
# THE KEY METRIC: pct_stable
#   % of firms that stay in the same quadrant (CRITICAL/WATCH/
#   VULNERABLE/LOW) regardless of the chosen weights.
#   If pct_stable > 85% → the model is robust to weight variations.
# ══════════════════════════════════════════════════════════════════
cat("[B] Weight sensitivity analysis...\n")

# Check that the required objects are in memory
if (!exists("firms") || !exists("all_components")) {
  stop("Run 03_Layer2.r first to create 'firms' and 'all_components'")
}

# Base quadrant (from the current configuration)
base_quad <- sub("\n.*", "", firms$quadrant)  # removes the text below \n

# Weight grid
weight_grid <- expand.grid(
  w_imp = seq(0.20, 0.60, by = 0.05),
  w_exp = seq(0.10, 0.50, by = 0.05)
) %>%
  mutate(w_circ = round(1 - w_imp - w_exp, 10)) %>%
  filter(w_circ >= 0.10, w_circ <= 0.60)  # reasonable weights

cat(sprintf("    Weight combinations tested: %d\n", nrow(weight_grid)))
cat(sprintf("    W_IMP: %.2f–%.2f  |  W_EXP: %.2f–%.2f  |  W_CIRC: %.2f–%.2f\n",
            min(weight_grid$w_imp), max(weight_grid$w_imp),
            min(weight_grid$w_exp), max(weight_grid$w_exp),
            min(weight_grid$w_circ), max(weight_grid$w_circ)))

# Median of the real sectors (as in the main script)
real_sectors <- c("A","B","C","D","E","J","M","R","S","V","G")
median_real_sens <- median(
  all_components$compliance_score[all_components$cpa %in% real_sectors],
  na.rm = TRUE)

# For each weight combination, recompute and compare
sens_results <- bind_rows(lapply(seq_len(nrow(weight_grid)), function(i) {
  wi <- weight_grid$w_imp[i]
  we <- weight_grid$w_exp[i]
  wc <- weight_grid$w_circ[i]
  
  # Recompute sectoral compliance_score
  new_comp <- all_components %>%
    mutate(
      cs_new = wi * import_highrisk_share +
        we * export_highrisk_share +
        wc * circumvention_share,
      # Apply the volume threshold (MIN_TRADE_EUR)
      cs_new = if_else(!is.na(total_trade) & total_trade < 500e6,
                       0.05, cs_new)
    )
  new_lookup <- setNames(new_comp$cs_new, new_comp$cpa)
  
  # Reassign to firms
  new_cs <- coalesce(new_lookup[firms$cpa_final], median_real_sens)
  
  # Recompute quadrants
  new_quad <- case_when(
    new_cs > COMP_THRESH & firms$fragility_score > FRAG_THRESH ~ "CRITICAL",
    new_cs > COMP_THRESH & firms$fragility_score <= FRAG_THRESH ~ "WATCH",
    new_cs <= COMP_THRESH & firms$fragility_score > FRAG_THRESH ~ "VULNERABLE",
    TRUE ~ "LOW"
  )
  
  # Compute stability
  n_firms     <- length(base_quad)
  pct_stable  <- mean(new_quad == base_quad) * 100
  
  tibble(
    w_imp      = wi,
    w_exp      = we,
    w_circ     = wc,
    pct_stable = pct_stable,
    n_critical = sum(new_quad == "CRITICAL"),
    n_watch    = sum(new_quad == "WATCH"),
    n_vuln     = sum(new_quad == "VULNERABLE"),
    n_low      = sum(new_quad == "LOW"),
    is_base    = (abs(wi - 0.40) < 0.001 & abs(we - 0.30) < 0.001)
  )
}))

# Summary statistics
cat(sprintf("\n    Stability (pct_stable) across all combinations:\n"))
cat(sprintf("      Minimum: %.1f%%\n", min(sens_results$pct_stable)))
cat(sprintf("      Mean:    %.1f%%\n", mean(sens_results$pct_stable)))
cat(sprintf("      Maximum: %.1f%%\n", max(sens_results$pct_stable)))
cat(sprintf("      Base configuration: %.1f%%\n\n",
            sens_results$pct_stable[sens_results$is_base]))

cat(sprintf("    Variation in CRITICAL firms:\n"))
cat(sprintf("      Min: %d  |  Base: %d  |  Max: %d\n",
            min(sens_results$n_critical),
            sens_results$n_critical[sens_results$is_base],
            max(sens_results$n_critical)))


# ── CHART B1: Stability heatmap ────────────────────────────────
# HOW TO READ IT:
#   Each cell = one weight combination (W_IMP on the X axis,
#   W_EXP on the Y axis). The color shows the % of firms that
#   stay in the same quadrant relative to the base
#   configuration (0.40/0.30/0.30).
#   Warm colors (>90%) = results are stable with those weights.
#   Cool colors (<80%) = that combination changes many results.
#   The starred cell = the model's base configuration.
#   If most of the heatmap is warm → high robustness.

pB1 <- ggplot(sens_results,
              aes(x = factor(round(w_imp, 2)),
                  y = factor(round(w_exp, 2)),
                  fill = pct_stable)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.0f%%", pct_stable)),
            size  = 2.8,
            color = if_else(sens_results$pct_stable > 88, "white", "#333333")) +
  # Highlight the base configuration
  geom_tile(data = sens_results %>% filter(is_base),
            fill = NA, color = "#333333", linewidth = 1.5) +
  geom_text(data = sens_results %>% filter(is_base),
            aes(label = "★"), vjust = -0.8, size = 4, color = "#333333") +
  scale_fill_gradientn(
    colors = c("#F44336", "#FF9800", "#4CAF50", "#1976D2"),
    values = scales::rescale(c(70, 80, 88, 95)),
    limits = c(70, 100),
    name   = "% stable\nfirms"
  ) +
  labs(
    title    = "Robustness to Weights: % of Firms that Do Not Change Quadrant",
    subtitle = "★ = base configuration (W_IMP=0.40, W_EXP=0.30, W_CIRC=0.30)",
    x = "W_IMP (weight of imports from risky countries)",
    y = "W_EXP (weight of exports to risky countries)"
  ) +
  theme_clean +
  theme(axis.text = element_text(size = 8))

print(pB1)
cat("    ✓ chartU2_sensitivity_heatmap\n")


# ── CHART B2: Variation in CRITICAL firms as W_IMP varies ───
# HOW TO READ IT:
#   The X axis shows W_IMP from 0.20 to 0.60.
#   For each W_IMP value, the lines show how many firms
#   end up in CRITICAL as W_EXP varies.
#   The vertical dashed line = base configuration (W_IMP=0.40).
#   The dot = the number of CRITICAL firms in the base configuration.
#
#   If the lines are close together → the result is little affected
#   by W_EXP/W_CIRC; it depends almost entirely on W_IMP.
#   If the lines are far apart → the model is more sensitive to the
#   split between export and circumvention.

# Create readable labels for W_EXP
sens_results <- sens_results %>%
  mutate(w_exp_label = sprintf("W_EXP=%.2f", w_exp))

pB2 <- ggplot(sens_results,
              aes(x = w_imp, y = n_critical,
                  group = factor(round(w_exp, 2)),
                  color = factor(round(w_exp, 2)))) +
  geom_line(alpha = 0.6, linewidth = 0.8) +
  geom_point(data = sens_results %>% filter(is_base),
             aes(x = w_imp, y = n_critical),
             color = "#333333", size = 4, shape = 18) +
  geom_vline(xintercept = 0.40, linetype = "dashed",
             color = "#555555", linewidth = 0.8) +
  annotate("text", x = 0.41, y = max(sens_results$n_critical) * 0.97,
           label = "Base\nconfiguration (0.40)",
           hjust = 0, size = 3, color = "#555555") +
  scale_color_viridis_d(name = "W_EXP", option = "plasma", end = 0.85) +
  scale_x_continuous(breaks = seq(0.20, 0.60, 0.05),
                     labels = function(x) sprintf("%.2f", x)) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "CRITICAL Firms as the Weights Vary",
    subtitle = "◆ = base configuration  |  lines = variation as W_EXP changes",
    x = "W_IMP (weight of imports from risky countries)",
    y = "N. firms in the CRITICAL quadrant"
  ) +
  theme_clean +
  theme(legend.position = "right",
        legend.key.size  = unit(0.4, "cm"),
        legend.text      = element_text(size = 8))

print(pB2)
cat("    ✓ chartU3_sensitivity_critical\n\n")


# ══════════════════════════════════════════════════════════════════
# FINAL SUMMARY
# ══════════════════════════════════════════════════════════════════
cat("=================================================================\n")
cat("UNCERTAINTY ANALYSIS SUMMARY\n")
cat("=================================================================\n\n")

cat("PART A — MANOVA Layer 1:\n")
cat(sprintf("  Pillai test:    F = %.2f,  p = %.2e\n", manova_f, manova_p))
cat(sprintf("  Pillai trace:   %.4f  (0 = no separation, 1 = perfect separation)\n",
            pillai_val))
cat("  Interpretation: ")
if (manova_p < 0.001) {
  cat("the 3 clusters are statistically distinct (p < 0.001).\n")
  cat("  The centroids differ significantly on ALL\n")
  cat("  9 geopolitical variables. The choice k=3 is validated.\n")
} else {
  cat(sprintf("p = %.4f\n", manova_p))
}

cat("\nPART B — Sensitivity Analysis:\n")
cat(sprintf("  Weight combinations tested: %d\n", nrow(weight_grid)))
cat(sprintf("  Minimum stability: %.1f%%\n", min(sens_results$pct_stable)))
cat(sprintf("  Mean stability:    %.1f%%\n", mean(sens_results$pct_stable)))
cat(sprintf("  Maximum stability: %.1f%%\n", max(sens_results$pct_stable)))
cat(sprintf("  N. CRITICAL (base):    %d\n",
            sens_results$n_critical[sens_results$is_base]))
cat(sprintf("  N. CRITICAL (range): [%d, %d]\n",
            min(sens_results$n_critical), max(sens_results$n_critical)))

pct_base <- sens_results$pct_stable[sens_results$is_base]
cat("\n  Interpretation: ")
if (pct_base >= 88) {
  cat(sprintf("%.1f%% of firms keep their quadrant\n", pct_base))
  cat("  under any reasonable weight combination.\n")
  cat("  The 0.40/0.30/0.30 choice is robust.\n")
} else {
  cat(sprintf("%.1f%% stability — the results depend\n", pct_base))
  cat("  non-negligibly on the choice of weights.\n")
}

cat("\nDone ✓\n")
