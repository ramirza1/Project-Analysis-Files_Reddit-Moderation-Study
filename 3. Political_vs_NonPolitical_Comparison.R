# ==================================================
# POLITICAL vs NON-POLITICAL COMPARISON ANALYSIS
# 
# Tests whether political content is moderated more
# harshly than non-political content at equivalent 
# levels of incivility.
#
# Constructs participant-level aligned-opposed averages
# for political content, then compares to non-political
# baseline (manipulation check) memes within-subjects.
# ==================================================

## LOAD LIBRARIES
library(tidyverse)
library(rstatix)
library(effectsize)
library(ggpubr)

## LOAD DATA
df_long <- readRDS("Input data_long/Moderation_Data_Long_Format.rds")

cat("=== DATA SUMMARY ===\n")
cat("Total rows:", nrow(df_long), "\n")
cat("Unique participants:", n_distinct(df_long$ParticipantID), "\n\n")

## ========================================
## CONSTRUCT POLITICAL AVERAGE PER PARTICIPANT
## ========================================
df_political_avg <- df_long %>%
  filter(ContentType == "Political") %>%
  group_by(ParticipantID, Civility) %>%
  summarise(
    ViolationRecognition = mean(ViolationRecognition, na.rm = TRUE),
    EnforcementSeverity  = mean(EnforcementSeverity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(ContentType = "Political")

df_nonpolitical <- df_long %>%
  filter(ContentType == "ManipCheck") %>%
  select(ParticipantID, Civility, ViolationRecognition, EnforcementSeverity) %>%
  mutate(ContentType = "Non-political")

df_compare <- bind_rows(df_political_avg, df_nonpolitical) %>%
  mutate(
    Civility    = factor(Civility, levels = c("Civil", "Borderline", "Uncivil")),
    ContentType = factor(ContentType, levels = c("Non-political", "Political"))
  )

cat("Combined dataset:", nrow(df_compare), "rows\n")
cat("Expected:", n_distinct(df_long$ParticipantID) * 6,
    "(N participants x 3 civility x 2 content types)\n\n")

## ========================================
## DESCRIPTIVE STATISTICS
## ========================================
desc_with_ci_2 <- function(df, dv, group_var1, group_var2) {
  df %>%
    group_by({{ group_var1 }}, {{ group_var2 }}) %>%
    summarise(
      N    = n(),
      Mean = mean({{ dv }}, na.rm = TRUE),
      SD   = sd({{ dv }}, na.rm = TRUE),
      SE   = SD / sqrt(N),
      t_95 = qt(0.975, df = N - 1),
      CI95_lower = Mean - t_95 * SE,
      CI95_upper = Mean + t_95 * SE,
      .groups = "drop"
    )
}

desc_compare_vr <- desc_with_ci_2(df_compare, ViolationRecognition, Civility, ContentType)
desc_compare_es <- desc_with_ci_2(df_compare, EnforcementSeverity,  Civility, ContentType)

cat("--- Violation Recognition Descriptives ---\n")
print(desc_compare_vr)

cat("\n--- Enforcement Severity Descriptives ---\n")
print(desc_compare_es)

## ========================================
## 3 x 2 REPEATED-MEASURES ANOVAs
## ========================================

cat("\n========================================\n")
cat("ANOVA: Violation Recognition\n")
cat("========================================\n")
anova_compare_vr <- df_compare %>%
  anova_test(
    dv          = ViolationRecognition,
    wid         = ParticipantID,
    within      = c(Civility, ContentType),
    effect.size = "pes"
  )
print(anova_compare_vr)

cat("\n========================================\n")
cat("ANOVA: Enforcement Severity\n")
cat("========================================\n")
anova_compare_es <- df_compare %>%
  anova_test(
    dv          = EnforcementSeverity,
    wid         = ParticipantID,
    within      = c(Civility, ContentType),
    effect.size = "pes"
  )
print(anova_compare_es)

## ========================================
## SIMPLE EFFECTS: Political vs Non-political at each civility level
## ========================================

cat("\n========================================\n")
cat("SIMPLE EFFECTS - Political vs Non-political by Civility\n")
cat("========================================\n")

cat("\n--- Violation Recognition ---\n")
simple_vr <- df_compare %>%
  group_by(Civility) %>%
  pairwise_t_test(
    ViolationRecognition ~ ContentType,
    paired          = TRUE,
    p.adjust.method = "bonferroni"
  )
print(simple_vr)

cat("\n--- Enforcement Severity ---\n")
simple_es <- df_compare %>%
  group_by(Civility) %>%
  pairwise_t_test(
    EnforcementSeverity ~ ContentType,
    paired          = TRUE,
    p.adjust.method = "bonferroni"
  )
print(simple_es)

## ========================================
## SAVE RESULTS TO FILE
## ========================================

sink("txt output_full results/Political_vs_NonPolitical_Results.txt")

cat("========================================\n")
cat("POLITICAL vs NON-POLITICAL COMPARISON\n")
cat("========================================\n\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("N participants:", n_distinct(df_long$ParticipantID), "\n\n")

cat("--- Descriptives: Violation Recognition ---\n")
print(desc_compare_vr)
cat("\n--- Descriptives: Enforcement Severity ---\n")
print(desc_compare_es)

cat("\n--- ANOVA: Violation Recognition ---\n")
print(anova_compare_vr)
cat("\n--- ANOVA: Enforcement Severity ---\n")
print(anova_compare_es)

cat("\n--- Simple Effects: Violation Recognition ---\n")
print(simple_vr)
cat("\n--- Simple Effects: Enforcement Severity ---\n")
print(simple_es)

sink()
cat("\nResults saved to: Political_vs_NonPolitical_Results.txt\n")

## ========================================
## CHART: Political vs Non-political (side-by-side, VR + ES)
## ========================================

library(patchwork)

COLOR_NONPOL <- "#6B7280"
COLOR_POL    <- "#3D5A80"

desc_compare_vr <- desc_compare_vr %>%
  mutate(ContentType = factor(ContentType, levels = c("Non-political", "Political")))
desc_compare_es <- desc_compare_es %>%
  mutate(ContentType = factor(ContentType, levels = c("Non-political", "Political")))

base_theme_clean <- theme_pubr() +
  theme(
    axis.title       = element_text(face = "bold", size = 11),
    axis.text        = element_text(size = 10),
    legend.title     = element_blank(),
    legend.position  = "top",
    legend.text      = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# --- Panel 1: Violation Recognition ---
p_vr_compare <- ggplot(desc_compare_vr,
                       aes(x = Civility, y = Mean, fill = ContentType)) +
  geom_col(position = position_dodge(width = 0.75),
           width = 0.65, color = "white", linewidth = 0.3) +
  geom_errorbar(aes(ymin = CI95_lower, ymax = CI95_upper),
                position = position_dodge(width = 0.75),
                width = 0.15, linewidth = 0.5, color = "#333") +
  geom_text(aes(y = CI95_upper, label = sprintf("%.1f%%", Mean * 100)),
            position = position_dodge(width = 0.75),
            vjust = -0.5, size = 3.2, color = "#444") +
  scale_fill_manual(values = c(
    "Non-political" = COLOR_NONPOL,
    "Political"     = COLOR_POL
  ),
  labels = c("Non-political baseline",
             "Political content (avg. of aligned + opposed)")) +
  scale_y_continuous(
    limits = c(0, 1.10),
    breaks = c(0, 0.25, 0.5, 0.75, 1.0),
    labels = c("0%", "25%", "50%", "75%", "100%"),
    expand = c(0, 0)
  ) +
  labs(
    title = "Political vs. Non-political: Violation Recognition",
    x = "Civility Level",
    y = "Violation Recognition Rate"
  ) +
  base_theme_clean

# --- Panel 2: Enforcement Severity ---
p_es_compare <- ggplot(desc_compare_es,
                       aes(x = Civility, y = Mean, fill = ContentType)) +
  geom_col(position = position_dodge(width = 0.75),
           width = 0.65, color = "white", linewidth = 0.3) +
  geom_errorbar(aes(ymin = CI95_lower, ymax = CI95_upper),
                position = position_dodge(width = 0.75),
                width = 0.15, linewidth = 0.5, color = "#333") +
  geom_text(aes(y = CI95_upper, label = sprintf("%.2f", Mean)),
            position = position_dodge(width = 0.75),
            vjust = -0.5, size = 3.2, color = "#444") +
  scale_fill_manual(values = c(
    "Non-political" = COLOR_NONPOL,
    "Political"     = COLOR_POL
  ),
  labels = c("Non-political baseline",
             "Political content (avg. of aligned + opposed)")) +
  scale_y_continuous(
    limits = c(0, 3.2),
    breaks = seq(0, 3, 0.5),
    expand = c(0, 0)
  ) +
  labs(
    title = "Political vs. Non-political: Enforcement Severity",
    x = "Civility Level",
    y = "Enforcement Severity (0-4)"
  ) +
  base_theme_clean

# Combine side-by-side with shared legend
combined_compare <- (p_vr_compare | p_es_compare) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top")

ggsave(
  "Graph output_results/Political_vs_NonPolitical_Combined.png",
  combined_compare,
  width  = 12,
  height = 5,
  dpi    = 300,
  bg     = "white"
)

cat("\nChart saved to: Political_vs_NonPolitical_Combined.png\n")


## ========================================
## STANDALONE CHARTS: VR and ES with difference annotations
## ========================================

# Helper: compute per-civility diff and annotation positions
make_annot_df <- function(desc_df) {
  np_df <- desc_df %>%
    filter(ContentType == "Non-political") %>%
    select(Civility, Mean_NP = Mean, Upper_NP = CI95_upper)
  p_df  <- desc_df %>%
    filter(ContentType == "Political") %>%
    select(Civility, Mean_P = Mean, Upper_P = CI95_upper)
  np_df %>%
    left_join(p_df, by = "Civility") %>%
    mutate(
      diff      = Mean_P - Mean_NP,
      max_upper = pmax(Upper_NP, Upper_P),
      x_num     = as.numeric(Civility)
    )
}

# VR annotation frame
# threshold: |diff| <= 0.03 (i.e. ~3pp) treated as "no gap"
annot_vr <- make_annot_df(desc_compare_vr) %>%
  mutate(
    label       = case_when(
      diff >  0.03 ~ paste0("+", round(diff * 100), "pp"),
      diff < -0.03 ~ paste0(round(diff * 100), "pp"),
      TRUE         ~ "no gap"
    ),
    label_color = if_else(abs(diff) > 0.03, "#CC2200", "#888888"),
    x_pos       = x_num + if_else(abs(diff) <= 0.03, 0.18, 0),  # nudge "no gap" right
    y_pos       = max_upper + 0.05
  )

# ES annotation frame
# threshold: |diff| <= 0.10 treated as "no gap"
annot_es <- make_annot_df(desc_compare_es) %>%
  mutate(
    label       = case_when(
      diff >  0.10 ~ paste0("+", sprintf("%.2f", diff)),
      diff < -0.10 ~ sprintf("%.2f", diff),
      TRUE         ~ "no gap"
    ),
    label_color = if_else(abs(diff) > 0.10, "#CC2200", "#888888"),
    x_pos       = x_num + if_else(abs(diff) <= 0.10, 0.18, 0),
    y_pos       = max_upper + 0.10
  )

# Standalone Violation Recognition chart
p_vr_standalone <- p_vr_compare +
  labs(
    title = "Political content is judged more harshly than non-political\ncontent at the same level of incivility",
    y     = "Violation recognition rate"
  ) +
  theme(plot.title = element_text(face = "bold", size = 12)) +
  geom_text(
    data        = annot_vr,
    aes(x = x_pos, y = y_pos, label = label, color = label_color),
    inherit.aes = FALSE,
    size        = 4,
    fontface    = "bold"
  ) +
  scale_color_identity()

# Standalone Enforcement Severity chart
p_es_standalone <- p_es_compare +
  labs(
    title = "Same pattern holds for enforcement severity",
    y     = "Enforcement severity (0-4)"
  ) +
  theme(plot.title = element_text(face = "bold", size = 12)) +
  geom_text(
    data        = annot_es,
    aes(x = x_pos, y = y_pos, label = label, color = label_color),
    inherit.aes = FALSE,
    size        = 4,
    fontface    = "bold"
  ) +
  scale_color_identity()

ggsave(
  "Graph output_results/Political_vs_NonPolitical_VR.png",
  p_vr_standalone,
  width  = 7,
  height = 5,
  dpi    = 300,
  bg     = "white"
)

ggsave(
  "Graph output_results/Political_vs_NonPolitical_ES.png",
  p_es_standalone,
  width  = 7,
  height = 5,
  dpi    = 300,
  bg     = "white"
)

cat("\nStandalone charts saved: Political_vs_NonPolitical_VR.png and Political_vs_NonPolitical_ES.png\n")


## ========================================
## EXPORT TABLES
## ========================================
write_csv(desc_compare_vr, "csv descriptive_results/Political_vs_NonPolitical_VR_Descriptives.csv")
write_csv(desc_compare_es, "csv descriptive_results/Political_vs_NonPolitical_ES_Descriptives.csv")
cat("\nCSV tables exported\n")