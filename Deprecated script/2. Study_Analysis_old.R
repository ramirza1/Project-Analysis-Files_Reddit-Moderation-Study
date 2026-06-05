## LOAD DATA

library(tidyverse)
library(rstatix)
library(emmeans)
library(effectsize)

df_long <- readRDS("Moderation_Data_Long_Format.rds")

# Separate datasets for manipulation checks and political conditions
df_manip <- df_long %>% filter(ContentType == "ManipCheck")
df_political <- df_long %>% filter(ContentType == "Political")

cat("=== DATA SUMMARY ===\n")
cat("Manipulation checks:", nrow(df_manip), "rows (3 per participant)\n")
cat("Political content:", nrow(df_political), "rows (6 per participant)\n")
cat("Unique participants:", n_distinct(df_long$ParticipantID), "\n\n")

## Set up Descriptive Stats

desc_with_ci <- function(df, dv, group_var) {
  df %>%
    group_by({{ group_var }}) %>%
    summarise(
      N = n(),
      Mean = mean({{ dv }}, na.rm = TRUE),
      SD = sd({{ dv }}, na.rm = TRUE),
      SE = SD / sqrt(N),
      t_95 = qt(0.975, df = N - 1),
      t_99 = qt(0.995, df = N - 1),
      CI95_lower = Mean - t_95 * SE,
      CI95_upper = Mean + t_95 * SE,
      CI99_lower = Mean - t_99 * SE,
      CI99_upper = Mean + t_99 * SE
    ) %>%
    ungroup() %>%
    arrange(factor({{ group_var }}, 
                   levels = c("Civil", "Borderline", "Uncivil")))
}

desc_with_ci_2 <- function(df, dv, group_var1, group_var2) {
  df %>%
    group_by({{ group_var1 }}, {{ group_var2 }}) %>%
    summarise(
      N = n(),
      Mean = mean({{ dv }}, na.rm = TRUE),
      SD = sd({{ dv }}, na.rm = TRUE),
      SE = SD / sqrt(N),
      t_95 = qt(0.975, df = N - 1),
      t_99 = qt(0.995, df = N - 1),
      CI95_lower = Mean - t_95 * SE,
      CI95_upper = Mean + t_95 * SE,
      CI99_lower = Mean - t_99 * SE,
      CI99_upper = Mean + t_99 * SE,
      .groups = "drop"
    ) %>%
    arrange(
      factor({{ group_var1 }}, levels = c("Civil", "Borderline", "Uncivil")),
      factor({{ group_var2 }}, levels = c("Aligned", "Opposed"))
    )
}

## Manipulation Checks - Civility Effect

# Violation Recognition on MC
cat("--- H1a: Violation Recognition ---\n")
anova_h1a_mc <- df_manip %>%
  anova_test(
    dv = ViolationRecognition,
    wid = ParticipantID,
    within = Civility,
    effect.size = "pes"
  )
print(anova_h1a_mc)

# Post-hoc pairwise comparisons on MC VR (KEEP IN RESERVE)
#posthoc_h1a_mc <- df_manip %>%
 # pairwise_t_test(
  #  ViolationRecognition ~ Civility,
   # paired = TRUE,
    #p.adjust.method = "bonferroni"
  #)
#print(posthoc_h1a_mc)


# Descriptive statistics on MC ES
desc_h1a_mc <- desc_with_ci(
  df = df_manip,
  dv = ViolationRecognition,
  group_var = Civility
)

print(desc_h1a_mc)


# Enforcement Severity on MC
cat("\n--- H1b: Enforcement Severity ---\n")
anova_h1b_mc <- df_manip %>%
  anova_test(
    dv = EnforcementSeverity,
    wid = ParticipantID,
    within = Civility,
    effect.size = "pes"
  )
print(anova_h1b_mc)

# Post-hoc pairwise comparisons on MC ES
posthoc_h1b_mc <- df_manip %>%
  pairwise_t_test(
    EnforcementSeverity ~ Civility,
    paired = TRUE,
    p.adjust.method = "bonferroni"
  )
print(posthoc_h1b_mc)

# Descriptive statistics on MC ES
desc_h1b_mc <- desc_with_ci(
  df = df_manip,
  dv = EnforcementSeverity,
  group_var = Civility
)

print(desc_h1b_mc)

## Political Condition ANOVAs

# Violation recognition ANOVAs (H1a, H2a, H3a)
cat("--- DV: Violation Recognition ---\n")
anova_political_vr <- df_political %>%
  anova_test(
    dv = ViolationRecognition,
    wid = ParticipantID,
    within = c(Civility, Alignment),
    effect.size = "pes"
  )
print(anova_political_vr)

# Descriptive statistics on political conditions VR
desc_political_vr <- desc_with_ci_2(
  df = df_political,
  dv = ViolationRecognition,
  group_var1 = Civility,
  group_var2 = Alignment
)

print(desc_political_vr)

# Enforcement Severity ANOVAS (H1b, H2b, H3b)
cat("\n--- DV: Enforcement Severity ---\n")
anova_political_es <- df_political %>%
  anova_test(
    dv = EnforcementSeverity,
    wid = ParticipantID,
    within = c(Civility, Alignment),
    effect.size = "pes"
  )
print(anova_political_es)

# Descriptive statistics on political conditions ES
desc_political_es <- desc_with_ci_2(
  df = df_political,
  dv = EnforcementSeverity,
  group_var1 = Civility,
  group_var2 = Alignment
)
print(desc_political_es)


# Pairwise comparisons (KEEP IN RESERVE)
#cat("--- Violation Recognition: Alignment effect by Civility ---\n")
#simple_vr <- df_political %>%
 # group_by(Civility) %>%
  #pairwise_t_test(
   # ViolationRecognition ~ Alignment,
    #paired = TRUE,
    #p.adjust.method = "bonferroni")
#print(simple_vr)

#cat("\n--- Enforcement Severity: Alignment effect by Civility ---\n")
#simple_es <- df_political %>%
 # group_by(Civility) %>%
#  pairwise_t_test(
 #   EnforcementSeverity ~ Alignment,
  #  paired = TRUE,
   # p.adjust.method = "bonferroni")
#print(simple_es)

## Planned contrasts - borderline-speciifc alignment effects

# Calculate alignment gaps (Opposed vs aligned) for each civility level
alignment_gaps_vr <- df_political %>%
  select(ParticipantID, Civility, Alignment, ViolationRecognition) %>%
  pivot_wider(
    names_from = Alignment,
    values_from = ViolationRecognition
  ) %>%
  mutate(AlignmentGap = Opposed - Aligned)

alignment_gaps_es <- df_political %>%
  select(ParticipantID, Civility, Alignment, EnforcementSeverity) %>%
  pivot_wider(
    names_from = Alignment,
    values_from = EnforcementSeverity
  ) %>%
  mutate(AlignmentGap = Opposed - Aligned)

# H4a: Violation Recognition - test if borderline gap exceeds civil gap
cat("--- H4a: Violation Recognition ---\n")
cat("Testing if Borderline gap > Civil gap\n")
contrast_h4a_1 <- alignment_gaps_vr %>%
  select(ParticipantID, Civility, AlignmentGap) %>%
  pivot_wider(names_from = Civility, values_from = AlignmentGap) %>%
  mutate

t_test_h4a_1 <- t.test(contrast_h4a_1$Borderline_vs_Civil, mu = 0, paired = FALSE)
print(t_test_h4a_1)
cat("Mean difference (Borderline gap - Civil gap):", mean(contrast_h4a_1$Borderline_vs_Civil, na.rm = TRUE), "\n")
cat("Cohen's d:", cohens_d(contrast_h4a_1$Borderline_vs_Civil, mu = 0)$Cohens_d, "\n\n")

cat("Testing if Borderline gap > Uncivil gap\n")
contrast_h4a_2 <- alignment_gaps_vr %>%
  select(ParticipantID, Civility, AlignmentGap) %>%
  pivot_wider(names_from = Civility, values_from = AlignmentGap) %>%
  mutate(Borderline_vs_Uncivil = Borderline - Uncivil)

t_test_h4a_2 <- t.test(contrast_h4a_2$Borderline_vs_Uncivil, mu = 0, paired = FALSE)
print(t_test_h4a_2)
cat("Mean difference (Borderline gap - Uncivil gap):", mean(contrast_h4a_2$Borderline_vs_Uncivil, na.rm = TRUE), "\n")
cat("Cohen's d:", cohens_d(contrast_h4a_2$Borderline_vs_Uncivil, mu = 0)$Cohens_d, "\n\n")

# Summary of alignment gaps (Borderline, VR)
cat("Summary of alignment gaps (Opposed - Aligned):\n")
alignment_gaps_vr %>%
  group_by(Civility) %>%
  summarise(
    Mean_Gap = mean(AlignmentGap, na.rm = TRUE),
    SD_Gap = sd(AlignmentGap, na.rm = TRUE),
    SE_Gap = SD_Gap / sqrt(n())
  ) %>%
  print()

# H4b: Enforcement Severity - test if borderline gap exceeds civil gap
cat("\n--- H4b: Enforcement Severity ---\n")
cat("Testing if Borderline gap > Civil gap\n")
contrast_h4b_1 <- alignment_gaps_es %>%
  select(ParticipantID, Civility, AlignmentGap) %>%
  pivot_wider(names_from = Civility, values_from = AlignmentGap) %>%
  mutate(Borderline_vs_Civil = Borderline - Civil)

t_test_h4b_1 <- t.test(contrast_h4b_1$Borderline_vs_Civil, mu = 0, paired = FALSE)
print(t_test_h4b_1)
cat("Mean difference (Borderline gap - Civil gap):", mean(contrast_h4b_1$Borderline_vs_Civil, na.rm = TRUE), "\n")
cat("Cohen's d:", cohens_d(contrast_h4b_1$Borderline_vs_Civil, mu = 0)$Cohens_d, "\n\n")

cat("Testing if Borderline gap > Uncivil gap\n")
contrast_h4b_2 <- alignment_gaps_es %>%
  select(ParticipantID, Civility, AlignmentGap) %>%
  pivot_wider(names_from = Civility, values_from = AlignmentGap) %>%
  mutate(Borderline_vs_Uncivil = Borderline - Uncivil)

t_test_h4b_2 <- t.test(contrast_h4b_2$Borderline_vs_Uncivil, mu = 0, paired = FALSE)
print(t_test_h4b_2)
cat("Mean difference (Borderline gap - Uncivil gap):", mean(contrast_h4b_2$Borderline_vs_Uncivil, na.rm = TRUE), "\n")
cat("Cohen's d:", cohens_d(contrast_h4b_2$Borderline_vs_Uncivil, mu = 0)$Cohens_d, "\n\n")

# Summary of alignment gaps (ES - borderline)
cat("Summary of alignment gaps (Opposed - Aligned):\n")
alignment_gaps_es %>%
  group_by(Civility) %>%
  summarise(
    Mean_Gap = mean(AlignmentGap, na.rm = TRUE),
    SD_Gap = sd(AlignmentGap, na.rm = TRUE),
    SE_Gap = SD_Gap / sqrt(n())
  ) %>%
  print()


## Save results to file

sink("ANOVA_Results_Complete.txt")

cat("========================================\n")
cat("POLITICAL CONTENT MODERATION STUDY\n")
cat("ANOVA RESULTS\n")
cat("========================================\n\n")

cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Sample size:", n_distinct(df_long$ParticipantID), "participants\n\n")

cat("========================================\n")
cat("H1: MANIPULATION CHECK - CIVILITY EFFECT\n")
cat("========================================\n\n")

cat("--- H1a: Violation Recognition ---\n")
print(anova_h1a_mc)
cat("\nDescriptive Statistics:\n")
print(desc_h1a_mc)

cat("\n--- H1b: Enforcement Severity ---\n")
print(anova_h1b_mc)
cat("\nDescriptive Statistics:\n")
print(desc_h1b_mc)

cat("\n========================================\n")
cat("POLITICAL CONTENT: CIVILITY × ALIGNMENT\n")
cat("========================================\n\n")

cat("--- Violation Recognition ---\n")
print(anova_political_vr)
cat("\nDescriptive Statistics:\n")
print(desc_political_vr)

cat("\n--- Enforcement Severity ---\n")
print(anova_political_es)
cat("\nDescriptive Statistics:\n")
print(desc_political_es)

cat("\n========================================\n")
cat("H4: BORDERLINE-SPECIFIC CONTRASTS\n")
cat("========================================\n\n")

cat("--- H4a: Violation Recognition ---\n")
cat("Borderline vs Civil:\n")
print(t_test_h4a_1)
cat("\nBorderline vs Uncivil:\n")
print(t_test_h4a_2)

cat("\n--- H4b: Enforcement Severity ---\n")
cat("Borderline vs Civil:\n")
print(t_test_h4b_1)
cat("\nBorderline vs Uncivil:\n")
print(t_test_h4b_2)

sink()

cat("\n✅ Results saved to: ANOVA_Results_Complete.txt\n")

## Plot results

library(ggpubr)
library(patchwork)
library(ggplot2)

# Ensure ordering
desc_h1a_mc <- desc_h1a_mc %>%
  mutate(Civility = factor(Civility, levels = c("Civil", "Borderline", "Uncivil")))

desc_h1b_mc <- desc_h1b_mc %>%
  mutate(Civility = factor(Civility, levels = c("Civil", "Borderline", "Uncivil")))

desc_political_vr <- desc_political_vr %>%
  mutate(
    Civility  = factor(Civility,  levels = c("Civil", "Borderline", "Uncivil")),
    Alignment = factor(Alignment, levels = c("Aligned", "Opposed"))
  )

desc_political_es <- desc_political_es %>%
  mutate(
    Civility  = factor(Civility,  levels = c("Civil", "Borderline", "Uncivil")),
    Alignment = factor(Alignment, levels = c("Aligned", "Opposed"))
  )

base_theme <- theme_pubr() +
  theme(
    plot.title  = element_text(face = "bold"),
    axis.title  = element_text(face = "bold")
  )

# ---------- Top row: Manipulation checks (bars) ----------

p_manip_vr <- desc_h1a_mc %>%
  ggplot(aes(x = Civility, y = Mean, fill = Civility)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = CI95_lower, ymax = CI95_upper), width = 0.15) +
  scale_fill_manual(values = c(
    "Civil"      = "#81C784",
    "Borderline" = "#FFB74D",
    "Uncivil"    = "#EF5350"
  )) +
  labs(
    title = "Manipulation Check: Violation Recognition",
    x = "Civility Level",
    y = "Violation Recognition Rate"
  ) +
  base_theme +
  theme(legend.position = "none") +
  ylim(0, 1)

p_manip_es <- desc_h1b_mc %>%
  ggplot(aes(x = Civility, y = Mean, fill = Civility)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = CI95_lower, ymax = CI95_upper), width = 0.15) +
  scale_fill_manual(values = c(
    "Civil"      = "#81C784",
    "Borderline" = "#FFB74D",
    "Uncivil"    = "#EF5350"
  )) +
  labs(
    title = "Manipulation Check: Enforcement Severity",
    x = "Civility Level",
    y = "Enforcement Severity (0–4)"
  ) +
  base_theme +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, 0.5))

# position dodge for aligned vs opposed
pd <- position_dodge(width = 0.25)

base_theme <- theme_pubr() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

# --- bottom left: Political Content – Violation Recognition ---
p_vr <- desc_political_vr %>%
  ggplot(aes(x = Civility, y = Mean,
             color = Alignment, group = Alignment)) +
  geom_line(position = pd, linewidth = 1) +
  geom_point(position = pd, size = 3) +
  geom_errorbar(
    aes(ymin = CI95_lower, ymax = CI95_upper),
    position = pd, width = 0.1
  ) +
  scale_color_manual(values = c("Aligned" = "#2E7D32", "Opposed" = "#C62828")) +
  labs(
    title = "Political Content: Violation Recognition",
    x = "Civility Level",
    y = "Violation Recognition Rate"
  ) +
  base_theme +
  theme(legend.position = "top") +
  ylim(0, 1)

# --- bottom right: Political Content – Enforcement Severity ---
p_es <- desc_political_es %>%
  ggplot(aes(x = Civility, y = Mean,
             color = Alignment, group = Alignment)) +
  geom_line(position = pd, linewidth = 1) +
  geom_point(position = pd, size = 3) +
  geom_errorbar(
    aes(ymin = CI95_lower, ymax = CI95_upper),
    position = pd, width = 0.1
  ) +
  scale_color_manual(values = c("Aligned" = "#2E7D32", "Opposed" = "#C62828")) +
  labs(
    title = "Political Content: Enforcement Severity",
    x = "Civility Level",
    y = "Enforcement Severity (0–4)"
  ) +
  base_theme +
  theme(legend.position = "top") +
  scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, 0.5))

# ---------- Combine: 2 x 2 grid, no global title ----------

combined_plot <- (p_manip_vr | p_manip_es) /
  (p_vr       | p_es)

ggsave(
  "Moderation_Results_Combined.png",
  combined_plot,
  width = 12,
  height = 10,
  dpi = 300,
  bg = "white"
)

cat("\n✅ Plots saved to: Moderation_Results_Combined.png\n")


## Table plots

library(dplyr)
library(readr)

format_anova_table <- function(aov_obj, digits = 3) {
  aov_obj$ANOVA %>%
    mutate(
      across(c(DFn, DFd), round, 0),
      across(c(F, p, pes), ~ round(., digits))
    ) %>%
    rename(
      Effect = Effect,
      df1   = DFn,
      df2   = DFd,
      F     = F,
      p     = p,
      pes   = pes
    )
}

# Manipulation checks
tab_H1a_VR <- format_anova_table(anova_h1a_mc)
tab_H1b_ES <- format_anova_table(anova_h1b_mc)

# Political content
tab_VR_political <- format_anova_table(anova_political_vr)
tab_ES_political <- format_anova_table(anova_political_es)

# Inspect in console
tab_H1a_VR
tab_VR_political

# Export to CSV
write_csv(tab_H1a_VR, "ANOVA_H1a_ViolationRecognition.csv")
write_csv(tab_H1b_ES, "ANOVA_H1b_EnforcementSeverity.csv")
write_csv(tab_VR_political, "ANOVA_Political_ViolationRecognition.csv")
write_csv(tab_ES_political, "ANOVA_Political_EnforcementSeverity.csv")

# Export to word
library(knitr)
kable(tab_VR_political, caption = "ANOVA for Political Violation Recognition")
