## LOAD LIBRARIES
library(tidyverse)
library(rstatix)
library(effectsize)
library(ggpubr)
library(patchwork)
library(ggplot2)

## LOAD DATA

df_long <- readRDS("Input_data_long/Moderation_Data_Long_Format.rds")

# Separate datasets for non-political and political conditions
df_baseline <- df_long %>% filter(ContentType == "Baseline")
df_political <- df_long %>% filter(ContentType == "Political")

cat("=== DATA SUMMARY ===\n")
cat("Non-political baseline:", nrow(df_baseline), "rows (3 per participant)\n")
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

## Non-Political Baseline - Civility Effect

# Violation Recognition on Non-political Baseline
cat("--H1a: Violation Recognition--\n")
anova_h1a_base <- df_baseline %>%
  anova_test(
    dv = ViolationRecognition,
    wid = ParticipantID,
    within = Civility,
    effect.size = "pes"
  )
print(anova_h1a_base)

# Post-hoc pairwise comparisons on non-political baseline VR (KEEP IN RESERVE)
#posthoc_h1a_base <- df_baseline %>%
 # pairwise_t_test(
  #  ViolationRecognition ~ Civility,
   # paired = TRUE,
    #p.adjust.method = "bonferroni"
  #)
#print(posthoc_h1a_base)


# Descriptive statistics on non-political baseline VR
desc_h1a_base <- desc_with_ci(
  df = df_baseline,
  dv = ViolationRecognition,
  group_var = Civility
)

print(desc_h1a_base)


# Enforcement Severity on Non-political Baseline
cat("\n--H1b: Enforcement Severity--\n")
anova_h1b_base <- df_baseline %>%
  anova_test(
    dv = EnforcementSeverity,
    wid = ParticipantID,
    within = Civility,
    effect.size = "pes"
  )
print(anova_h1b_base)

# Post-hoc pairwise comparisons on non-political baseline ES (KEEP IN RESERVE)
#posthoc_h1b_base <- df_baseline %>%
 # pairwise_t_test(
  #  EnforcementSeverity ~ Civility,
   # paired = TRUE,
  #  p.adjust.method = "bonferroni"
#  )
#print(posthoc_h1b_base)

# Descriptive statistics on non-political baseline ES
desc_h1b_base <- desc_with_ci(
  df = df_baseline,
  dv = EnforcementSeverity,
  group_var = Civility
)

print(desc_h1b_base)

## Political Condition ANOVAs

# Violation recognition ANOVAs (H1a, H2a, H3a)
cat("--DV: Violation Recognition--\n")
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
cat("\n--DV: Enforcement Severity--\n")
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
#cat("--Violation Recognition: Alignment effect by Civility--\n")
#simple_vr <- df_political %>%
# group_by(Civility) %>%
#pairwise_t_test(
# ViolationRecognition ~ Alignment,
#paired = TRUE,
#p.adjust.method = "bonferroni") #Bonferroni not strictly needed as we just compare aligned vs opposed
#print(simple_vr)

#cat("\n--Enforcement Severity: Alignment effect by Civility--\n")
#simple_es <- df_political %>%
# group_by(Civility) %>%
#  pairwise_t_test(
#   EnforcementSeverity ~ Alignment,
#  paired = TRUE,
# p.adjust.method = "bonferroni") #Bonferroni not strictly needed as we just compare aligned vs opposed
#print(simple_es)

## Planned contrasts - borderline-specific alignment effects (KEEP IN RESERVE)

# Calculate alignment gaps (Opposed vs aligned) for each civility level
# alignment_gaps_vr <- df_political %>%
#  select(ParticipantID, Civility, Alignment, ViolationRecognition) %>%
#  pivot_wider(
#    names_from = Alignment,
#    values_from = ViolationRecognition
#  ) %>%
#  mutate(AlignmentGap = Opposed - Aligned)

# alignment_gaps_es <- df_political %>%
#  select(ParticipantID, Civility, Alignment, EnforcementSeverity) %>%
#  pivot_wider(
#    names_from = Alignment,
#    values_from = EnforcementSeverity
#  ) %>%
#  mutate(AlignmentGap = Opposed - Aligned)

# Violation Recognition - borderline gap contrasts
# cat("Testing if Borderline gap > Civil gap (VR)\n")
# contrast_vr_1 <- alignment_gaps_vr %>%
#  select(ParticipantID, Civility, AlignmentGap) %>%
#  pivot_wider(names_from = Civility, values_from = AlignmentGap) %>%
#  mutate(Borderline_vs_Civil = Borderline - Civil)

# t_test_vr_1 <- t.test(contrast_vr_1$Borderline_vs_Civil, mu = 0)
# print(t_test_vr_1)
# cat("Mean difference (Borderline gap - Civil gap):", mean(contrast_vr_1$Borderline_vs_Civil, na.rm = TRUE), "\n")
# cat("Cohen's d:", cohens_d(contrast_vr_1$Borderline_vs_Civil, mu = 0)$Cohens_d, "\n\n")

# cat("Testing if Borderline gap > Uncivil gap (VR)\n")
# contrast_vr_2 <- alignment_gaps_vr %>%
#  select(ParticipantID, Civility, AlignmentGap) %>%
#  pivot_wider(names_from = Civility, values_from = AlignmentGap) %>%
#  mutate(Borderline_vs_Uncivil = Borderline - Uncivil)

# t_test_vr_2 <- t.test(contrast_vr_2$Borderline_vs_Uncivil, mu = 0, paired = FALSE)
# print(t_test_vr_2)
# cat("Mean difference (Borderline gap - Uncivil gap):", mean(contrast_vr_2$Borderline_vs_Uncivil, na.rm = TRUE), "\n")
# cat("Cohen's d:", cohens_d(contrast_vr_2$Borderline_vs_Uncivil, mu = 0)$Cohens_d, "\n\n")

# Summary of alignment gaps by civility (VR)
# cat("Summary of alignment gaps (Opposed - Aligned, VR):\n")
# alignment_gaps_vr %>%
#  group_by(Civility) %>%
#  summarise(
#    Mean_Gap = mean(AlignmentGap, na.rm = TRUE),
#    SD_Gap = sd(AlignmentGap, na.rm = TRUE),
#    SE_Gap = SD_Gap / sqrt(n())
#  ) %>%
#  print()

# Enforcement Severity - borderline gap contrasts
# cat("Testing if Borderline gap > Civil gap (ES)\n")
# contrast_es_1 <- alignment_gaps_es %>%
#  select(ParticipantID, Civility, AlignmentGap) %>%
#  pivot_wider(names_from = Civility, values_from = AlignmentGap) %>%
#  mutate(Borderline_vs_Civil = Borderline - Civil)

# t_test_es_1 <- t.test(contrast_es_1$Borderline_vs_Civil, mu = 0, paired = FALSE)
# print(t_test_es_1)
# cat("Mean difference (Borderline gap - Civil gap):", mean(contrast_es_1$Borderline_vs_Civil, na.rm = TRUE), "\n")
# cat("Cohen's d:", cohens_d(contrast_es_1$Borderline_vs_Civil, mu = 0)$Cohens_d, "\n\n")

# cat("Testing if Borderline gap > Uncivil gap (ES)\n")
# contrast_es_2 <- alignment_gaps_es %>%
#  select(ParticipantID, Civility, AlignmentGap) %>%
#  pivot_wider(names_from = Civility, values_from = AlignmentGap) %>%
#  mutate(Borderline_vs_Uncivil = Borderline - Uncivil)

# t_test_es_2 <- t.test(contrast_es_2$Borderline_vs_Uncivil, mu = 0, paired = FALSE)
# print(t_test_es_2)
# cat("Mean difference (Borderline gap - Uncivil gap):", mean(contrast_es_2$Borderline_vs_Uncivil, na.rm = TRUE), "\n")
# cat("Cohen's d:", cohens_d(contrast_es_2$Borderline_vs_Uncivil, mu = 0)$Cohens_d, "\n\n")

# Summary of alignment gaps by civility (ES)
# cat("Summary of alignment gaps (Opposed - Aligned, ES):\n")
# alignment_gaps_es %>%
#  group_by(Civility) %>%
#  summarise(
#    Mean_Gap = mean(AlignmentGap, na.rm = TRUE),
#    SD_Gap = sd(AlignmentGap, na.rm = TRUE),
#    SE_Gap = SD_Gap / sqrt(n())
#  ) %>%
#  print()


## Save results to file

sink("txt_output_full_results/ANOVA_Results_Complete.txt")

cat("--POLITICAL CONTENT MODERATION STUDY: ANOVA RESULTS--\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Sample size:", n_distinct(df_long$ParticipantID), "participants\n\n")

cat("--NON-POLITICAL BASELINE - CIVILITY EFFECT--\n")
cat("--- H1a: Violation Recognition ---\n")
print(anova_h1a_base)
cat("\nDescriptive Statistics:\n")
print(desc_h1a_base)

cat("\n--- H1b: Enforcement Severity ---\n")
print(anova_h1b_base)
cat("\nDescriptive Statistics:\n")
print(desc_h1b_base)

cat("--POLITICAL CONTENT: CIVILITY x ALIGNMENT--\n")
cat("--- Violation Recognition ---\n")
print(anova_political_vr)
cat("\nDescriptive Statistics:\n")
print(desc_political_vr)

cat("\n---Enforcement Severity---\n")
print(anova_political_es)
cat("\nDescriptive Statistics:\n")
print(desc_political_es)

# cat("-- Borderline-specific alignment gap contrasts --\n") #KEEP IN RESERVE
# cat("--- Violation Recognition ---\n")
# cat("Borderline vs Civil:\n")
# print(t_test_vr_1)
# cat("\nBorderline vs Uncivil:\n")
# print(t_test_vr_2)

# cat("\n--- Enforcement Severity ---\n")
# cat("Borderline vs Civil:\n")
# print(t_test_es_1)
# cat("\nBorderline vs Uncivil:\n")
# print(t_test_es_2)

sink()

cat("\n Results saved to: ANOVA_Results_Complete.txt")


## Plot results

# Ensure ordering
desc_h1a_base <- desc_h1a_base %>%
  mutate(Civility = factor(Civility, levels = c("Civil", "Borderline", "Uncivil")))

desc_h1b_base <- desc_h1b_base %>%
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

# Set theme for non-political baseline charts

base_theme <- theme_pubr() +
  theme(
    plot.title  = element_text(face = "bold"),
    axis.title  = element_text(face = "bold")
  )

# Top row: Non-political Baseline (bars)

p_base_vr <- desc_h1a_base %>%
  ggplot(aes(x = Civility, y = Mean, fill = Civility)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = CI95_lower, ymax = CI95_upper), width = 0.15) +
  scale_fill_manual(values = c(
    "Civil"      = "#81C784",
    "Borderline" = "#FFB74D",
    "Uncivil"    = "#EF5350"
  )) +
  labs(
    title = "Non-political Baseline: Violation Recognition",
    x = "Civility Level",
    y = "Violation Recognition Rate"
  ) +
  base_theme +
  theme(legend.position = "none") +
  ylim(0, 1)

p_base_es <- desc_h1b_base %>%
  ggplot(aes(x = Civility, y = Mean, fill = Civility)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = CI95_lower, ymax = CI95_upper), width = 0.15) +
  scale_fill_manual(values = c(
    "Civil"      = "#81C784",
    "Borderline" = "#FFB74D",
    "Uncivil"    = "#EF5350"
  )) +
  labs(
    title = "Non-political Baseline: Enforcement Severity",
    x = "Civility Level",
    y = "Enforcement Severity (0–4)"
  ) +
  base_theme +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, 0.5))

# Set theme for political condition charts

# position dodge for aligned vs opposed
pd <- position_dodge(width = 0.25)

base_theme <- theme_pubr() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

# Political Content – Violation Recognition 
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

# Political Content – Enforcement Severity
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
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, 0.5))

# ---------- Combine: 2 x 2 grid, no global title ----------

combined_plot <- (p_base_vr | p_base_es) /
  (p_vr       | p_es)

ggsave(
  "Graph_output_results/Moderation_Results_Combined.png",
  combined_plot,
  width = 12,
  height = 10,
  dpi = 300,
  bg = "white"
)

cat ("\n Plots saved to: Moderation_Results_Combined.png\n")

## STANDALONE GRAPHS: split by content type (VR + ES side by side)
## Matches slide layout; combined file above is retained


# Non-political baseline: VR + ES bar charts
nonpolitical_combined <- (p_base_vr | p_base_es)

ggsave(
  "Graph_output_results/Moderation_Results_NonPolitical.png",
  nonpolitical_combined,
  width  = 12,
  height = 5,
  dpi    = 300,
  bg     = "white"
)

# Political content: VR + ES line charts (shared legend)
political_combined <- (p_vr | p_es) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top")

ggsave(
  "Graph_output_results/Moderation_Results_Political.png",
  political_combined,
  width  = 12,
  height = 5,
  dpi    = 300,
  bg     = "white"
)

cat("\n Standalone plots saved: Moderation_Results_NonPolitical.png and Moderation_Results_Political.png\n")

## Table plots


# Helper function to clean ANOVA output for reporting:
# extracts the ANOVA table, rounds key statistics, and renames degrees-of-freedom columns

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


# Non-political content
tab_H1a_VR <- format_anova_table(anova_h1a_base)
tab_H1b_ES <- format_anova_table(anova_h1b_base)

# Political content
tab_VR_political <- format_anova_table(anova_political_vr)
tab_ES_political <- format_anova_table(anova_political_es)

# Inspect in console
tab_H1a_VR
tab_VR_political

# Export to CSV
write_csv(tab_H1a_VR, "csv_output_results/ANOVA_H1a_ViolationRecognition.csv") #ANOVA violation recognition, non-political condition - spot check
write_csv(tab_H1b_ES, "csv_output_results/ANOVA_H1b_EnforcementSeverity.csv") #ANOVA enforcement severity, non-political condition - spot check
write_csv(tab_VR_political, "csv_output_results/ANOVA_Political_ViolationRecognition.csv") #ANOVA violation recognition, political condition - spot check
write_csv(tab_ES_political, "csv_output_results/ANOVA_Political_EnforcementSeverity.csv") #ANOVA enforcement severity, political condition - spot check

write_csv(get_anova_table(anova_h1a_base,     correction="GG"), "csv_output_results/ANOVA_GG_BaseVR.csv") #ANOVA violation recognition, non-political condition
write_csv(get_anova_table(anova_h1b_base,     correction="GG"), "csv_output_results/ANOVA_GG_BaseES.csv") #ANOVA enforcement severity, non-political condition
write_csv(get_anova_table(anova_political_vr, correction="GG"), "csv_output_results/ANOVA_GG_PolVR.csv") #ANOVA violation recognition, political condition
write_csv(get_anova_table(anova_political_es, correction="GG"), "csv_output_results/ANOVA_GG_PolES.csv") #ANOVA enforcement severity, political condition

write_csv(desc_h1a_base,    "csv_descriptive_results/Desc_BaseVR.csv") #Descriptive results, violation recognition, non-political condition
write_csv(desc_h1b_base,    "csv_descriptive_results/Desc_BaseES.csv") #Descriptive results, enforcement severity, non-political condition
write_csv(desc_political_vr,"csv_descriptive_results/Desc_PolVR.csv") #Descriptive results, violation recognition, political condition
write_csv(desc_political_es,"csv_descriptive_results/Desc_PolES.csv") #Descriptive results, enforcement severity, political condition


