
## PARTISAN COMPARISON ANALYSIS - EXPLORATORY

## LOAD LIBRARIES
library(tidyverse)
library(rstatix)
library(effectsize)
library(ggpubr)
library(patchwork)

## LOAD DATA
df_long <- readRDS("Input_data_long/Moderation_Data_Long_Format.rds")

# Political conditions only
df_political <- df_long %>% filter(ContentType == "Political")

# Create Democrat and Republican subsets
df_dem <- df_political %>% filter(PoliticalLeaning == "Democrat (Left-aligned)")
df_rep <- df_political %>% filter(PoliticalLeaning == "Republican (Right-aligned)")

# Sample sizes
cat("=== SAMPLE SIZES ===\n")
cat("Democrats:", n_distinct(df_dem$ParticipantID), "participants\n")
cat("Republicans:", n_distinct(df_rep$ParticipantID), "participants\n\n")

cat("=== DATA SUMMARY ===\n")
cat("Political content:", nrow(df_political), "rows (6 per participant)\n")
cat("Unique participants:", n_distinct(df_long$ParticipantID), "\n\n")

## Set up Descriptive Stats
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


## DEMOCRATS ANALYSIS


cat("DEMOCRATS (N = ", n_distinct(df_dem$ParticipantID), ")\n")

# Violation recognition ANOVA
cat("--- ANOVA: Violation Recognition ---\n")
anova_dem_vr <- df_dem %>%
  anova_test(
    dv = ViolationRecognition,
    wid = ParticipantID,
    within = c(Civility, Alignment),
    effect.size = "pes"
  )
print(anova_dem_vr)

# Descriptive statistics VR
desc_dem_vr <- desc_with_ci_2(
  df = df_dem,
  dv = ViolationRecognition,
  group_var1 = Civility,
  group_var2 = Alignment
)

cat("\n--- Violation Recognition Descriptives ---\n")
print(desc_dem_vr)

# Enforcement Severity ANOVA
cat("\n--- ANOVA: Enforcement Severity ---\n")
anova_dem_es <- df_dem %>%
  anova_test(
    dv = EnforcementSeverity,
    wid = ParticipantID,
    within = c(Civility, Alignment),
    effect.size = "pes"
  )
print(anova_dem_es)

# Descriptive statistics ES
desc_dem_es <- desc_with_ci_2(
  df = df_dem,
  dv = EnforcementSeverity,
  group_var1 = Civility,
  group_var2 = Alignment
)

cat("\n--- Enforcement Severity Descriptives ---\n")
print(desc_dem_es)


## REPUBLICANS ANALYSIS

cat("REPUBLICANS (N = ", n_distinct(df_rep$ParticipantID), ")\n")

# Violation Recognition ANOVA
cat("--- ANOVA: Violation Recognition ---\n")
anova_rep_vr <- df_rep %>%
  anova_test(
    dv = ViolationRecognition,
    wid = ParticipantID,
    within = c(Civility, Alignment),
    effect.size = "pes"
  )
print(anova_rep_vr)

# Violation Recognition Descriptives
desc_rep_vr <- desc_with_ci_2(
  df = df_rep,
  dv = ViolationRecognition,
  group_var1 = Civility,
  group_var2 = Alignment
)

cat("\n--- Violation Recognition Descriptives ---\n")
print(desc_rep_vr)

# Enforcement Severity ANOVA
cat("\n--- ANOVA: Enforcement Severity ---\n")
anova_rep_es <- df_rep %>%
  anova_test(
    dv = EnforcementSeverity,
    wid = ParticipantID,
    within = c(Civility, Alignment),
    effect.size = "pes"
  )
print(anova_rep_es)

# Enforcement Severity Descriptives
desc_rep_es <- desc_with_ci_2(
  df = df_rep,
  dv = EnforcementSeverity,
  group_var1 = Civility,
  group_var2 = Alignment
)

cat("\n--- Enforcement Severity Descriptives ---\n")
print(desc_rep_es)

## SAVE RESULTS TO FILE

sink("txt_output_full_results/Partisan_Comparison_Results.txt")


cat("PARTISAN COMPARISON ANALYSIS\n")
cat("Political Content Moderation\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")


cat("DEMOCRATS (N = ", n_distinct(df_dem$ParticipantID), ")\n")


cat("--- Descriptive Statistics: Violation Recognition ---\n")
print(desc_dem_vr)

cat("\n--- Descriptive Statistics: Enforcement Severity ---\n")
print(desc_dem_es)

cat("\n--- ANOVA: Violation Recognition ---\n")
print(anova_dem_vr)

cat("\n--- ANOVA: Enforcement Severity ---\n")
print(anova_dem_es)


cat("REPUBLICANS (N = ", n_distinct(df_rep$ParticipantID), ")\n")


cat("--- Descriptive Statistics: Violation Recognition ---\n")
print(desc_rep_vr)

cat("\n--- Descriptive Statistics: Enforcement Severity ---\n")
print(desc_rep_es)

cat("\n--- ANOVA: Violation Recognition ---\n")
print(anova_rep_vr)

cat("\n--- ANOVA: Enforcement Severity ---\n")
print(anova_rep_es)

sink()

cat("\nResults saved to: Partisan_Comparison_Results.txt\n")


## CREATE PLOTS

# Ensure proper factor ordering
desc_dem_vr <- desc_dem_vr %>%
  mutate(
    Civility = factor(Civility, levels = c("Civil", "Borderline", "Uncivil")),
    Alignment = factor(Alignment, levels = c("Aligned", "Opposed"))
  )

desc_dem_es <- desc_dem_es %>%
  mutate(
    Civility = factor(Civility, levels = c("Civil", "Borderline", "Uncivil")),
    Alignment = factor(Alignment, levels = c("Aligned", "Opposed"))
  )

desc_rep_vr <- desc_rep_vr %>%
  mutate(
    Civility = factor(Civility, levels = c("Civil", "Borderline", "Uncivil")),
    Alignment = factor(Alignment, levels = c("Aligned", "Opposed"))
  )

desc_rep_es <- desc_rep_es %>%
  mutate(
    Civility = factor(Civility, levels = c("Civil", "Borderline", "Uncivil")),
    Alignment = factor(Alignment, levels = c("Aligned", "Opposed"))
  )

# Base theme
base_theme <- theme_pubr() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 10),
    legend.position = "top"
  )

pd <- position_dodge(width = 0.25)

# Democrats - Violation Recognition
p_dem_vr <- desc_dem_vr %>%
  ggplot(aes(x = Civility, y = Mean, color = Alignment, group = Alignment)) +
  geom_line(position = pd, linewidth = 1) +
  geom_point(position = pd, size = 3) +
  geom_errorbar(aes(ymin = CI95_lower, ymax = CI95_upper),
                position = pd, width = 0.1) +
  scale_color_manual(values = c("Aligned" = "#2E7D32", "Opposed" = "#C62828")) +
  labs(
    title = "Democrats: Violation Recognition",
    x = "Civility Level",
    y = "Violation Recognition Rate"
  ) +
  base_theme +
  ylim(0, 1)

# Democrats - Enforcement Severity
p_dem_es <- desc_dem_es %>%
  ggplot(aes(x = Civility, y = Mean, color = Alignment, group = Alignment)) +
  geom_line(position = pd, linewidth = 1) +
  geom_point(position = pd, size = 3) +
  geom_errorbar(aes(ymin = CI95_lower, ymax = CI95_upper),
                position = pd, width = 0.1) +
  scale_color_manual(values = c("Aligned" = "#2E7D32", "Opposed" = "#C62828")) +
  labs(
    title = "Democrats: Enforcement Severity",
    x = "Civility Level",
    y = "Enforcement Severity (0–4)"
  ) +
  base_theme +
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, 0.5))

# Republicans - Violation Recognition
p_rep_vr <- desc_rep_vr %>%
  ggplot(aes(x = Civility, y = Mean, color = Alignment, group = Alignment)) +
  geom_line(position = pd, linewidth = 1) +
  geom_point(position = pd, size = 3) +
  geom_errorbar(aes(ymin = CI95_lower, ymax = CI95_upper),
                position = pd, width = 0.1) +
  scale_color_manual(values = c("Aligned" = "#2E7D32", "Opposed" = "#C62828")) +
  labs(
    title = "Republicans: Violation Recognition",
    x = "Civility Level",
    y = "Violation Recognition Rate"
  ) +
  base_theme +
  ylim(0, 1)

# Republicans - Enforcement Severity
p_rep_es <- desc_rep_es %>%
  ggplot(aes(x = Civility, y = Mean, color = Alignment, group = Alignment)) +
  geom_line(position = pd, linewidth = 1) +
  geom_point(position = pd, size = 3) +
  geom_errorbar(aes(ymin = CI95_lower, ymax = CI95_upper),
                position = pd, width = 0.1) +
  scale_color_manual(values = c("Aligned" = "#2E7D32", "Opposed" = "#C62828")) +
  labs(
    title = "Republicans: Enforcement Severity",
    x = "Civility Level",
    y = "Enforcement Severity (0–4)"
  ) +
  base_theme +
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, 0.5))

# Combine plots: 2x2 grid
combined_partisan <- (p_dem_vr | p_rep_vr) / (p_dem_es | p_rep_es)

# Save
ggsave(
  "Graph_output_results/Partisan_Comparison_Plots.png",
  combined_partisan,
  width = 12,
  height = 10,
  dpi = 300,
  bg = "white"
)

cat("\n Plots saved to: Partisan_Comparison_Plots.png\n")


## STANDALONE GRAPHS: split by party (VR + ES per party)
## Combined file above is retained


# Democrats: VR + ES (shared legend)
democrats_combined <- (p_dem_vr | p_dem_es) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top")

ggsave(
  "Graph_output_results/Partisan_Democrats.png",
  democrats_combined,
  width  = 12,
  height = 5,
  dpi    = 300,
  bg     = "white"
)

# Republicans: VR + ES (shared legend)
republicans_combined <- (p_rep_vr | p_rep_es) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top")

ggsave(
  "Graph_output_results/Partisan_Republicans.png",
  republicans_combined,
  width  = 12,
  height = 5,
  dpi    = 300,
  bg     = "white"
)

cat("\n Standalone plots saved: Partisan_Democrats.png and Partisan_Republicans.png\n")


## EXPORT TABLES TO CSV


write_csv(desc_dem_vr, "csv_descriptive_results/Democrats_ViolationRecognition_Descriptives.csv") #Descriptive results, violation recognition, Democrats
write_csv(desc_dem_es, "csv_descriptive_results/Democrats_EnforcementSeverity_Descriptives.csv") #Descriptive results, enforcement severity, Democrats
write_csv(desc_rep_vr, "csv_descriptive_results/Republicans_ViolationRecognition_Descriptives.csv") #Descriptive results, violation recognition, Republican
write_csv(desc_rep_es, "csv_descriptive_results/Republicans_EnforcementSeverity_Descriptives.csv") #Descriptive results, enforcement severity, Republican
 
write_csv(get_anova_table(anova_dem_vr, correction="GG"), "csv_output_results/ANOVA_GG_DemVR.csv") #ANOVA, violation recognition, Democrats
write_csv(get_anova_table(anova_dem_es, correction="GG"), "csv_output_results/ANOVA_GG_DemES.csv") #ANOVA, enforcement severity, Democrats
write_csv(get_anova_table(anova_rep_vr, correction="GG"), "csv_output_results/ANOVA_GG_RepVR.csv") #ANOVA, violation recognition, Republican
write_csv(get_anova_table(anova_rep_es, correction="GG"), "csv_output_results/ANOVA_GG_RepES.csv") #ANOVA, enforcement severity, Republican

cat("\nCSV tables exported\n")