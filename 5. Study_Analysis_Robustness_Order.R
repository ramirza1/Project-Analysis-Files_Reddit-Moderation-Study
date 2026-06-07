# ==================================================
# ROBUSTNESS CHECK: Condition stability across order blocks
#
# Tests whether condition means are stable across the
# six counterbalancing order blocks. If stable, this
# supports the partial counterbalancing approach against
# the concern that order effects within sequences confound
# the main results.
#
# Run AFTER Study_Analysis.R to use the same df_political
# object. Or load directly from the .rds file.
# ==================================================

## LOAD LIBRARIES
library(tidyverse)
library(rstatix)
library(ggpubr)
library(patchwork)

## LOAD DATA (if not already in environment)
df_long <- readRDS("Input_data_long/Moderation_Data_Long_Format.rds")
df_political <- df_long %>% filter(ContentType == "Political")

# Confirm Order column exists - adapt name if different in your data
# Common names: Order, OrderBlock, Block, RandomOrder, Counterbalance
cat("Columns available:\n")
print(names(df_political))

# IMPORTANT: rename below if your column is named differently.
# Assuming the column is called "Order"
ORDER_COL <- "Order"

if (!ORDER_COL %in% names(df_political)) {
  stop(paste("Column", ORDER_COL, "not found. Edit ORDER_COL to match your data."))
}

cat("\nN per order block:\n")
df_political %>%
  distinct(ParticipantID, .data[[ORDER_COL]]) %>%
  count(.data[[ORDER_COL]]) %>%
  print()

## ========================================
## DESCRIPTIVES BY ORDER BLOCK x CONDITION
## ========================================

# Helper - mean + 95% CI by 3 grouping vars
desc_by_order <- function(df, dv) {
  df %>%
    group_by(.data[[ORDER_COL]], Civility, Alignment) %>%
    summarise(
      N    = n(),
      Mean = mean({{ dv }}, na.rm = TRUE),
      SD   = sd({{ dv }}, na.rm = TRUE),
      SE   = SD / sqrt(N),
      CI95_lower = Mean - qt(0.975, df = N - 1) * SE,
      CI95_upper = Mean + qt(0.975, df = N - 1) * SE,
      .groups = "drop"
    ) %>%
    mutate(
      Civility  = factor(Civility,  levels = c("Civil", "Borderline", "Uncivil")),
      Alignment = factor(Alignment, levels = c("Aligned", "Opposed"))
    )
}

desc_order_vr <- desc_by_order(df_political, ViolationRecognition)
desc_order_es <- desc_by_order(df_political, EnforcementSeverity)

cat("\n--- Violation Recognition by Order x Condition ---\n")
print(desc_order_vr, n = 36)

cat("\n--- Enforcement Severity by Order x Condition ---\n")
print(desc_order_es, n = 36)

## ========================================
## ANOVA: Order as between-subjects factor
## Tests whether order block significantly moderates the
## main effects of civility and alignment.
## ========================================

# Convert order to factor
df_political <- df_political %>%
  mutate(Order_f = factor(.data[[ORDER_COL]]))

cat("\n========================================\n")
cat("ANOVA: VR with Order as between-subjects factor\n")
cat("========================================\n")
anova_order_vr <- df_political %>%
  anova_test(
    dv          = ViolationRecognition,
    wid         = ParticipantID,
    within      = c(Civility, Alignment),
    between     = Order_f,
    effect.size = "pes"
  )
print(anova_order_vr)

cat("\n========================================\n")
cat("ANOVA: ES with Order as between-subjects factor\n")
cat("========================================\n")
anova_order_es <- df_political %>%
  anova_test(
    dv          = EnforcementSeverity,
    wid         = ParticipantID,
    within      = c(Civility, Alignment),
    between     = Order_f,
    effect.size = "pes"
  )
print(anova_order_es)

## ========================================
## VISUAL CHECK: Condition means across orders
## ========================================

# Color scheme matching existing scripts
COLOR_ALIGNED <- "#2E7D32"
COLOR_OPPOSED <- "#C62828"

base_theme_clean <- theme_pubr() +
  theme(
    plot.title       = element_text(face = "bold", size = 11),
    axis.title       = element_text(face = "bold", size = 10),
    axis.text        = element_text(size = 9),
    legend.title     = element_blank(),
    legend.position  = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold")
  )

pd <- position_dodge(width = 0.3)

# Faceted plot: one panel per order block, condition means by civility x alignment
p_order_vr <- ggplot(desc_order_vr,
                     aes(x = Civility, y = Mean,
                         color = Alignment, group = Alignment)) +
  geom_line(position = pd, linewidth = 0.8) +
  geom_point(position = pd, size = 2.5) +
  geom_errorbar(aes(ymin = CI95_lower, ymax = CI95_upper),
                position = pd, width = 0.15) +
  scale_color_manual(values = c("Aligned" = COLOR_ALIGNED,
                                "Opposed" = COLOR_OPPOSED)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  facet_wrap(~ .data[[ORDER_COL]], nrow = 2,
             labeller = labeller(.default = function(x) gsub("Order", "Order ", x))) +
  labs(
    title = "Violation Recognition by Order Block",
    x     = "Civility Level",
    y     = "Violation Recognition Rate"
  ) +
  base_theme_clean

p_order_es <- ggplot(desc_order_es,
                     aes(x = Civility, y = Mean,
                         color = Alignment, group = Alignment)) +
  geom_line(position = pd, linewidth = 0.8) +
  geom_point(position = pd, size = 2.5) +
  geom_errorbar(aes(ymin = CI95_lower, ymax = CI95_upper),
                position = pd, width = 0.15) +
  scale_color_manual(values = c("Aligned" = COLOR_ALIGNED,
                                "Opposed" = COLOR_OPPOSED)) +
  scale_y_continuous(limits = c(0, 3.5), breaks = seq(0, 3, 0.5)) +
  facet_wrap(~ .data[[ORDER_COL]], nrow = 2,
             labeller = labeller(.default = function(x) gsub("Order", "Order ", x))) +
  labs(
    title = "Enforcement Severity by Order Block",
    x     = "Civility Level",
    y     = "Enforcement Severity (0-4)"
  ) +
  base_theme_clean

ggsave("Graph_output_results/Robustness_Order_VR.png", p_order_vr,
       width = 12, height = 7, dpi = 300, bg = "white")
ggsave("Graph_output_results/Robustness_Order_ES.png", p_order_es,
       width = 12, height = 7, dpi = 300, bg = "white")

cat("\nFacet plots saved:\n")
cat("  Robustness_Order_VR.png\n")
cat("  Robustness_Order_ES.png\n")

## ========================================
## SAVE RESULTS
## ========================================

sink("txt_output_full_results/Robustness_Order_Results.txt")

cat("========================================\n")
cat("ROBUSTNESS CHECK: ORDER STABILITY\n")
cat("========================================\n\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

cat("--- Sample size by order block ---\n")
df_political %>%
  distinct(ParticipantID, .data[[ORDER_COL]]) %>%
  count(.data[[ORDER_COL]]) %>%
  print()

cat("\n--- Violation Recognition: condition means by order block ---\n")
print(desc_order_vr, n = 36)

cat("\n--- Enforcement Severity: condition means by order block ---\n")
print(desc_order_es, n = 36)

cat("\n--- ANOVA: VR with Order as between factor ---\n")
print(anova_order_vr)

cat("\n--- ANOVA: ES with Order as between factor ---\n")
print(anova_order_es)

cat("\n========================================\n")
cat("INTERPRETATION GUIDE\n")
cat("========================================\n")
cat("If main effects of Civility and Alignment hold across orders\n")
cat("(no significant interactions involving Order_f),\n")
cat("the partial counterbalancing approach is supported.\n\n")
cat("Look for:\n")
cat("  - Order_f main effect: is there a baseline shift across orders?\n")
cat("  - Order_f:Civility: do civility effects vary across orders?\n")
cat("  - Order_f:Alignment: do alignment effects vary across orders?\n")
cat("  - Order_f:Civility:Alignment: does the interaction vary?\n")
cat("Non-significant interactions support stability.\n")

sink()

cat("\nResults saved to: Robustness_Order_Results.txt\n")

## Export descriptives
write_csv(desc_order_vr, "csv_descriptive_results/Robustness_Order_VR_Descriptives.csv") #Descriptive results, order robustness, violation recognition
write_csv(desc_order_es, "csv_descriptive_results/Robustness_Order_ES_Descriptives.csv") #Descriptive results, order robustness, enforcement severity

write_csv(get_anova_table(anova_order_vr, correction="auto"), "csv_output_results/ANOVA_GG_OrderVR.csv") #ANOVA, order robustness, violation recognition
write_csv(get_anova_table(anova_order_es, correction="auto"), "csv_output_results/ANOVA_GG_OrderES.csv") #ANOVA, order robustness, enforcement severity


cat("CSV tables exported.\n")