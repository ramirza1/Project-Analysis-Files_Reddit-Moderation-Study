# ==================================================
# WRITE LATEX VALUES — CSV version
#
# Fully standalone: reads from CSV outputs only.
# No R analysis objects needed in environment.
#
# Pre-requisite: run scripts 2–5 at least once with
# the extra write_csv() lines added to each script.
#
# Output: study_outputs.tex
# LaTeX preamble: \input{study_outputs.tex}
# ==================================================

library(readr)
library(dplyr)

# ---- Paths ----
ANOVA_DIR <- "csv_output_results/"
DESC_DIR  <- "csv_descriptive_results/"

# ---- Helpers ----

fmt_p <- function(p) {
  if (p < 0.001) "$p<0.001$" else sprintf("$p=%.3f$", p)
}

fmt_pes <- function(pes) {
  if (pes < 0.001) "partial $\\eta^2 < .001$"
  else {
    val <- sub("^0\\.", ".", sprintf("%.3f", pes))
    sprintf("partial $\\eta^2 = %s$", val)
  }
}

fmt_df <- function(x) {
  if (abs(x - round(x)) < 0.005) sprintf("%d", as.integer(round(x)))
  else sprintf("%.2f", x)
}

fmt_row <- function(df, effect, show_pes = TRUE) {
  row <- df[df$Effect == effect, ]
  if (nrow(row) == 0) stop(paste("Effect not found:", effect,
                                 "| Available:", paste(df$Effect, collapse=", ")))
  f <- sprintf("$F(%s,\\,%s) = %.2f$", fmt_df(row$DFn), fmt_df(row$DFd), row$F)
  p <- fmt_p(row$p)
  if (show_pes) paste(f, p, fmt_pes(row$pes), sep=", ") else paste(f, p, sep=", ")
}

pull_mean <- function(df, ...) {
  filters <- list(...)
  for (col in names(filters)) df <- df[df[[col]] == filters[[col]], ]
  if (nrow(df) == 0) stop(paste("No match:", paste(names(filters), filters, sep="=", collapse=", ")))
  df$Mean[1]
}

avg_by <- function(df, col, val) mean(df$Mean[df[[col]] == val])

pull_p_adj <- function(df, civility_level) {
  df$p.adj[df$Civility == civility_level]
}

# Leave-one-out helpers -------------------------------------------------

# Test statistic + p for one effect in one drop.
# Keys off the DV NAME (not a StatType column) so it cannot mislabel:
#   Enforcement severity  = linear mixed model  -> F
#   Violation recognition = logistic mixed model -> chi-square
fmt_leave_stat <- function(df, effect, dropped) {
  row <- df[df$Effect == effect & df$Dropped == dropped, ]
  if (nrow(row) == 0) stop(paste("Leave-out-analysis row not found:", effect, dropped,
                                 "| Available effects:", paste(unique(df$Effect), collapse=", "),
                                 "| drops:", paste(unique(df$Dropped), collapse=", ")))
  if (grepl("Enforcement", row$DV[1])) {
    sprintf("$F = %.2f$, %s", row$Statistic, fmt_p(row$p))
  } else {
    sprintf("$\\chi^2 = %.2f$, %s", row$Statistic, fmt_p(row$p))
  }
}

# p-value only for an effect in one drop
fmt_leave_p <- function(df, effect, dropped) {
  row <- df[df$Effect == effect & df$Dropped == dropped, ]
  fmt_p(row$p)
}

# Largest p across all six drops for an effect (worst case)
leave_p_max <- function(df, effect) {
  ps <- df$p[df$Effect == effect]
  max(ps)
}

# Model-estimated gap + 95% CI for one drop.
# unit = "es" -> scale points (2 dp); unit = "vr" -> probability as pp (1 dp)
fmt_leave_gap <- function(df, dropped, unit) {
  row <- df[df$Dropped == dropped, ]
  if (nrow(row) == 0) stop(paste("Gap row not found:", dropped))
  if (unit == "es") {
    sprintf("%.2f\\,[%.2f,\\,%.2f]", row$Gap, row$CI_low, row$CI_high)
  } else {
    sprintf("%.1f\\,pp\\,[%.1f,\\,%.1f]",
            row$Gap*100, row$CI_low*100, row$CI_high*100)
  }
}

# ---- Read CSVs ----

# ANOVA tables (GG-corrected)
a_base_vr <- read_csv(paste0(ANOVA_DIR, "ANOVA_GG_BaseVR.csv"),      show_col_types=FALSE)
a_base_es <- read_csv(paste0(ANOVA_DIR, "ANOVA_GG_BaseES.csv"),      show_col_types=FALSE)
a_pol_vr  <- read_csv(paste0(ANOVA_DIR, "ANOVA_GG_PolVR.csv"),       show_col_types=FALSE)
a_pol_es  <- read_csv(paste0(ANOVA_DIR, "ANOVA_GG_PolES.csv"),       show_col_types=FALSE)
a_cmp_vr  <- read_csv(paste0(ANOVA_DIR, "ANOVA_GG_CompareVR.csv"),   show_col_types=FALSE)
a_cmp_es  <- read_csv(paste0(ANOVA_DIR, "ANOVA_GG_CompareES.csv"),   show_col_types=FALSE)
a_dem_vr  <- read_csv(paste0(ANOVA_DIR, "ANOVA_GG_DemVR.csv"),       show_col_types=FALSE)
a_dem_es  <- read_csv(paste0(ANOVA_DIR, "ANOVA_GG_DemES.csv"),       show_col_types=FALSE)
a_rep_vr  <- read_csv(paste0(ANOVA_DIR, "ANOVA_GG_RepVR.csv"),       show_col_types=FALSE)
a_rep_es  <- read_csv(paste0(ANOVA_DIR, "ANOVA_GG_RepES.csv"),       show_col_types=FALSE)
a_ord_vr  <- read_csv(paste0(ANOVA_DIR, "ANOVA_GG_OrderVR.csv"),     show_col_types=FALSE)
a_ord_es  <- read_csv(paste0(ANOVA_DIR, "ANOVA_GG_OrderES.csv"),     show_col_types=FALSE)

# Simple effects (pairwise t-tests from script 3)
s_cmp_vr  <- read_csv(paste0(ANOVA_DIR, "SimpleEffects_CompareVR.csv"), show_col_types=FALSE)
s_cmp_es  <- read_csv(paste0(ANOVA_DIR, "SimpleEffects_CompareES.csv"), show_col_types=FALSE)

# Descriptive stats
d_base_vr <- read_csv(paste0(DESC_DIR, "Desc_BaseVR.csv"),  show_col_types=FALSE)
d_base_es <- read_csv(paste0(DESC_DIR, "Desc_BaseES.csv"),  show_col_types=FALSE)
d_pol_vr  <- read_csv(paste0(DESC_DIR, "Desc_PolVR.csv"),   show_col_types=FALSE)
d_pol_es  <- read_csv(paste0(DESC_DIR, "Desc_PolES.csv"),   show_col_types=FALSE)
d_cmp_vr  <- read_csv(paste0(DESC_DIR, "Political_vs_NonPolitical_VR_Descriptives.csv"), show_col_types=FALSE)
d_cmp_es  <- read_csv(paste0(DESC_DIR, "Political_vs_NonPolitical_ES_Descriptives.csv"), show_col_types=FALSE)
d_dem_vr  <- read_csv(paste0(DESC_DIR, "Democrats_ViolationRecognition_Descriptives.csv"), show_col_types=FALSE)
d_dem_es  <- read_csv(paste0(DESC_DIR, "Democrats_EnforcementSeverity_Descriptives.csv"),  show_col_types=FALSE)
d_rep_vr  <- read_csv(paste0(DESC_DIR, "Republicans_ViolationRecognition_Descriptives.csv"), show_col_types=FALSE)
d_rep_es  <- read_csv(paste0(DESC_DIR, "Republicans_EnforcementSeverity_Descriptives.csv"),  show_col_types=FALSE)

#Leave-one-out analysis
leave_es     <- read_csv(paste0(ANOVA_DIR, "Robustness_LeaveOneOut_ES.csv"),     show_col_types=FALSE)
leave_vr     <- read_csv(paste0(ANOVA_DIR, "Robustness_LeaveOneOut_VR.csv"),     show_col_types=FALSE)
leave_gap_es <- read_csv(paste0(ANOVA_DIR, "Robustness_LeaveOneOut_Gap_ES.csv"), show_col_types=FALSE)
leave_gap_vr <- read_csv(paste0(ANOVA_DIR, "Robustness_LeaveOneOut_Gap_VR.csv"), show_col_types=FALSE)
cat("All CSVs loaded\n")

# COLLECT VALUES

vals <- list()

# ---- 1. Non-political baseline: civility effects ----

vals$BaseVRCivility   <- fmt_row(a_base_vr, "Civility", show_pes=FALSE)
vals$BaseVRCivil      <- sprintf("%.1f\\%%", pull_mean(d_base_vr, Civility="Civil")      * 100)
vals$BaseVRBorderline <- sprintf("%.1f\\%%", pull_mean(d_base_vr, Civility="Borderline") * 100)
vals$BaseVRUncivil    <- sprintf("%.1f\\%%", pull_mean(d_base_vr, Civility="Uncivil")    * 100)

vals$BaseESCivility   <- fmt_row(a_base_es, "Civility", show_pes=FALSE)
vals$BaseESCivil      <- sprintf("%.2f", pull_mean(d_base_es, Civility="Civil"))
vals$BaseESBorderline <- sprintf("%.2f", pull_mean(d_base_es, Civility="Borderline"))
vals$BaseESUncivil    <- sprintf("%.2f", pull_mean(d_base_es, Civility="Uncivil"))

# ---- 2. Political conditions: civility main effects (H1) ----

vals$PolVRCivility    <- fmt_row(a_pol_vr, "Civility")
vals$PolVRCivil       <- sprintf("%.1f\\%%", avg_by(d_pol_vr, "Civility", "Civil")      * 100)
vals$PolVRBorderline  <- sprintf("%.1f\\%%", avg_by(d_pol_vr, "Civility", "Borderline") * 100)
vals$PolVRUncivil     <- sprintf("%.1f\\%%", avg_by(d_pol_vr, "Civility", "Uncivil")    * 100)

vals$PolESCivility    <- fmt_row(a_pol_es, "Civility")
vals$PolESCivil       <- sprintf("%.2f", avg_by(d_pol_es, "Civility", "Civil"))
vals$PolESBorderline  <- sprintf("%.2f", avg_by(d_pol_es, "Civility", "Borderline"))
vals$PolESUncivil     <- sprintf("%.2f", avg_by(d_pol_es, "Civility", "Uncivil"))

# ---- 3. Political conditions: alignment main effects (H2) ----

vals$PolVRAlignment   <- fmt_row(a_pol_vr, "Alignment")
vals$PolVRAligned     <- sprintf("%.1f\\%%", avg_by(d_pol_vr, "Alignment", "Aligned") * 100)
vals$PolVROpposed     <- sprintf("%.1f\\%%", avg_by(d_pol_vr, "Alignment", "Opposed") * 100)

vals$PolESAlignment   <- fmt_row(a_pol_es, "Alignment")
vals$PolESAligned     <- sprintf("%.2f", avg_by(d_pol_es, "Alignment", "Aligned"))
vals$PolESOpposed     <- sprintf("%.2f", avg_by(d_pol_es, "Alignment", "Opposed"))

# ---- 4. Political conditions: interaction (H3) ----

vals$PolVRInteraction <- fmt_row(a_pol_vr, "Civility:Alignment")
vals$PolESInteraction <- fmt_row(a_pol_es, "Civility:Alignment")

# ---- 5. Political vs non-political comparison ----

vals$CompVRInteraction <- fmt_row(a_cmp_vr, "Civility:ContentType")
vals$CompESInteraction <- fmt_row(a_cmp_es, "Civility:ContentType")

for (civ in c("Civil", "Borderline", "Uncivil")) {
  tag <- sub("line", "", civ)   # Civil, Bord, Uncivil
  
  pol_vr <- pull_mean(d_cmp_vr, Civility=civ, ContentType="Political")
  np_vr  <- pull_mean(d_cmp_vr, Civility=civ, ContentType="Non-political")
  gap_vr <- (pol_vr - np_vr) * 100
  vals[[paste0("CompVR", tag, "Pol")]] <- sprintf("%.1f\\%%", pol_vr * 100)
  vals[[paste0("CompVR", tag, "NP")]]  <- sprintf("%.1f\\%%", np_vr  * 100)
  vals[[paste0("CompVR", tag, "Gap")]] <- if (gap_vr > 0) sprintf("+%.1f", gap_vr) else sprintf("%.1f", gap_vr)
  vals[[paste0("CompVR", tag, "P")]]   <- fmt_p(pull_p_adj(s_cmp_vr, civ))
  
  pol_es <- pull_mean(d_cmp_es, Civility=civ, ContentType="Political")
  np_es  <- pull_mean(d_cmp_es, Civility=civ, ContentType="Non-political")
  gap_es <- pol_es - np_es
  vals[[paste0("CompES", tag, "Pol")]] <- sprintf("%.2f", pol_es)
  vals[[paste0("CompES", tag, "NP")]]  <- sprintf("%.2f", np_es)
  vals[[paste0("CompES", tag, "Gap")]] <- if (gap_es > 0) sprintf("+%.2f", gap_es) else sprintf("%.2f", gap_es)
  vals[[paste0("CompES", tag, "P")]]   <- fmt_p(pull_p_adj(s_cmp_es, civ))
}

# ---- 6. Subgroup analysis: Democrats and Republicans ----

vals$DemVRAlignment <- fmt_row(a_dem_vr, "Alignment")
vals$DemVRAligned   <- sprintf("%.1f\\%%", avg_by(d_dem_vr, "Alignment", "Aligned") * 100)
vals$DemVROpposed   <- sprintf("%.1f\\%%", avg_by(d_dem_vr, "Alignment", "Opposed") * 100)

vals$RepVRAlignment <- fmt_row(a_rep_vr, "Alignment")
vals$RepVRAligned   <- sprintf("%.1f\\%%", avg_by(d_rep_vr, "Alignment", "Aligned") * 100)
vals$RepVROpposed   <- sprintf("%.1f\\%%", avg_by(d_rep_vr, "Alignment", "Opposed") * 100)

vals$DemESAlignment <- fmt_row(a_dem_es, "Alignment")
vals$DemESAligned   <- sprintf("%.2f", avg_by(d_dem_es, "Alignment", "Aligned"))
vals$DemESOpposed   <- sprintf("%.2f", avg_by(d_dem_es, "Alignment", "Opposed"))

vals$RepESAlignment <- fmt_row(a_rep_es, "Alignment")
vals$RepESAligned   <- sprintf("%.2f", avg_by(d_rep_es, "Alignment", "Aligned"))
vals$RepESOpposed   <- sprintf("%.2f", avg_by(d_rep_es, "Alignment", "Opposed"))

# ---- 7. Robustness: order effects ----

vals$RobVROrderMain     <- fmt_row(a_ord_vr, "Order_f")
vals$RobVROrderAlign    <- fmt_row(a_ord_vr, "Order_f:Alignment")
vals$RobVROrderCiv      <- fmt_row(a_ord_vr, "Order_f:Civility")

vals$RobESOrderMain     <- fmt_row(a_ord_es, "Order_f")
vals$RobESOrderAlign    <- fmt_row(a_ord_es, "Order_f:Alignment")
vals$RobESOrderThreeWay <- fmt_row(a_ord_es, "Order_f:Civility:Alignment")


# ---- 8. Robustness: leave-one-out analysis ----

# ---- 8. Robustness: leave-one-topic-out ----
# Macro NAMES use letter-only suffixes (Tone..Tsix) because LaTeX
# \newcommand names may not contain digits. CSV filters still use
# "Drop T1".."Drop T6" (the data is unchanged).

drops    <- paste0("Drop T", 1:6)
name_sfx <- c("Tone", "Ttwo", "Tthree", "Tfour", "Tfive", "Tsix")

# Per-drop tests for ALL three effects (ES: F ; VR: chi-square)
for (i in 1:6) {
  sfx <- name_sfx[i]
  d   <- drops[i]
  vals[[paste0("LeaveESAlign", sfx)]] <- fmt_leave_stat(leave_es, "Alignment",          d)
  vals[[paste0("LeaveVRAlign", sfx)]] <- fmt_leave_stat(leave_vr, "Alignment",          d)
  vals[[paste0("LeaveESCiv",   sfx)]] <- fmt_leave_stat(leave_es, "Civility",           d)
  vals[[paste0("LeaveVRCiv",   sfx)]] <- fmt_leave_stat(leave_vr, "Civility",           d)
  vals[[paste0("LeaveESInt",   sfx)]] <- fmt_leave_stat(leave_es, "Civility:Alignment", d)
  vals[[paste0("LeaveVRInt",   sfx)]] <- fmt_leave_stat(leave_vr, "Civility:Alignment", d)
}

# Per-drop model-estimated gaps with CIs
for (i in 1:6) {
  sfx <- name_sfx[i]
  d   <- drops[i]
  vals[[paste0("LeaveESGap", sfx)]] <- fmt_leave_gap(leave_gap_es, d, "es")
  vals[[paste0("LeaveVRGap", sfx)]] <- fmt_leave_gap(leave_gap_vr, d, "vr")
}

# Summary values across drops (no digits in these names)
vals$LeaveESAlignMaxP <- fmt_p(leave_p_max(leave_es, "Alignment"))
vals$LeaveVRAlignMaxP <- fmt_p(leave_p_max(leave_vr, "Alignment"))
vals$LeaveESCivMaxP   <- fmt_p(leave_p_max(leave_es, "Civility"))
vals$LeaveVRCivMaxP   <- fmt_p(leave_p_max(leave_vr, "Civility"))
vals$LeaveESIntMinP   <- fmt_p(min(leave_es$p[leave_es$Effect == "Civility:Alignment"]))
vals$LeaveVRIntMinP   <- fmt_p(min(leave_vr$p[leave_vr$Effect == "Civility:Alignment"]))


# WRITE study_outputs.tex


lines <- mapply(function(name, value) {
  sprintf("\\newcommand{\\%s}{%s}", name, value)
}, names(vals), vals, SIMPLIFY=TRUE)

writeLines(c(
    "% AUTO-GENERATED BY write_values.R — DO NOT EDIT BY HAND",
  sprintf("%% Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  lines
), "study_outputs.tex")


cat(sprintf("\n study_outputs.text written with %d command. \n", length(lines)))
cat("Add \\input{study_outputs.tex} to your LaTeX preamble.\n\n")
cat("Commands written:\n")
cat(paste(" ", names(vals)), sep="\n")