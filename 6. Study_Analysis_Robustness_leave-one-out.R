# ==================================================
# ROBUSTNESS CHECK: Leave-one-topic-out stability
# (Mixed-effects specification)
#
# WHY A MIXED MODEL HERE (and not the within-subjects ANOVA):
# In the main analysis each participant contributes all six
# condition cells, so the design is fully within-subjects. But
# topic is confounded with the (Order x Party x Alignment x
# Civility) cell: each participant sees each topic exactly once,
# in exactly one condition. Dropping a topic therefore removes
# one cell from EVERY participant, making everyone incomplete —
# a within-subjects repeated-measures ANOVA then deletes all
# cases (0 non-NA cases error).
#
# A mixed-effects model with a random intercept per participant
# tolerates this incompleteness: it estimates each person's
# baseline level from their remaining observations, so nobody is
# dropped. It keeps the full 3x2 fixed-effects structure
# (Civility, Alignment, and their interaction) while absorbing
# individual moderation "style" into the random intercept —
# preserving the each-person-is-their-own-control logic of the
# main design.
#
# OUTCOMES:
#   Enforcement severity (0-4) -> LINEAR mixed model (lmer),
#     treating the ordered scale as numeric (as in the main paper).
#   Violation recognition (0/1) -> LOGISTIC mixed model
#     (glmer, binomial), respecting the binary outcome.
#
# WHAT THIS CAN CLAIM: the alignment effect (and civility effect,
# and interaction) is recoverable and the alignment effect holds
# when any single topic is removed -> not driven by one topic.
#
# WHAT IT CANNOT CLAIM: it does not cleanly separate civility from
# topic (that confound is structural), and it is reported in a
# different framework (mixed models) than the main ANOVAs. The
# mixed model is used HERE specifically because it tolerates the
# incompleteness that leave-one-out induces.
#
# CAVEAT (same as order-block check): because topic is confounded
# with the condition cell, each drop slightly unbalances which
# civility levels contribute. Interpreted as a stability check,
# not a clean re-randomization.
#
# Input: Input_data_long/Moderation_Data_Long_Format.rds
#        (the Topic column is already present from script 1)
# Run standalone.
# ==================================================

## LOAD LIBRARIES
library(tidyverse)
library(lme4)       # lmer / glmer
library(lmerTest)   # p-values (Satterthwaite) for linear models
library(ggpubr)
library(patchwork)

# Sum-to-zero contrasts: makes the Type III main-effect tests
# contrast-invariant (interpretable as marginal effects) in a model
# that also contains the Civility x Alignment interaction. This matches
# the Type III logic of the main-paper ANOVAs. Set BEFORE fitting.
options(contrasts = c("contr.sum", "contr.poly"))

## LOAD DATA (preserves factor level ordering)
df_long <- readRDS("Input_data_long/Moderation_Data_Long_Format.rds")
df_political <- df_long %>% filter(ContentType == "Political")

# Ensure factor coding is explicit and ordered as in the main analysis
df_political <- df_political %>%
  mutate(
    ParticipantID = factor(ParticipantID),
    Civility  = factor(Civility,  levels = c("Civil", "Borderline", "Uncivil")),
    Alignment = factor(Alignment, levels = c("Aligned", "Opposed")),
    Topic     = factor(Topic)
  )

cat("=== LEAVE-ONE-OUT (MIXED MODELS) DATA SUMMARY ===\n")
cat("Political rows:", nrow(df_political), "\n")
cat("Unique participants:", n_distinct(df_political$ParticipantID), "\n")
cat("Topics present:", paste(levels(df_political$Topic), collapse = ", "), "\n\n")
df_political %>% count(Topic) %>% print()
cat("\n")


# ==================================================
# HELPER: extract fixed-effect ANOVA-style tests
# ==================================================
# For lmer: anova() gives Type III F-tests (Satterthwaite via lmerTest).
# For glmer: car::Anova(type=3) gives Wald chi-square tests.
# We standardise both into a tidy row per effect.

extract_lmer <- function(model, dropped, dv_label) {
  at <- anova(model)   # lmerTest -> F, Df, p
  tibble(
    DV       = dv_label,
    Dropped  = dropped,
    Effect   = rownames(at),
    Statistic = at$`F value`,
    StatType  = "F",
    p         = at$`Pr(>F)`
  )
}

extract_glmer <- function(model, dropped, dv_label) {
  # Wald chi-square Type III tests
  at <- car::Anova(model, type = 3)
  tibble(
    DV       = dv_label,
    Dropped  = dropped,
    Effect   = rownames(at),
    Statistic = at$Chisq,
    StatType  = "Chisq",
    p         = at$`Pr(>Chisq)`
  ) %>%
    filter(Effect != "(Intercept)")
}

# Convergence reporter (prints cleanly for each fit)
report_conv <- function(model, tag) {
  msgs <- model@optinfo$conv$lme4$messages
  if (is.null(msgs)) {
    cat("   [", tag, "] converged cleanly\n", sep = "")
  } else {
    cat("   [", tag, "] WARNING: ", paste(msgs, collapse = " | "), "\n", sep = "")
  }
}


# ==================================================
# FULL-SAMPLE MODELS (baseline: should reproduce the ANOVA findings)
# ==================================================

cat("\n===== FULL-SAMPLE MIXED MODELS =====\n")

# Enforcement severity: linear mixed model
cat("\n-- Enforcement Severity (linear mixed model) --\n")
m_es_full <- lmer(EnforcementSeverity ~ Civility * Alignment + (1 | ParticipantID),
                  data = df_political, REML = TRUE)
report_conv(m_es_full, "ES full")
print(anova(m_es_full))

# Violation recognition: logistic mixed model
cat("\n-- Violation Recognition (logistic mixed model) --\n")
m_vr_full <- glmer(ViolationRecognition ~ Civility * Alignment + (1 | ParticipantID),
                   data = df_political, family = binomial,
                   control = glmerControl(optimizer = "bobyqa",
                                          optCtrl = list(maxfun = 2e5)))
report_conv(m_vr_full, "VR full")
print(car::Anova(m_vr_full, type = 3))


# ==================================================
# LEAVE-ONE-OUT: six drops, written out explicitly (no loop)
# ==================================================

# --- Datasets ---
df_drop_T1 <- df_political %>% filter(Topic != "T1") %>% droplevels()
df_drop_T2 <- df_political %>% filter(Topic != "T2") %>% droplevels()
df_drop_T3 <- df_political %>% filter(Topic != "T3") %>% droplevels()
df_drop_T4 <- df_political %>% filter(Topic != "T4") %>% droplevels()
df_drop_T5 <- df_political %>% filter(Topic != "T5") %>% droplevels()
df_drop_T6 <- df_political %>% filter(Topic != "T6") %>% droplevels()


## ----- ENFORCEMENT SEVERITY: linear mixed model, per drop -----

cat("\n\n===== LEAVE-ONE-OUT: ENFORCEMENT SEVERITY (linear) =====\n")

cat("\nDrop T1 (Mamdani; Pooh)\n")
m_es_T1 <- lmer(EnforcementSeverity ~ Civility * Alignment + (1 | ParticipantID),
                data = df_drop_T1, REML = TRUE); report_conv(m_es_T1, "ES drop T1"); print(anova(m_es_T1))

cat("\nDrop T2 (Election sec.; Simpsons)\n")
m_es_T2 <- lmer(EnforcementSeverity ~ Civility * Alignment + (1 | ParticipantID),
                data = df_drop_T2, REML = TRUE); report_conv(m_es_T2, "ES drop T2"); print(anova(m_es_T2))

cat("\nDrop T3 (Gov. shutdown; Farquaad)\n")
m_es_T3 <- lmer(EnforcementSeverity ~ Civility * Alignment + (1 | ParticipantID),
                data = df_drop_T3, REML = TRUE); report_conv(m_es_T3, "ES drop T3"); print(anova(m_es_T3))

cat("\nDrop T4 (Economy; Simpsons)\n")
m_es_T4 <- lmer(EnforcementSeverity ~ Civility * Alignment + (1 | ParticipantID),
                data = df_drop_T4, REML = TRUE); report_conv(m_es_T4, "ES drop T4"); print(anova(m_es_T4))

cat("\nDrop T5 (Laptop/Epstein; Spongebob)\n")
m_es_T5 <- lmer(EnforcementSeverity ~ Civility * Alignment + (1 | ParticipantID),
                data = df_drop_T5, REML = TRUE); report_conv(m_es_T5, "ES drop T5"); print(anova(m_es_T5))

cat("\nDrop T6 (Green energy; Spongebob)\n")
m_es_T6 <- lmer(EnforcementSeverity ~ Civility * Alignment + (1 | ParticipantID),
                data = df_drop_T6, REML = TRUE); report_conv(m_es_T6, "ES drop T6"); print(anova(m_es_T6))


## ----- VIOLATION RECOGNITION: logistic mixed model, per drop -----

cat("\n\n===== LEAVE-ONE-OUT: VIOLATION RECOGNITION (logistic) =====\n")
gc_ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

cat("\nDrop T1 (Mamdani; Pooh)\n")
m_vr_T1 <- glmer(ViolationRecognition ~ Civility * Alignment + (1 | ParticipantID),
                 data = df_drop_T1, family = binomial, control = gc_ctrl)
report_conv(m_vr_T1, "VR drop T1"); print(car::Anova(m_vr_T1, type = 3))

cat("\nDrop T2 (Election sec.; Simpsons)\n")
m_vr_T2 <- glmer(ViolationRecognition ~ Civility * Alignment + (1 | ParticipantID),
                 data = df_drop_T2, family = binomial, control = gc_ctrl)
report_conv(m_vr_T2, "VR drop T2"); print(car::Anova(m_vr_T2, type = 3))

cat("\nDrop T3 (Gov. shutdown; Farquaad)\n")
m_vr_T3 <- glmer(ViolationRecognition ~ Civility * Alignment + (1 | ParticipantID),
                 data = df_drop_T3, family = binomial, control = gc_ctrl)
report_conv(m_vr_T3, "VR drop T3"); print(car::Anova(m_vr_T3, type = 3))

cat("\nDrop T4 (Economy; Simpsons)\n")
m_vr_T4 <- glmer(ViolationRecognition ~ Civility * Alignment + (1 | ParticipantID),
                 data = df_drop_T4, family = binomial, control = gc_ctrl)
report_conv(m_vr_T4, "VR drop T4"); print(car::Anova(m_vr_T4, type = 3))

cat("\nDrop T5 (Laptop/Epstein; Spongebob)\n")
m_vr_T5 <- glmer(ViolationRecognition ~ Civility * Alignment + (1 | ParticipantID),
                 data = df_drop_T5, family = binomial, control = gc_ctrl)
report_conv(m_vr_T5, "VR drop T5"); print(car::Anova(m_vr_T5, type = 3))

cat("\nDrop T6 (Green energy; Spongebob)\n")
m_vr_T6 <- glmer(ViolationRecognition ~ Civility * Alignment + (1 | ParticipantID),
                 data = df_drop_T6, family = binomial, control = gc_ctrl)
report_conv(m_vr_T6, "VR drop T6"); print(car::Anova(m_vr_T6, type = 3))


# ==================================================
# SUMMARY TABLES: alignment & civility effects across drops
# ==================================================

# Order rows by EFFECT first (Civility -> Alignment -> interaction),
# then by topic dropped. Makes "does this effect hold across drops?"
# read down a single block instead of through repeating triplets.
effect_order <- c("Civility", "Alignment", "Civility:Alignment")

summary_es <- bind_rows(
  extract_lmer(m_es_T1, "Drop T1", "EnforcementSeverity"),
  extract_lmer(m_es_T2, "Drop T2", "EnforcementSeverity"),
  extract_lmer(m_es_T3, "Drop T3", "EnforcementSeverity"),
  extract_lmer(m_es_T4, "Drop T4", "EnforcementSeverity"),
  extract_lmer(m_es_T5, "Drop T5", "EnforcementSeverity"),
  extract_lmer(m_es_T6, "Drop T6", "EnforcementSeverity")
) %>%
  arrange(factor(Effect, levels = effect_order),
          factor(Dropped, levels = paste0("Drop T", 1:6)))

summary_vr <- bind_rows(
  extract_glmer(m_vr_T1, "Drop T1", "ViolationRecognition"),
  extract_glmer(m_vr_T2, "Drop T2", "ViolationRecognition"),
  extract_glmer(m_vr_T3, "Drop T3", "ViolationRecognition"),
  extract_glmer(m_vr_T4, "Drop T4", "ViolationRecognition"),
  extract_glmer(m_vr_T5, "Drop T5", "ViolationRecognition"),
  extract_glmer(m_vr_T6, "Drop T6", "ViolationRecognition")
) %>%
  arrange(factor(Effect, levels = effect_order),
          factor(Dropped, levels = paste0("Drop T", 1:6)))

# Focused views: the Alignment row is the key one for the topic-confound concern
align_es <- summary_es %>% filter(Effect == "Alignment")
align_vr <- summary_vr %>% filter(Effect == "Alignment")

cat("\n\n--- SUMMARY: Alignment effect across drops (Enforcement Severity, linear) ---\n")
print(align_es)
cat("\n--- SUMMARY: Alignment effect across drops (Violation Recognition, logistic) ---\n")
print(align_vr)

cat("\n--- FULL fixed-effect tables across drops (ES) ---\n"); print(summary_es, n = 30)
cat("\n--- FULL fixed-effect tables across drops (VR) ---\n"); print(summary_vr, n = 30)


# ==================================================
# SAVE RESULTS TO TXT
# ==================================================

sink("txt_output_full_results/Robustness_LeaveOneOut_Results.txt")

cat("ROBUSTNESS CHECK: LEAVE-ONE-TOPIC-OUT (MIXED MODELS)\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Political sample:", n_distinct(df_political$ParticipantID), "participants\n\n")

cat("Enforcement severity: linear mixed model (lmer), scale treated as numeric.\n")
cat("Violation recognition: logistic mixed model (glmer, binomial).\n")
cat("Both: Civility * Alignment fixed effects, random intercept per participant.\n\n")

cat("===== FULL-SAMPLE MODELS =====\n")
cat("\n-- Enforcement Severity (linear) --\n"); print(anova(m_es_full))
cat("\n-- Violation Recognition (logistic) --\n"); print(car::Anova(m_vr_full, type = 3))

cat("\n\n===== ENFORCEMENT SEVERITY: leave-one-out (linear) =====\n")
cat("\n-- Drop T1 --\n"); print(anova(m_es_T1))
cat("\n-- Drop T2 --\n"); print(anova(m_es_T2))
cat("\n-- Drop T3 --\n"); print(anova(m_es_T3))
cat("\n-- Drop T4 --\n"); print(anova(m_es_T4))
cat("\n-- Drop T5 --\n"); print(anova(m_es_T5))
cat("\n-- Drop T6 --\n"); print(anova(m_es_T6))

cat("\n\n===== VIOLATION RECOGNITION: leave-one-out (logistic) =====\n")
cat("\n-- Drop T1 --\n"); print(car::Anova(m_vr_T1, type = 3))
cat("\n-- Drop T2 --\n"); print(car::Anova(m_vr_T2, type = 3))
cat("\n-- Drop T3 --\n"); print(car::Anova(m_vr_T3, type = 3))
cat("\n-- Drop T4 --\n"); print(car::Anova(m_vr_T4, type = 3))
cat("\n-- Drop T5 --\n"); print(car::Anova(m_vr_T5, type = 3))
cat("\n-- Drop T6 --\n"); print(car::Anova(m_vr_T6, type = 3))

cat("\n\n===== SUMMARY: Alignment effect across drops =====\n")
cat("\n-- Enforcement Severity --\n"); print(align_es)
cat("\n-- Violation Recognition --\n"); print(align_vr)

sink()

cat("\nResults saved to: Robustness_LeaveOneOut_Results.txt\n")


# ==================================================
# EFFECT-SIZE VIEW: alignment gap (opposed - aligned) with 95% CI
# ==================================================
# The F / chi-square statistics above carry significance, not magnitude.
# For an interpretable "how big, and does it survive" view we extract the
# model-estimated alignment gap (opposed minus aligned) per drop, with 95% CI:
#   - Enforcement severity (linear): gap in scale points (0-4), averaged
#     over civility via estimated marginal means.
#   - Violation recognition (logistic): gap in PROBABILITY, back-transformed
#     from the model and averaged over civility (regrid = "response").
# Because the gap comes from the same model whose p-values we report, the
# chart and the significance table tell one consistent story. Averaging the
# probability gap over civility is an approximation (the logistic gap varies
# slightly by baseline rate) - noted in a methods footnote.

library(emmeans)

# --- Helper: opposed - aligned gap + 95% CI from a fitted model ---
# mode = "linear"  -> lmer, gap in raw scale points
# mode = "logit"   -> glmer, gap in probability (response scale)
# NB emmeans names CI columns lower.CL/upper.CL (linear) OR
#    asymp.LCL/asymp.UCL (some glmm cases); grab robustly.
grab_ci <- function(cd) {
  lo <- if ("lower.CL" %in% names(cd)) cd$lower.CL else cd$asymp.LCL
  hi <- if ("upper.CL" %in% names(cd)) cd$upper.CL else cd$asymp.UCL
  list(lo = lo, hi = hi)
}

gap_from_model <- function(model, dropped, dv_label, mode) {
  if (mode == "linear") {
    emm <- emmeans(model, ~ Alignment)                    # scale-point means
    ct  <- contrast(emm, method = "revpairwise")          # Opposed - Aligned
    cd  <- as.data.frame(confint(ct))
    ci  <- grab_ci(cd)
    tibble(DV = dv_label, Dropped = dropped,
           Gap = cd$estimate, CI_low = ci$lo, CI_high = ci$hi,
           Scale = "Scale points (0-4)")
  } else {
    emm <- emmeans(model, ~ Alignment, regrid = "response")# probabilities
    ct  <- contrast(emm, method = "revpairwise")          # Opposed - Aligned (prob)
    cd  <- as.data.frame(confint(ct))
    ci  <- grab_ci(cd)
    tibble(DV = dv_label, Dropped = dropped,
           Gap = cd$estimate, CI_low = ci$lo, CI_high = ci$hi,
           Scale = "Probability")
  }
}

# --- Enforcement severity gaps (scale points) ---
gap_es <- bind_rows(
  gap_from_model(m_es_T1, "Drop T1", "Enforcement Severity", "linear"),
  gap_from_model(m_es_T2, "Drop T2", "Enforcement Severity", "linear"),
  gap_from_model(m_es_T3, "Drop T3", "Enforcement Severity", "linear"),
  gap_from_model(m_es_T4, "Drop T4", "Enforcement Severity", "linear"),
  gap_from_model(m_es_T5, "Drop T5", "Enforcement Severity", "linear"),
  gap_from_model(m_es_T6, "Drop T6", "Enforcement Severity", "linear")
)

# --- Violation recognition gaps (probability) ---
gap_vr <- bind_rows(
  gap_from_model(m_vr_T1, "Drop T1", "Violation Recognition", "logit"),
  gap_from_model(m_vr_T2, "Drop T2", "Violation Recognition", "logit"),
  gap_from_model(m_vr_T3, "Drop T3", "Violation Recognition", "logit"),
  gap_from_model(m_vr_T4, "Drop T4", "Violation Recognition", "logit"),
  gap_from_model(m_vr_T5, "Drop T5", "Violation Recognition", "logit"),
  gap_from_model(m_vr_T6, "Drop T6", "Violation Recognition", "logit")
)

cat("\n--- Alignment gap (opposed - aligned), Enforcement Severity [scale points] ---\n")
print(gap_es)
cat("\n--- Alignment gap (opposed - aligned), Violation Recognition [probability] ---\n")
print(gap_vr)

# --- House-style chart: point + 95% CI per drop, reference line at 0 ---
# Both panels use navy (political-content color from the main figures);
# the gap is a single concept (opposed - aligned), so one color is clearer
# than reusing the red/green that denote opposed/aligned elsewhere.
# Y-axes fixed (ES 0-0.5, VR 0-0.2) so CIs are never clipped.
NAVY <- "#2C3E66"  # political-content navy from main figures

theme_gap <- theme_pubr() +
  theme(
    plot.title  = element_text(face = "bold", size = 12),
    axis.title  = element_text(face = "bold", size = 10),
    legend.position = "none"
  )

make_gap_panel <- function(df, ylab, ymax) {
  ggplot(df, aes(x = factor(Dropped, levels = paste0("Drop T", 1:6)), y = Gap)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.15,
                  color = NAVY, linewidth = 0.7) +
    geom_point(size = 3, color = NAVY) +
    scale_y_continuous(limits = c(0, ymax)) +
    labs(x = "Topic removed", y = ylab) +
    theme_gap
}

p_gap_es <- make_gap_panel(gap_es, "Opposed − Aligned\n(severity, 0–4)", 0.5) +
  ggtitle("Enforcement Severity")

p_gap_vr <- make_gap_panel(gap_vr, "Opposed − Aligned\n(violation prob.)", 0.2) +
  ggtitle("Violation Recognition")

# Combined two-panel version (retained)
p_gap <- p_gap_es | p_gap_vr
ggsave("Graph_output_results/Robustness_LeaveOneOut_Gap.png", p_gap,
       width = 11, height = 4.5, dpi = 300, bg = "white")

# Separate single-panel versions
ggsave("Graph_output_results/Robustness_LeaveOneOut_Gap_ES.png", p_gap_es,
       width = 6, height = 4.5, dpi = 300, bg = "white")
ggsave("Graph_output_results/Robustness_LeaveOneOut_Gap_VR.png", p_gap_vr,
       width = 6, height = 4.5, dpi = 300, bg = "white")

cat("\nGap charts saved: combined (_Gap.png) + separate (_Gap_ES.png, _Gap_VR.png)\n")


# ==================================================
# EXPORT CSV (feeds script 7)
# ==================================================

write_csv(summary_es, "csv_output_results/Robustness_LeaveOneOut_ES.csv")  # ES fixed effects, each drop
write_csv(summary_vr, "csv_output_results/Robustness_LeaveOneOut_VR.csv")  # VR fixed effects, each drop
write_csv(gap_es, "csv_output_results/Robustness_LeaveOneOut_Gap_ES.csv")  # ES alignment gap (scale pts) + CI, each drop
write_csv(gap_vr, "csv_output_results/Robustness_LeaveOneOut_Gap_VR.csv")  # VR alignment gap (probability) + CI, each drop

cat("CSV summaries exported.\n")