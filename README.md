# Motivated Moderation: Reddit-Style Community Moderation Study

Analysis files for **"Motivated Moderation: Political Content and Partisan Opposition Result in Stricter Civility Enforcement in Community Moderation Settings"** (Rehan A. Mirza, Annenberg School for Communication, University of Pennsylvania).

The study is a 3 × 2 within-subjects survey experiment that simulates a community moderator queue. U.S. participants (N = 254; 139 Democrats, 115 Republicans, recruited via Forthright) applied a set of explicit community rules to social media memes that varied in **partisan alignment** (aligned vs. opposed to the participant's party) and **civility** (civil, borderline uncivil, uncivil), then chose an enforcement action for each post.

This repository contains the prepared data, the R analysis pipeline, every table and figure it produces, and the LaTeX source for the paper.

## Key findings

1. **Civility drives moderation.** Less civil content is more often judged as rule-breaking and receives harsher enforcement, for both political and non-political posts (H1a/H1b supported).
2. **Partisan opposition increases stringency.** Opposed content is flagged more often and enforced more severely than aligned content (H2a/H2b supported).
3. **The alignment effect is uniform.** It does not vary across civility levels (H3a/H3b not supported).
4. **Political content is judged more harshly** than matched non-political content at the civil and borderline levels; the gap disappears for clearly uncivil content (exploratory).

## Outcome measures

Each post was rated on a five-point action scale: *take no action*, *issue a warning*, *delete the post*, *delete + temporary ban*, *delete + permanent ban*. Two dependent variables are derived from it:

| Variable | Coding | Meaning |
|---|---|---|
| `ViolationRecognition` | 0 = no action, 1 = any action | Did the participant judge the post as breaking the rules? |
| `EnforcementSeverity` | 0 (no action) to 4 (permanent ban) | How stringent was the chosen response? |

## Repository structure

```
.
├── 1. Longform_data prep_script.R              # wide → long data prep
├── 2. Study_Analysis.R                         # main hypotheses (H1–H3)
├── 3. Political_vs_NonPolitical_Comparison.R   # political vs. non-political baseline
├── 4. Study_Analysis_Dem and Rep.R             # exploratory partisan subgroups
├── 5. Study_Analysis_Robustness_Order.R        # order-block robustness check
├── 6. Study_Analysis_Robustness_leave-one-out.R# leave-one-topic-out robustness check
├── 7_Write_study_outputs_to_tex.R              # writes reported statistics to LaTeX macros
│
├── Input_data_wide/        # prepared survey export (one row per participant) + topic map
├── Input_data_long/        # analysis-ready long data (one row per participant × post)
├── csv_output_results/     # ANOVA, simple-effects and mixed-model result tables
├── csv_descriptive_results/# condition means, SDs and 95% CIs
├── txt_output_full_results/# full console output of each analysis (sink logs)
├── Graph_output_results/   # figures used in the paper
├── Study_Exhibits/         # study-design exhibits (procedure, rules, example screens)
├── Deprecated_scripts_and_outputs/  # superseded scripts, kept for reference only
│
├── main.tex                          # paper source
├── study_outputs.tex                 # auto-generated statistics macros (do not edit)
├── Community Moderation Experiment.bib  # bibliography used by main.tex
├── references.bib                    # older bibliography (not referenced by main.tex)
└── POLISCI_6150_Final Project.Rproj  # RStudio project
```

## Data

### `Input_data_wide/`

- **`Prepped data_Wide_Format.csv`**: 254 rows, one per participant. Holds the order block, party, both outcomes for every post (non-political `NP 1–3`, political `AL-`/`OP-` × civility, spam fillers `S1`/`SS`) and demographics (age, gender, ethnicity, employment, education). Respondent IDs are anonymised panel identifiers.
- **`Topic_Mapping.csv`**: maps each order block × party × alignment × civility cell to its meme topic (T1–T6) and label, e.g. `Mamdani; Pooh` or `Green energy; Spongebob`.

### `Input_data_long/`

Created by script 1. The full dataset has 2,794 rows, which is 254 participants × 11 posts.

| File | Contents |
|---|---|
| `Moderation_Data_Long_Format.rds` / `.csv` | All posts: 1,524 political, 762 baseline, 508 spam filler |
| `Moderation_Data_Political_Only.csv` | Six political conditions only |
| `Moderation_Data_Baseline.csv` | Three non-political baseline posts only |

The analysis scripts read the `.rds` file. Its columns are:

| Column | Description |
|---|---|
| `ParticipantID` | Anonymised respondent ID |
| `Order` | Counterbalancing block (`Order1`–`Order6`) |
| `PoliticalLeaning` | `Democrat (Left-aligned)` or `Republican (Right-aligned)` |
| `Condition` | Cell label, e.g. `AL-Borderline`, `OP-Uncivil`, `NP 1`, `S1` |
| `Alignment` | `Aligned` / `Opposed` (NA for non-political and filler posts) |
| `Civility` | `Civil` / `Borderline` / `Uncivil` (`ClearSpam` / `SubtleSpam` for fillers) |
| `ViolationRecognition` | 0/1 |
| `EnforcementSeverity` | 0–4 |
| `ContentType` | `Political` / `Baseline` / `Filler` |
| `Age`, `Gender`, `Ethnicity`, `Employment`, `Education` | Demographics |
| `Topic`, `TopicLabel` | Meme topic (political posts only) |

## Analysis pipeline

Run the scripts in numbered order from the project root. They use relative paths, so opening `POLISCI_6150_Final Project.Rproj` in RStudio sets the working directory correctly.

| # | Script | What it does | Main outputs |
|---|---|---|---|
| 1 | `1. Longform_data prep_script.R` | Pivots the wide survey export to long format, labels conditions, and joins topic labels | `Input_data_long/*` |
| 2 | `2. Study_Analysis.R` | Runs one-way RM-ANOVAs on the non-political baseline and 3 × 2 RM-ANOVAs (civility × alignment) on the political posts, each with Mauchly's test and Greenhouse-Geisser correction | `ANOVA_GG_Base*/Pol*`, `Desc_Base*/Pol*`, `Moderation_Results_*.png`, `ANOVA_Results_Complete.txt` |
| 3 | `3. Political_vs_NonPolitical_Comparison.R` | Runs civility × content-type RM-ANOVAs with simple effects, plus a three-way comparison (non-political vs. aligned vs. opposed) | `ANOVA_GG_Compare*`, `SimpleEffects_Compare*`, `Political_vs_NonPolitical_*.png`, `Political_Aligned_Opposed_vs_NonPolitical_*` |
| 4 | `4. Study_Analysis_Dem and Rep.R` | Runs the 3 × 2 ANOVAs separately for Democrats and Republicans (exploratory) | `ANOVA_GG_Dem*/Rep*`, `Democrats_*`/`Republicans_*` descriptives, `Partisan_*.png` |
| 5 | `5. Study_Analysis_Robustness_Order.R` | Runs mixed ANOVAs with order block as a between-subjects factor | `ANOVA_GG_Order*`, `Robustness_Order_*` |
| 6 | `6. Study_Analysis_Robustness_leave-one-out.R` | Refits the models six times, dropping one topic each time. Uses mixed models with a participant random intercept: `lmer` for severity, logistic `glmer` for recognition | `Robustness_LeaveOneOut_*` |
| 7 | `7_Write_study_outputs_to_tex.R` | Reads the CSVs from steps 2–6 and writes formatted statistics as LaTeX macros | `study_outputs.tex` |

Script 7 needs no R objects in memory. It only needs the CSVs that scripts 2–6 produce.

### Requirements

The analysis was run in **R 4.5.2**. To install the packages it uses:

```r
install.packages(c(
  "tidyverse", "rstatix", "effectsize", "ggpubr", "patchwork",
  "lme4", "lmerTest", "emmeans"
))
```

## Building the paper

`main.tex` loads `study_outputs.tex` to get every reported statistic (for example `\PolVRAlignment` or `\BaseESCivil`). If you re-run the pipeline, the paper's numbers update automatically. The paper uses `biblatex` with the `biber` backend:

```bash
pdflatex main.tex
biber main
pdflatex main.tex
pdflatex main.tex
```

Figures are read from `Graph_output_results/` and `Study_Exhibits/`.

## Design notes

- **Counterbalancing.** Participants were randomly assigned to one of six order blocks that rotate the six political conditions across positions and topics (Table 1 in the paper; `Study_Exhibits/Tab_1_Counterbalancing_Scheme.jpg`). Each block shows 11 posts: 6 political, 3 non-political, and 2 spam fillers. The non-political and filler posts appear in the same positions in every block.
- **Fillers.** Spam posts were included to reduce demand effects and are not analysed.
- **Preregistration.** The study was not preregistered. The political vs. non-political comparison and the partisan subgroup analyses are treated as exploratory.

## Deprecated material

`Deprecated_scripts_and_outputs/` holds earlier versions of the analysis, a manipulation-check extract, and the script for the enlarged Stanford TSRC poster figure. `Graph_output_results/Political_vs_NonPolitical_Combined_poster.png` came from that poster script. None of these files are part of the current pipeline.
