# Load packages
library(tidyverse)

# Read dataset
df_wide <- read_csv ("Input data_wide/Prepped data_Wide_Format.csv")
dim(df_wide)
names(df_wide)

## Produce pivoted rows for political conditions
  # Pivot violation recognition

df_viol_political <- df_wide %>%
  select(
    RESPONDENT_ID,
    `Randomization Group`,
    `Political leaning (label)`,
    `Violation Recognition (AL-Civil)`,
    `Violation Recognition (OP-Civil)`,
    `Violation Recognition (AL-Borderline)`,
    `Violation Recognition (OP-Borderline)`,
    `Violation Recognition (AL-Uncivil)`,
    `Violation Recognition (OP-Uncivil)`,
    Age, Gender, Ethnicity, Employment, Education
  ) %>%
  pivot_longer(
    cols = c(
      `Violation Recognition (AL-Civil)`,
      `Violation Recognition (OP-Civil)`,
      `Violation Recognition (AL-Borderline)`,
      `Violation Recognition (OP-Borderline)`,
      `Violation Recognition (AL-Uncivil)`,
      `Violation Recognition (OP-Uncivil)`
    ),
    names_to = "Condition",
    values_to = "ViolationRecognition"
  ) %>%
  mutate(
    Condition = str_remove(Condition, "Violation Recognition \\("),
    Condition = str_remove(Condition, "\\)")
  )
head(df_viol_political)

# Check for duplicates
df_viol_political %>%
  count(RESPONDENT_ID, Condition) %>%
  filter(n > 1)

  # Pivot enforcement severity

df_sev_political <- df_wide %>%
  select(
    RESPONDENT_ID,
    `Severity Scale (AL-Civil)`,
    `Severity Scale (OP-Civil)`,
    `Severity Scale (AL-Borderline)`,
    `Severity Scale (OP-Borderline)`,
    `Severity Scale (AL-Uncivil)`,
    `Severity Scale (OP-Uncivil)`
  ) %>%
  pivot_longer(
    cols = c(
      `Severity Scale (AL-Civil)`,
      `Severity Scale (OP-Civil)`,
      `Severity Scale (AL-Borderline)`,
      `Severity Scale (OP-Borderline)`,
      `Severity Scale (AL-Uncivil)`,
      `Severity Scale (OP-Uncivil)`
    ),
    names_to = "Condition",
    values_to = "EnforcementSeverity"  # ✅ Kept as EnforcementSeverity
  ) %>%
  mutate(
    Condition = str_remove(Condition, "Severity Scale \\("),
    Condition = str_remove(Condition, "\\)")
  )
head(df_sev_political)

#Check for duplicates

df_sev_political %>%
  count(RESPONDENT_ID, Condition) %>%
  filter(n > 1)  # Should be empty now

#Join violations and severity
df_political <- df_viol_political %>%
  left_join(df_sev_political, by = c("RESPONDENT_ID", "Condition"))
nrow(df_viol_political)  
nrow(df_political)      

# Create Civility and Alignment variables
df_political <- df_political %>%
  mutate(
    Alignment = case_when(
      str_starts(Condition, "AL-") ~ "Aligned",
      str_starts(Condition, "OP-") ~ "Opposed"
    ),
    Civility = str_remove(Condition, "^(AL-|OP-)"),
    Civility = factor(Civility, levels = c("Civil", "Borderline", "Uncivil")),
    ContentType = "Political"
  )

# Convert to factors
df_political <- df_political %>%
  rename(
    ParticipantID = RESPONDENT_ID,
    Order = `Randomization Group`,
    PoliticalLeaning = `Political leaning (label)`
  ) %>%
  mutate(
    ParticipantID = factor(ParticipantID),
    Order = factor(Order),
    Condition = factor(Condition),
    Alignment = factor(Alignment, levels = c("Aligned", "Opposed")),
    ContentType = factor(ContentType)
  )
  
# Check results

glimpse(df_political)

# Should show 6 combinations
df_political %>% count(Civility, Alignment)

# Should show 6 distinct conditions
df_political %>% count(Condition)

# Each participant should have 6 rows
df_political %>% 
  count(ParticipantID) %>% 
  summary()

# View first participant's data
df_political %>% 
  filter(ParticipantID == first(ParticipantID)) %>%
  arrange(Condition) %>%
  print(n = 6)


## Produce pivoted rows for non-political baseline

#Pivot violation recognition

df_viol_baseline <- df_wide %>%
  select(
    RESPONDENT_ID,
    `Randomization Group`,
    `Political leaning (label)`,
    `Violation Recognition (MC 1)`,
    `Violation Recognition (MC 2)`,
    `Violation Recognition (MC 3)`,
    Age, Gender, Ethnicity, Employment, Education
  ) %>%
  pivot_longer(
    cols = c(
      `Violation Recognition (MC 1)`,
      `Violation Recognition (MC 2)`,
      `Violation Recognition (MC 3)`
    ),
    names_to = "Condition",
    values_to = "ViolationRecognition"
  ) %>%
  mutate(Condition = str_extract(Condition, "MC \\d"))  # Extract "MC 1", "MC 2", "MC 3"

#Pivot enforcement severity

df_sev_baseline <- df_wide %>%
  select(
    RESPONDENT_ID,
    `Severity Scale (MC 1)`,
    `Severity Scale (MC 2)`,
    `Severity Scale (MC 3)`
  ) %>%
  pivot_longer(
    cols = c(
      `Severity Scale (MC 1)`,
      `Severity Scale (MC 2)`,
      `Severity Scale (MC 3)`
    ),
    names_to = "Condition",
    values_to = "EnforcementSeverity"
  ) %>%
  mutate(Condition = str_extract(Condition, "MC \\d"))

# Joint VR and ES for non-political conditions
df_baseline <- df_viol_baseline %>%
  left_join(df_sev_baseline, by = c("RESPONDENT_ID", "Condition"))

# Create civility variable and rename
df_baseline <- df_baseline %>%
  mutate(
    Civility = case_when(
      Condition == "MC 1" ~ "Civil",
      Condition == "MC 2" ~ "Borderline",
      Condition == "MC 3" ~ "Uncivil"
    ),
    Alignment = NA_character_,  # No alignment for non-political conditions
    ContentType = "Baseline"
  ) %>%
  rename(
    ParticipantID = RESPONDENT_ID,
    Order = `Randomization Group`,
    PoliticalLeaning = `Political leaning (label)`
  ) %>%
  mutate(
    ParticipantID = factor(ParticipantID),
    Order = factor(Order),
    Condition = factor(Condition),
    Civility = factor(Civility, levels = c("Civil", "Borderline", "Uncivil")),
    ContentType = factor(ContentType)
  )

#Check data
df_baseline %>% count(ParticipantID) %>% summary()  # Should be 3 per participant
df_baseline %>% count(Condition, Civility)

## Produce pivoted rows for fillers

#Pivot violation recognition for fillers

df_viol_filler <- df_wide %>%
  select(
    RESPONDENT_ID,
    `Randomization Group`,
    `Political leaning (label)`,
    `Violation Recognition (S1)`,
    `Violation Recognition (SS)`,
    Age, Gender, Ethnicity, Employment, Education
  ) %>%
  pivot_longer(
    cols = c(
      `Violation Recognition (S1)`,
      `Violation Recognition (SS)`
    ),
    names_to = "Condition",
    values_to = "ViolationRecognition"
  ) %>%
  mutate(Condition = str_extract(Condition, "S1|SS"))

# Pivot enforcement severity for fillers
df_sev_filler <- df_wide %>%
  select(
    RESPONDENT_ID,
    `Severity Scale (S1)`,
    `Severity Scale (SS)`
  ) %>%
  pivot_longer(
    cols = c(
      `Severity Scale (S1)`,
      `Severity Scale (SS)`
    ),
    names_to = "Condition",
    values_to = "EnforcementSeverity"
  ) %>%
  mutate(Condition = str_extract(Condition, "S1|SS"))


#Join
df_filler <- df_viol_filler %>%
  left_join(df_sev_filler, by = c("RESPONDENT_ID", "Condition"))

#Create variables and rename

df_filler <- df_filler %>%
  mutate(
    Civility = case_when(
      Condition == "S1" ~ "ClearSpam",     
      Condition == "SS" ~ "SubtleSpam"    
    ),
    Alignment = NA_character_,
    ContentType = "Filler"
  ) %>%
  rename(
    ParticipantID = RESPONDENT_ID,
    Order = `Randomization Group`,
    PoliticalLeaning = `Political leaning (label)`
  ) %>%
  mutate(
    ParticipantID = factor(ParticipantID),
    Order = factor(Order),
    Condition = factor(Condition),
    ContentType = factor(ContentType)
  )

#Check
df_filler %>% count(ParticipantID) %>% summary()
df_filler %>% count(Condition, Civility)

## Combine all datasets

df_long <- bind_rows(df_political, df_baseline, df_filler) %>%
  arrange(ParticipantID, ContentType, Condition) %>%

#Reorder columns
select(
  ParticipantID,
  Order,
  PoliticalLeaning,
  # Experimental conditions
  Condition,
  Alignment,
  Civility,
  # DVs
  ViolationRecognition,
  EnforcementSeverity,
  # Content type
  ContentType,
  # Demographics
  Age,
  Gender,
  Ethnicity,
  Employment,
  Education
)

cat("\n=== FINAL DATA STRUCTURE ===\n")
df_long %>% 
  count(ParticipantID) %>% 
  summary()

cat("\n=== ROWS BY CONTENT TYPE ===\n")
df_long %>% count(ContentType)

cat("\n=== COLUMN NAMES ===\n")
names(df_long)

cat("\n=== SAMPLE PARTICIPANT (all 11 rows) ===\n")
df_long %>% 
  filter(ParticipantID == first(ParticipantID)) %>%
  select(ParticipantID, ContentType, Condition, Civility, Alignment, 
         ViolationRecognition, EnforcementSeverity, Age, Gender) %>%
  print(n = 11)


## Save data

write_csv(df_long, "Input data_long/Moderation_Data_Long_Format.csv")
cat("✅ Saved: Moderation_Data_Long_Format.csv\n")

df_long %>%
  filter(ContentType == "Political") %>%
  write_csv("Input data_long/Moderation_Data_Political_Only.csv")
cat("✅ Saved: Moderation_Data_Political_Only.csv\n")

df_long %>%
  filter(ContentType == "Baseline") %>%
  write_csv("Input data_long/Moderation_Data_Baseline.csv")

cat("✅ Saved: Moderation_Data_Baseline.csv\n")

saveRDS(df_long, "Input data_long/Moderation_Data_Long_Format.rds")
cat("✅ Saved: Moderation_Data_Long_Format.rds\n")