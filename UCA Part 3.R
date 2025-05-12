#Uterine Contractions and Anatomy Analysis - Part 3 - Full dataset
#Written by Sarah Darnell, began 5.12.25, lasted edited 5.12.25

library(readr)
library(tableone)
library(Hmisc)
library(dplyr)
library(flextable)
library(officer)


setwd("C:/Users/Eli S/Documents/Sarah work stuff/2025 Data Projects/Uterine Contractions and Anatomy")

#load in eh16 dataset
eh16_full <- read_csv("EH16-263_full_cleaned.csv")

#load in eh19 dataset
eh19_full <- read_csv("EH19-040_full_cleaned.csv")

#merge into one
uca <- merge(eh16_full, eh19_full, all = TRUE)

#adding variable for study identifier
uca <- uca %>%
  mutate(study = if_else(record_number > 3000, "EH19", "EH16"))

uca <- uca %>%
  mutate(Race = case_when(
    white_race == 1 ~ "White",
    black_race == 1 ~ "Black", 
    asian_race == 1 ~ "Asian", 
    other_race == 1 ~ "Other", 
    missing_race == 1 ~ "Missing", 
    multi_race == 1 ~ "Multiple Race"
  ))

#Adding new column for ethnicity as a categorical variable
uca <- uca %>%
  mutate(Ethnicity = case_when(
    mh4_ethnicity == 1 ~ "Hispanic or Latino/a/x", 
    mh4_ethnicity == 2 ~ "Not Hispanic or Latino/a/x", 
    mh4_ethnicity == 3 ~ "Missing"
  ))

#Adding new column for education as a categorical variable
uca <- uca %>%
  mutate(Education = case_when(
    mh5_education == 1 ~ "Grade School", 
    mh5_education == 2 ~ "Completed High School",
    mh5_education == 3 ~ "Some College", 
    mh5_education == 4 ~ "Associate's Degree", 
    mh5_education == 5 ~ "Bachelor's Degree", 
    mh5_education == 6 ~ "Postgraduate Degree"
  ))

#Adding new column for employment as a categorical var
uca <- uca %>%
  mutate(Employment = case_when(
    mh6_employment___7 == 1 ~ "Unemployed",
    mh6_employment___1 == 1 ~ "Work Full-time", 
    mh6_employment___2 == 1 ~ "Work Part-time", 
    mh6_employment___3 == 1 ~ "Homemaker", 
    mh6_employment___4 == 1 ~ "Retired", 
    mh6_employment___5 == 1 ~ "Disabled", 
    mh6_employment___6 == 1 ~ "Student"
  ))

#Adding new column for cigarette usage as a cat var
uca <- uca %>%
  mutate('Do you smoke cigarettes?' = case_when(
    mh9a_cigs_yn == 1 ~ "Yes", 
    mh9a_cigs_yn == 0 ~ "No", 
    mh9a_cigs_yn == 2 ~ "Not currently, but I used to"
  ))

#Adding new column for alcohol consumption as a cat var
uca <- uca %>%
  mutate('Do you drink alcohol?' = case_when(
    mh9b_alcohol_yn == 1 ~ "Yes", 
    mh9b_alcohol_yn == 0 ~ "No", 
    mh9b_alcohol_yn == 2 ~ "Not currently, but I used to"
  ))

#Adding new column for regular menstrual cycles as a cat var
uca <- uca %>%
  mutate('Menstrual Cycle Regularity' = case_when(
    mh24 == 2 ~ "Always regular (22-34 days)", 
    mh24 == 1 ~ "Sometimes irregular", 
    mh24 == 0 ~ "Usually or always irregular"
  ))

#Adding new column for current bc usage as a cat var
uca <- uca %>%
  mutate('Current usage of birth control pills' = case_when(
    mh16_bcps___1 == 1 ~ "Yes", 
    mh16_bcps___1 == 0 ~ "No"
  ))

#Adding new column for past bc usage as a cat var
uca <- uca %>%
  mutate('Past usage of birth control pills' = case_when(
    mh17_bcps___1 == 1 ~ "Yes", 
    mh17_bcps___1 == 0 ~ "No"
  ))

#Adding new columns for werf variables as cat
uca <- uca %>%
  mutate('Bleeding amount on heaviest day of menstrual period' = case_when(
    werf_a2_10 == 1 ~ "Spotting", 
    werf_a2_10 == 2 ~ "Light", 
    werf_a2_10 == 3 ~ "Moderate", 
    werf_a2_10 == 4 ~ "Heavy"
  ))

uca <- uca %>%
  mutate('Bleeding amount on average during menstrual period' = case_when(
    werf_a2_11 == 1 ~ "Spotting",
    werf_a2_11 == 2 ~ "Light", 
    werf_a2_11 == 3 ~ "Moderate", 
    werf_a2_11 == 4 ~ "Heavy"
  ))

uca <- uca %>%
  mutate('Diagnosed with endometriosis' = case_when(
    werf_d8 == 1 ~ "Yes", 
    werf_d8 == 0 ~ "No"
  ))

#renaming variables for ease of reading in the table
uca <- uca %>%
  rename(Age = "mh2_age") %>%
  rename('Average menstrual pain (last 90 days without pain relievers)' = 
           "mh23") %>%
  rename('Average menstrual pain (last 90 days with use of NSAIDs)' = 
           "mh23a") %>%
  rename('Days of missed work/school/activities due to menstrual pain (last 90 days)' = 
           "mh21") %>%
  rename('Average length of menstrual cycle' = "mh25") %>%
  rename('Average length of menstrual period' = "mh27") %>%
  rename('Number of pregnancies' = "mh13_ofpregs") %>%
  rename('Number of deliveries' = "mh14_deliveries") %>%
  rename('Number of vaginal births' = "mh15_vagbirths") %>%
  rename(BMI = "bmi") %>%
  rename('Painful Bladder Syndrome or Interstitial Cystitis' = "have_you_ever_been_diagnos___1") %>%
  rename('Chronic Pelvic Pain' = "have_you_ever_been_diagnos___2") %>%
  rename(Fibroids = "have_you_ever_been_diagnos___3") %>%
  rename(Endometriosis = "have_you_ever_been_diagnos___4") %>%
  rename('Ovarian Cysts' = "have_you_ever_been_diagnos___5") %>%
  rename('Pelvic Inflammatory Disease' = "have_you_ever_been_diagnos___6") %>%
  rename(Dysmenorrhea = "have_you_ever_been_diagnos___7") %>%
  rename('Kidney Stones' = "have_you_ever_been_diagnos___8") %>%
  rename('Inflammatory Bowel Disease' = "have_you_ever_been_diagnos___9") %>%
  rename('Irritable Bowel Disease' = "have_you_ever_been_diagnos___10") %>%
  rename('Chronic Constipation' = "have_you_ever_been_diagnos___11") %>%
  rename('Chronic Diarrhea' = "have_you_ever_been_diagnos___12") %>%
  rename('Migraine Headaches' = "have_you_ever_been_diagnos___13") %>%
  rename(Hypertension = "have_you_ever_been_diagnos___14") %>%
  rename(Arthritis = "have_you_ever_been_diagnos___15") %>%
  rename('Lower Back Pain' = "have_you_ever_been_diagnos___16") %>%
  rename(Cancer = "have_you_ever_been_diagnos___17") %>%
  rename(Diabetes = "have_you_ever_been_diagnos___18") %>%
  rename(Fibromyalgia = "have_you_ever_been_diagnos___19") %>%
  rename(None = "have_you_ever_been_diagnos___0") %>%
  rename(Depression = "eddep_total") %>%
  rename(Anxiety = "total_anxiety_score") %>%
  rename('Number of Body Pain Sites (0-19)' = "bodymap") %>%
  rename('Sleep Disturbance' = "sleep_disturbance_total") %>%
  rename(Fatigue = "total_fatigue_score") %>%
  rename('Visit 1: Baseline pain between cramps' = "menses_q1_bs_cramp_MRI1") %>%
  rename('Visit 1: Baseline maximum cramping pain' = "menses_q1_max_cramp_MRI1") %>%
  rename('Visit 1: Pain between cramps scan 1' = "menses_q2_baseline_MRI1") %>%
  rename('Visit 1: Maximum cramping pain scan 1' = "menses_q2_maximum_MRI1") %>%
  rename('Visit 1: Pain between cramps 90-minutes post drug' = "menses_q4_5_baseline_MRI1") %>%
  rename('Visit 1: Maximum cramping pain 90-minutes post drug' = "menses_q4_5_max_MRI1") %>%
  rename('Visit 1: Pain between cramps pre scan 2' = "menses_q5_bs_cramp_MRI1") %>%
  rename('Visit 1: Maximum cramping pain pre scan 2' = "menses_q5_max_cramp_MRI1") %>%
  rename('Visit 1: Pain between cramps scan 2' = "menses_q6_baseline_MRI1") %>%
  rename('Visit 1: Maximum cramping pain scan 2' = "menses_q6_maximum_MRI1") %>%
  rename('Visit 2: Baseline pain between cramps' = "menses_q1_bs_cramp_MRI2") %>%
  rename('Visit 2: Baseline maximum cramping pain' = "menses_q1_max_cramp_MRI2") %>%
  rename('Visit 2: Pain between cramps scan 1' = "menses_q2_baseline_MRI2") %>%
  rename('Visit 2: Maximum cramping pain scan 1' = "menses_q2_maximum_MRI2") %>%
  rename('Visit 2: Pain between cramps 90-minutes post drug' = "menses_q4_5_baseline_MRI2") %>%
  rename('Visit 2: Maximum cramping pain 90-minutes post drug' = "menses_q4_5_max_MRI2") %>%
  rename('Visit 2: Pain between cramps pre scan 2' = "menses_q5_bs_cramp_MRI2") %>%
  rename('Visit 2: Maximum cramping pain pre scan 2' = "menses_q5_max_cramp_MRI2") %>%
  rename('Visit 2: Pain between cramps scan 2' = "menses_q6_baseline_MRI2") %>%
  rename('Visit 2: Maximum cramping pain scan 2' = "menses_q6_maximum_MRI2") %>%
  rename('Menses Visit: Baseline average cramping pain' = "avg_pain_arrival_m") %>%
  rename('Menses Visit: Baseline maximum cramping pain' = "max_pain_arrival_m") %>%
  rename('Menses Visit: Average cramping pain post scan' = "avg_pain_scan_m") %>%
  rename('Menses Visit: Maximum cramping pain post scan' = "max_pain_scan_m") %>%
  rename('Non-menses Visit: Baseline average cramping pain' = "avg_pain_arrival_nm") %>%
  rename('Non-menses Visit: Baseline maximum cramping pain' = "max_pain_arrival_nm") %>%
  rename('Non-menses Visit: Average cramping pain post scan' = "avg_pain_scan_nm") %>%
  rename('Non-menses Visit: Maximum cramping pain post scan' = "max_pain_scan_nm") %>%
  rename('Days of menstrual pain in an average month' = "mh20") %>%
  rename('Sharp menstrual pain severity' = "mh23b_sharp") %>%
  rename('Pressing menstrual pain severity' = "mh23c_pressing") %>%
  rename('Dull menstrual pain severity' = "mh23d_dull") %>%
  rename('Prickling menstrual pain severity' = "mh23e_prickling") %>%
  rename('McGill pain score menses visit' = "mcgill") %>%
  rename('McGill pain score visit 1' = "mcgill_MRI1") %>%
  rename('McGill pain score visit 2' = "mcgill_MRI2") %>%
  rename('Worst menstrual pain last 12 months' = "werf_c10")

#saving file
write_csv(uca, "C:/Users/Eli S/Documents/Sarah work stuff/2025 Data Projects/Uterine Contractions and Anatomy/uca_final.csv")

#Creating demographics table, using tableone()
vars <- c("Race", "Age", "Ethnicity", "Education", "Employment", 
          "Do you smoke cigarettes?", "Do you drink alcohol?", 
          "Average menstrual pain (last 90 days without pain relievers)", 
          "Average menstrual pain (last 90 days with use of NSAIDs)", 
          "Days of missed work/school/activities due to menstrual pain (last 90 days)", 
          "Average length of menstrual cycle", "Average length of menstrual period", 
          "Menstrual Cycle Regularity", "Current usage of birth control pills", 
          "Past usage of birth control pills", "Number of pregnancies", 
          "Number of deliveries", "Number of vaginal births", "BMI", 
          "Painful Bladder Syndrome or Interstitial Cystitis", "Chronic Pelvic Pain", 
          "Fibroids", "Endometriosis", "Ovarian Cysts", "Pelvic Inflammatory Disease", 
          "Dysmenorrhea", "Kidney Stones", "Inflammatory Bowel Disease", 
          "Irritable Bowel Disease", "Chronic Constipation", "Chronic Diarrhea", 
          "Migraine Headaches", "Hypertension", "Arthritis", "Lower Back Pain", 
          "Cancer", "Diabetes", "Fibromyalgia", "Visit 1: Baseline pain between cramps", 
          "Visit 1: Baseline maximum cramping pain", "Visit 1: Pain between cramps scan 1", 
          "Visit 1: Maximum cramping pain scan 1", "Visit 1: Pain between cramps 90-minutes post drug",
          "Visit 1: Maximum cramping pain 90-minutes post drug", "Visit 1: Pain between cramps pre scan 2", 
          "Visit 1: Maximum cramping pain pre scan 2", "Visit 1: Pain between cramps scan 2", 
          "Visit 1: Maximum cramping pain scan 2", "Visit 2: Baseline pain between cramps", 
          "Visit 2: Baseline maximum cramping pain", "Visit 2: Pain between cramps scan 1", 
          "Visit 2: Maximum cramping pain scan 1", "Visit 2: Pain between cramps 90-minutes post drug",
          "Visit 2: Maximum cramping pain 90-minutes post drug", "Visit 2: Pain between cramps pre scan 2", 
          "Visit 2: Maximum cramping pain pre scan 2", "Visit 2: Pain between cramps scan 2", 
          "Visit 2: Maximum cramping pain scan 2", "Menses Visit: Baseline average cramping pain", 
          "Menses Visit: Baseline maximum cramping pain", "Menses Visit: Average cramping pain post scan", 
          "Menses Visit: Maximum cramping pain post scan", "Non-menses Visit: Baseline average cramping pain", 
          "Non-menses Visit: Baseline maximum cramping pain", "Non-menses Visit: Average cramping pain post scan",
          "Non-menses Visit: Maximum cramping pain post scan", "Days of menstrual pain in an average month",
          "Sharp menstrual pain severity", "Pressing menstrual pain severity", 
          "Dull menstrual pain severity", "Prickling menstrual pain severity",
          "McGill pain score menses visit", "McGill pain score visit 1", 
          "McGill pain score visit 2", "Worst menstrual pain last 12 months")

factors <- c("Race", "Ethnicity", "Education", "Unemployment", 
             "Do you smoke cigarettes?", "Do you drink alcohol?", 
             "Current usage of birth control pills", "Past usage of birth control pills", 
             "Bleeding amount on heaviest day of menstrual period", 
             "Bleeding amount on average during menstrual period", "Diagnosed with endometriosis", 
             "Painful Bladder Syndrome or Interstitial Cystitis", "Chronic Pelvic Pain", 
             "Fibroids", "Endometriosis", "Ovarian Cysts", "Pelvic Inflammatory Disease", 
             "Dysmenorrhea", "Kidney Stones", "Inflammatory Bowel Disease", 
             "Irritable Bowel Disease", "Chronic Constipation", "Chronic Diarrhea", 
             "Migraine Headaches", "Hypertension", "Arthritis", "Lower Back Pain", 
             "Cancer", "Diabetes", "Fibromyalgia")

demo <- CreateTableOne(vars, data = uca, factorVars = factors, strata = "study") 
#change strata argument to "study" or "group" based on what is needed

print(demo, nonnormal = c("Age", "BMI", 
                          "Average menstrual pain (last 90 days without pain relievers)", 
                          "Average menstrual pain (last 90 days with use of NSAIDs)"), showAllLevels = TRUE)
 
#to save file for exporting
demo_df <- as.data.frame(print(demo, 
                               nonnormal = c("Age", "BMI", 
                                             "Average menstrual pain (last 90 days without pain relievers)", 
                                             "Average menstrual pain (last 90 days with use of NSAIDs)"),
                               printToggle = FALSE,
                               quote = FALSE,
                               noSpaces = TRUE,
                               showAllLevels = TRUE))

#chat gpt wrote this part
# Remove p-value/test columns
cols_to_remove <- c("p", "test")
demo_df <- demo_df[, !colnames(demo_df) %in% cols_to_remove]

#Step 1: save rownames as a column
demo_df <- data.frame(rowname = rownames(demo_df), demo_df, row.names = NULL)

# Step 2: Create an empty output data frame
restructured_df <- data.frame()

# Step 3: Loop through rows and insert variable name before its levels
current_var <- NA

for (i in seq_len(nrow(demo_df))) {
  row_label <- demo_df$rowname[i]
  if (!startsWith(row_label, "  ")) {
    # Continuous variable row — use as is
    current_var <- row_label
    new_row <- demo_df[i, ]
    new_row$Variable <- current_var
    restructured_df <- bind_rows(restructured_df, new_row)
  } else {
    # Categorical level — insert variable name row if not already added
    if (!identical(tail(restructured_df$Variable, 1), current_var)) {
      var_row <- demo_df[i, ]
      var_row[2:ncol(var_row)] <- ""  # clear values
      var_row$Variable <- current_var
      restructured_df <- bind_rows(restructured_df, var_row)
    }
    level_row <- demo_df[i, ]
    level_row$Variable <- ""
    restructured_df <- bind_rows(restructured_df, level_row)
  }
}

# Step 4: Drop original rowname and reorder
restructured_df <- restructured_df[, c("Variable", setdiff(names(restructured_df), c("rowname", "Variable")))]

#building flextable
ft <- flextable(demo_df) %>%
  bold(i = which(demo_df$Variable != ""), j = 1) %>%            # Bold variable rows
  align(align = "left", part = "all") %>%                        # Align left
  fontsize(size = 9, part = "all") %>%                           # Reduce font size
  set_table_properties(layout = "fixed", width = 1) %>%          # Fixed width layout
  width(j = 1, width = 2.25) %>%                                 # Widen first column for variable names
  width(j = 2:ncol(demo_df), width = 1.25) %>%                   # Narrow group columns
  theme_vanilla()

read_docx() %>%
  body_add_flextable(ft) %>%
  print(target = "demo_table_study.docx")




