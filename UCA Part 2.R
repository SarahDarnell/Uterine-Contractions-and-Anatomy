#Uterine Contractions and Anatomy Analysis - Part 2 - EH19-040
#Written by Sarah Darnell, began 5.2.25, lasted edited 5.2.25

library(readr)
library(dplyr)
library(tidyr)

setwd("C:/Users/Eli S/Documents/Sarah work stuff/2025 Data Projects/Uterine Contractions and Anatomy")

#import HSP variables from redcap, remove redcap event names
eh19 <- read_csv("EH19-040_redcap.csv", 
                 col_types = cols(redcap_event_name = col_skip(), 
                                  redcap_repeat_instrument = col_skip(), 
                                  redcap_repeat_instance = col_skip()))

#copy bmi and promis values across rows, rename bmi
eh19_new <- eh19 %>% 
  group_by(ps_record_id) %>% 
  mutate(ps_bmi = first(na.omit(ps_bmi)),
         total_fatigue_score = first(na.omit(total_fatigue_score)),
         sleep_disturbance_total = first(na.omit(sleep_disturbance_total)),
         eddep_total = first(na.omit(eddep_total)),
         total_anxiety_score = first(na.omit(total_anxiety_score))) %>% 
  rename(bmi = 2) %>%
  ungroup()

#removing subjects who didn't come in for an MRI
eh19_MRI <- eh19_new %>% 
  group_by(ps_record_id) %>%
  filter(n()>2) %>% #removes rows if less than 3 instances
  ungroup()

#copy cmsi variables across rows
eh19_MRI <- eh19_MRI %>%
  group_by(ps_record_id) %>% 
  mutate(cmsi_fibro1___99 = first(na.omit(cmsi_fibro1___99)),
         cmsi_fibro1___1 = first(na.omit(cmsi_fibro1___1)),
         cmsi_fibro1___2 = first(na.omit(cmsi_fibro1___2)),
         cmsi_fibro1___3 = first(na.omit(cmsi_fibro1___3)),
         cmsi_fibro1___4 = first(na.omit(cmsi_fibro1___4)),
         cmsi_fibro1___5 = first(na.omit(cmsi_fibro1___5)),
         cmsi_fibro1___6 = first(na.omit(cmsi_fibro1___6)),
         cmsi_fibro1___7 = first(na.omit(cmsi_fibro1___7)),
         cmsi_fibro1___8 = first(na.omit(cmsi_fibro1___8)),
         cmsi_fibro1___9 = first(na.omit(cmsi_fibro1___9)),
         cmsi_fibro1___10 = first(na.omit(cmsi_fibro1___10)),
         cmsi_fibro1___11 = first(na.omit(cmsi_fibro1___11)),
         cmsi_fibro1___12 = first(na.omit(cmsi_fibro1___12)),
         cmsi_fibro1___13 = first(na.omit(cmsi_fibro1___13)),
         cmsi_fibro1___14 = first(na.omit(cmsi_fibro1___14)),
         cmsi_fibro1___15 = first(na.omit(cmsi_fibro1___15)),
         cmsi_fibro1___16 = first(na.omit(cmsi_fibro1___16)),
         cmsi_fibro1___17 = first(na.omit(cmsi_fibro1___17)),
         cmsi_fibro1___18 = first(na.omit(cmsi_fibro1___18)),
         cmsi_fibro1___19 = first(na.omit(cmsi_fibro1___19))) %>%
  ungroup()


#isolate MRI 2 variables
eh19_MRI2 <- eh19_MRI %>%
  group_by(ps_record_id) %>%
  filter(n()>3) %>%
  slice_tail %>%
  select(53:56, 81:86, 92:113) %>%
  ungroup()

#renaming MRI 2 variables
eh19_MRI2 <- eh19_MRI2 %>% 
  rename(menses_q1_bs_cramp_MRI2 = 2) %>%
  rename(menses_q1_max_cramp_MRI2 = 3) %>%
  rename(menses_q4_5_baseline_MRI2 = 4) %>%
  rename(menses_q4_5_max_MRI2 = 5) %>%
  rename(menses_q2_baseline_MRI2 = 6) %>%
  rename(menses_q2_maximum_MRI2 = 7) %>%
  rename(menses_q5_bs_cramp_MRI2 = 8) %>%
  rename(menses_q5_max_cramp_MRI2 = 9) %>%
  rename(menses_q6_baseline_MRI2 = 10) %>%
  rename(menses_q6_maximum_MRI2 = 11)


#add new variable, named mcgill_MRI2, that is the sum of all mcgill pain variables
eh19_MRI2 <- eh19_MRI2 %>%
  group_by(ps_record_id) %>%
  mutate(mcgill_MRI2 = sum(mcgill1a, mcgill1b, mcgill1c, 
                      mcgill1d, mcgill1e, mcgill1f,
                      mcgill1g, mcgill1h, mcgill1i,
                      mcgill1j, mcgill1k, mcgill1l,
                      mcgill1m, mcgill1n, mcgill1o,
                      mcgill1p, mcgill1q, mcgill1r,
                      mcgill1s, mcgill1t, mcgill1u, 
                      mcgill1v)) %>%
  ungroup()

#remove individual mcgill variables from MRI2 dataset
eh19_MRI2 <- eh19_MRI2 %>%
  group_by(ps_record_id) %>%
  select(1:11, 34) %>%
  ungroup()


#removing phone screen and MRI2 variables from larger dataset
eh19_MRI <- eh19_MRI %>%
  group_by(ps_record_id) %>% 
  filter(n() < 4 | row_number() < 4) %>% #checks if 4 lines, removes 4th
  filter(row_number() > 1) %>% #removes first line of all groups
  ungroup()

#isolate MR1 variables
eh19_MRI1 <- eh19_MRI %>%
  group_by(ps_record_id) %>%
  filter(n()>1) %>%
  slice_tail %>%
  select(53:56, 81:86,92:113) %>%
  ungroup()

#renaming MRI 1 variables
eh19_MRI1 <- eh19_MRI1 %>% 
  rename(menses_q1_bs_cramp_MRI1 = 2) %>%
  rename(menses_q1_max_cramp_MRI1 = 3) %>%
  rename(menses_q4_5_baseline_MRI1 = 4) %>%
  rename(menses_q4_5_max_MRI1 = 5) %>%
  rename(menses_q2_baseline_MRI1 = 6) %>%
  rename(menses_q2_maximum_MRI1 = 7) %>%
  rename(menses_q5_bs_cramp_MRI1 = 8) %>%
  rename(menses_q5_max_cramp_MRI1 = 9) %>%
  rename(menses_q6_baseline_MRI1 = 10) %>%
  rename(menses_q6_maximum_MRI1 = 11)

#add new variable, named mcgill_MRI1, that is the sum of all mcgill pain variables
eh19_MRI1 <- eh19_MRI1 %>%
  group_by(ps_record_id) %>%
  mutate(mcgill_MRI1 = sum(mcgill1a, mcgill1b, mcgill1c, 
                           mcgill1d, mcgill1e, mcgill1f,
                           mcgill1g, mcgill1h, mcgill1i,
                           mcgill1j, mcgill1k, mcgill1l,
                           mcgill1m, mcgill1n, mcgill1o,
                           mcgill1p, mcgill1q, mcgill1r,
                           mcgill1s, mcgill1t, mcgill1u, 
                           mcgill1v)) %>%
  ungroup()

#remove individual mcgill variables from MRI1 dataset
eh19_MRI1 <- eh19_MRI1 %>%
  group_by(ps_record_id) %>%
  select(1:11, 34) %>%
  ungroup()


#add new variable, named mcgill_screen, that is the sum of all mcgill pain variables
eh19_MRI <- eh19_MRI %>%
  group_by(ps_record_id) %>%
  mutate(mcgill_screen = sum(mcgill1a, mcgill1b, mcgill1c, 
                           mcgill1d, mcgill1e, mcgill1f,
                           mcgill1g, mcgill1h, mcgill1i,
                           mcgill1j, mcgill1k, mcgill1l,
                           mcgill1m, mcgill1n, mcgill1o,
                           mcgill1p, mcgill1q, mcgill1r,
                           mcgill1s, mcgill1t, mcgill1u, 
                           mcgill1v)) %>%
  ungroup()

#remove MRI1 variables from screen dataset
eh19_screen <- eh19_MRI %>% 
  group_by(ps_record_id) %>% 
  select(1:52,57:80, 87:91, 114:118) %>%
  slice_head %>%
  ungroup()


#Join MRI1 variables to screen dataset, in wide form
eh19_wide <- merge(eh19_screen, eh19_MRI1, all = TRUE)

#Join MR2 variables to rest of merged dataset, in wide form
eh19_wide <- merge(eh19_wide, eh19_MRI2, all = TRUE)

#add new variable, named bodymap, that is the sum of all cmsi pain variables
eh19_wide <- eh19_wide %>%
  group_by(ps_record_id) %>%
  mutate(bodymap = sum(cmsi_fibro1___1, cmsi_fibro1___2, cmsi_fibro1___3, 
                       cmsi_fibro1___4, cmsi_fibro1___5, cmsi_fibro1___6,
                       cmsi_fibro1___7, cmsi_fibro1___8, cmsi_fibro1___9,
                       cmsi_fibro1___10, cmsi_fibro1___11, cmsi_fibro1___12,
                       cmsi_fibro1___13, cmsi_fibro1___14, cmsi_fibro1___15,
                       cmsi_fibro1___16, cmsi_fibro1___17, cmsi_fibro1___18,
                       cmsi_fibro1___19)) %>%
  ungroup()

##recoding race categories##

#adding variable for multi race
eh19_wide <- eh19_wide %>%
  mutate(multi_race = ifelse(rowSums(
    select(., mh3_race___1:mh3_race___5, mh3_race___7)) > 1, 1, 0))

#adding varibale for other race, excluding those with multiple races
eh19_wide <- eh19_wide %>%
  mutate(other_race = ifelse((mh3_race___1 == "1") |
                               (mh3_race___3 == "1") |
                               (mh3_race___7 == "1") &
                               (multi_race != 1), 1, 0))

#adding new variable for white race, excluding those with multiple races
eh19_wide <- eh19_wide %>%
  mutate(white_race = ifelse((mh3_race___5 == "1") &
                               (multi_race != 1), 1, 0))

#adding new variable for asian race, excluding those with multiple races
eh19_wide <- eh19_wide %>%
  mutate(asian_race = ifelse((mh3_race___2 == "1") &
                               (multi_race != 1), 1, 0))

#adding new variable for black race, excluding those with multiple races
eh19_wide <- eh19_wide %>%
  mutate(black_race = ifelse((mh3_race___4 == "1") &
                               (multi_race != 1), 1, 0))

#adding new variable for missing race, excluding those with multiple races
eh19_wide <- eh19_wide %>%
  mutate(missing_race = ifelse((mh3_race___6 == "1") &
                                 (multi_race != 1), 1, 0))

#adding 3000 to the record_number for merge later, renaming variable
eh19_wide <- eh19_wide %>%
  mutate(ps_record_id = ps_record_id + 3000) %>%
  rename(record_number = 1)

#converting back to a tibble
eh19_clean <- as_tibble(eh19_wide)

#saving file
write_csv(eh19_clean, "C:/Users/Eli S/Documents/Sarah work stuff/2025 Data Projects/Uterine Contractions and Anatomy/EH19-040_cleaned.csv")
