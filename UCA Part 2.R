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

#isolate MR1 mcgill variables
eh19_MRI1 <- eh19_MRI %>%
  group_by(ps_record_id) %>%
  filter(n()>1) %>%
  slice_tail %>%
  select(92:113) %>%
  ungroup()

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
  select(1, 24) %>%
  ungroup()