#Uterine Contractions and Anatomy Analysis - Part 1 - EH16-263
#Written by Sarah Darnell, began 5.2.25, lasted edited 7.7.25

library(readr)
library(dplyr)
library(tidyr)

setwd("C:/Users/Eli S/Documents/Sarah work stuff/2025 Data Projects/Uterine Contractions and Anatomy")

#import HSP variables from redcap, remove redcap event names
eh16 <- read_csv("EH16-263_redcap.csv", col_types = cols(redcap_event_name = col_skip()))

#copy bmi values across rows, rename 
eh16_new <- eh16 %>% 
  group_by(record_number) %>% 
  mutate(q4_bmi = first(na.omit(q4_bmi))) %>% 
  rename(bmi = 2) %>%
  ungroup()

#transform from long to wide format
eh16_wide <- eh16_new %>% 
  group_by(record_number) %>%
  filter(n()>1) %>% #removes row if only a single instance
  slice_tail(n=1) %>% #keeps last row if 2 instances
  ungroup()

#add new variable, named bodymap, that is the sum of all cmsi pain variables
eh16_wide <- eh16_wide %>%
  group_by(record_number) %>%
  mutate(bodymap = sum(cmsi_fibro1___1, cmsi_fibro1___2, cmsi_fibro1___3, 
                       cmsi_fibro1___4, cmsi_fibro1___5, cmsi_fibro1___6,
                       cmsi_fibro1___7, cmsi_fibro1___8, cmsi_fibro1___9,
                       cmsi_fibro1___10, cmsi_fibro1___11, cmsi_fibro1___12,
                       cmsi_fibro1___13, cmsi_fibro1___14, cmsi_fibro1___15,
                       cmsi_fibro1___16, cmsi_fibro1___17, cmsi_fibro1___18,
                       cmsi_fibro1___19)) %>%
  ungroup()

#add new variable, named mcgill, that is the sum of all mcgill pain variables
eh16_wide <- eh16_wide %>%
  group_by(record_number) %>%
  mutate(mcgill = sum(mcgill1a, mcgill1b, mcgill1c, 
                      mcgill1d, mcgill1e, mcgill1f,
                      mcgill1g, mcgill1h, mcgill1i,
                      mcgill1j, mcgill1k, mcgill1l,
                      mcgill1m, mcgill1n, mcgill1o,
                      mcgill1p, mcgill1q, mcgill1r,
                      mcgill1s, mcgill1t, mcgill1u, 
                      mcgill1v)) %>%
  ungroup()

#import menses visit information from tracking log
eh16_menses <- read_csv("EH16_263_MRI_pain_fixed.csv")

#merge menses variables with rest of dataset
eh16_all <- merge(eh16_wide, eh16_menses, all = TRUE)

##recoding race categories##

#adding variable for multi race
eh16_all <- eh16_all %>%
  mutate(multi_race = ifelse(rowSums(
    select(., mh3_race___1:mh3_race___5)) > 1, 1, 0))

#adding variable for other race, excluding those with multiple races
eh16_all <- eh16_all %>%
  mutate(other_race = ifelse((mh3_race___1 == "1") |
                               (mh3_race___3 == "1") &
                               (multi_race != 1), 1, 0))

#adding new variable for white race, excluding those with multiple races
eh16_all <- eh16_all %>%
  mutate(white_race = ifelse((mh3_race___5 == "1") &
                               (multi_race != 1), 1, 0))

#adding new variable for asian race, excluding those with multiple races
eh16_all <- eh16_all %>%
  mutate(asian_race = ifelse((mh3_race___2 == "1") &
                               (multi_race != 1), 1, 0))


#adding new variable for black race, excluding those with multiple races
eh16_all <- eh16_all %>%
  mutate(black_race = ifelse((mh3_race___4 == "1") &
                               (multi_race != 1), 1, 0))

#adding new variable for missing race, excluding those with multiple races
eh16_all <- eh16_all %>%
  mutate(missing_race = ifelse((mh3_race___1 != "1") & (mh3_race___2 != "1") &
                                 (mh3_race___3 != "1") & (mh3_race___4 != "1") &
                                 (mh3_race___5 != "1") & (multi_race != 1), 1, 0))

#adding 2000 to the record_number for merge later
eh16_all <- eh16_all %>%
  mutate(record_number = record_number + 2000)

#removing test records (2001 & 2002)
eh16_all <- eh16_all %>%
  filter(record_number > 2002)

#converting back to a tibble
eh16_clean <- as_tibble(eh16_all)

#saving file
write_csv(eh16_clean, "C:/Users/Eli S/Documents/Sarah work stuff/2025 Data Projects/Uterine Contractions and Anatomy/EH16-263_cleaned.csv")

#load in contraction and anatomy data
eh16_anatomy <- read_csv("eh16_anatomy_FIX.csv") #7.7.25 using _FIX file per KMH

#compute average of double-scored variables
eh16_anatomy <- eh16_anatomy %>%
  group_by(id_visit) %>%
  mutate(avg_contractions = mean(number_contractions_nsaids)) %>%
  mutate(avg_frame_duration = mean(mean_frame_duration)) %>%
  mutate(avg_anterior_jz = mean(anterior_jz)) %>%
  mutate(avg_anterior_outer = mean(anterior_outer)) %>%
  mutate(avg_posterior_jz = mean(posterior_jz)) %>%
  mutate(avg_posterior_outer = mean(posterior_outer)) %>%
  ungroup()

#remove second instance of each scan, grab only averaged variables for merge
eh16_anatomy <- eh16_anatomy %>%
  group_by(id_visit) %>%
  slice_head() %>%
  select(3, 6, 8:11, 18:24)

#isolate menses visit variables
eh16_anatomy_m <- eh16_anatomy %>%
  group_by(record_number) %>%
  filter(menses == 1)

#rename menses visit variables
eh16_anatomy_m <- eh16_anatomy_m %>%
  rename(avg_contractions_m = 8) %>%
  rename(avg_frame_duration_m = 9) %>%
  rename(avg_anterior_jz_m = 10) %>%
  rename(avg_anterior_outer_m = 11) %>%
  rename(avg_posterior_jz_m = 12) %>%
  rename(avg_posterior_outer_m = 13)

#isolate nonmenses visit variables
eh16_anatomy_nm <- eh16_anatomy %>%
  group_by(record_number) %>%
  filter(menses == 0)

#rename visit 2 variables
eh16_anatomy_nm <- eh16_anatomy_nm %>%
  rename(avg_contractions_nm = 8) %>%
  rename(avg_frame_duration_nm = 9) %>%
  rename(avg_anterior_jz_nm = 10) %>%
  rename(avg_anterior_outer_nm = 11) %>%
  rename(avg_posterior_jz_nm = 12) %>%
  rename(avg_posterior_outer_nm = 13)

#keep only contraction variables from nonmenses visits
eh16_anatomy_nm <- eh16_anatomy_nm %>%
  select(8:13)

#merge menses and nonmemses variables, wide form
eh16_anatomy_clean <- merge(eh16_anatomy_m, eh16_anatomy_nm, all = TRUE)

#remove visit_id and menses variables to limit confusion
eh16_anatomy_clean <- eh16_anatomy_clean %>%
  select(!2:3)

#converting back to a tibble
eh16_anatomy_clean <- as_tibble(eh16_anatomy_clean)

#saving file
write_csv(eh16_anatomy_clean, "C:/Users/Eli S/Documents/Sarah work stuff/2025 Data Projects/Uterine Contractions and Anatomy/EH16-263_anatomy_cleaned.csv")           

#merge eh16 redcap data with anatomy data
eh16_full <- merge(eh16_clean, eh16_anatomy_clean, all = TRUE)

#converting back to a tibble
eh16_full <- as_tibble(eh16_full)

#saving file
write_csv(eh16_full, "C:/Users/Eli S/Documents/Sarah work stuff/2025 Data Projects/Uterine Contractions and Anatomy/EH16-263_full_cleaned.csv")           
