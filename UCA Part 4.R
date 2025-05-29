#Uterine Contractions and Anatomy Analysis - Part 4 - Full dataset - table 2s
#Written by Sarah Darnell, began 5.15.25, lasted edited 5.29.25

library(readr)
library(tableone)
library(Hmisc)
library(dplyr)
library(flextable)
library(officer)
library(tidyr)
library(ggplot2)



setwd("C:/Users/Eli S/Documents/Sarah work stuff/2025 Data Projects/Uterine Contractions and Anatomy")

#load in full dataset dataset
uca <- read_csv("uca_final.csv")

#Convert t1group to a factor 
uca$t1group <- as.factor(uca$t1group)

#Define continuous variables
contvar<- c("avg_contractions_v1", "avg_frame_duration_v1", "avg_anterior_jz_v1", 
            "avg_anterior_outer_v1", "avg_posterior_jz_v1", "avg_posterior_outer_v1",
            "avg_contractions_v2", "avg_frame_duration_v2", "avg_anterior_jz_v2", 
            "avg_anterior_outer_v2", "avg_posterior_jz_v2", "avg_posterior_outer_v2", 
            "avg_contractions_v1_s1", "avg_frame_duration_v1_s1", "avg_anterior_jz_v1_s1", 
            "avg_anterior_outer_v1_s1", "avg_posterior_jz_v1_s1", "avg_posterior_outer_v1_s1", 
            "avg_contractions_v1_s2", "avg_frame_duration_v1_s2", "avg_anterior_jz_v1_s2", 
            "avg_anterior_outer_v1_s2", "avg_posterior_jz_v1_s2", "avg_posterior_outer_v1_s2", 
            "avg_contractions_v2_s1", "avg_frame_duration_v2_s1", "avg_anterior_jz_v2_s1", 
            "avg_anterior_outer_v2_s1", "avg_posterior_jz_v2_s1", "avg_posterior_outer_v2_s1", 
            "avg_contractions_v2_s2", "avg_frame_duration_v2_s2", "avg_anterior_jz_v2_s2", 
            "avg_anterior_outer_v2_s2", "avg_posterior_jz_v2_s2", "avg_posterior_outer_v2_s2")

#Define continuous variables for nonmenses (eh16 only)
contvar_nonmenses <- c("avg_contractions_v1", "avg_frame_duration_v1", "avg_anterior_jz_v1", 
            "avg_anterior_outer_v1", "avg_posterior_jz_v1", "avg_posterior_outer_v1",
            "avg_contractions_v2", "avg_frame_duration_v2", "avg_anterior_jz_v2", 
            "avg_anterior_outer_v2", "avg_posterior_jz_v2", "avg_posterior_outer_v2")


#Note for table 2s: eh16 has 6 nm v1s and 13 nm v2s; in order to filter these out, 
#i'll be pulling them out and than re-merging into menses and nonmenses datasets

#this includes menses v1 and all eh19
uca_menses_v1 <- uca %>% 
  filter(menses_v1 == 1 | is.na(menses_v1)) %>%
  select(!129:135)

#this includes only menses v2 from eh16
uca_menses_v2 <- uca %>%
  filter(menses_v2 == 1) %>%
  select(1, 117, 129:135, 202) %>%
  mutate(record_number = record_number + 2000)

#merge all menses visits together
uca_menses <- merge(uca_menses_v1, uca_menses_v2, by = c("record_number", "t1group"), 
                    all = TRUE)

#this includes nonmenses v1 only for eh16
uca_nonmenses_v1 <- uca %>% 
  filter(menses_v1 == 0) %>%
  select(1, 122:128, 202)

#this includes nonmenses v2 only for eh16
uca_nonmenses_v2 <- uca %>% 
  filter(menses_v2 == 0) %>%
  select(1, 129:135, 202)

#merge all nonmenses visits together (only eh16)
uca_nonmenses <- merge(uca_nonmenses_v1, uca_nonmenses_v2, by = c("record_number", "t1group"), 
                       all = TRUE)

##Table 2b - median results from endo and fibroid groups, menses only##
table2b <- uca_menses %>%
  select(all_of(contvar), t1group) %>%
  filter(t1group %in% c("Endometriosis", "Fibroid")) %>%
  pivot_longer(cols = -t1group, names_to = "Item", values_to = "Value") %>% 
  group_by(t1group, Item) %>%
  dplyr::summarize(`Median [IQR]` = sprintf("%.1f [%.1f-%.1f]", 
                                           median(Value, na.rm = TRUE), 
                                           quantile(Value, 0.25, na.rm = TRUE),
                                           quantile(Value, 0.75, na.rm = TRUE)),
            .groups = "drop") %>%
  pivot_wider(names_from = t1group, values_from = `Median [IQR]`) 

# Create a flextable object
ft <- flextable(table2b) %>%
  bold(i = 1, part = "header") %>%               # Bold the header row
  align(align = "left", part = "all") %>%         # Align left for all parts
  fontsize(size = 10, part = "all") %>%           # Set font size
  set_table_properties(layout = "fixed", width = 1) %>% # Fixed width layout
  theme_vanilla()                                # Apply a vanilla theme

read_docx() %>%
  body_add_flextable(ft) %>%
  print(target = "table2b.docx")

##Tables 2c-e will help determine what will be in final 2a##
#These only include DYs and HC

#table 2c - nonmenses data (eh16 only) for contvars
table2c <- uca_nonmenses %>%
  select(all_of(contvar_nonmenses), t1group) %>%
  filter(t1group %in% c("Dysmenorrhea", "Pain Free Control")) %>%
  pivot_longer(cols = -t1group, names_to = "Item", values_to = "Value") %>% 
  group_by(t1group, Item) %>%
  dplyr::summarize(`Median [IQR]` = sprintf("%.1f [%.1f-%.1f]", 
                                            median(Value, na.rm = TRUE), 
                                            quantile(Value, 0.25, na.rm = TRUE),
                                            quantile(Value, 0.75, na.rm = TRUE)),
                   .groups = "drop") %>%
  pivot_wider(names_from = t1group, values_from = `Median [IQR]`) 

#kruskal wallis test
uca_filtered_groups <- uca_nonmenses %>%
  filter(t1group %in% c("Dysmenorrhea", "Pain Free Control"))

kw <- lapply(contvar_nonmenses, function(var) {
  formula <- as.formula(paste(var, "~t1group"))
  # Check if variable has enough data to run test
  temp <- uca_filtered_groups[, c(var, "t1group")]
  temp <- temp[complete.cases(temp), ]  # remove NAs
  
  if (length(unique(temp$t1group)) < 2 || length(unique(temp[[var]])) < 2) {
    return(NULL)  # skip this variable
  }
  test_result <- kruskal.test(formula, data = temp)
  
  data.frame(
    Variable = var, 
    Chi_Square = test_result$statistic, 
    df = test_result$parameter, 
    p_value = test_result$p.value, 
    stringsAsFactors = FALSE
  )
})

kw_results <- do.call(rbind, kw)

#remove df column, and add column with n for each variable
n_values <- sapply(contvar_nonmenses, function(var) {
  sum(!is.na(uca_filtered_groups[[var]]))
})

n_df <- data.frame(Variable = contvar_nonmenses, n = n_values, stringsAsFactors = FALSE)

kw_results_no_df <- kw_results %>% select(-df)

kw_results_final <- left_join(kw_results_no_df, n_df, by = "Variable")

#combine into one table
names(table2c)[1] <- "Variable"

table2c_full <- left_join(table2c, kw_results_final, by = "Variable")

# Create a flextable object
ft2 <- flextable(table2c_full) %>%
  bold(i = 1, part = "header") %>%               # Bold the header row
  align(align = "left", part = "all") %>%         # Align left for all parts
  fontsize(size = 10, part = "all") %>%           # Set font size
  set_table_properties(layout = "fixed", width = 1) %>% # Fixed width layout
  theme_vanilla()                                # Apply a vanilla theme

read_docx() %>%
  body_add_flextable(ft2) %>%
  print(target = "table2c.docx")

#table 2d - only menses data for contvars
table2d <- uca_menses %>%
  select(all_of(contvar), t1group) %>%
  filter(t1group %in% c("Dysmenorrhea", "Pain Free Control")) %>%
  pivot_longer(cols = -t1group, names_to = "Item", values_to = "Value") %>% 
  group_by(t1group, Item) %>%
  dplyr::summarize(`Median [IQR]` = sprintf("%.1f [%.1f-%.1f]", 
                                            median(Value, na.rm = TRUE), 
                                            quantile(Value, 0.25, na.rm = TRUE),
                                            quantile(Value, 0.75, na.rm = TRUE)),
                   .groups = "drop") %>%
  pivot_wider(names_from = t1group, values_from = `Median [IQR]`) 


#kruskal wallis test
uca_filtered_groups <- uca_menses %>%
  filter(t1group %in% c("Dysmenorrhea", "Pain Free Control"))

kw <- lapply(contvar, function(var) {
  formula <- as.formula(paste(var, "~t1group"))
  # Check if variable has enough data to run test
  temp <- uca_filtered_groups[, c(var, "t1group")]
  temp <- temp[complete.cases(temp), ]  # remove NAs
  
  if (length(unique(temp$t1group)) < 2 || length(unique(temp[[var]])) < 2) {
    return(NULL)  # skip this variable
  }
  test_result <- kruskal.test(formula, data = temp)
  
  data.frame(
    Variable = var, 
    Chi_Square = test_result$statistic, 
    df = test_result$parameter, 
    p_value = test_result$p.value, 
    stringsAsFactors = FALSE
  )
})

kw_results <- do.call(rbind, kw)

#remove df column, and add column with n for each variable
n_values <- sapply(contvar, function(var) {
  sum(!is.na(uca_filtered_groups[[var]]))
})

n_df <- data.frame(Variable = contvar, n = n_values, stringsAsFactors = FALSE)

kw_results_no_df <- kw_results %>% select(-df)

kw_results_final <- left_join(kw_results_no_df, n_df, by = "Variable")

#combine into one table
names(table2d)[1] <- "Variable"

table2d_full <- left_join(table2d, kw_results_final, by = "Variable")

# Create a flextable object
ft2 <- flextable(table2d_full) %>%
  bold(i = 1, part = "header") %>%               # Bold the header row
  align(align = "left", part = "all") %>%         # Align left for all parts
  fontsize(size = 10, part = "all") %>%           # Set font size
  set_table_properties(layout = "fixed", width = 1) %>% # Fixed width layout
  theme_vanilla()                                # Apply a vanilla theme

read_docx() %>%
  body_add_flextable(ft2) %>%
  print(target = "table2d.docx")

#table 2e - only menses data for contvars, with HC not able to have 
#pain over a 1, and dys having to have pain over a 2



##this needs to be workshopped - not sure filtering correctly##
##also need to change all tables to have n per group instead of per variable##

#Remove HC w/pain and DYS w/o pain
uca_filtered_menses_pain <- uca_menses %>%
  filter(
      #EH16, DYS, pain > 2
      (t1group == "Dysmenorrhea" & study == "EH16" &
          `Menses Visit: Maximum cramping pain post scan.x` > 2 |
          `Menses Visit: Maximum cramping pain post scan.y` > 2)|
      #EH19, DYS, pain > 2
      (t1group == "Dysmenorrhea" & study == "EH19" &
         (is.na(`Visit 1: Maximum cramping pain scan 1`) | 
            `Visit 1: Maximum cramping pain scan 1` > 2) & 
         (is.na(`Visit 1: Maximum cramping pain scan 2`) | 
            `Visit 1: Maximum cramping pain scan 2` > 2) &
         (is.na(`Visit 2: Maximum cramping pain scan 1`) | 
            `Visit 2: Maximum cramping pain scan 1` > 2) &
         (is.na(`Visit 2: Maximum cramping pain scan 2`) | 
            `Visit 2: Maximum cramping pain scan 2` > 2)) |
      #EH16, HC, pain = 0
        (t1group == "Dysmenorrhea" & study == "EH16" &
           `Menses Visit: Maximum cramping pain post scan.x` == 0 |
           `Menses Visit: Maximum cramping pain post scan.y` == 0)|
      #EH19, HC, pain = 0
      (t1group == "Pain Free Control" & study == "EH19" &
         (is.na(`Visit 1: Maximum cramping pain scan 1`) | 
            `Visit 1: Maximum cramping pain scan 1` == 0) & 
         (is.na(`Visit 1: Maximum cramping pain scan 2`) | 
            `Visit 1: Maximum cramping pain scan 2` == 0) &
         (is.na(`Visit 2: Maximum cramping pain scan 1`) | 
            `Visit 2: Maximum cramping pain scan 1` == 0) &
         (is.na(`Visit 2: Maximum cramping pain scan 2`) | 
            `Visit 2: Maximum cramping pain scan 2` == 0))
  )
    
table2e <- uca_filtered_menses_pain %>%
  select(all_of(contvar), t1group) %>%
  filter(t1group %in% c("Dysmenorrhea", "Pain Free Control")) %>%
  pivot_longer(cols = -t1group, names_to = "Item", values_to = "Value") %>% 
  group_by(t1group, Item) %>%
  dplyr::summarize(`Median [IQR]` = sprintf("%.1f [%.1f-%.1f]", 
                                            median(Value, na.rm = TRUE), 
                                            quantile(Value, 0.25, na.rm = TRUE),
                                            quantile(Value, 0.75, na.rm = TRUE)),
                   .groups = "drop") %>%
  pivot_wider(names_from = t1group, values_from = `Median [IQR]`) 

#kruskal wallis test
uca_filtered_groups <- uca_filtered_menses_pain %>%
  filter(t1group %in% c("Dysmenorrhea", "Pain Free Control"))

kw <- lapply(contvar, function(var) {
  formula <- as.formula(paste(var, "~t1group"))
  # Check if variable has enough data to run test
  temp <- uca_filtered_groups[, c(var, "t1group")]
  temp <- temp[complete.cases(temp), ]  # remove NAs
  
  if (length(unique(temp$t1group)) < 2 || length(unique(temp[[var]])) < 2) {
    return(NULL)  # skip this variable
  }
  test_result <- kruskal.test(formula, data = temp)
  
  data.frame(
    Variable = var, 
    Chi_Square = test_result$statistic, 
    df = test_result$parameter, 
    p_value = test_result$p.value, 
    stringsAsFactors = FALSE
  )
})

kw_results <- do.call(rbind, kw)

#remove df column, and add column with n for each variable
n_values <- sapply(contvar, function(var) {
  sum(!is.na(uca_filtered_groups[[var]]))
})

n_df <- data.frame(Variable = contvar, n = n_values, stringsAsFactors = FALSE)

kw_results_no_df <- kw_results %>% select(-df)

kw_results_final <- left_join(kw_results_no_df, n_df, by = "Variable")

#combine into one table
names(table2e)[1] <- "Variable"

table2e_full <- left_join(table2e, kw_results_final, by = "Variable")

# Create a flextable object
ft2 <- flextable(table2e_full) %>%
  bold(i = 1, part = "header") %>%               # Bold the header row
  align(align = "left", part = "all") %>%         # Align left for all parts
  fontsize(size = 10, part = "all") %>%           # Set font size
  set_table_properties(layout = "fixed", width = 1) %>% # Fixed width layout
  theme_vanilla()                                # Apply a vanilla theme

read_docx() %>%
  body_add_flextable(ft2) %>%
  print(target = "table2e.docx")


##WAIT TO RUN UNTIL FINAL PARAMETERS DECIDED##
##Table 2a - median results and kruskal wallis results for dys and hc - menses only

table2a <- uca %>%
  select(all_of(contvar), t1group) %>%
  filter(!t1group %in% c("Fibroid", "Endometriosis")) %>%
  group_by(t1group) %>%
  summarize(across(everything(), ~ sprintf("%.1f [%.1f-%.1f]", 
                                           median(., na.rm = TRUE), 
                                           quantile(., 0.25, na.rm = TRUE),
                                           quantile(., 0.75, na.rm = TRUE))),
            .groups = "drop") %>%
  pivot_longer(cols = -t1group, names_to = "Item", values_to = "Median [IQR]") %>% 
  pivot_wider(names_from = t1group, values_from = `Median [IQR]`) 

#kruskal wallis test
uca_filtered_groups <- uca %>%
  filter(t1group %in% c("Dysmenorrhea", "Pain Free Control"))

kw <- lapply(contvar, function(var) {
  formula <- as.formula(paste(var, "~t1group"))
  test_result <- kruskal.test(formula, data = uca_filtered_groups)
  
  data.frame(
    Variable = var, 
    Chi_Square = test_result$statistic, 
    df = test_result$parameter, 
    p_value = test_result$p.value, 
    stringsAsFactors = FALSE
  )
})

kw_results <- do.call(rbind, kw)

#combine into one table
names(table2a)[1] <- "Variable"

table2a_full <- left_join(table2a, kw_results, by = "Variable")

# Create a flextable object
ft2 <- flextable(table2a_full) %>%
  bold(i = 1, part = "header") %>%               # Bold the header row
  align(align = "left", part = "all") %>%         # Align left for all parts
  fontsize(size = 10, part = "all") %>%           # Set font size
  set_table_properties(layout = "fixed", width = 1) %>% # Fixed width layout
  theme_vanilla()                                # Apply a vanilla theme

read_docx() %>%
  body_add_flextable(ft2) %>%
  print(target = "table2a.docx")