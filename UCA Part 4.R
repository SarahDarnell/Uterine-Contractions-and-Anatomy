#Uterine Contractions and Anatomy Analysis - Part 4 - Full dataset - table 2s
#Written by Sarah Darnell, began 5.15.25, lasted edited 6.12.25

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

#Define continuous variables for menses visits (eh16 and eh19)
contvar_m <- c("avg_contractions_m", "avg_frame_duration_m", "avg_anterior_jz_m", 
            "avg_anterior_outer_m", "avg_posterior_jz_m", "avg_posterior_outer_m",
            "avg_contractions_v1_s1", "avg_frame_duration_v1_s1", "avg_anterior_jz_v1_s1", 
            "avg_anterior_outer_v1_s1", "avg_posterior_jz_v1_s1", "avg_posterior_outer_v1_s1", 
            "avg_contractions_v1_s2", "avg_frame_duration_v1_s2", "avg_anterior_jz_v1_s2", 
            "avg_anterior_outer_v1_s2", "avg_posterior_jz_v1_s2", "avg_posterior_outer_v1_s2", 
            "avg_contractions_v2_s1", "avg_frame_duration_v2_s1", "avg_anterior_jz_v2_s1", 
            "avg_anterior_outer_v2_s1", "avg_posterior_jz_v2_s1", "avg_posterior_outer_v2_s1", 
            "avg_contractions_v2_s2", "avg_frame_duration_v2_s2", "avg_anterior_jz_v2_s2", 
            "avg_anterior_outer_v2_s2", "avg_posterior_jz_v2_s2", "avg_posterior_outer_v2_s2")

#Define continuous variables for nonmenses (eh16 only)
contvar_nm <- c("avg_contractions_nm", "avg_frame_duration_nm", "avg_anterior_jz_nm", 
            "avg_anterior_outer_nm", "avg_posterior_jz_nm", "avg_posterior_outer_nm")


##Table 2b - median results from endo and fibroid groups, menses only##
table2b <- uca %>%
  select(all_of(contvar_m), t1group) %>%
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
#These only include DYS and HC

#table 2c - nonmenses data (eh16 only) for contvars
table2c <- uca %>%
  select(all_of(contvar_nm), t1group) %>%
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
uca_filtered_groups <- uca %>%
  filter(t1group %in% c("Dysmenorrhea", "Pain Free Control"))

kw <- lapply(contvar_nm, function(var) {
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
kw_results <- kw_results %>%
  select(-df)


#combine into one table
names(table2c)[1] <- "Variable"

table2c_full <- left_join(table2c, kw_results, by = "Variable")

#add in n per group per variable
n_long <- uca %>%
  select(all_of(contvar_nm), t1group) %>%
  filter(t1group %in% c("Dysmenorrhea", "Pain Free Control")) %>%
  pivot_longer(cols = -t1group, names_to = "Variable", values_to = "Value") %>%
  dplyr::group_by(t1group, Variable) %>%
  dplyr::summarize(n = sum(!is.na(Value)), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = t1group, values_from = n) %>%
  dplyr::rename(
    DYS_n = Dysmenorrhea,
    HC_n = `Pain Free Control`
  )

# merge into table 2c
table2c_full_n <- table2c_full %>%
  left_join(n_long, by = "Variable") %>%
  mutate(
    Dysmenorrhea = paste0(Dysmenorrhea, " (n=", DYS_n, ")"),
    `Pain Free Control` = paste0(`Pain Free Control`, " (n=", HC_n, ")")
  ) %>%
  select(Variable, Dysmenorrhea, `Pain Free Control`, Chi_Square, p_value)


# Create a flextable object
ft2 <- flextable(table2c_full_n) %>%
  bold(i = 1, part = "header") %>%               # Bold the header row
  align(align = "left", part = "all") %>%         # Align left for all parts
  fontsize(size = 10, part = "all") %>%           # Set font size
  set_table_properties(layout = "fixed", width = 1) %>% # Fixed width layout
  theme_vanilla()                                # Apply a vanilla theme

read_docx() %>%
  body_add_flextable(ft2) %>%
  print(target = "table2c.docx")

#table 2d - only menses data for contvars
table2d <- uca %>%
  select(all_of(contvar_m), t1group) %>%
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
uca_filtered_groups <- uca %>%
  filter(t1group %in% c("Dysmenorrhea", "Pain Free Control"))

kw <- lapply(contvar_m, function(var) {
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
kw_results <- kw_results %>%
  select(-df)


#combine into one table
names(table2d)[1] <- "Variable"

table2d_full <- left_join(table2d, kw_results, by = "Variable")

#add in n per group per variable
n_long <- uca %>%
  select(all_of(contvar_m), t1group) %>%
  filter(t1group %in% c("Dysmenorrhea", "Pain Free Control")) %>%
  pivot_longer(cols = -t1group, names_to = "Variable", values_to = "Value") %>%
  dplyr::group_by(t1group, Variable) %>%
  dplyr::summarize(n = sum(!is.na(Value)), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = t1group, values_from = n) %>%
  dplyr::rename(
    DYS_n = Dysmenorrhea,
    HC_n = `Pain Free Control`
  )

# merge into table 2d
table2d_full_n <- table2d_full %>%
  left_join(n_long, by = "Variable") %>%
  mutate(
    Dysmenorrhea = paste0(Dysmenorrhea, " (n=", DYS_n, ")"),
    `Pain Free Control` = paste0(`Pain Free Control`, " (n=", HC_n, ")")
  ) %>%
  select(Variable, Dysmenorrhea, `Pain Free Control`, Chi_Square, p_value)


# Create a flextable object
ft2 <- flextable(table2d_full_n) %>%
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

#Remove HC w/pain and DYS w/o pain
uca_filtered_pain <- uca_filtered_groups %>%
  filter(
      #EH16, DYS, pain > 2
      (t1group == "Dysmenorrhea" & study == "EH16" &
          `Menses Visit: Maximum cramping pain post scan` > 2)|
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
        (t1group == "Pain Free Control" & study == "EH16" &
           `Menses Visit: Maximum cramping pain post scan` == 0)|
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

#uncomment to view rows filtered out
#filter_test <- anti_join(uca_filtered_groups, uca_filtered_pain)
#filter_test_new <- filter_test %>%
#  group_by(record_number) %>%
#  select(1, 117, 146, 150, 157, 161, 187, 200) %>%
#  ungroup()

table2e <- uca_filtered_pain %>%
  select(all_of(contvar_m), t1group) %>%
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
kw <- lapply(contvar_m, function(var) {
  formula <- as.formula(paste(var, "~t1group"))
  # Check if variable has enough data to run test
  temp <- uca_filtered_pain[, c(var, "t1group")]
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
kw_results <- kw_results %>%
  select(-df)


#combine into one table
names(table2e)[1] <- "Variable"

table2e_full <- left_join(table2e, kw_results, by = "Variable")

#add in n per group per variable
n_long <- uca_filtered_pain %>%
  select(all_of(contvar_m), t1group) %>%
  filter(t1group %in% c("Dysmenorrhea", "Pain Free Control")) %>%
  pivot_longer(cols = -t1group, names_to = "Variable", values_to = "Value") %>%
  dplyr::group_by(t1group, Variable) %>%
  dplyr::summarize(n = sum(!is.na(Value)), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = t1group, values_from = n) %>%
  dplyr::rename(
    DYS_n = Dysmenorrhea,
    HC_n = `Pain Free Control`
  )

# merge into table 2e
table2e_full_n <- table2e_full %>%
  left_join(n_long, by = "Variable") %>%
  mutate(
    Dysmenorrhea = paste0(Dysmenorrhea, " (n=", DYS_n, ")"),
    `Pain Free Control` = paste0(`Pain Free Control`, " (n=", HC_n, ")")
  ) %>%
  select(Variable, Dysmenorrhea, `Pain Free Control`, Chi_Square, p_value)


# Create a flextable object
ft2 <- flextable(table2e_full_n) %>%
  bold(i = 1, part = "header") %>%               # Bold the header row
  align(align = "left", part = "all") %>%         # Align left for all parts
  fontsize(size = 10, part = "all") %>%           # Set font size
  set_table_properties(layout = "fixed", width = 1) %>% # Fixed width layout
  theme_vanilla()                                # Apply a vanilla theme

read_docx() %>%
  body_add_flextable(ft2) %>%
  print(target = "table2e.docx")

#table 2f - menses visits only, HC pain must be 0, and DYS pain must be 3 or above
#new variable that takes average of scan 1 pain across eh19 visits 1 and 2, and 
#menses pain from eh16

#make new variable called menses_scan_1_pain, this isn't in table, just for info
uca <- uca %>%
  mutate(menses_scan_1_pain = case_when(
    #eh16, menses visit, dys with pain of 3 or above
    t1group == "Dysmenorrhea" & study == "EH16" & 
      `Menses Visit: Maximum cramping pain post scan` > 2 ~ 
      `Menses Visit: Maximum cramping pain post scan`, 
    #eh16, menses visit, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH16" & 
      `Menses Visit: Maximum cramping pain post scan` == 0 ~ 
      `Menses Visit: Maximum cramping pain post scan`, 
    #eh19, scan 1 of v1 and v2, dys with pain of 3 or above (averaged)
    t1group == "Dysmenorrhea" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` > 2 & 
      `Visit 2: Maximum cramping pain scan 1` > 2 ~ 
      rowMeans(cbind(`Visit 1: Maximum cramping pain scan 1`,
                     `Visit 2: Maximum cramping pain scan 1` )), 
    #eh19, scan 1 of v1 when v2 is NA or too low, dys with pain of 3 or above
    t1group == "Dysmenorrhea" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` > 2 & 
      (is.na(`Visit 2: Maximum cramping pain scan 1`) | 
         `Visit 2: Maximum cramping pain scan 1` < 3) ~ 
      `Visit 1: Maximum cramping pain scan 1`,
    #eh19, scan 1 of v2 when v1 is NA or too low, dys with pain of 3 or above
    t1group == "Dysmenorrhea" & study == "EH19" &
      `Visit 2: Maximum cramping pain scan 1` > 2 & 
      (is.na(`Visit 1: Maximum cramping pain scan 1`) | 
         `Visit 1: Maximum cramping pain scan 1` < 3) ~ 
      `Visit 2: Maximum cramping pain scan 1`, 
    #eh19, scan 1 of v1 and v2, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` == 0 & 
      `Visit 2: Maximum cramping pain scan 1` == 0 ~ 0.0, 
    #eh19, scan 1 of v1 when v2 is NA or too high, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` == 0 & 
      (is.na(`Visit 2: Maximum cramping pain scan 1`) | 
         `Visit 2: Maximum cramping pain scan 1` > 0) ~ 0.0, 
    #eh19, scan 1 of v2 when v1 is NA or too high, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH19" &
      `Visit 2: Maximum cramping pain scan 1` == 0 & 
      (is.na(`Visit 1: Maximum cramping pain scan 1`) | 
         `Visit 1: Maximum cramping pain scan 1` > 0) ~ 0.0
  ))

#uncomment to view rows filtered out, dys and hc only
#anti_2f_vars <- uca %>%
 #group_by(record_number) %>%
  #select(1, 117, 146, 157, 187, 200, 202) %>%
  #ungroup() %>%
  #slice(c(167, 165, 66, 34, 63, 83))

#make new variable for avg contractions for eh16 and eh19, but only for dys w/
#pain of 3 or higher, and HC with pain of 0. For eh19, take avg if pain at both
#visits meets criteria, otherwise take only 1 visit. If neither meet, take none.
uca_m_s1 <- uca %>%
  mutate(avg_contractions_m_s1 = case_when(
    #eh16, menses visit, dys with pain of 3 or above
    t1group == "Dysmenorrhea" & study == "EH16" & 
      `Menses Visit: Maximum cramping pain post scan` > 2 ~ 
      avg_contractions_m, 
    #eh16, menses visit, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH16" & 
      `Menses Visit: Maximum cramping pain post scan` == 0 ~ 
      avg_contractions_m, 
    #eh19, scan 1 of v1 and v2, dys with pain of 3 or above 
    t1group == "Dysmenorrhea" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` > 2 & 
      `Visit 2: Maximum cramping pain scan 1` > 2 ~ 
      rowMeans(cbind(avg_contractions_v1_s1, avg_contractions_v2_s1), na.rm = TRUE), 
    #eh19, scan 1 of v1 when v2 is NA or too low, dys with pain of 3 or above
    t1group == "Dysmenorrhea" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` > 2 & 
      (is.na(`Visit 2: Maximum cramping pain scan 1`) | 
         `Visit 2: Maximum cramping pain scan 1` < 3) ~ avg_contractions_v1_s1,
    #eh19, scan 1 of v2 when v1 is NA or too low, dys with pain of 3 or above
    t1group == "Dysmenorrhea" & study == "EH19" &
      `Visit 2: Maximum cramping pain scan 1` > 2 & 
      (is.na(`Visit 1: Maximum cramping pain scan 1`) | 
         `Visit 1: Maximum cramping pain scan 1` < 3) ~ avg_contractions_v2_s1, 
    #eh19, scan 1 of v1 and v2, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` == 0 & 
      `Visit 2: Maximum cramping pain scan 1` == 0 ~ 
      rowMeans(cbind(avg_contractions_v1_s1, avg_contractions_v2_s1), na.rm = TRUE), 
    #eh19, scan 1 of v1 when v2 is NA or too high, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` == 0 & 
      (is.na(`Visit 2: Maximum cramping pain scan 1`) | 
         `Visit 2: Maximum cramping pain scan 1` > 0) ~ avg_contractions_v1_s1, 
    #eh19, scan 1 of v2 when v1 is NA or too high, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH19" &
      `Visit 2: Maximum cramping pain scan 1` == 0 & 
      (is.na(`Visit 1: Maximum cramping pain scan 1`) | 
         `Visit 1: Maximum cramping pain scan 1` > 0) ~ avg_contractions_v2_s1
  ))

#make new variable for avg contractions for eh16 and eh19, but only for dys w/
#pain of 3 or higher, and HC with pain of 0. For eh19, take avg if pain at both
#visits meets criteria, otherwise take only 1 visit. If neither meet, take none.
uca_m_s1 <- uca %>%
  mutate(avg_contractions_m_s1 = case_when(
    #eh16, menses visit, dys with pain of 3 or above
    t1group == "Dysmenorrhea" & study == "EH16" & 
      `Menses Visit: Maximum cramping pain post scan` > 2 ~ 
      avg_contractions_m, 
    #eh16, menses visit, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH16" & 
      `Menses Visit: Maximum cramping pain post scan` == 0 ~ 
      avg_contractions_m, 
    #eh19, scan 1 of v1 and v2, dys with pain of 3 or above 
    t1group == "Dysmenorrhea" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` > 2 & 
      `Visit 2: Maximum cramping pain scan 1` > 2 ~ 
      rowMeans(cbind(avg_contractions_v1_s1, avg_contractions_v2_s1), na.rm = TRUE), 
    #eh19, scan 1 of v1 when v2 is NA or too low, dys with pain of 3 or above
    t1group == "Dysmenorrhea" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` > 2 & 
      (is.na(`Visit 2: Maximum cramping pain scan 1`) | 
         `Visit 2: Maximum cramping pain scan 1` < 3) ~ avg_contractions_v1_s1,
    #eh19, scan 1 of v2 when v1 is NA or too low, dys with pain of 3 or above
    t1group == "Dysmenorrhea" & study == "EH19" &
      `Visit 2: Maximum cramping pain scan 1` > 2 & 
      (is.na(`Visit 1: Maximum cramping pain scan 1`) | 
         `Visit 1: Maximum cramping pain scan 1` < 3) ~ avg_contractions_v2_s1, 
    #eh19, scan 1 of v1 and v2, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` == 0 & 
      `Visit 2: Maximum cramping pain scan 1` == 0 ~ 
      rowMeans(cbind(avg_contractions_v1_s1, avg_contractions_v2_s1), na.rm = TRUE), 
    #eh19, scan 1 of v1 when v2 is NA or too high, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` == 0 & 
      (is.na(`Visit 2: Maximum cramping pain scan 1`) | 
         `Visit 2: Maximum cramping pain scan 1` > 0) ~ avg_contractions_v1_s1, 
    #eh19, scan 1 of v2 when v1 is NA or too high, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH19" &
      `Visit 2: Maximum cramping pain scan 1` == 0 & 
      (is.na(`Visit 1: Maximum cramping pain scan 1`) | 
         `Visit 1: Maximum cramping pain scan 1` > 0) ~ avg_contractions_v2_s1
  ))

#make new variable for avg frame duration w/ same parameters
uca_m_s1 <- uca_m_s1 %>%
  mutate(avg_frame_duration_m_s1 = case_when(
    #eh16, menses visit, dys with pain of 3 or above
    t1group == "Dysmenorrhea" & study == "EH16" & 
      `Menses Visit: Maximum cramping pain post scan` > 2 ~ 
      avg_frame_duration_m, 
    #eh16, menses visit, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH16" & 
      `Menses Visit: Maximum cramping pain post scan` == 0 ~ 
      avg_frame_duration_m, 
    #eh19, scan 1 of v1 and v2, dys with pain of 3 or above 
    t1group == "Dysmenorrhea" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` > 2 & 
      `Visit 2: Maximum cramping pain scan 1` > 2 ~ 
      rowMeans(cbind(avg_frame_duration_v1_s1, avg_frame_duration_v2_s1), na.rm = TRUE), 
    #eh19, scan 1 of v1 when v2 is NA or too low, dys with pain of 3 or above
    t1group == "Dysmenorrhea" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` > 2 & 
      (is.na(`Visit 2: Maximum cramping pain scan 1`) | 
         `Visit 2: Maximum cramping pain scan 1` < 3) ~ avg_frame_duration_v1_s1,
    #eh19, scan 1 of v2 when v1 is NA or too low, dys with pain of 3 or above
    t1group == "Dysmenorrhea" & study == "EH19" &
      `Visit 2: Maximum cramping pain scan 1` > 2 & 
      (is.na(`Visit 1: Maximum cramping pain scan 1`) | 
         `Visit 1: Maximum cramping pain scan 1` < 3) ~ avg_frame_duration_v2_s1, 
    #eh19, scan 1 of v1 and v2, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` == 0 & 
      `Visit 2: Maximum cramping pain scan 1` == 0 ~ 
      rowMeans(cbind(avg_frame_duration_v1_s1, avg_frame_duration_v2_s1), na.rm = TRUE), 
    #eh19, scan 1 of v1 when v2 is NA or too high, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` == 0 & 
      (is.na(`Visit 2: Maximum cramping pain scan 1`) | 
         `Visit 2: Maximum cramping pain scan 1` > 0) ~ avg_frame_duration_v1_s1, 
    #eh19, scan 1 of v2 when v1 is NA or too high, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH19" &
      `Visit 2: Maximum cramping pain scan 1` == 0 & 
      (is.na(`Visit 1: Maximum cramping pain scan 1`) | 
         `Visit 1: Maximum cramping pain scan 1` > 0) ~ avg_frame_duration_v2_s1
  ))

#make new variable for avg anterior jz w/ same parameters
uca_m_s1 <- uca_m_s1 %>%
  mutate(avg_anterior_jz_m_s1 = case_when(
    #eh16, menses visit, dys with pain of 3 or above
    t1group == "Dysmenorrhea" & study == "EH16" & 
      `Menses Visit: Maximum cramping pain post scan` > 2 ~ 
      avg_anterior_jz_m, 
    #eh16, menses visit, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH16" & 
      `Menses Visit: Maximum cramping pain post scan` == 0 ~ 
      avg_anterior_jz_m, 
    #eh19, scan 1 of v1 and v2, dys with pain of 3 or above 
    t1group == "Dysmenorrhea" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` > 2 & 
      `Visit 2: Maximum cramping pain scan 1` > 2 ~ 
      rowMeans(cbind(avg_anterior_jz_v1_s1, avg_anterior_jz_v2_s1), na.rm = TRUE), 
    #eh19, scan 1 of v1 when v2 is NA or too low, dys with pain of 3 or above
    t1group == "Dysmenorrhea" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` > 2 & 
      (is.na(`Visit 2: Maximum cramping pain scan 1`) | 
         `Visit 2: Maximum cramping pain scan 1` < 3) ~ avg_anterior_jz_v1_s1,
    #eh19, scan 1 of v2 when v1 is NA or too low, dys with pain of 3 or above
    t1group == "Dysmenorrhea" & study == "EH19" &
      `Visit 2: Maximum cramping pain scan 1` > 2 & 
      (is.na(`Visit 1: Maximum cramping pain scan 1`) | 
         `Visit 1: Maximum cramping pain scan 1` < 3) ~ avg_anterior_jz_v2_s1, 
    #eh19, scan 1 of v1 and v2, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` == 0 & 
      `Visit 2: Maximum cramping pain scan 1` == 0 ~ 
      rowMeans(cbind(avg_anterior_jz_v1_s1, avg_anterior_jz_v2_s1), na.rm = TRUE), 
    #eh19, scan 1 of v1 when v2 is NA or too high, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` == 0 & 
      (is.na(`Visit 2: Maximum cramping pain scan 1`) | 
         `Visit 2: Maximum cramping pain scan 1` > 0) ~ avg_anterior_jz_v1_s1, 
    #eh19, scan 1 of v2 when v1 is NA or too high, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH19" &
      `Visit 2: Maximum cramping pain scan 1` == 0 & 
      (is.na(`Visit 1: Maximum cramping pain scan 1`) | 
         `Visit 1: Maximum cramping pain scan 1` > 0) ~ avg_anterior_jz_v2_s1
  ))

#make new variable for avg anterior outer w/ same parameters
uca_m_s1 <- uca_m_s1 %>%
  mutate(avg_anterior_outer_m_s1 = case_when(
    #eh16, menses visit, dys with pain of 3 or above
    t1group == "Dysmenorrhea" & study == "EH16" & 
      `Menses Visit: Maximum cramping pain post scan` > 2 ~ 
      avg_anterior_outer_m, 
    #eh16, menses visit, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH16" & 
      `Menses Visit: Maximum cramping pain post scan` == 0 ~ 
      avg_anterior_outer_m, 
    #eh19, scan 1 of v1 and v2, dys with pain of 3 or above 
    t1group == "Dysmenorrhea" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` > 2 & 
      `Visit 2: Maximum cramping pain scan 1` > 2 ~ 
      rowMeans(cbind(avg_anterior_outer_v1_s1, avg_anterior_outer_v2_s1), na.rm = TRUE), 
    #eh19, scan 1 of v1 when v2 is NA or too low, dys with pain of 3 or above
    t1group == "Dysmenorrhea" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` > 2 & 
      (is.na(`Visit 2: Maximum cramping pain scan 1`) | 
         `Visit 2: Maximum cramping pain scan 1` < 3) ~ avg_anterior_outer_v1_s1,
    #eh19, scan 1 of v2 when v1 is NA or too low, dys with pain of 3 or above
    t1group == "Dysmenorrhea" & study == "EH19" &
      `Visit 2: Maximum cramping pain scan 1` > 2 & 
      (is.na(`Visit 1: Maximum cramping pain scan 1`) | 
         `Visit 1: Maximum cramping pain scan 1` < 3) ~ avg_anterior_outer_v2_s1, 
    #eh19, scan 1 of v1 and v2, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` == 0 & 
      `Visit 2: Maximum cramping pain scan 1` == 0 ~ 
      rowMeans(cbind(avg_anterior_outer_v1_s1, avg_anterior_outer_v2_s1), na.rm = TRUE), 
    #eh19, scan 1 of v1 when v2 is NA or too high, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` == 0 & 
      (is.na(`Visit 2: Maximum cramping pain scan 1`) | 
         `Visit 2: Maximum cramping pain scan 1` > 0) ~ avg_anterior_outer_v1_s1, 
    #eh19, scan 1 of v2 when v1 is NA or too high, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH19" &
      `Visit 2: Maximum cramping pain scan 1` == 0 & 
      (is.na(`Visit 1: Maximum cramping pain scan 1`) | 
         `Visit 1: Maximum cramping pain scan 1` > 0) ~ avg_anterior_outer_v2_s1
  ))

#make new variable for avg posterior jz w/ same parameters
uca_m_s1 <- uca_m_s1 %>%
  mutate(avg_posterior_jz_m_s1 = case_when(
    #eh16, menses visit, dys with pain of 3 or above
    t1group == "Dysmenorrhea" & study == "EH16" & 
      `Menses Visit: Maximum cramping pain post scan` > 2 ~ 
      avg_posterior_jz_m, 
    #eh16, menses visit, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH16" & 
      `Menses Visit: Maximum cramping pain post scan` == 0 ~ 
      avg_posterior_jz_m, 
    #eh19, scan 1 of v1 and v2, dys with pain of 3 or above 
    t1group == "Dysmenorrhea" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` > 2 & 
      `Visit 2: Maximum cramping pain scan 1` > 2 ~ 
      rowMeans(cbind(avg_posterior_jz_v1_s1, avg_posterior_jz_v2_s1), na.rm = TRUE), 
    #eh19, scan 1 of v1 when v2 is NA or too low, dys with pain of 3 or above
    t1group == "Dysmenorrhea" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` > 2 & 
      (is.na(`Visit 2: Maximum cramping pain scan 1`) | 
         `Visit 2: Maximum cramping pain scan 1` < 3) ~ avg_posterior_jz_v1_s1,
    #eh19, scan 1 of v2 when v1 is NA or too low, dys with pain of 3 or above
    t1group == "Dysmenorrhea" & study == "EH19" &
      `Visit 2: Maximum cramping pain scan 1` > 2 & 
      (is.na(`Visit 1: Maximum cramping pain scan 1`) | 
         `Visit 1: Maximum cramping pain scan 1` < 3) ~ avg_posterior_jz_v2_s1, 
    #eh19, scan 1 of v1 and v2, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` == 0 & 
      `Visit 2: Maximum cramping pain scan 1` == 0 ~ 
      rowMeans(cbind(avg_posterior_jz_v1_s1, avg_posterior_jz_v2_s1), na.rm = TRUE), 
    #eh19, scan 1 of v1 when v2 is NA or too high, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` == 0 & 
      (is.na(`Visit 2: Maximum cramping pain scan 1`) | 
         `Visit 2: Maximum cramping pain scan 1` > 0) ~ avg_posterior_jz_v1_s1, 
    #eh19, scan 1 of v2 when v1 is NA or too high, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH19" &
      `Visit 2: Maximum cramping pain scan 1` == 0 & 
      (is.na(`Visit 1: Maximum cramping pain scan 1`) | 
         `Visit 1: Maximum cramping pain scan 1` > 0) ~ avg_posterior_jz_v2_s1
  ))

#make new variable for avg posterior outer w/ same parameters
uca_m_s1 <- uca_m_s1 %>%
  mutate(avg_posterior_outer_m_s1 = case_when(
    #eh16, menses visit, dys with pain of 3 or above
    t1group == "Dysmenorrhea" & study == "EH16" & 
      `Menses Visit: Maximum cramping pain post scan` > 2 ~ 
      avg_posterior_outer_m, 
    #eh16, menses visit, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH16" & 
      `Menses Visit: Maximum cramping pain post scan` == 0 ~ 
      avg_posterior_outer_m, 
    #eh19, scan 1 of v1 and v2, dys with pain of 3 or above 
    t1group == "Dysmenorrhea" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` > 2 & 
      `Visit 2: Maximum cramping pain scan 1` > 2 ~ 
      rowMeans(cbind(avg_posterior_outer_v1_s1, avg_posterior_outer_v2_s1), na.rm = TRUE), 
    #eh19, scan 1 of v1 when v2 is NA or too low, dys with pain of 3 or above
    t1group == "Dysmenorrhea" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` > 2 & 
      (is.na(`Visit 2: Maximum cramping pain scan 1`) | 
         `Visit 2: Maximum cramping pain scan 1` < 3) ~ avg_posterior_outer_v1_s1,
    #eh19, scan 1 of v2 when v1 is NA or too low, dys with pain of 3 or above
    t1group == "Dysmenorrhea" & study == "EH19" &
      `Visit 2: Maximum cramping pain scan 1` > 2 & 
      (is.na(`Visit 1: Maximum cramping pain scan 1`) | 
         `Visit 1: Maximum cramping pain scan 1` < 3) ~ avg_posterior_outer_v2_s1, 
    #eh19, scan 1 of v1 and v2, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` == 0 & 
      `Visit 2: Maximum cramping pain scan 1` == 0 ~ 
      rowMeans(cbind(avg_posterior_outer_v1_s1, avg_posterior_outer_v2_s1), na.rm = TRUE), 
    #eh19, scan 1 of v1 when v2 is NA or too high, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH19" &
      `Visit 1: Maximum cramping pain scan 1` == 0 & 
      (is.na(`Visit 2: Maximum cramping pain scan 1`) | 
         `Visit 2: Maximum cramping pain scan 1` > 0) ~ avg_posterior_outer_v1_s1, 
    #eh19, scan 1 of v2 when v1 is NA or too high, hc with pain of 0
    t1group == "Pain Free Control" & study == "EH19" &
      `Visit 2: Maximum cramping pain scan 1` == 0 & 
      (is.na(`Visit 1: Maximum cramping pain scan 1`) | 
         `Visit 1: Maximum cramping pain scan 1` > 0) ~ avg_posterior_outer_v2_s1
  ))

#Define continuous variables for menses visits (eh16 and eh19), menses and s1 only
contvar_m_s1 <- c("avg_contractions_m_s1", "avg_frame_duration_m_s1", "avg_anterior_jz_m_s1", 
                  "avg_anterior_outer_m_s1", "avg_posterior_jz_m_s1", "avg_posterior_outer_m_s1")

table2f <- uca_m_s1 %>%
  select(all_of(contvar_m_s1), t1group) %>%
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
kw <- lapply(contvar_m_s1, function(var) {
  formula <- as.formula(paste(var, "~t1group"))
  # Check if variable has enough data to run test
  temp <- uca_m_s1[, c(var, "t1group")]
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
kw_results <- kw_results %>%
  select(-df)


#combine into one table
names(table2f)[1] <- "Variable"

table2f_full <- left_join(table2f, kw_results, by = "Variable")

#add in n per group per variable
n_long <- uca_m_s1 %>%
  select(all_of(contvar_m_s1), t1group) %>%
  filter(t1group %in% c("Dysmenorrhea", "Pain Free Control")) %>%
  pivot_longer(cols = -t1group, names_to = "Variable", values_to = "Value") %>%
  dplyr::group_by(t1group, Variable) %>%
  dplyr::summarize(n = sum(!is.na(Value)), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = t1group, values_from = n) %>%
  dplyr::rename(
    DYS_n = Dysmenorrhea,
    HC_n = `Pain Free Control`
  )

# merge into table 2f
table2f_full_n <- table2f_full %>%
  left_join(n_long, by = "Variable") %>%
  mutate(
    Dysmenorrhea = paste0(Dysmenorrhea, " (n=", DYS_n, ")"),
    `Pain Free Control` = paste0(`Pain Free Control`, " (n=", HC_n, ")")
  ) %>%
  select(Variable, Dysmenorrhea, `Pain Free Control`, Chi_Square, p_value)

# Create a flextable object
ft2 <- flextable(table2f_full_n) %>%
  bold(i = 1, part = "header") %>%               # Bold the header row
  align(align = "left", part = "all") %>%         # Align left for all parts
  fontsize(size = 10, part = "all") %>%           # Set font size
  set_table_properties(layout = "fixed", width = 1) %>% # Fixed width layout
  theme_vanilla()                                # Apply a vanilla theme

read_docx() %>%
  body_add_flextable(ft2) %>%
  print(target = "table2f.docx")

#Welch's t-test

uca_m_s1_groups <- uca_m_s1 %>%
  filter(t1group %in% c("Dysmenorrhea", "Pain Free Control"))

ttest_results <- lapply(contvar_m_s1, function(var) {
  formula <- as.formula(paste(var, "~t1group"))
  temp <- uca_m_s1_groups[, c(var, "t1group")]
  temp <- temp[complete.cases(temp), ]  # remove NAs
  
  if (length(unique(temp$t1group)) < 2 || length(unique(temp[[var]])) < 2) {
    return(NULL)
  }
  # Perform Welch's t-test (handles unequal variances)
  test_result <- t.test(formula, data = temp, var.equal = TRUE)
  
  data.frame(
    Variable = var,
    t_statistic = test_result$statistic,
    df = test_result$parameter,
    p_value = test_result$p.value,
    mean_Dysmenorrhea = mean(temp[[var]][temp$t1group == "Dysmenorrhea"]),
    mean_Control = mean(temp[[var]][temp$t1group == "Pain Free Control"]),
    stringsAsFactors = FALSE
  )
})

# Combine into a single data frame
ttest_df <- do.call(rbind, ttest_results)

# Create a flextable object
ft2 <- flextable(ttest_df) %>%
  bold(i = 1, part = "header") %>%               # Bold the header row
  align(align = "left", part = "all") %>%         # Align left for all parts
  fontsize(size = 10, part = "all") %>%           # Set font size
  set_table_properties(layout = "fixed", width = 1) %>% # Fixed width layout
  theme_vanilla()                                # Apply a vanilla theme

read_docx() %>%
  body_add_flextable(ft2) %>%
  print(target = "table2f_parametric_v2.docx")


#boot strap test
set.seed(123)  # for reproducibility

bootstrap_median_results <- lapply(contvar_m_s1, function(var) {
  temp <- uca_m_s1_groups[, c(var, "t1group")]
  temp <- temp[complete.cases(temp), ]
  
  if (length(unique(temp$t1group)) < 2 || length(unique(temp[[var]])) < 2) {
    return(NULL)
  }
  
  # Split data
  dys_vals <- temp[[var]][temp$t1group == "Dysmenorrhea"]
  hc_vals <- temp[[var]][temp$t1group == "Pain Free Control"]
  
  # Observed difference in medians
  obs_diff <- median(dys_vals) - median(hc_vals)
  
  # Bootstrap sampling
  n_boot <- 10000
  boot_diffs <- replicate(n_boot, {
    median(sample(dys_vals, replace = TRUE)) - median(sample(hc_vals, replace = TRUE))
  })
  
  # Two-sided p-value
  p_boot <- mean(abs(boot_diffs) >= abs(obs_diff))
  
  data.frame(
    Variable = var,
    median_Dysmenorrhea = median(dys_vals),
    median_Control = median(hc_vals),
    observed_median_diff = obs_diff,
    bootstrap_p_value = p_boot,
    stringsAsFactors = FALSE
  )
})

# Combine results into data frame
bootstrap_median_df <- do.call(rbind, bootstrap_median_results)

# Create a flextable object
ft2 <- flextable(bootstrap_median_df) %>%
  bold(i = 1, part = "header") %>%               # Bold the header row
  align(align = "left", part = "all") %>%         # Align left for all parts
  fontsize(size = 10, part = "all") %>%           # Set font size
  set_table_properties(layout = "fixed", width = 1) %>% # Fixed width layout
  theme_vanilla()                                # Apply a vanilla theme

read_docx() %>%
  body_add_flextable(ft2) %>%
  print(target = "table2f_bootstrap.docx")

#permutation
set.seed(123)  # for reproducibility

# Permutation test of median differences
permutation_median_results <- lapply(contvar_m_s1, function(var) {
  temp <- uca_m_s1_groups[, c(var, "t1group")]
  temp <- temp[complete.cases(temp), ]
  
  if (length(unique(temp$t1group)) < 2 || length(unique(temp[[var]])) < 2) {
    return(NULL)
  }
  
  # Split data
  dys_vals <- temp[[var]][temp$t1group == "Dysmenorrhea"]
  hc_vals <- temp[[var]][temp$t1group == "Pain Free Control"]
  obs_diff <- median(dys_vals) - median(hc_vals)
  
  # Combine all values and labels
  combined_vals <- temp[[var]]
  labels <- temp$t1group
  
  # Permutation test
  n_perm <- 10000
  perm_diffs <- replicate(n_perm, {
    permuted_labels <- sample(labels)
    median(combined_vals[permuted_labels == "Dysmenorrhea"]) -
      median(combined_vals[permuted_labels == "Pain Free Control"])
  })
  
  # Two-sided empirical p-value
  p_perm <- mean(abs(perm_diffs) >= abs(obs_diff))
  
  data.frame(
    Variable = var,
    median_Dysmenorrhea = median(dys_vals),
    median_Control = median(hc_vals),
    observed_median_diff = obs_diff,
    permutation_p_value = p_perm,
    stringsAsFactors = FALSE
  )
})

# Combine results into data frame
permutation_median_df <- do.call(rbind, permutation_median_results)

# Create a flextable
ft2 <- flextable(permutation_median_df) %>%
  bold(i = 1, part = "header") %>%
  align(align = "left", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  set_table_properties(layout = "fixed", width = 1) %>%
  theme_vanilla()

read_docx() %>%
  body_add_flextable(ft2) %>%
  print(target = "table2f_permutation.docx")

#Determine what % of groups have 0 contractions at both visits
uca_m_s1_groups %>%
  count(t1group)

dys_zero <- uca_m_s1_groups %>%
  filter(t1group == "Dysmenorrhea", avg_contractions_m_s1 == 0)

dys_zero_half <- uca_m_s1_groups %>%
  filter(t1group == "Dysmenorrhea", avg_contractions_m_s1 == 0.5)

hc_zero <- uca_m_s1_groups %>%
  filter(t1group == "Pain Free Control", avg_contractions_m_s1 == 0)

hc_zero_half <- uca_m_s1_groups %>%
  filter(t1group == "Pain Free Control", avg_contractions_m_s1 == 0.5)



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