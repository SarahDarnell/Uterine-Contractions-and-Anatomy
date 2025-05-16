#Uterine Contractions and Anatomy Analysis - Part 4 - Full dataset - table 2s
#Written by Sarah Darnell, began 5.15.25, lasted edited 5.15.25

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

#Remove nonmenses visits from eh16
uca_filtered_menses <- uca %>%
  filter(menses == 1 | is.na(menses))

##Table 2b - median results from endo and fibroid groups##
table2b <- uca_filtered_menses %>%
  select(all_of(contvar), t1group) %>%
  filter(!t1group %in% c("Dysmenorrhea", "Pain Free Control")) %>%
  group_by(t1group) %>%
  summarize(across(everything(), ~ sprintf("%.1f [%.1f-%.1f]", 
                                           median(., na.rm = TRUE), 
                                           quantile(., 0.25, na.rm = TRUE),
                                           quantile(., 0.75, na.rm = TRUE))),
            .groups = "drop") %>%
  pivot_longer(cols = -t1group, names_to = "Item", values_to = "Median [IQR]") %>% 
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

##Table 2a - median results and kruskal wallis results for dys and hc

#Remove DYs w/o pain, and HC w/ pain
#visualize distribution of pain across variables for Dys/Hc

painvar <- c("Menses Visit: Baseline average cramping pain", 
             "Menses Visit: Baseline maximum cramping pain", 
             "Visit 1: Baseline pain between cramps", 
             "Visit 1: Baseline maximum cramping pain", 
             "Visit 2: Baseline pain between cramps", 
             "Visit 2: Baseline maximum cramping pain")

uca_pain <- uca_filtered_menses %>%
  select(all_of(painvar), t1group) %>%
  filter(!t1group %in% c("Fibroid", "Endometriosis")) %>%
  pivot_longer(cols = -t1group, names_to = "Variable", values_to = "Value")

ggplot(uca_pain, aes(x = t1group, y = Value, fill = t1group)) +
  geom_boxplot() +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Distribution of Pain Variables by Group",
       x = "Group",
       y = "Value") +
  theme_minimal()


##below here needs to be work shopped##

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