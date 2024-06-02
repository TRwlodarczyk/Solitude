# Tabloes







{
  library(ggplot2)
  library(ggpubr)
  library(dplyr)
  library(data.table)
  library(reshape2)
  library(reshape)
  library("readxl")
  library(ggpubr)
  library(agricolae)
  library(tidyverse)
  library (readr) #to read URL
  library(stringr) # For str_replace_all
  library(psych)
  library(car)
  library(openxlsx)
  library(writexl)
  
}

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/Tables")
dt <-read.delim("pXRF_tables.txt")


glimpse(dt)



library(dplyr)

# Function to calculate summary statistics
calculate_summary <- function(data, element) {
  element_ICP <- paste0(element, "_ICP")
  element_cv <- paste0("cv_", tolower(element))
  element_PXRF <- paste0(element, "_PXRF")
  element_error <- paste0(element, "_error")
  
  summary_df <- data %>%
    group_by(Form) %>%
    summarize(
      mean_ICP = mean(get(element_ICP), na.rm = TRUE),
      median_ICP = median(get(element_ICP), na.rm = TRUE),
      min_ICP = min(get(element_ICP), na.rm = TRUE),
      max_ICP = max(get(element_ICP), na.rm = TRUE),
      mean_cv = mean(get(element_cv)[get(element_cv) != 0], na.rm = TRUE),
      median_cv = median(get(element_cv)[get(element_cv) != 0], na.rm = TRUE),
      min_cv = min(get(element_cv)[get(element_cv) != 0], na.rm = TRUE),
      max_cv = max(get(element_cv)[get(element_cv) != 0], na.rm = TRUE),
      mean_PXRF = mean(get(element_PXRF), na.rm = TRUE),
      median_PXRF = median(get(element_PXRF), na.rm = TRUE),
      min_PXRF = min(get(element_PXRF), na.rm = TRUE),
      max_PXRF = max(get(element_PXRF), na.rm = TRUE),
      mean_error = mean(get(element_error), na.rm = TRUE),
      median_error = median(get(element_error), na.rm = TRUE),
      min_error = min(get(element_error), na.rm = TRUE),
      max_error = max(get(element_error), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(Element = element) %>%
    select(Element, everything())
  
  return(summary_df)
}

# Elements to process
elements <- c("Cu", "Zn", "Mn", "Se", "Re", "Fe")

# Generate the summary statistics for each element
summary_list <- lapply(elements, function(element) calculate_summary(dt, element))

# Combine all summaries into one dataframe
summary_df <- bind_rows(summary_list)

# Remove rows with equal min and max values for cv
summary_df <- summary_df %>%
  filter(min_cv != max_cv)

# Function to conditionally round values
conditional_round <- function(x) {
  if_else(abs(x) >= 10, round(x, 0), round(x, 1))
}

# Apply the conditional rounding to all numeric columns
summary_df <- summary_df %>%
  mutate(across(where(is.numeric), conditional_round))

# Print the result
print(summary_df)



write.xlsx(summary_df, "Table-Element-Form2.xlsx")







#### Summary table for Total Weight

library(dplyr)

# Function to calculate summary statistics for Total_Weight
calculate_total_weight_summary <- function(data) {
  summary_df <- data %>%
    group_by(Form) %>%
    summarize(
      mean_Total_Weight = mean(Total_Weight, na.rm = TRUE),
      median_Total_Weight = median(Total_Weight, na.rm = TRUE),
      min_Total_Weight = min(Total_Weight, na.rm = TRUE),
      max_Total_Weight = max(Total_Weight, na.rm = TRUE)
    ) %>%
    ungroup()
  
  return(summary_df)
}

# Generate the summary statistics for Total_Weight
total_weight_summary_df <- calculate_total_weight_summary(dt)


write.xlsx(total_weight_summary_df, "Table-TW-Form.xlsx")


