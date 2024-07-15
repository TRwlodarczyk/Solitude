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






#NIST summary table 

{
  
  setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/NIST-test")
  dt <-read.delim("NIST_FINAL_May24.txt")
  
  dt1 <- dt %>% 
    filter(Sample != "QA" &
             Method == "cup" &
             Optimization == "T1.5" &
             Total_Weight=="2")
  
  
  
  
  
  
  
  library(dplyr)
  
  # Function to calculate summary statistics
  calculate_summary <- function(data, element) {
    element_NIST <- paste0(element, "_NIST")
    element_sd <- paste0(element, "_NIST_SD")
    
    summary_df <- data %>%
      group_by(Sample_ID) %>%
      summarize(
        NIST_value = first(get(element_NIST), na.rm = TRUE),
        NIST_SD = first(get(element_sd), na.rm = TRUE),
        mean_value = mean(as.numeric(get(element)), na.rm = TRUE),
        median_value = median(as.numeric(get(element)), na.rm = TRUE),
        min_value = min(as.numeric(get(element)), na.rm = TRUE),
        max_value = max(as.numeric(get(element)), na.rm = TRUE),
        sd_value = sd(as.numeric(get(element)), na.rm = TRUE)
      ) %>%
      ungroup() %>%
      mutate(Element = element) %>%
      select(Element, Sample_ID, everything())
    
    return(summary_df)
  }
  
  # Elements to process
  elements <- c("Cu", "Zn", "Mn", "Se", "Re", "Fe", "P", "S", "K", "Ca")
  
  # Generate the summary statistics for each element
  summary_list <- lapply(elements, function(element) calculate_summary(dt1, element))
  
  # Combine all summaries into one dataframe
  summary_df <- bind_rows(summary_list)
  
  # Print the result
  print(summary_df)
  
  
  
  # Function to conditionally round values, with special handling for Se
  conditional_round <- function(x, element) {
    if (element == "Se") {
      if_else(abs(x) >= 10, round(x, 0), round(x, 2))
    } else {
      if_else(abs(x) >= 30, round(x, 0), round(x, 1))
    }
  }
  
  # Apply the conditional rounding to specific numeric columns, excluding `_NIST` and `_NIST_SD`
  summary_df <- summary_df %>%
    rowwise() %>%
    mutate(
      mean_value = conditional_round(mean_value, Element),
      median_value = conditional_round(median_value, Element),
      min_value = conditional_round(min_value, Element),
      max_value = conditional_round(max_value, Element),
      sd_value = conditional_round(sd_value, Element)
    ) %>%
    ungroup()
  
  
  write.xlsx(summary_df, "NIST_summary_df.xlsx")
}



# Soil table

{
  
  setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/Tables")
  dt <-read.delim("SLT_Soil_Brookside.txt")
  
  dt1 <- dt %>%
    filter(Layer=="S"&
             Plot!="Nat")
  
  # Function to calculate summary statistics for Total_Weight
  calculate_total_weight_summary <- function(data) {
    summary_df <- data %>%
      group_by(Plot) %>%
      summarize(
        mean_Cu = mean(Cu, na.rm = TRUE),
        sd_cu = sd(Cu, na.rm = TRUE),
        mean_zn = mean(Zn, na.rm = TRUE),
        sd_zn = sd(Zn, na.rm = TRUE),
        mean_mn = mean(Mn, na.rm = TRUE),
        sd_mn = sd(Mn, na.rm = TRUE),
        mean_fe = mean(Fe, na.rm = TRUE),
        sd_fe = sd(Fe, na.rm = TRUE),
        mean_re = mean(Re, na.rm = TRUE),
        sd_re = sd(Re, na.rm = TRUE),
        mean_se = mean(Se, na.rm = TRUE),
        sd_se = sd(Se, na.rm = TRUE),
        mean_ph = mean(pH, na.rm = TRUE),
        sd_ph = sd(pH, na.rm = TRUE)

      ) %>%
      ungroup()
    
    return(summary_df)
  }
  
  # Generate the summary statistics for Total_Weight
  total_weight_summary_df <- calculate_total_weight_summary(dt1)
  
  
  write.xlsx(total_weight_summary_df, "Table-Soil.xlsx")
  
  
  
  
  
}


