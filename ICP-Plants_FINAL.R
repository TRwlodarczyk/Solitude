#Final Dataset

#


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
  
}


setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/")
dt <-read.delim("Solitude_PXRF_ICP_ALL_FINAL.txt")

#Creating mean and error dataset
{
# Summarize ICP data to have mean and then do error
dt_ICP_summarized <- dt %>%
  filter(Method == "ICP") %>%
  group_by(SampleID) %>%
  summarise(across(c(P, S, Ti, Cr, Mn, Fe, Cu, Zn, As, Se, Re), mean, na.rm = TRUE),
            across(c(Scientific_Name, Family, Status, Form, Duration, Site, Group, Plot, Sample_Name, Tube_No, Type_of_Sample, Total_Weight, Substrate_RT, Method, SampleID2, ICP), first),
            .groups = 'drop') %>%
  mutate(Method = "ICP_Averaged")

# Ensure PXRF data is selected with the necessary columns
dt_PXRF_selected <- dt %>%
  filter(Method == "PXRF") %>%
  select(SampleID, P, S, Ti, Cr, Mn, Fe, Cu, Zn, As, Se, Re, Scientific_Name, Family, Status, Form, Duration, Site, Group, Plot, Sample_Name, Tube_No, Type_of_Sample, Total_Weight, Substrate_RT, Method, SampleID2, ICP)

# Combine ICP summarized data with PXRF selected data
combined_dt <- bind_rows(dt_ICP_summarized, dt_PXRF_selected)

# Calculate percent error for each element between ICP and PXRF for matched samples
results_with_error <- combined_dt %>%
  group_by(SampleID) %>%
  filter(n() > 1) %>% # Ensures there's both ICP and PXRF data
  summarise(across(c(P, S, Ti, Cr, Mn, Fe, Cu, Zn, As, Se, Re), ~abs(.x[Method == "ICP_Averaged"] - .x[Method == "PXRF"]) / .x[Method == "ICP_Averaged"] * 100, .names = "{.col}_error"),
            .groups = 'drop')

# View the result
print(results_with_error)

#Move Scientifc_Name etc. to the front
combined_dt <- combined_dt %>%
  select(13:28, 1:12)

#add metadata to error dataset

glimpse(combined_dt)

library(dplyr)
library(tidyr)

# Assuming 'combined_dt' is your dataset and it has already been loaded

# Step 1: Separate ICP and PXRF data, and calculate percent error
dt_ICP <- combined_dt %>% 
  filter(Method == "ICP_Averaged")

dt_PXRF <- combined_dt %>% 
  filter(Method == "PXRF")

# Assuming each SampleID in PXRF matches exactly one SampleID in ICP
# Calculate percent error for elements P to Re (assuming these are columns 18 to 28)
percent_errors <- dt_PXRF %>%
  left_join(dt_ICP, by = "SampleID", suffix = c("_PXRF", "_ICP")) %>%
  mutate(across(P_ICP:Re_ICP, ~abs(. - get(str_replace(cur_column(), "_ICP", "_PXRF"))) / . * 100, .names = "error_{.col}")) %>%
  select(SampleID, starts_with("error_"))

# Step 2: Format and Combine Data
# Keeping metadata and appending _ICP or _PXRF
formatted_ICP <- dt_ICP %>%
  rename_with(~str_replace(., "^(P|S|Ti|Cr|Mn|Fe|Cu|Zn|As|Se|Re)$", "\\1_ICP"), P:Re) %>%
  select(SampleID, P_ICP:Re_ICP)

formatted_PXRF <- dt_PXRF %>%
  rename_with(~str_replace(., "^(P|S|Ti|Cr|Mn|Fe|Cu|Zn|As|Se|Re)$", "\\1_PXRF"), P:Re) %>%
  select(SampleID, P_PXRF:Re_PXRF)

# Combine the errors with ICP and PXRF data
final_dataset <- combined_dt %>%
  select(SampleID, Scientific_Name:ICP) %>%
  distinct(SampleID, .keep_all = TRUE) %>%
  left_join(percent_errors, by = "SampleID") %>%
  left_join(formatted_ICP, by = "SampleID") %>%
  left_join(formatted_PXRF, by = "SampleID")


final_dataset_renamed <- final_dataset %>%
  rename_with(~gsub("error_(.*)_(ICP|PXRF)$", "\\1_error", .x))

}

#Dataset adjusting
dt <- final_dataset_renamed

dt <- dt %>%
  mutate(Tube_No = if_else(Tube_No == "three", "two", Tube_No))


write.xlsx(dt, "ICP-PXRF-MEAN.xlsx")
##############


dt <- dt %>%
  filter(Type_of_Sample != "stem", Type_of_Sample != "root")



# Add a new column 'Shape' to the dataset based on conditions
dt <- dt %>%
  mutate(Shape = case_when(
    Form == "Forb" & Type_of_Sample == "leaf" ~ 16,  # Filled dot
    Form == "Shrub" & Type_of_Sample == "leaf" ~ 17,  # Filled triangle
    Form == "Tree" & Type_of_Sample == "leaf" ~ 15,  # Filled square
    Form == "Grass" & Type_of_Sample == "leaf" ~ 18,  # Filled diamond
    Form == "Forb" & Type_of_Sample == "leaf-stem" ~ 1,  # Empty dot circle
    Form == "Shrub" & Type_of_Sample == "leaf-stem" ~ 2,  # Empty triangle
    Form == "Tree" & Type_of_Sample == "leaf-stem" ~ 0,  # Empty square
    Form == "Grass" & Type_of_Sample == "leaf-stem" ~ 5   # Empty diamond
  ))

# Plotting with customized shapes
ggplot(dt, aes(x = Total_Weight, y = Cu_error, color = Tube_No, size = Cu_ICP, shape = factor(Shape))) +
  geom_point() +  # Plot points
  labs(x = "Total Weight", y = "Cu Error", title = "All Points") +
  scale_shape_manual(values = c(`16` = 16, `17` = 17, `15` = 15, `18` = 18, `1` = 1, `2` = 2, `0` = 0, `5` = 5)) +  # Map shapes manually
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))


###sprawdzam jak error wyglada dla jednej probki - dataset with Cu_PXRF and Cu_ICP osobno
{
Cu <- ggplot(dt, aes(x = Total_Weight, y = Cu_error, color = Tube_No)) +
  geom_point() +  # Plot points
  labs(x = "Total_Weight", y = "Cu Error", title = "All tubes, means") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))

cor(dt$Cu_error, dt$Cu_PXRF, method = "spearman", use = "complete.obs")

dt_one <- dt[dt$Tube_No == "one",]
cor(dt_one$Cu_error, dt_one$Cu_PXRF, method = "spearman", use = "complete.obs")

dt_two <- dt[dt$Tube_No == "two",]
cor(dt_two$Cu_error, dt_two$Cu_PXRF, method = "spearman", use = "complete.obs")



}


glimpse(combined_dt)




#anova
combined_dt <- combined_dt %>%
  mutate(Tube_No = if_else(Tube_No == "three", "two", Tube_No))



#Anova for all elements 
{
  
  aovCu1 <- aov(Cu ~ Method + Total_Weight + Tube_No + Form + Type_of_Sample, data = combined_dt)
  summary(aovCu1)
  
  aovCu2 <- aov(Cu_ICP ~ Cu_PXRF + Total_Weight + Tube_No + Form + Type_of_Sample, data = dt)
  summary(aovCu2)
  
  aovZn1 <- aov(Zn ~ Method + Total_Weight + Tube_No + Form + Type_of_Sample, data = combined_dt)
  summary(aovZn1)
  
  aovZn2 <- aov(Zn_ICP ~ Zn_PXRF + Total_Weight + Tube_No + Form + Type_of_Sample, data = dt)
  summary(aovZn2)
  
  aovSe1 <- aov(Se ~ Method + Total_Weight + Tube_No + Form + Type_of_Sample, data = combined_dt)
  summary(aovSe1)
  
  aovSe2 <- aov(Se_ICP ~ Se_PXRF + Total_Weight + Tube_No + Form + Type_of_Sample, data = dt)
  summary(aovSe2)
  
  aovRe1 <- aov(Re ~ Method + Total_Weight + Tube_No + Form + Type_of_Sample, data = combined_dt)
  summary(aovRe1)
  
  aovRe2 <- aov(Re_ICP ~ Re_PXRF + Total_Weight + Tube_No + Form + Type_of_Sample, data = dt)
  summary(aovRe2)
  
  aovMn1 <- aov(Mn ~ Method + Total_Weight + Tube_No + Form + Type_of_Sample, data = combined_dt)
  summary(aovMn1)
  
  aovMn2 <- aov(Mn_ICP ~ Mn_PXRF + Total_Weight + Tube_No + Form + Type_of_Sample, data = dt)
  summary(aovMn2)
  
  aovFe1 <- aov(Fe ~ Method + Total_Weight + Tube_No + Form + Type_of_Sample, data = combined_dt)
  summary(aovFe1)
  
  aovFe2 <- aov(Fe_ICP ~ Fe_PXRF + Total_Weight + Tube_No + Form + Type_of_Sample, data = dt)
  summary(aovFe2)
}

# LM to check the influence of Weight etc.
{
lm_model <- lm(Cu_ICP ~ Cu_PXRF + Total_Weight + Tube_No + Form + Type_of_Sample, data = dt)
summary(lm_model)
Anova(lm_model, type="II")

lm_model <- lm(Zn_ICP ~ Zn_PXRF + Total_Weight + Tube_No + Form + Type_of_Sample, data = dt)
summary(lm_model)
Anova(lm_model, type="II")

lm_model <- lm(Se_ICP ~ Se_PXRF + Total_Weight + Tube_No + Form + Type_of_Sample, data = dt)
summary(lm_model)
Anova(lm_model, type="II")

lm_model <- lm(Re_ICP ~ Re_PXRF + Total_Weight + Tube_No + Form + Type_of_Sample, data = dt)
summary(lm_model)
Anova(lm_model, type="II")

lm_model <- lm(Mn_ICP ~ Mn_PXRF + Total_Weight + Tube_No + Form + Type_of_Sample, data = dt)
summary(lm_model)
Anova(lm_model, type="II")

lm_model <- lm(Fe_ICP ~ Fe_PXRF + Total_Weight + Tube_No + Form + Type_of_Sample, data = dt)
summary(lm_model)
Anova(lm_model, type="II")
}

#Check mixed effect model to nest the data and nest homogenity of sample (single tube) to check ICP.
{
  #THIS IS FIRST DT not the second DT!!!!!!!!!!!!!!!!!
  dt <- dt %>%
    mutate(Tube_No = if_else(Tube_No == "three", "two", Tube_No))
  
  dt <- dt %>%
    filter(Type_of_Sample != "stem", Type_of_Sample != "root")
  
  dt <- dt %>%
    filter(ICP == "y")
  
  dt <- dt %>%
    filter(Method != "PXRF")
  
  dt <- dt %>%
    mutate(Category = case_when(
      grepl("_1$", SampleID2) ~ "_1",
      grepl("_2$", SampleID2) ~ "_2",
      grepl("_3$", SampleID2) ~ "_3",
      TRUE ~ "Other"
    ))
  
  dt_count <- dt %>%
    group_by(SampleID) %>%
    summarise(ReplicateCount = n()) %>%
    filter(ReplicateCount == 3)
  
  dt_filtered <- dt %>%
    filter(SampleID %in% dt_count$SampleID)
  
  library(lme4)
  
  dt_filtered <- dt_filtered[dt_filtered$Tube_No == "one",]

  model <- lmer(Cu ~ Scientific_Name + Plot + (1|SampleID), data = dt_filtered)
  
  model_nested <- lmer(Cu ~ (1|Scientific_Name/Plot/SampleID/Category), data = dt_filtered)
  
  simplified_model <- lmer(Cu ~ (1|Scientific_Name) + (1|Plot) + (1|SampleID), data = dt_filtered)
  model_technical_replicates <- lmer(Cu ~ 1 + (1|SampleID), data = dt_filtered)
  
  summary(model)
  summary(simplified_model)
  summary(model_technical_replicates)
  
  #anova nested
  
  anova_model_nested <- aov(Cu ~ Scientific_Name/Plot/SampleID/Category, data = dt_filtered)
  summary(anova_model_nested)
  
}

glimpse(dt_filtered)

#Check the homogenity of variance for ICP between technical replicates and for one tube and two tubes separately
{
# Ensure SampleID is treated as a factor. Only Catogyr with 3 replicates, only One sample, no roots, no stems, only ICP, no PXRF.
  dt <-read.delim("Solitude_PXRF_ICP_ALL_FINAL.txt")
  dt <- dt %>%
    mutate(Tube_No = if_else(Tube_No == "three", "two", Tube_No))
  
  dt <- dt %>%
    filter(Type_of_Sample != "stem", Type_of_Sample != "root")
  
  dt <- dt %>%
    filter(ICP == "y")
  
  dt <- dt %>%
    filter(Method != "PXRF")
  
  dt <- dt %>%
    mutate(Category = case_when(
      grepl("_1$", SampleID2) ~ "_1",
      grepl("_2$", SampleID2) ~ "_2",
      grepl("_3$", SampleID2) ~ "_3",
      TRUE ~ "Other"
    ))
  
  dt_count <- dt %>%
    group_by(SampleID) %>%
    summarise(ReplicateCount = n()) %>%
    filter(ReplicateCount == 3)
  
  dt_filtered <- dt %>%
    filter(SampleID %in% dt_count$SampleID)
  

  

#For one tube
dt_filtered <- dt_filtered[dt_filtered$Tube_No == "one",]
dt_filtered$SampleID <- factor(dt_filtered$SampleID)
  
leveneTest(Cu ~ SampleID, data = dt_filtered)
leveneTest(Se ~ SampleID, data = dt_filtered)
leveneTest(Re ~ SampleID, data = dt_filtered)
leveneTest(Zn ~ SampleID, data = dt_filtered)
leveneTest(Mn ~ SampleID, data = dt_filtered)
leveneTest(Fe ~ SampleID, data = dt_filtered)


#For two tubes
dt_filtered2 <- dt_filtered[dt_filtered$Tube_No == "two",]
dt_filtered2$SampleID <- factor(dt_filtered2$SampleID)

leveneTest(Cu ~ SampleID, data = dt_filtered2)
leveneTest(Se ~ SampleID, data = dt_filtered2)
leveneTest(Re ~ SampleID, data = dt_filtered2)
leveneTest(Zn ~ SampleID, data = dt_filtered2)
leveneTest(Mn ~ SampleID, data = dt_filtered2)
leveneTest(Fe ~ SampleID, data = dt_filtered2)

}


#Correlation

# Error is not sample related! but element specific. 
library(corrplot)
library(Hmisc)
library(dplyr)

error_data <- dt %>% 
  select(Cu_error, Re_error, Se_error, Zn_error, Mn_error, Fe_error, Total_Weight, Substrate_RT)

# Check the summary to ensure the data looks correct
print(summary(error_data))

error_data_clean <- na.omit(error_data)

# Ensure there's enough data for meaningful correlation calculations
if(nrow(error_data_clean) > 1) {
  # Compute the Spearman correlation matrix on the cleaned data
  cor_matrix_spearman <- cor(error_data_clean, method = "spearman")
  
  # Check the correlation matrix
  print(cor_matrix_spearman)
  
  # If the above prints a valid matrix, proceed with corrplot or other visualization steps
} else {
  cat("Not enough data for correlation calculation.\n")
}

library(openxlsx)
cor_matrix_spearman_df <- as.data.frame(cor_matrix_spearman)
write.xlsx(cor_matrix_spearman_df, 'Error_Correlation.xlsx')



#PLS-R - ON MEAN VALUES

{
  library(pls)
  pls_model <- plsr(Cu_error ~ Total_Weight + Form + Tube_No + Type_of_Sample + Plot + Cu_ICP, data = dt, scale = TRUE)
  summary(pls_model)
  coef(pls_model)
  
}
