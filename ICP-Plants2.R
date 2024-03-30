# Solitude Plant ICP-analysis
# Tomasz Wlodarczyk
# twlodarczyk@arizona.edu
# 2024 - 17 - 03
#Using dataset with all ICP points and pXRF points 
#Testing homogenity of ICP results for single tube samples etc.
#testing anova

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
}


setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/")
dt <-read.delim("Solitude_pXRF_ICP_AllPoints_metadata.txt")

#results2 dataset creation
{
# Creating new datasets
library(dplyr)

# Assume 'dt' is your original dataset
dt_ICP <- dt %>% filter(Method == "ICP")
dt_PXRF <- dt %>% filter(Method == "PXRF")

results2 <- data.frame()
unique_sample_ids <- unique(dt_ICP$SampleID)

for (sample_id in unique_sample_ids) {
  icp_rows <- dt_ICP %>% filter(SampleID == sample_id)
  pxrf_row <- dt_PXRF %>% filter(SampleID == sample_id)
  
  if (nrow(pxrf_row) == 1) {
    for (i in 1:nrow(icp_rows)) {
      # Calculating percent error
      percent_error <- abs((icp_rows[i, 17:27] - pxrf_row[, 17:27]) / icp_rows[i, 17:27]) * 100
      
      # Naming the percent error columns with '_error' suffix
      names(percent_error) <- paste0(names(icp_rows)[17:27], "_error")
      
      # Extracting the element concentration columns from the ICP and PXRF rows
      icp_element_concentration <- icp_rows[i, 17:27]
      pxrf_element_concentration <- pxrf_row[, 17:27]
      
      # Naming the concentration columns to distinguish between ICP and PXRF
      names(icp_element_concentration) <- paste0(names(icp_rows)[17:27], "_ICP")
      names(pxrf_element_concentration) <- paste0(names(pxrf_row)[17:27], "_PXRF")
      
      # Combining the ICP and PXRF concentrations, and percent error into a single row
      temp_result <- c(icp_rows[i, c("Scientific_Name","Family","Status","Form","Duration","Site","Group","Plot","Sample_Name","Tube_No","Type_of_Sample","Total_Weight","Substrate_RT","SampleID", "SampleID2", "Method")], 
                       icp_element_concentration, 
                       pxrf_element_concentration, 
                       percent_error)
      
      # Append the results
      results2 <- rbind(results2, temp_result)
    }
  }
}

# Convert row names to a proper column if needed and check the results
results2 <- data.frame(ID = row.names(results2), results2)
rownames(results2) <- NULL
print(results2)
}


  
# Assuming results2 is your dataset and it already includes the columns "Form" and "Type_of_Sample"
library(dplyr)
library(ggplot2)

# Add a new column 'Shape' to the dataset based on conditions
results2 <- results2 %>%
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
ggplot(results2, aes(x = Total_Weight, y = Cu_error, color = Tube_No, size = Cu_ICP, shape = factor(Shape))) +
  geom_point() +  # Plot points
  labs(x = "Total Weight", y = "Cu Error", title = "All Points") +
  scale_shape_manual(values = c(`16` = 16, `17` = 17, `15` = 15, `18` = 18, `1` = 1, `2` = 2, `0` = 0, `5` = 5)) +  # Map shapes manually
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
    theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))




#Wilcoxon
{
 wilcox.test(results2_mean$Cu_ICP, results2_mean$Cu_PXRF, paired = TRUE)
  boxplot(results2_mean$Cu_ICP, results2_mean$Cu_PXRF,
          names = c("Cu_ICP", "Cu_PXRF"),
          ylab = "Concentration")
  }

#Anova
{
library(dplyr)
library(ggplot2)
library(lmtest)
library(car) 
#model <- lm(Cu_PXRF ~ Cu_ICP * Form + Cu_ICP * Total_Weight + Cu_ICP * Tube_No + Cu_ICP * Plot, data = results2)
#model <- lm(Cu_ICP ~ Cu_PXRF * Form + Cu_PXRF * Total_Weight + Cu_PXRF * Tube_No + Cu_PXRF * Plot, data = results2)
#anova_result <- Anova(model, type="II")  
#print(anova_result)

#model <- lm(Cu_ICP ~ Cu_PXRF/SampleID * Form + Cu_PXRF * Total_Weight + Cu_PXRF * Tube_No + Cu_PXRF * Plot, data = results2)

#summary(model)

#Cu_ICP ~ Cu_PXRF + Form + Total_Weight + Tube_No + Plot + Scientific_Name + (interaction terms of interest)


#I need to average the results
library(dplyr)
#lm(Cu~Method + Total_Weight + Form + Tube_No

   
   library(dplyr)
   
   # Summarize ICP data, including necessary variables
   dt_ICP_means <- dt %>%
     filter(Method == "ICP") %>%
     group_by(SampleID) %>%
     summarise(
       Cu = mean(Cu, na.rm = TRUE),  # Directly naming the mean as 'Cu'
       Total_Weight = first(Total_Weight),  # Assuming Total_Weight doesn't vary within SampleID
       Tube_No = first(Tube_No),             # Assuming Tube_No doesn't vary within SampleID
       Form = first(Form),                   # Assuming Form doesn't vary within SampleID
       .groups = 'drop'
     ) %>%
     mutate(Method = "ICP_Averaged")  # Add method as a new column
   
   # Select PXRF data, ensuring to include the same columns for consistency
   dt_PXRF <- dt %>%
     filter(Method == "PXRF") %>%
     select(SampleID, Cu, Total_Weight, Tube_No, Form, Method)
   
   # Combine the datasets
   combined_dt <- bind_rows(dt_ICP_means, dt_PXRF)

   
   library(car)
   
   # Fit a linear model
   lm_model <- lm(Cu ~ Method + Total_Weight + Tube_No + Form, data = combined_dt)
   
   # Conduct Type II ANOVA on the linear model
   anova_result <- Anova(lm_model, type="II")
   
   # Display the ANOVA table
   print(anova_result)
   
   # You might also be interested in checking the summary of the linear model
   print(summary(lm_model))
   
###############
   #Anova only for ICP data to check variability. No mean.
   
library(lme4)
   model <- lmer(Cu_ICP ~ Total_Weight + Tube_No + Form + 
                   (1 | Scientific_Name/Plot/SampleID), data = results2)
   
   summary(model)   
   anova(model)
   
   #anova, but we need within SampleID not between...
   anova_model <- aov(Cu_ICP ~ as.factor(SampleID), data=results2)
   summary(anova_model)
   
   within_sd <- results2 %>%
     group_by(SampleID) %>%
     summarize(Cu_ICP_sd = sd(Cu_ICP, na.rm = TRUE),
               Cu_ICP_var = var(Cu_ICP, na.rm = TRUE),
               n = n()) %>%
     filter(n > 1) 
   
}

#We now know that number of tubes is a significant factor. R squared, ramse for one tube and two tubes. 
# Calculate variability and correlation for each tube number
{
head(results2)

  numeric_cols <- names(results2)[18:50]
  
  # Use mutate across for the numeric columns to replace them with their mean by SampleID
  # Then, you will group by all other columns to keep them as they are (as metadata)
  results2_mean <- results2 %>%
    group_by(SampleID) %>%
    mutate(across(numeric_cols, ~mean(.x, na.rm = TRUE))) %>%
      group_by(across(1:(which(names(results2) == "SampleID")[1]-1))) %>%
    distinct() %>%
    ungroup() 
  
  

  
one <- subset(results2_mean, Tube_No=="one")
two <- subset(results2_mean, Tube_No=="two")
three <- subset(results2_mean, Tube_No=="three")
twothree <- subset(results2_mean, Tube_No=="three"|Tube_No=="two")
 
rmse <- sqrt(mean((results2_mean$Cu_ICP - results2_mean$Cu_PXRF)^2, na.rm = TRUE)) 
rmse_one <- sqrt(mean((one$Cu_ICP - one$Cu_PXRF)^2, na.rm = TRUE))
rmse_two <- sqrt(mean((two$Cu_ICP - two$Cu_PXRF)^2, na.rm = TRUE))
rmse_three <- sqrt(mean((three$Cu_ICP - three$Cu_PXRF)^2, na.rm = TRUE))
rmse_twothree <- sqrt(mean((twothree$Cu_ICP - twothree$Cu_PXRF)^2, na.rm = TRUE))

r_squared <- summary(lm(Cu_ICP ~ Cu_PXRF, data = results2_mean))$r.squared
r_squared_one <- summary(lm(Cu_ICP ~ Cu_PXRF, data = one))$r.squared
r_squared_two <- summary(lm(Cu_ICP ~ Cu_PXRF, data = two))$r.squared
r_squared_three <- summary(lm(Cu_ICP ~ Cu_PXRF, data = three))$r.squared
r_squared_twothree <- summary(lm(Cu_ICP ~ Cu_PXRF, data = twothree))$r.squared

}
#Mixed effect models
{
library(lme4)

# Assuming 'Cu_ICP' is your response variable and 'Cu_PXRF' is a predictor,
# along with 'Total_Weight', 'Tube_No', and 'Form' as fixed effects.
# 'Scientific_Name', 'Plot', and 'SampleID2' are considered nested random effects.
model <- lmer(Cu_ICP ~ Cu_PXRF + Total_Weight + Tube_No + Form +
                (1|Plot/Scientific_Name/SampleID2), 
              data = results2)

simplified_model <- lmer(Cu_ICP ~ Cu_PXRF + Total_Weight + Tube_No + Form + (1|Plot), data = results2)
more_complex_model <- lmer(Cu_ICP ~ Cu_PXRF + Total_Weight + Tube_No + Form + (1|Plot) + (1|Plot/Scientific_Name), data = results2)


summary(simplified_model)

summary(more_complex_model)




model2 <- lmer(Cu_ICP ~ Cu_PXRF + Total_Weight + Tube_No + Form + 
                (1 | Plot/Scientific_Name/SampleID), 
              data = results2)

summary(model2)




model3 <- lmer(Cu_ICP ~ Cu_PXRF + Total_Weight + Tube_No + Form + 
                 (1 | Plot/Scientific_Name/SampleID), 
               data = results2)
model4 <- lmer(Cu_ICP ~ Cu_PXRF + Tube_No + Form + 
                       (1 | Plot/Scientific_Name/SampleID), 
                     data = results2)
anova(model3, model4)


model3 <- lmer(Cu_ICP ~ Cu_PXRF + Total_Weight + Tube_No + Form + 
                 (1 | Plot/Scientific_Name/SampleID), 
               data = results2)
model4 <- lmer(Cu_ICP ~ Cu_PXRF + Total_Weight + Form + 
                 (1 | Plot/Scientific_Name/SampleID), 
               data = results2)
anova(model3, model4)


model3 <- lmer(Cu_ICP ~ Cu_PXRF + Total_Weight + Tube_No + Form + 
                 (1 | Plot/Scientific_Name/SampleID), 
               data = results2)
model4 <- lmer(Cu_ICP ~ Cu_PXRF + Total_Weight + Tube_No + 
                 (1 | Plot/Scientific_Name/SampleID), 
               data = results2)
anova(model3, model4)



library(lme4)

model222 <- lmer(Cu_ICP ~ Cu_PXRF + Form + Total_Weight + Tube_No + Scientific_Name + (1|Plot/SampleID), data = results2)
summary(model222)

model222 <- lmer(Cu_PXRF ~ Cu_ICP + Form + Total_Weight + Tube_No + Scientific_Name + (1|Plot/SampleID), data = results2)
summary(model222)

}
