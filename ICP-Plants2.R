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
  library(car)
  
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



results2_mean <- results2 %>%
  group_by(SampleID) %>%
  mutate(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  distinct(SampleID, .keep_all = TRUE) %>%
  ungroup()

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
       Form = first(Form), 
       Type_of_Sample=first(Type_of_Sample),
       .groups = 'drop'
     ) %>%
     mutate(Method = "ICP_Averaged")  # Add method as a new column
   
   # Select PXRF data, ensuring to include the same columns for consistency
   dt_PXRF <- dt %>%
     filter(Method == "PXRF") %>%
     select(SampleID, Cu, Total_Weight, Tube_No, Form, Method, Type_of_Sample)
   

   combined_dt <- bind_rows(dt_ICP_means, dt_PXRF)

   
   
   #change three to two in tubes
   combined_dt <- combined_dt %>%
     mutate(Tube_No = if_else(Tube_No == "three", "two", Tube_No))
   
   
   
   # Plotting with customized shapes
   ggplot(combined_dt, aes(x = Tube_No, y = Cu)) +
     geom_boxplot()   # Plot points
   cu_one <- combined_dt$Cu[combined_dt$Tube_No == "one"]
   cu_two <- combined_dt$Cu[combined_dt$Tube_No == "two"]
   wilcox.test(cu_one, cu_two) # there's no difference

   
   library(car)
   
 head(combined_dt)
   lm_model <- lm(Cu ~ Method + Total_Weight + Tube_No + Form + Type_of_Sample, data = combined_dt)
   anova_result <- Anova(lm_model, type="II")
   print(anova_result)
   print(summary(lm_model))
   
   
   results2_mean <- results2_mean %>%
     mutate(Tube_No = if_else(Tube_No == "three", "two", Tube_No))
   
   lm_model2 <- lm(Cu_ICP~Cu_PXRF + Total_Weight + Tube_No + Form + Type_of_Sample, data = results2_mean)
   anova_result2 <- Anova(lm_model2, type="II")
   print(anova_result2)
   print(summary(lm_model2))
   
   
   lm_model2 <- lm(Zn_ICP~Zn_PXRF + Total_Weight + Tube_No + Form + Type_of_Sample, data = results2_mean)
   anova_result2 <- Anova(lm_model2, type="II")
   print(anova_result2)
   print(summary(lm_model2))
   
   
   lm_model2 <- lm(Fe_ICP~Fe_PXRF + Total_Weight + Tube_No + Form + Type_of_Sample, data = results2_mean)
   anova_result2 <- Anova(lm_model2, type="II")
   print(anova_result2)
   print(summary(lm_model2))
   
   lm_model2 <- lm(Se_ICP~Se_PXRF + Total_Weight + Tube_No + Form + Type_of_Sample, data = results2_mean)
   anova_result2 <- Anova(lm_model2, type="II")
   print(anova_result2)
   print(summary(lm_model2))
   
   lm_model2 <- lm(Mn_ICP~Mn_PXRF + Total_Weight + Tube_No + Form + Type_of_Sample, data = results2_mean)
   anova_result2 <- Anova(lm_model2, type="II")
   print(anova_result2)
   print(summary(lm_model2))
   
   lm_model2 <- lm(Re_ICP~Re_PXRF + Total_Weight + Tube_No + Form + Type_of_Sample, data = results2_mean)
   anova_result2 <- Anova(lm_model2, type="II")
   print(anova_result2)
   print(summary(lm_model2))
   
   lm_model2 <- lm(Ti_ICP~Ti_PXRF + Total_Weight + Tube_No + Form + Type_of_Sample, data = results2_mean)
   anova_result2 <- Anova(lm_model2, type="II")
   print(anova_result2)
   print(summary(lm_model2))
   
   
   #Tidyverse again to have a column method but I want to keep all my columns and elements
   ## Summarize ICP data in to have Method column and all elements

   {
   library(dplyr)
   
   # Calculate mean values for ICP data for each SampleID
   dt_ICP_means <- dt %>%
     filter(Method == "ICP") %>%
     group_by(SampleID) %>%
     summarise(across(everything(), ~ if(is.numeric(.x)) mean(.x, na.rm = TRUE) else first(.x)),
               .groups = 'drop')
   
   # Ensure that the Method column reflects the averaged ICP data
   dt_ICP_means <- dt_ICP_means %>%
     mutate(Method = "ICP_Averaged")
   
   # Select PXRF data
   # No need to summarise PXRF data as it assumes only one row per SampleID
   dt_PXRF <- dt %>%
     filter(Method == "PXRF")
   
   # Combine the averaged ICP data with PXRF data
   combined_dt <- bind_rows(dt_ICP_means, dt_PXRF)
   

   
   ## Add Percent Error to combined_dt
   library(dplyr)
   
   # Split data into ICP and PXRF
   dt_ICP <- combined_dt %>% filter(Method == "ICP_Averaged")
   dt_PXRF <- combined_dt %>% filter(Method == "PXRF")
   
   # Initialize an empty list to store the results
   results_list <- list()
   
   unique_sample_ids <- unique(dt_ICP$SampleID)
   
   for (sample_id in unique_sample_ids) {
     icp_rows <- dt_ICP %>% filter(SampleID == sample_id)
     pxrf_row <- dt_PXRF %>% filter(SampleID == sample_id)
     
     if (nrow(pxrf_row) == 1) {
       for (i in 1:nrow(icp_rows)) {
         # Calculating percent error for each element
         percent_error <- abs((icp_rows[i, 17:27] - pxrf_row[, 17:27]) / icp_rows[i, 17:27]) * 100
         names(percent_error) <- paste0(names(icp_rows)[17:27], "_error")
         
         # Combine metadata, ICP and PXRF concentrations, and percent error into a single row
         temp_result <- c(icp_rows[i, 1:16], icp_rows[i, 17:27], pxrf_row[, 17:27], percent_error)
         names(temp_result)[17:27] <- paste0(names(temp_result)[17:27], "_ICP")
         names(temp_result)[28:38] <- paste0(names(temp_result)[17:27], "_PXRF", sep = "")
         
         # Append the temp_result to the results_list
         results_list[[length(results_list) + 1]] <- temp_result
       }
     }
   }
   
   # Combine all rows in the list into a single data frame
   results3 <- do.call(rbind, results_list)
   
   # Convert the results into a data frame and assign row names as ID
   results3 <- data.frame(ID = 1:nrow(results3), results3)
   rownames(results3) <- NULL
   
   # Print the results
   print(head(results3))
   
}
   
   results3[,18:50] <- sapply(results3[,18:50],as.numeric)
   results3[,1:17] <- sapply(results3[,1:17],as.character)
   results3[,14:15] <- sapply(results3[,14:15],as.numeric)
   
   results3 <- results3 %>%
     mutate(Tube_No = if_else(Tube_No == "three", "two", Tube_No))
   
   #lm_model <- lm(Cu ~ Method + Total_Weight + Tube_No + Form + Type_of_Sample, data = dt_ICP_means)
   #anova_result <- Anova(lm_model, type="II")
   #print(anova_result)
   #print(summary(lm_model))
   
   #lm_model <- lm(Zn ~ Method + Total_Weight + Tube_No + Form + Type_of_Sample, data = combined_dt)
   #anova_result <- Anova(lm_model, type="II")
   #print(anova_result)
   #print(summary(lm_model))
   
   
   lm_model <- lm(Cu_error ~ Tube_No, data = results3)
   anova_result <- Anova(lm_model, type="II")
   print(anova_result)
   print(summary(lm_model))
   aov1 <- aov(Cu_error~Tube_No, data=results3)
   summary(aov1)
   
#Checking tube 1 vs tube 2
   {
  ggplot(results3, aes(x = Total_Weight, y = Cu_error, color = Tube_No)) +
     geom_point() +  # Plot points
     labs(x = "Total Weight", y = "Cu", title = "All Points") +
     theme_minimal() +
     theme(axis.text.x = element_text(hjust = 1))
  
  #ggplot(results2, aes(x = S_PXRF, y = Scientific_Name, color = Plot)) +
  #  geom_boxplot() +  # Plot points
  #  labs(x = "Total Weight", y = "Cu", title = "All Points") +
  #  theme_minimal() +
  #  theme(axis.text.x = element_text(hjust = 1))
   
   Cu <- ggplot(results3, aes(x = Tube_No, y =Cu_error, color = Tube_No)) +
     geom_boxplot() +  # Plot points
     labs(x = "number of tubes", y = "Cu Error", title = "All Points") +
     theme_minimal() +
     theme(axis.text.x = element_text(hjust = 1))
   
   Se <- ggplot(results3, aes(x = Tube_No, y = Se_error, color = Tube_No)) +
     geom_boxplot() +  # Plot points
     labs(x = "number of tubes", y = "Se Error", title = "All Points") +
     theme_minimal() +
     theme(axis.text.x = element_text(hjust = 1))
   
   Re <- ggplot(results3, aes(x = Tube_No, y = Re_error, color = Tube_No)) +
     geom_boxplot() +  # Plot points
     labs(x = "number of tubes", y = "Re Error", title = "All Points") +
     theme_minimal() +
     theme(axis.text.x = element_text(hjust = 1))
   
   Zn <- ggplot(results3, aes(x = Tube_No, y = Zn_error, color = Tube_No)) +
     geom_boxplot() +  # Plot points
     labs(x = "number of tubes", y = "Zn Error", title = "All Points") +
     theme_minimal() +
     theme(axis.text.x = element_text(hjust = 1))
   
   Mn <- ggplot(results3, aes(x = Tube_No, y = Mn_error, color = Tube_No)) +
     geom_boxplot() +  # Plot points
     labs(x = "number of tubes", y = "Mn Error", title = "All Points") +
     theme_minimal() +
     theme(axis.text.x = element_text(hjust = 1))
   
   Fe <-  ggplot(results3, aes(x = Tube_No, y = Fe_error, color = Tube_No)) +
     geom_boxplot() +  # Plot points
     labs(x = "number of tubes", y = "Fe Error", title = "All Points") +
     theme_minimal() +
     theme(axis.text.x = element_text(hjust = 1))
   
   Ti <-  ggplot(results3, aes(x = Tube_No, y = Ti_error, color = Tube_No)) +
     geom_boxplot() +  # Plot points
     labs(x = "number of tubes", y = "Ti Error", title = "All Points") +
     theme_minimal() +
     theme(axis.text.x = element_text(hjust = 1))
   P <-  ggplot(results3, aes(x = Tube_No, y = P_error, color = Tube_No)) +
     geom_boxplot() +  # Plot points
     labs(x = "number of tubes", y = "P Error", title = "All Points") +
     theme_minimal() +
     theme(axis.text.x = element_text(hjust = 1))
   
   S <-  ggplot(results3, aes(x = Tube_No, y = S_error, color = Tube_No)) +
     geom_boxplot() +  # Plot points
     labs(x = "number of tubes", y = "S Error", title = "All Points") +
     theme_minimal() +
     theme(axis.text.x = element_text(hjust = 1))
   
   
   ggarrange(Cu,Se,Re,Zn,Mn,Fe,Ti,P,S,
             ncol = 3, nrow = 3, 
             common.legend = FALSE, legend = "bottom")
   
   wilcox.test(Cu_error~Tube_No, data=results3)
   wilcox.test(Se_error~Tube_No, data=results3)
   wilcox.test(Re_error~Tube_No, data=results3)
   wilcox.test(Zn_error~Tube_No, data=results3)
   wilcox.test(Mn_error~Tube_No, data=results3)
   wilcox.test(Fe_error~Tube_No, data=results3)
   wilcox.test(Ti_error~Tube_No, data=results3)
   wilcox.test(P_error~Tube_No, data=results3)
   wilcox.test(S_error~Tube_No, data=results3)
   
   str(results3)
   
   }
   
   
   
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
