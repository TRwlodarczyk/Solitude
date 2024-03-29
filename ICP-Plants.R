# Solitude Plant ICP-analysis
# Tomasz Wlodarczyk
# twlodarczyk@arizona.edu
# 2024 - 17 - 03


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
setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/ICP-Data")
dt <-read.delim("ICP_Combined.txt")

summary_dt <- dt %>%
  group_by(SampleID) %>%
  summarise(across(everything(), list(mean = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE)), .names = "{.col}_{.fn}"))


 write.xlsx(summary_dt, "ICP_Combined_summary.xlsx")
 
 
 
 #Check how ICP deviate from pXRF
 setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/")
 dt <-read.delim("Solitude_pXRF_ICP_AllPoints.txt")
 
 
 Cu <- ggplot(dt, aes(x = reorder(SampleID, Cu, FUN = median),
                      y = Cu, SampleID=SampleID)) +
   geom_point(size = 1.3, stroke = 1, aes(color = Method, fill = Method)) +
   theme_classic() +
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=11),
         axis.title.x = element_text(size = 19),
         axis.text.y = element_text(size=12, face="italic"),
         axis.title.y = element_blank(),
         legend.key.size = unit(1, "lines"),
         legend.text = element_text(size = 11), 
         legend.title = element_text(size=15, face = "bold"))+
   guides(color = guide_legend(override.aes = list(size = 3.5)),
          shape = guide_legend(override.aes = list(size = 3.5))) +
   ylab("Validation")
 
 Cu 

 

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
       percent_error <- abs((icp_rows[i, 4:15] - pxrf_row[, 4:15]) / icp_rows[i, 4:15]) * 100
       
       # Naming the percent error columns with '_error' suffix
       names(percent_error) <- paste0(names(icp_rows)[4:15], "_error")
       
       # Extracting the element concentration columns from the ICP and PXRF rows
       icp_element_concentration <- icp_rows[i, 4:15]
       pxrf_element_concentration <- pxrf_row[, 4:15]
       
       # Naming the concentration columns to distinguish between ICP and PXRF
       names(icp_element_concentration) <- paste0(names(icp_rows)[4:15], "_ICP")
       names(pxrf_element_concentration) <- paste0(names(pxrf_row)[4:15], "_PXRF")
       
       # Combining the ICP and PXRF concentrations, and percent error into a single row
       temp_result <- c(icp_rows[i, c("SampleID", "SampleID2", "Method", "Total_Weight", "Substrate_RT")], 
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
 
 
 #filtruj punkty ktore maja blad powyzej 50% i wyrzucaj je
sum(!is.na(results2$Cu_ICP))
sum(!is.na(results2_error50$Cu_ICP))

sum(!is.na(results2$Zn_ICP))
sum(!is.na(results2_Znerror50$Zn_ICP))

sum(!is.na(results2$Fe_ICP))
sum(!is.na(results2_Feerror50$Fe_ICP))

#results2_error50 <- results2 %>%
#  mutate(Cu_ICP = ifelse(Cu_error > 50, NA, Cu_ICP))

#results2_Znerror50 <- results2 %>%
#  mutate(Zn_ICP = ifelse(Zn_error > 50, NA, Zn_ICP))

results2 <- results2 %>%
  mutate(Zn_ICP = ifelse(Zn_error > 50, NA, Zn_ICP))

results2_Feerror50 <- results2 %>%
  mutate(Fe_ICP = ifelse(Fe_error > 50, NA, Fe_ICP))

results2_mean <- results2_error50 %>%
  group_by(SampleID) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE), .groups = 'drop') 


#for Zn
results2_mean <- results2_Znerror50 %>%
  group_by(SampleID) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE), .groups = 'drop') 

#for Iron
results2_mean <- results2_Feerror50 %>%
  group_by(SampleID) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE), .groups = 'drop') 
 #str(results2)
 
 
 
 results2_median <- results2 %>%
   group_by(SampleID) %>%
   summarize(across(where(is.numeric), median, na.rm = TRUE), .groups = 'drop')
 
 
 results2_mean <- results2 %>%
   group_by(SampleID) %>%
   summarize(across(where(is.numeric), mean, na.rm = TRUE), .groups = 'drop')
 
 
 results2_min <- results2 %>%
   group_by(SampleID) %>%
   summarize(across(where(is.numeric), ~ if (all(is.na(.x))) {NA} else {min(.x, na.rm = TRUE)}), .groups = 'drop')

 
 a <- ggplot(results2_median, aes(x = Cu_PXRF, y = Cu_error, color = Substrate_RT)) +
   geom_point() +  # Plot points
   labs(x = "Cu PXRF", y = "Cu_Error", title="Median") +
   theme_minimal() +
   theme(axis.text.x = element_text(hjust = 1))
 
 a2 <- ggplot(results2_median, aes(x = Cu_PXRF, y = Cu_error, color = Total_Weight)) +
   geom_point() +  # Plot points
   labs(x = "Cu PXRF", y = "Cu_Error", title="Median") +
   theme_minimal() +
   theme(axis.text.x = element_text(hjust = 1))
 
 b <- ggplot(results2_median, aes(x = Total_Weight, y = Cu_error, color = Cu_PXRF)) +
   geom_point() +  # Plot points
   labs(x = "Total_weight", y = "Cu_Error", title="Median") +
   theme_minimal() +
   theme(axis.text.x = element_text(hjust = 1))
 
 
 c <- ggplot(results2_median, aes(x = Substrate_RT, y = Cu_error, color = Cu_PXRF)) +
   geom_point() +  # Plot points
   labs(x = "Substrate_RT", y = "Cu_Error", title="Median") +
   theme_minimal() +
   theme(axis.text.x = element_text(hjust = 1))
 
 
 
 d <- ggplot(results2_mean, aes(x = Cu_PXRF, y = Cu_error, color = Substrate_RT)) +
   geom_point() +  # Plot points
   labs(x = "Cu PXRF", y = "Cu_Error", title="Mean") +
   theme_minimal() +
   theme(axis.text.x = element_text(hjust = 1))
 
 
 e <- ggplot(results2_mean, aes(x = Total_Weight, y = Cu_error, color = Cu_PXRF)) +
   geom_point() +  # Plot points
   labs(x = "Total_weight", y = "Cu_Error", title="Mean") +
   theme_minimal() +
   theme(axis.text.x = element_text(hjust = 1))
 
 
 f <- ggplot(results2_mean, aes(x = Substrate_RT, y = Cu_error, color = Cu_PXRF)) +
   geom_point() +  # Plot points
   labs(x = "Substrate_RT", y = "Cu_Error", title="Mean") +
   theme_minimal() +
   theme(axis.text.x = element_text(hjust = 1))
 
 
 
 g <- ggplot(results2_min, aes(x = Cu_PXRF, y = Cu_error, color = Substrate_RT)) +
   geom_point() +  # Plot points
   labs(x = "Cu PXRF", y = "Cu_Error", title="Min") +
   theme_minimal() +
   theme(axis.text.x = element_text(hjust = 1))
 
 
 h <- ggplot(results2_min, aes(x = Total_Weight, y = Cu_error, color = Cu_PXRF)) +
   geom_point() +  # Plot points
   labs(x = "Total_weight", y = "Cu_Error", title="Min") +
   theme_minimal() +
   theme(axis.text.x = element_text(hjust = 1))
 
 
 i <- ggplot(results2_min, aes(x = Substrate_RT, y = Cu_error, color = Cu_PXRF)) +
   geom_point() +  # Plot points
   labs(x = "Substrate_RT", y = "Cu_Error", title="Min") +
   theme_minimal() +
   theme(axis.text.x = element_text(hjust = 1))
 
 
 
 
 ggarrange(a,b,c,d,e,f,g,h,i,
           ncol = 3, nrow = 3, 
           common.legend = FALSE, legend = "bottom")

 
cor(results2_median$Cu_error, results2_median$Cu_PXRF, method = "spearman", use = "complete.obs")
cor(results2_median$Cu_error, results2_median$Total_Weight, method = "spearman", use = "complete.obs")
cor(results2_median$Cu_error, results2_median$Substrate_RT, method = "spearman", use = "complete.obs")



cor(results2_mean$Cu_error, results2_mean$Cu_PXRF, method = "spearman", use = "complete.obs")
cor(results2_mean$Cu_error, results2_mean$Total_Weight, method = "spearman", use = "complete.obs")
cor(results2_mean$Cu_error, results2_mean$Substrate_RT, method = "spearman", use = "complete.obs")


cor(results2_min$Cu_error, results2_min$Cu_PXRF, method = "spearman", use = "complete.obs")
cor(results2_min$Cu_error, results2_min$Total_Weight, method = "spearman", use = "complete.obs")
cor(results2_min$Cu_error, results2_min$Substrate_RT, method = "spearman", use = "complete.obs")

results2 <- results2 %>%
  mutate(Category = case_when(
    grepl("_1$", SampleID2) ~ "_1",
    grepl("_2$", SampleID2) ~ "_2",
    grepl("_3$", SampleID2) ~ "_3",
    TRUE ~ "Other"
  ))

results2 <- results2[order(results$Total_Weight),]

ggplot(results2, aes(x = Total_Weight, y = Cu_error, color = Category)) +
  geom_point() +  # Plot points
  geom_text(aes(label = SampleID2), nudge_y = 0.05, check_overlap = TRUE, angle = 45, size = 2) +  # Add text labels
  labs(x = "Total Weight", y = "Cu Percent Error 100* (PXRF - ICP)/ICP", color = "Category") +
  scale_color_manual(values = c("_1" = "blue", "_2" = "green", "_3" = "red", "Other" = "black")) +  # Define custom colors
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))


results2 <- dt
#Creating models

# Creating Predicting values - MEDIAN
{
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Cu <- glm(Cu_ICP ~ Cu_PXRF, data = results2_median, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Cu <- glm(Cu_ICP ~ Cu_PXRF + Total_Weight, data = results2_median, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Cu <- glm(Cu_ICP ~ Cu_PXRF + Substrate_RT, data = results2_median, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Cu)
  
  dt_M1Cu <- results2_median
  cooks_distance <- cooks.distance(M1Cu)
  threshold <- 4 / nrow(results2_median)
  outliers <- which(cooks_distance > threshold)
  dt_M1Cu[outliers, "Cu_ICP"] <- NA
  
  M1Cu_adjusted <- glm(Cu_ICP ~ Cu_PXRF, data = dt_M1Cu, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Cu_adjusted)
  #
  results2_median$Predicted_Cu_M1 <- predict(M1Cu_adjusted, newdata = results2_median, type = "response")
  #
  dt_M2Cu <- results2_median
  cooks_distance <- cooks.distance(M2Cu)
  threshold <- 4 / nrow(results2_median)
  outliers <- which(cooks_distance > threshold)
  dt_M2Cu[outliers, "Cu_ICP"] <- NA
  
  M2Cu_adjusted <- glm(Cu_ICP ~ Cu_PXRF + Total_Weight, data = dt_M2Cu, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M2Cu_adjusted)
  #
  results2_median$Predicted_Cu_M2 <- predict(M2Cu_adjusted, newdata = results2_median, type = "response")
  #
  dt_M3Cu <- results2_median
  cooks_distance <- cooks.distance(M3Cu)
  threshold <- 4 / nrow(results2_median)
  outliers <- which(cooks_distance > threshold)
  dt_M3Cu[outliers, "Cu_ICP"] <- NA
  
  M3Cu_adjusted <- glm(Cu_ICP ~ Cu_PXRF + Substrate_RT, data = dt_M3Cu, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M3Cu_adjusted)
  
  #
  results2_median$Predicted_Cu_M3 <- predict(M3Cu_adjusted, newdata = results2_median, type = "response")
  #
  
  
  #Selenium
  results2_median <- results2_median %>% 
    filter(Se_ICP > 0) 
  
  start_intercept <- mean(results2_median$Se_ICP)  # Basic starting value for intercept
  start_predictors <- c(0, 0)  # Small starting values for predictors
  start_Se_PXRF <- 0  # Example starting value for Se_PXRF
  
  # Fit the model with starting values
  M2Se <- glm(Se_ICP ~ Se_PXRF + Total_Weight, data = results2_median, 
              family = Gamma(link = "identity"), 
              control = glm.control(maxit = 50),
              start = c(start_intercept, start_predictors))
  

  M1Se <- glm(Se_ICP ~ Se_PXRF, data = results2_median, 
              family = Gamma(link = "identity"), 
              control = glm.control(maxit = 50),
              start = c(intercept = start_intercept, Se_PXRF = start_Se_PXRF))
  
  M3Se <- glm(Se_ICP ~ Se_PXRF + Total_Weight, data = results2_median, 
              family = Gamma(link = "identity"), 
              control = glm.control(maxit = 50),
              start = c(start_intercept, start_predictors))
  
  
  
  dt_M1Se <- results2_median
  cooks_distance <- cooks.distance(M1Se)
  threshold <- 4 / nrow(results2_median)
  outliers <- which(cooks_distance > threshold)
  dt_M1Se[outliers, "Se_ICP"] <- NA
  
  M1Se_adjusted <- glm(Se_ICP ~ Se_PXRF, data = dt_M1Se %>% filter(Se_ICP > 0), family = Gamma(link = "identity"), control = glm.control(maxit = 50),start = c(intercept = start_intercept, Se_PXRF = start_Se_PXRF))
  
  dt_M1Se$Predicted_Se_M1 <- predict(M1Se_adjusted, newdata = dt_M1Se, type = "response")
  #
  results2_median$Predicted_Se_M1 <- predict(M1Se_adjusted, newdata = results2_median, type = "response")
  #
  dt_M2Se <- results2_median
  cooks_distance <- cooks.distance(M2Se)
  threshold <- 4 / nrow(results2_median)
  outliers <- which(cooks_distance > threshold)
  dt_M2Se[outliers, "Se_ICP"] <- NA
  
  M2Se_adjusted <- glm(Se_ICP ~ Se_PXRF + Total_Weight, data = dt_M2Se%>% filter(Se_ICP > 0), family = Gamma(link = "identity"), control = glm.control(maxit = 50),start = c(start_intercept, start_predictors))
  
  dt_M2Se$Predicted_Se_M2 <- predict(M2Se_adjusted, newdata = dt_M2Se, type = "response")
  #
  results2_median$Predicted_Se_M2 <- predict(M2Se_adjusted, newdata = results2_median, type = "response")
  #
  dt_M3Se <- results2_median
  cooks_distance <- cooks.distance(M3Se)
  threshold <- 4 / nrow(results2_median)
  outliers <- which(cooks_distance > threshold)
  dt_M3Se[outliers, "Se_ICP"] <- NA
  
  M3Se_adjusted <- glm(Se_ICP ~ Se_PXRF + Substrate_RT, data = dt_M3Se%>% filter(Se_ICP > 0), family = Gamma(link = "identity"), control = glm.control(maxit = 50),start = c(start_intercept, start_predictors))
  
  dt_M3Se$Predicted_Se_M3 <- predict(M3Se_adjusted, newdata = dt_M3Se, type = "response")
  
  #
  results2_median$Predicted_Se_M3 <- predict(M3Se_adjusted, newdata = results2_median, type = "response")
  #
  
  
  
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Re <- glm(Re_ICP ~ Re_PXRF, data = results2_median, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Re <- glm(Re_ICP ~ Re_PXRF + Total_Weight, data = results2_median, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Re <- glm(Re_ICP ~ Re_PXRF + Substrate_RT, data = results2_median, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Re)
  summary(M3Re)
  
  dt_M1Re <- results2_median
  cooks_distance <- cooks.distance(M1Re)
  threshold <- 4 / nrow(results2_median)
  outliers <- which(cooks_distance > threshold)
  dt_M1Re[outliers, "Re_ICP"] <- NA
  
  M1Re_adjusted <- glm(Re_ICP ~ Re_PXRF, data = dt_M1Re, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Re_adjusted)
  #
  results2_median$Predicted_Re_M1 <- predict(M1Re_adjusted, newdata = results2_median, type = "response")
  #
  dt_M2Re <- results2_median
  cooks_distance <- cooks.distance(M2Re)
  threshold <- 4 / nrow(results2_median)
  outliers <- which(cooks_distance > threshold)
  dt_M2Re[outliers, "Re_ICP"] <- NA
  
  M2Re_adjusted <- glm(Re_ICP ~ Re_PXRF + Total_Weight, data = dt_M2Re, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  #
  results2_median$Predicted_Re_M2 <- predict(M2Re_adjusted, newdata = results2_median, type = "response")
  #
  dt_M3Re <- results2_median
  cooks_distance <- cooks.distance(M3Re)
  threshold <- 4 / nrow(results2_median)
  outliers <- which(cooks_distance > threshold)
  dt_M3Re[outliers, "Re_ICP"] <- NA
  
  M3Re_adjusted <- glm(Re_ICP ~ Re_PXRF + Substrate_RT, data = dt_M3Re, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M3Re_adjusted)
  #
  results2_median$Predicted_Re_M3 <- predict(M3Re_adjusted, newdata = results2_median, type = "response")
  #
  
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Zn <- glm(Zn_ICP ~ Zn_PXRF, data = results2_median, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Zn <- glm(Zn_ICP ~ Zn_PXRF + Total_Weight, data = results2_median, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Zn <- glm(Zn_ICP ~ Zn_PXRF + Substrate_RT, data = results2_median, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M4Zn <- glm(Zn_ICP ~ Zn_PXRF + Substrate_RT + Total_Weight, data = results2_median, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M3Zn)
  
  dt_M1Zn <- results2_median
  cooks_distance <- cooks.distance(M1Zn)
  threshold <- 4 / nrow(results2_median)
  outliers <- which(cooks_distance > threshold)
  dt_M1Zn[outliers, "Zn_ICP"] <- NA
  
  M1Zn_adjusted <- glm(Zn_ICP ~ Zn_PXRF, data = dt_M1Zn, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  #
  results2_median$Predicted_Zn_M1 <- predict(M1Zn_adjusted, newdata = results2_median, type = "response")
  #
  dt_M2Zn <- results2_median
  cooks_distance <- cooks.distance(M2Zn)
  threshold <- 4 / nrow(results2_median)
  outliers <- which(cooks_distance > threshold)
  dt_M2Zn[outliers, "Zn_ICP"] <- NA
  
  M2Zn_adjusted <- glm(Zn_ICP ~ Zn_PXRF + Total_Weight, data = dt_M2Zn, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  #
  results2_median$Predicted_Zn_M2 <- predict(M2Zn_adjusted, newdata = results2_median, type = "response")
  #
  dt_M3Zn <- results2_median
  cooks_distance <- cooks.distance(M3Zn)
  threshold <- 4 / nrow(results2_median)
  outliers <- which(cooks_distance > threshold)
  dt_M3Zn[outliers, "Zn_ICP"] <- NA
  
  M3Zn_adjusted <- glm(Zn_ICP ~ Zn_PXRF + Substrate_RT, data = dt_M3Zn, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M3Zn_adjusted)
  #
  results2_median$Predicted_Zn_M3 <- predict(M3Zn_adjusted, newdata = results2_median, type = "response")
  #
  
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Mn <- glm(Mn_ICP ~ Mn_PXRF, data = results2_median, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Mn <- glm(Mn_ICP ~ Mn_PXRF + Total_Weight, data = results2_median, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Mn <- glm(Mn_ICP ~ Mn_PXRF + Substrate_RT, data = results2_median, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Mn)
  
  dt_M1Mn <- results2_median
  cooks_distance <- cooks.distance(M1Mn)
  threshold <- 4 / nrow(results2_median)
  outliers <- which(cooks_distance > threshold)
  dt_M1Mn[outliers, "Mn_ICP"] <- NA
  
  M1Mn_adjusted <- glm(Mn_ICP ~ Mn_PXRF, data = dt_M1Mn, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  #
  results2_median$Predicted_Mn_M1 <- predict(M1Mn_adjusted, newdata = results2_median, type = "response")
  #
  dt_M2Mn <- results2_median
  cooks_distance <- cooks.distance(M2Mn)
  threshold <- 4 / nrow(results2_median)
  outliers <- which(cooks_distance > threshold)
  dt_M2Mn[outliers, "Mn_ICP"] <- NA
  
  M2Mn_adjusted <- glm(Mn_ICP ~ Mn_PXRF + Total_Weight, data = dt_M2Mn, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  #
  results2_median$Predicted_Mn_M2 <- predict(M2Mn_adjusted, newdata = results2_median, type = "response")
  #
  dt_M3Mn <- results2_median
  cooks_distance <- cooks.distance(M3Mn)
  threshold <- 4 / nrow(results2_median)
  outliers <- which(cooks_distance > threshold)
  dt_M3Mn[outliers, "Mn_ICP"] <- NA
  
  M3Mn_adjusted <- glm(Mn_ICP ~ Mn_PXRF + Substrate_RT, data = dt_M3Mn, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  
  #
  results2_median$Predicted_Mn_M3 <- predict(M3Mn_adjusted, newdata = results2_median, type = "response")
  #
  
  
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Fe <- glm(Fe_ICP ~ Fe_PXRF, data = results2_median, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Fe <- glm(Fe_ICP ~ Fe_PXRF + Total_Weight, data = results2_median, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Fe <- glm(Fe_ICP ~ Fe_PXRF + Substrate_RT, data = results2_median, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  
  summary(M3Fe)
  dt_M1Fe <- results2_median
  cooks_distance <- cooks.distance(M1Fe)
  threshold <- 4 / nrow(results2_median)
  outliers <- which(cooks_distance > threshold)
  dt_M1Fe[outliers, "Fe_ICP"] <- NA
  
  M1Fe_adjusted <- glm(Fe_ICP ~ Fe_PXRF, data = dt_M1Fe, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Fe_adjusted)
  #
  results2_median$Predicted_Fe_M1 <- predict(M1Fe_adjusted, newdata = results2_median, type = "response")
  #
  dt_M2Fe <- results2_median
  cooks_distance <- cooks.distance(M2Fe)
  threshold <- 4 / nrow(results2_median)
  outliers <- which(cooks_distance > threshold)
  dt_M2Fe[outliers, "Fe_ICP"] <- NA
  
  M2Fe_adjusted <- glm(Fe_ICP ~ Fe_PXRF + Total_Weight, data = dt_M2Fe, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  #
  results2_median$Predicted_Fe_M2 <- predict(M2Fe_adjusted, newdata = results2_median, type = "response")
  #
  dt_M3Fe <- results2_median
  cooks_distance <- cooks.distance(M3Fe)
  threshold <- 4 / nrow(results2_median)
  outliers <- which(cooks_distance > threshold)
  dt_M3Fe[outliers, "Fe_ICP"] <- NA
  
  M3Fe_adjusted <- glm(Fe_ICP ~ Fe_PXRF + Substrate_RT, data = dt_M3Fe, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  
  #
  results2_median$Predicted_Fe_M3 <- predict(M3Fe_adjusted, newdata = results2_median, type = "response")
  #
  
  #write.xlsx(dt, "Solitude2022_Predicted_Cooks-Final2.xlsx")
  #write.table(dt, file='C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/Solitude2022_Predicted_Cooks-Final.csv', sep=",", row.names = F)
  
  }
# SAVING MODEL PERFORMANCE METRICS - MEDIAN
{
#RMSE with col names and save to excel
{
  elements <- c("Cu", "Se", "Re", "Zn", "Mn", "Fe")
  raw_cols <- paste0(elements, "_PXRF")
  icp_cols <- paste0(elements, "_ICP")
  predicted_cols <- lapply(elements, function(el) paste0("Predicted_", el, "_M", 1:3))
  
  # Flattening the list of predicted columns for easier access
  predicted_cols_flat <- unlist(predicted_cols)
  
  library(openxlsx)
  
  # Initialize a data frame to store RMSE results
  rmse_results <- data.frame(Element = character(), Model = character(), RMSE = numeric(), stringsAsFactors = FALSE)
  
  for (i in 1:length(elements)) {
    # RMSE for RAW
    rmse_raw <- sqrt(mean((results2_median[[icp_cols[i]]] - results2_median[[raw_cols[i]]])^2, na.rm = TRUE))
    temp_df <- data.frame(Element = elements[i], Model = "RAW", RMSE = rmse_raw, stringsAsFactors = FALSE)
    rmse_results <- rbind(rmse_results, temp_df)
    
    # RMSE for Predicted models
    for (j in 1:3) {
      pred_col_name <- predicted_cols[[i]][j]
      rmse_pred <- sqrt(mean((results2_median[[icp_cols[i]]] - results2_median[[pred_col_name]])^2, na.rm = TRUE))
      temp_df <- data.frame(Element = elements[i], Model = paste("M", j, sep=""), RMSE = rmse_pred, stringsAsFactors = FALSE)
      rmse_results <- rbind(rmse_results, temp_df)
    }
  }
  
  rmse_results$RMSE <- as.numeric(rmse_results$RMSE)
  colnames(rmse_results) <- c("Element", "Model", "RMSE")
  wb <- createWorkbook()
  addWorksheet(wb, "RMSE Results")
  writeData(wb, "RMSE Results", rmse_results)
  
  # Save the workbook
  saveWorkbook(wb, "RMSE_Cook.xlsx", overwrite = TRUE)
  
}


#NRMSE with col names and save to excel

# Initialize a data frame to store NRMSE results
nrmse_results <- data.frame(Element = character(), Model = character(), NRMSE = numeric(), stringsAsFactors = FALSE)

for (i in 1:length(elements)) {
  mean_icp <- mean(results2_median[[icp_cols[i]]], na.rm = TRUE)  # Calculate mean of ICP values for the element
  
  # NRMSE for RAW
  rmse_raw <- sqrt(mean((results2_median[[icp_cols[i]]] - results2_median[[raw_cols[i]]])^2, na.rm = TRUE))
  nrmse_raw <- rmse_raw / mean_icp
  temp_df <- data.frame(Element = elements[i], Model = "RAW", NRMSE = nrmse_raw, stringsAsFactors = FALSE)
  nrmse_results <- rbind(nrmse_results, temp_df)
  
  # NRMSE for Predicted models
  for (j in 1:3) {
    pred_col_name <- predicted_cols[[i]][j]
    rmse_pred <- sqrt(mean((results2_median[[icp_cols[i]]] - results2_median[[pred_col_name]])^2, na.rm = TRUE))
    nrmse_pred <- rmse_pred / mean_icp
    temp_df <- data.frame(Element = elements[i], Model = paste("M", j, sep=""), NRMSE = nrmse_pred, stringsAsFactors = FALSE)
    nrmse_results <- rbind(nrmse_results, temp_df)
  }
}


wb_nrmse <- createWorkbook()
addWorksheet(wb_nrmse, "NRMSE Results")
writeData(wb_nrmse, "NRMSE Results", nrmse_results)
saveWorkbook(wb_nrmse, "NRMSE_Cook.xlsx", overwrite = TRUE)



#MAE
####MAE with col names and save to excel######

# Initialize a data frame to store MAE results
mae_results <- data.frame(Element = character(), Model = character(), MAE = numeric(), stringsAsFactors = FALSE)

for (i in 1:length(elements)) {
  # MAE for RAW
  mae_raw <- mean(abs(results2_median[[icp_cols[i]]] - results2_median[[raw_cols[i]]]), na.rm = TRUE)
  temp_df <- data.frame(Element = elements[i], Model = "RAW", MAE = mae_raw, stringsAsFactors = FALSE)
  mae_results <- rbind(mae_results, temp_df)
  
  # MAE for Predicted models
  for (j in 1:3) {
    pred_col_name <- predicted_cols[[i]][j]
    mae_pred <- mean(abs(results2_median[[icp_cols[i]]] - results2_median[[pred_col_name]]), na.rm = TRUE)
    temp_df <- data.frame(Element = elements[i], Model = paste("M", j, sep=""), MAE = mae_pred, stringsAsFactors = FALSE)
    mae_results <- rbind(mae_results, temp_df)
  }
}

library(openxlsx)

wb_mae <- createWorkbook()
addWorksheet(wb_mae, "MAE Results")
writeData(wb_mae, "MAE Results", mae_results)
saveWorkbook(wb_mae, "MAE_Cook.xlsx", overwrite = TRUE)



#R-Squared
####R-squared with col names and save to excel######

# Initialize a data frame to store R-squared results
r_squared_results <- data.frame(Element = character(), Model = character(), R_squared = numeric(), stringsAsFactors = FALSE)

for (i in 1:length(elements)) {
  # R-squared for RAW
  lm_model_raw <- lm(results2_median[[icp_cols[i]]] ~ results2_median[[raw_cols[i]]], data = results2_median, na.action = na.exclude)
  r_squared_raw <- summary(lm_model_raw)$r.squared
  temp_df <- data.frame(Element = elements[i], Model = "RAW", R_squared = r_squared_raw, stringsAsFactors = FALSE)
  r_squared_results <- rbind(r_squared_results, temp_df)
  
  # R-squared for Predicted models
  for (j in 1:3) {
    pred_col_name <- predicted_cols[[i]][j]
    lm_model_pred <- lm(results2_median[[icp_cols[i]]] ~ results2_median[[pred_col_name]], data = results2_median, na.action = na.exclude)
    r_squared_pred <- summary(lm_model_pred)$r.squared
    temp_df <- data.frame(Element = elements[i], Model = paste("M", j, sep=""), R_squared = r_squared_pred, stringsAsFactors = FALSE)
    r_squared_results <- rbind(r_squared_results, temp_df)
  }
}


wb_r_squared <- createWorkbook()
addWorksheet(wb_r_squared, "R-squared Results")
writeData(wb_r_squared, "R-squared Results", r_squared_results)
saveWorkbook(wb_r_squared, "R_Cook.xlsx", overwrite = TRUE)



#RPD
#RPD with col names and save to excel

# Initialize a data frame to store RPD results
rpd_results <- data.frame(Element = character(), Model = character(), RPD = numeric(), stringsAsFactors = FALSE)

for (i in 1:length(elements)) {
  # Standard deviation for ICP data
  sdr <- sd(results2_median[[icp_cols[i]]], na.rm = TRUE)
  
  # RPD for RAW
  rmse_raw <- sqrt(mean((results2_median[[icp_cols[i]]] - results2_median[[raw_cols[i]]])^2, na.rm = TRUE))
  rpd_raw <- sdr / rmse_raw
  temp_df <- data.frame(Element = elements[i], Model = "RAW", RPD = rpd_raw, stringsAsFactors = FALSE)
  rpd_results <- rbind(rpd_results, temp_df)
  
  # RPD for Predicted models
  for (j in 1:3) {
    pred_col_name <- predicted_cols[[i]][j]
    rmse_pred <- sqrt(mean((results2_median[[icp_cols[i]]] - results2_median[[pred_col_name]])^2, na.rm = TRUE))
    rpd_pred <- sdr / rmse_pred
    temp_df <- data.frame(Element = elements[i], Model = paste("M", j, sep=""), RPD = rpd_pred, stringsAsFactors = FALSE)
    rpd_results <- rbind(rpd_results, temp_df)
  }
}

wb_rpd <- createWorkbook()
addWorksheet(wb_rpd, "RPD Results")
writeData(wb_rpd, "RPD Results", rpd_results)
saveWorkbook(wb_rpd, "RPD_Cook.xlsx", overwrite = TRUE)


#ICC
#ICC with col names, only ICC2 for single random raster value


# Initialize a data frame to store ICC results
icc_results <- data.frame(Element = character(), Model = character(), ICC_Value = numeric(), stringsAsFactors = FALSE)

for (i in 1:length(elements)) {
  # ICC for RAW
  dt_icc_raw <- results2_median[, c(icp_cols[i], raw_cols[i])]
  dt_icc_raw <- na.omit(dt_icc_raw)
  icc_raw <- ICC(dt_icc_raw, missing=TRUE, alpha=.05, lmer=TRUE, check.keys=FALSE)
  icc_value_raw <- icc_raw$results["Single_random_raters", "ICC"]
  temp_df <- data.frame(Element = elements[i], Model = "RAW", ICC_Value = icc_value_raw, stringsAsFactors = FALSE)
  icc_results <- rbind(icc_results, temp_df)
  
  # ICC for Predicted models
  for (j in 1:3) {
    pred_col_name <- predicted_cols[[i]][j]
    dt_icc_pred <- results2_median[, c(icp_cols[i], pred_col_name)]
    dt_icc_pred <- na.omit(dt_icc_pred)
    icc_pred <- ICC(dt_icc_pred, missing=TRUE, alpha=.05, lmer=TRUE, check.keys=FALSE)
    icc_value_pred <- icc_pred$results["Single_random_raters", "ICC"]
    temp_df <- data.frame(Element = elements[i], Model = paste("M", j, sep=""), ICC_Value = icc_value_pred, stringsAsFactors = FALSE)
    icc_results <- rbind(icc_results, temp_df)
  }
}

wb_icc <- createWorkbook()
addWorksheet(wb_icc, "ICC Results")
writeData(wb_icc, "ICC Results", icc_results)
saveWorkbook(wb_icc, "ICC_Cook.xlsx", overwrite = TRUE)

}



#Creating models

# Creating Predicting values - MIN
{
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Cu <- glm(Cu_ICP ~ Cu_PXRF, data = results2_min, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Cu <- glm(Cu_ICP ~ Cu_PXRF + Total_Weight, data = results2_min, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Cu <- glm(Cu_ICP ~ Cu_PXRF + Substrate_RT, data = results2_min, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Cu)
  
  dt_M1Cu <- results2_min
  cooks_distance <- cooks.distance(M1Cu)
  threshold <- 4 / nrow(results2_min)
  outliers <- which(cooks_distance > threshold)
  dt_M1Cu[outliers, "Cu_ICP"] <- NA
  
  M1Cu_adjusted <- glm(Cu_ICP ~ Cu_PXRF, data = dt_M1Cu, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Cu_adjusted)
  #
  results2_min$Predicted_Cu_M1 <- predict(M1Cu_adjusted, newdata = results2_min, type = "response")
  #
  dt_M2Cu <- results2_min
  cooks_distance <- cooks.distance(M2Cu)
  threshold <- 4 / nrow(results2_min)
  outliers <- which(cooks_distance > threshold)
  dt_M2Cu[outliers, "Cu_ICP"] <- NA
  
  M2Cu_adjusted <- glm(Cu_ICP ~ Cu_PXRF + Total_Weight, data = dt_M2Cu, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M2Cu_adjusted)
  #
  results2_min$Predicted_Cu_M2 <- predict(M2Cu_adjusted, newdata = results2_min, type = "response")
  #
  dt_M3Cu <- results2_min
  cooks_distance <- cooks.distance(M3Cu)
  threshold <- 4 / nrow(results2_min)
  outliers <- which(cooks_distance > threshold)
  dt_M3Cu[outliers, "Cu_ICP"] <- NA
  
  M3Cu_adjusted <- glm(Cu_ICP ~ Cu_PXRF + Substrate_RT, data = dt_M3Cu, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M3Cu_adjusted)
  
  #
  results2_min$Predicted_Cu_M3 <- predict(M3Cu_adjusted, newdata = results2_min, type = "response")
  #
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  #NEW CODE HERE
  # Ensuring Se_ICP is strictly greater than 0
  results2_min <- results2_min %>% 
    filter(Se_ICP > 0) 
  
  start_intercept <- mean(results2_min$Se_ICP)  # Basic starting value for intercept
  start_predictors <- c(0, 0)  # Small starting values for predictors
  
  # Fit the model with starting values
  M2Se <- glm(Se_ICP ~ Se_PXRF + Total_Weight, data = results2_min, 
              family = Gamma(link = "identity"), 
              control = glm.control(maxit = 50),
              start = c(start_intercept, start_predictors))
  
  start_Se_PXRF <- 0  # Example starting value for Se_PXRF
  
  # Fit the model with adjusted starting values
  M1Se <- glm(Se_ICP ~ Se_PXRF, data = results2_min, 
              family = Gamma(link = "identity"), 
              control = glm.control(maxit = 50),
              start = c(intercept = start_intercept, Se_PXRF = start_Se_PXRF))
  
  M3Se <- glm(Se_ICP ~ Se_PXRF + Total_Weight, data = results2_min, 
              family = Gamma(link = "identity"), 
              control = glm.control(maxit = 50),
              start = c(start_intercept, start_predictors))
  

  
  dt_M1Se <- results2_min
  cooks_distance <- cooks.distance(M1Se)
  threshold <- 4 / nrow(results2_min)
  outliers <- which(cooks_distance > threshold)
  dt_M1Se[outliers, "Se_ICP"] <- NA
  
  M1Se_adjusted <- glm(Se_ICP ~ Se_PXRF, data = dt_M1Se %>% filter(Se_ICP > 0), family = Gamma(link = "identity"), control = glm.control(maxit = 50),start = c(intercept = start_intercept, Se_PXRF = start_Se_PXRF))

  dt_M1Se$Predicted_Se_M1 <- predict(M1Se_adjusted, newdata = dt_M1Se, type = "response")
  #
  results2_min$Predicted_Se_M1 <- predict(M1Se_adjusted, newdata = results2_min, type = "response")
  #
  dt_M2Se <- results2_min
  cooks_distance <- cooks.distance(M2Se)
  threshold <- 4 / nrow(results2_min)
  outliers <- which(cooks_distance > threshold)
  dt_M2Se[outliers, "Se_ICP"] <- NA
  
  M2Se_adjusted <- glm(Se_ICP ~ Se_PXRF + Total_Weight, data = dt_M2Se%>% filter(Se_ICP > 0), family = Gamma(link = "identity"), control = glm.control(maxit = 50),start = c(start_intercept, start_predictors))

  dt_M2Se$Predicted_Se_M2 <- predict(M2Se_adjusted, newdata = dt_M2Se, type = "response")
  #
  results2_min$Predicted_Se_M2 <- predict(M2Se_adjusted, newdata = results2_min, type = "response")
  #
  dt_M3Se <- results2_min
  cooks_distance <- cooks.distance(M3Se)
  threshold <- 4 / nrow(results2_min)
  outliers <- which(cooks_distance > threshold)
  dt_M3Se[outliers, "Se_ICP"] <- NA
  
  M3Se_adjusted <- glm(Se_ICP ~ Se_PXRF + Substrate_RT, data = dt_M3Se%>% filter(Se_ICP > 0), family = Gamma(link = "identity"), control = glm.control(maxit = 50),start = c(start_intercept, start_predictors))

  dt_M3Se$Predicted_Se_M3 <- predict(M3Se_adjusted, newdata = dt_M3Se, type = "response")
  
  #
  results2_min$Predicted_Se_M3 <- predict(M3Se_adjusted, newdata = results2_min, type = "response")
  #
  
  
  
  
  
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Re <- glm(Re_ICP ~ Re_PXRF, data = results2_min, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Re <- glm(Re_ICP ~ Re_PXRF + Total_Weight, data = results2_min, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Re <- glm(Re_ICP ~ Re_PXRF + Substrate_RT, data = results2_min, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Re)
  summary(M3Re)
  
  dt_M1Re <- results2_min
  cooks_distance <- cooks.distance(M1Re)
  threshold <- 4 / nrow(results2_min)
  outliers <- which(cooks_distance > threshold)
  dt_M1Re[outliers, "Re_ICP"] <- NA
  
  M1Re_adjusted <- glm(Re_ICP ~ Re_PXRF, data = dt_M1Re, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Re_adjusted)
  #
  results2_min$Predicted_Re_M1 <- predict(M1Re_adjusted, newdata = results2_min, type = "response")
  #
  dt_M2Re <- results2_min
  cooks_distance <- cooks.distance(M2Re)
  threshold <- 4 / nrow(results2_min)
  outliers <- which(cooks_distance > threshold)
  dt_M2Re[outliers, "Re_ICP"] <- NA
  
  M2Re_adjusted <- glm(Re_ICP ~ Re_PXRF + Total_Weight, data = dt_M2Re, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  #
  results2_min$Predicted_Re_M2 <- predict(M2Re_adjusted, newdata = results2_min, type = "response")
  #
  dt_M3Re <- results2_min
  cooks_distance <- cooks.distance(M3Re)
  threshold <- 4 / nrow(results2_min)
  outliers <- which(cooks_distance > threshold)
  dt_M3Re[outliers, "Re_ICP"] <- NA
  
  M3Re_adjusted <- glm(Re_ICP ~ Re_PXRF + Substrate_RT, data = dt_M3Re, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M3Re_adjusted)
  #
  results2_min$Predicted_Re_M3 <- predict(M3Re_adjusted, newdata = results2_min, type = "response")
  #
  
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Zn <- glm(Zn_ICP ~ Zn_PXRF, data = results2_min, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Zn <- glm(Zn_ICP ~ Zn_PXRF + Total_Weight, data = results2_min, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Zn <- glm(Zn_ICP ~ Zn_PXRF + Substrate_RT, data = results2_min, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M4Zn <- glm(Zn_ICP ~ Zn_PXRF + Substrate_RT + Total_Weight, data = results2_min, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M3Zn)
  
  dt_M1Zn <- results2_min
  cooks_distance <- cooks.distance(M1Zn)
  threshold <- 4 / nrow(results2_min)
  outliers <- which(cooks_distance > threshold)
  dt_M1Zn[outliers, "Zn_ICP"] <- NA
  
  M1Zn_adjusted <- glm(Zn_ICP ~ Zn_PXRF, data = dt_M1Zn, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  #
  results2_min$Predicted_Zn_M1 <- predict(M1Zn_adjusted, newdata = results2_min, type = "response")
  #
  dt_M2Zn <- results2_min
  cooks_distance <- cooks.distance(M2Zn)
  threshold <- 4 / nrow(results2_min)
  outliers <- which(cooks_distance > threshold)
  dt_M2Zn[outliers, "Zn_ICP"] <- NA
  
  M2Zn_adjusted <- glm(Zn_ICP ~ Zn_PXRF + Total_Weight, data = dt_M2Zn, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  #
  results2_min$Predicted_Zn_M2 <- predict(M2Zn_adjusted, newdata = results2_min, type = "response")
  #
  dt_M3Zn <- results2_min
  cooks_distance <- cooks.distance(M3Zn)
  threshold <- 4 / nrow(results2_min)
  outliers <- which(cooks_distance > threshold)
  dt_M3Zn[outliers, "Zn_ICP"] <- NA
  
  M3Zn_adjusted <- glm(Zn_ICP ~ Zn_PXRF + Substrate_RT, data = dt_M3Zn, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M3Zn_adjusted)
  #
  results2_min$Predicted_Zn_M3 <- predict(M3Zn_adjusted, newdata = results2_min, type = "response")
  #
  
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Mn <- glm(Mn_ICP ~ Mn_PXRF, data = results2_min, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Mn <- glm(Mn_ICP ~ Mn_PXRF + Total_Weight, data = results2_min, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Mn <- glm(Mn_ICP ~ Mn_PXRF + Substrate_RT, data = results2_min, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Mn)
  
  dt_M1Mn <- results2_min
  cooks_distance <- cooks.distance(M1Mn)
  threshold <- 4 / nrow(results2_min)
  outliers <- which(cooks_distance > threshold)
  dt_M1Mn[outliers, "Mn_ICP"] <- NA
  
  M1Mn_adjusted <- glm(Mn_ICP ~ Mn_PXRF, data = dt_M1Mn, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  #
  results2_min$Predicted_Mn_M1 <- predict(M1Mn_adjusted, newdata = results2_min, type = "response")
  #
  dt_M2Mn <- results2_min
  cooks_distance <- cooks.distance(M2Mn)
  threshold <- 4 / nrow(results2_min)
  outliers <- which(cooks_distance > threshold)
  dt_M2Mn[outliers, "Mn_ICP"] <- NA
  
  M2Mn_adjusted <- glm(Mn_ICP ~ Mn_PXRF + Total_Weight, data = dt_M2Mn, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  #
  results2_min$Predicted_Mn_M2 <- predict(M2Mn_adjusted, newdata = results2_min, type = "response")
  #
  dt_M3Mn <- results2_min
  cooks_distance <- cooks.distance(M3Mn)
  threshold <- 4 / nrow(results2_min)
  outliers <- which(cooks_distance > threshold)
  dt_M3Mn[outliers, "Mn_ICP"] <- NA
  
  M3Mn_adjusted <- glm(Mn_ICP ~ Mn_PXRF + Substrate_RT, data = dt_M3Mn, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  
  #
  results2_min$Predicted_Mn_M3 <- predict(M3Mn_adjusted, newdata = results2_min, type = "response")
  #
  
  
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Fe <- glm(Fe_ICP ~ Fe_PXRF, data = results2_min, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Fe <- glm(Fe_ICP ~ Fe_PXRF + Total_Weight, data = results2_min, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Fe <- glm(Fe_ICP ~ Fe_PXRF + Substrate_RT, data = results2_min, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  
  summary(M3Fe)
  dt_M1Fe <- results2_min
  cooks_distance <- cooks.distance(M1Fe)
  threshold <- 4 / nrow(results2_min)
  outliers <- which(cooks_distance > threshold)
  dt_M1Fe[outliers, "Fe_ICP"] <- NA
  
  M1Fe_adjusted <- glm(Fe_ICP ~ Fe_PXRF, data = dt_M1Fe, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Fe_adjusted)
  #
  results2_min$Predicted_Fe_M1 <- predict(M1Fe_adjusted, newdata = results2_min, type = "response")
  #
  dt_M2Fe <- results2_min
  cooks_distance <- cooks.distance(M2Fe)
  threshold <- 4 / nrow(results2_min)
  outliers <- which(cooks_distance > threshold)
  dt_M2Fe[outliers, "Fe_ICP"] <- NA
  
  M2Fe_adjusted <- glm(Fe_ICP ~ Fe_PXRF + Total_Weight, data = dt_M2Fe, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  #
  results2_min$Predicted_Fe_M2 <- predict(M2Fe_adjusted, newdata = results2_min, type = "response")
  #
  dt_M3Fe <- results2_min
  cooks_distance <- cooks.distance(M3Fe)
  threshold <- 4 / nrow(results2_min)
  outliers <- which(cooks_distance > threshold)
  dt_M3Fe[outliers, "Fe_ICP"] <- NA
  
  M3Fe_adjusted <- glm(Fe_ICP ~ Fe_PXRF + Substrate_RT, data = dt_M3Fe, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  
  #
  results2_min$Predicted_Fe_M3 <- predict(M3Fe_adjusted, newdata = results2_min, type = "response")
  #
  
  #write.xlsx(dt, "Solitude2022_Predicted_Cooks-Final2.xlsx")
  #write.table(dt, file='C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/Solitude2022_Predicted_Cooks-Final.csv', sep=",", row.names = F)
  
}
# SAVING MODEL PERFORMANCE METRICS - MEDIAN
{
  #RMSE with col names and save to excel
  {
    elements <- c("Cu", "Se", "Re", "Zn", "Mn", "Fe")
    raw_cols <- paste0(elements, "_PXRF")
    icp_cols <- paste0(elements, "_ICP")
    predicted_cols <- lapply(elements, function(el) paste0("Predicted_", el, "_M", 1:3))
    
    # Flattening the list of predicted columns for easier access
    predicted_cols_flat <- unlist(predicted_cols)
    
    library(openxlsx)
    
    # Initialize a data frame to store RMSE results
    rmse_results <- data.frame(Element = character(), Model = character(), RMSE = numeric(), stringsAsFactors = FALSE)
    
    for (i in 1:length(elements)) {
      # RMSE for RAW
      rmse_raw <- sqrt(mean((results2_min[[icp_cols[i]]] - results2_min[[raw_cols[i]]])^2, na.rm = TRUE))
      temp_df <- data.frame(Element = elements[i], Model = "RAW", RMSE = rmse_raw, stringsAsFactors = FALSE)
      rmse_results <- rbind(rmse_results, temp_df)
      
      # RMSE for Predicted models
      for (j in 1:3) {
        pred_col_name <- predicted_cols[[i]][j]
        rmse_pred <- sqrt(mean((results2_min[[icp_cols[i]]] - results2_min[[pred_col_name]])^2, na.rm = TRUE))
        temp_df <- data.frame(Element = elements[i], Model = paste("M", j, sep=""), RMSE = rmse_pred, stringsAsFactors = FALSE)
        rmse_results <- rbind(rmse_results, temp_df)
      }
    }
    
    rmse_results$RMSE <- as.numeric(rmse_results$RMSE)
    colnames(rmse_results) <- c("Element", "Model", "RMSE")
    wb <- createWorkbook()
    addWorksheet(wb, "RMSE Results")
    writeData(wb, "RMSE Results", rmse_results)
    
    # Save the workbook
    saveWorkbook(wb, "RMSE_Cook.xlsx", overwrite = TRUE)
    
  }
  
  
  #NRMSE with col names and save to excel
  
  # Initialize a data frame to store NRMSE results
  nrmse_results <- data.frame(Element = character(), Model = character(), NRMSE = numeric(), stringsAsFactors = FALSE)
  
  for (i in 1:length(elements)) {
    mean_icp <- mean(results2_min[[icp_cols[i]]], na.rm = TRUE)  # Calculate mean of ICP values for the element
    
    # NRMSE for RAW
    rmse_raw <- sqrt(mean((results2_min[[icp_cols[i]]] - results2_min[[raw_cols[i]]])^2, na.rm = TRUE))
    nrmse_raw <- rmse_raw / mean_icp
    temp_df <- data.frame(Element = elements[i], Model = "RAW", NRMSE = nrmse_raw, stringsAsFactors = FALSE)
    nrmse_results <- rbind(nrmse_results, temp_df)
    
    # NRMSE for Predicted models
    for (j in 1:3) {
      pred_col_name <- predicted_cols[[i]][j]
      rmse_pred <- sqrt(mean((results2_min[[icp_cols[i]]] - results2_min[[pred_col_name]])^2, na.rm = TRUE))
      nrmse_pred <- rmse_pred / mean_icp
      temp_df <- data.frame(Element = elements[i], Model = paste("M", j, sep=""), NRMSE = nrmse_pred, stringsAsFactors = FALSE)
      nrmse_results <- rbind(nrmse_results, temp_df)
    }
  }
  
  
  wb_nrmse <- createWorkbook()
  addWorksheet(wb_nrmse, "NRMSE Results")
  writeData(wb_nrmse, "NRMSE Results", nrmse_results)
  saveWorkbook(wb_nrmse, "NRMSE_Cook.xlsx", overwrite = TRUE)
  
  
  
  #MAE
  ####MAE with col names and save to excel######
  
  # Initialize a data frame to store MAE results
  mae_results <- data.frame(Element = character(), Model = character(), MAE = numeric(), stringsAsFactors = FALSE)
  
  for (i in 1:length(elements)) {
    # MAE for RAW
    mae_raw <- mean(abs(results2_min[[icp_cols[i]]] - results2_min[[raw_cols[i]]]), na.rm = TRUE)
    temp_df <- data.frame(Element = elements[i], Model = "RAW", MAE = mae_raw, stringsAsFactors = FALSE)
    mae_results <- rbind(mae_results, temp_df)
    
    # MAE for Predicted models
    for (j in 1:3) {
      pred_col_name <- predicted_cols[[i]][j]
      mae_pred <- mean(abs(results2_min[[icp_cols[i]]] - results2_min[[pred_col_name]]), na.rm = TRUE)
      temp_df <- data.frame(Element = elements[i], Model = paste("M", j, sep=""), MAE = mae_pred, stringsAsFactors = FALSE)
      mae_results <- rbind(mae_results, temp_df)
    }
  }
  
  library(openxlsx)
  
  wb_mae <- createWorkbook()
  addWorksheet(wb_mae, "MAE Results")
  writeData(wb_mae, "MAE Results", mae_results)
  saveWorkbook(wb_mae, "MAE_Cook.xlsx", overwrite = TRUE)
  
  
  
  #R-Squared
  ####R-squared with col names and save to excel######
  
  # Initialize a data frame to store R-squared results
  r_squared_results <- data.frame(Element = character(), Model = character(), R_squared = numeric(), stringsAsFactors = FALSE)
  
  for (i in 1:length(elements)) {
    # R-squared for RAW
    lm_model_raw <- lm(results2_min[[icp_cols[i]]] ~ results2_min[[raw_cols[i]]], data = results2_min, na.action = na.exclude)
    r_squared_raw <- summary(lm_model_raw)$r.squared
    temp_df <- data.frame(Element = elements[i], Model = "RAW", R_squared = r_squared_raw, stringsAsFactors = FALSE)
    r_squared_results <- rbind(r_squared_results, temp_df)
    
    # R-squared for Predicted models
    for (j in 1:3) {
      pred_col_name <- predicted_cols[[i]][j]
      lm_model_pred <- lm(results2_min[[icp_cols[i]]] ~ results2_min[[pred_col_name]], data = results2_min, na.action = na.exclude)
      r_squared_pred <- summary(lm_model_pred)$r.squared
      temp_df <- data.frame(Element = elements[i], Model = paste("M", j, sep=""), R_squared = r_squared_pred, stringsAsFactors = FALSE)
      r_squared_results <- rbind(r_squared_results, temp_df)
    }
  }
  
  
  wb_r_squared <- createWorkbook()
  addWorksheet(wb_r_squared, "R-squared Results")
  writeData(wb_r_squared, "R-squared Results", r_squared_results)
  saveWorkbook(wb_r_squared, "R_Cook.xlsx", overwrite = TRUE)
  
  
  
  #RPD
  #RPD with col names and save to excel
  
  # Initialize a data frame to store RPD results
  rpd_results <- data.frame(Element = character(), Model = character(), RPD = numeric(), stringsAsFactors = FALSE)
  
  for (i in 1:length(elements)) {
    # Standard deviation for ICP data
    sdr <- sd(results2_min[[icp_cols[i]]], na.rm = TRUE)
    
    # RPD for RAW
    rmse_raw <- sqrt(mean((results2_min[[icp_cols[i]]] - results2_min[[raw_cols[i]]])^2, na.rm = TRUE))
    rpd_raw <- sdr / rmse_raw
    temp_df <- data.frame(Element = elements[i], Model = "RAW", RPD = rpd_raw, stringsAsFactors = FALSE)
    rpd_results <- rbind(rpd_results, temp_df)
    
    # RPD for Predicted models
    for (j in 1:3) {
      pred_col_name <- predicted_cols[[i]][j]
      rmse_pred <- sqrt(mean((results2_min[[icp_cols[i]]] - results2_min[[pred_col_name]])^2, na.rm = TRUE))
      rpd_pred <- sdr / rmse_pred
      temp_df <- data.frame(Element = elements[i], Model = paste("M", j, sep=""), RPD = rpd_pred, stringsAsFactors = FALSE)
      rpd_results <- rbind(rpd_results, temp_df)
    }
  }
  
  wb_rpd <- createWorkbook()
  addWorksheet(wb_rpd, "RPD Results")
  writeData(wb_rpd, "RPD Results", rpd_results)
  saveWorkbook(wb_rpd, "RPD_Cook.xlsx", overwrite = TRUE)
  
  
  #ICC
  #ICC with col names, only ICC2 for single random raster value
  
  
  # Initialize a data frame to store ICC results
  icc_results <- data.frame(Element = character(), Model = character(), ICC_Value = numeric(), stringsAsFactors = FALSE)
  
  for (i in 1:length(elements)) {
    # ICC for RAW
    dt_icc_raw <- results2_min[, c(icp_cols[i], raw_cols[i])]
    dt_icc_raw <- na.omit(dt_icc_raw)
    icc_raw <- ICC(dt_icc_raw, missing=TRUE, alpha=.05, lmer=TRUE, check.keys=FALSE)
    icc_value_raw <- icc_raw$results["Single_random_raters", "ICC"]
    temp_df <- data.frame(Element = elements[i], Model = "RAW", ICC_Value = icc_value_raw, stringsAsFactors = FALSE)
    icc_results <- rbind(icc_results, temp_df)
    
    # ICC for Predicted models
    for (j in 1:3) {
      pred_col_name <- predicted_cols[[i]][j]
      dt_icc_pred <- results2_min[, c(icp_cols[i], pred_col_name)]
      dt_icc_pred <- na.omit(dt_icc_pred)
      icc_pred <- ICC(dt_icc_pred, missing=TRUE, alpha=.05, lmer=TRUE, check.keys=FALSE)
      icc_value_pred <- icc_pred$results["Single_random_raters", "ICC"]
      temp_df <- data.frame(Element = elements[i], Model = paste("M", j, sep=""), ICC_Value = icc_value_pred, stringsAsFactors = FALSE)
      icc_results <- rbind(icc_results, temp_df)
    }
  }
  
  wb_icc <- createWorkbook()
  addWorksheet(wb_icc, "ICC Results")
  writeData(wb_icc, "ICC Results", icc_results)
  saveWorkbook(wb_icc, "ICC_Cook.xlsx", overwrite = TRUE)
  
}





























#Check how ICP deviate from pXRF
setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/")
dt <-read.delim("Solitude_pXRF_ICP_AllPoints.txt")



#Creating new datasets
library(dplyr)

# Assume 'dt' is your original dataset
dt_ICP <- dt %>% filter(Method == "ICP")
dt_PXRF <- dt %>% filter(Method == "PXRF")

results3 <- data.frame()
unique_sample_ids <- unique(dt_ICP$SampleID)

for (sample_id in unique_sample_ids) {
  icp_rows <- dt_ICP %>% filter(SampleID == sample_id)
  pxrf_row <- dt_PXRF %>% filter(SampleID == sample_id)
  
  if (nrow(pxrf_row) == 1) {
    for (i in 1:nrow(icp_rows)) {
      # Calculating percent error
      difference <- (pxrf_row[, 4:15] - icp_rows[i, 4:15]) 
      
      # Naming the percent error columns with '_error' suffix
      names(difference) <- paste0(names(icp_rows)[4:15], "_difference")
      
      # Extracting the element concentration columns from the ICP and PXRF rows
      icp_element_concentration <- icp_rows[i, 4:15]
      pxrf_element_concentration <- pxrf_row[, 4:15]
      
      # Naming the concentration columns to distinguish between ICP and PXRF
      names(icp_element_concentration) <- paste0(names(icp_rows)[4:15], "_ICP")
      names(pxrf_element_concentration) <- paste0(names(pxrf_row)[4:15], "_PXRF")
      
      # Combining the ICP and PXRF concentrations, and percent error into a single row
      temp_result <- c(icp_rows[i, c("SampleID", "SampleID2", "Method", "Total_Weight", "Substrate_RT")], 
                       icp_element_concentration, 
                       pxrf_element_concentration, 
                       difference)
      
      # Append the results
      results3 <- rbind(results3, temp_result)
    }
  }
}

# Convert row names to a proper column if needed and check the results
results3 <- data.frame(ID = row.names(results3), results3)
rownames(results3) <- NULL
print(results3)



cor(results3$Cu_difference, results3$Total_Weight, method = "spearman", use = "complete.obs")
cor(results3$Cu_difference, results3$Substrate_RT, method = "spearman", use = "complete.obs")

results3 <- results3 %>%
  mutate(Category = case_when(
    grepl("_1$", SampleID2) ~ "_1",
    grepl("_2$", SampleID2) ~ "_2",
    grepl("_3$", SampleID2) ~ "_3",
    TRUE ~ "Other"
  ))





results3 <- results3[order(results3$Total_Weight),]


results3_1 <- subset(results3, Category=="_1")
results3_2 <- subset(results3, Category=="_2")
results3_3 <- subset(results3, Category=="_3")



#
cor(results3$Cu_difference, results3$Total_Weight, method = "spearman", use = "complete.obs")
cor(results3$Cu_difference, results3$Substrate_RT, method = "spearman", use = "complete.obs")

cor(results3_1$Cu_difference, results3_1$Total_Weight, method = "spearman", use = "complete.obs")
cor(results3_2$Cu_difference, results3_2$Total_Weight, method = "spearman", use = "complete.obs")
cor(results3_3$Cu_difference, results3_3$Total_Weight, method = "spearman", use = "complete.obs")

cor(results3_1$Cu_difference, results3_1$Substrate_RT, method = "spearman", use = "complete.obs")
cor(results3_2$Cu_difference, results3_2$Substrate_RT, method = "spearman", use = "complete.obs")
cor(results3_3$Cu_difference, results3_3$Substrate_RT, method = "spearman", use = "complete.obs")

#


a <- ggplot(results3, aes(x = Total_Weight, y = Cu_difference, size = Cu_PXRF)) +
  geom_point(shape=21, stroke=1) +
  labs(x = "Total_Weight", y = "Cu_difference", title="All points") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))

aa <- ggplot(results3, aes(x = Substrate_RT, y = Cu_difference, size = Cu_PXRF)) +
  geom_point(shape=21, stroke=1) +
  labs(x = "Substrate_RT", y = "Cu_difference", title="All points") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))

aaa <- ggplot(results3, aes(x = Substrate_RT, y = Cu_difference, size = Cu_PXRF)) +
  geom_point(shape=21, stroke=1) +
  labs(x = "Substrate_RT", y = "Cu_difference", title="All points") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))

b1 <- ggplot(results3_1, aes(x = Total_Weight, y = Cu_difference, size = Cu_PXRF)) +
  geom_point(shape=21, stroke=1) +
  labs(x = "Total_Weight", y = "Cu_difference", title="_1 point") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))

b2 <- ggplot(results3_2, aes(x = Total_Weight, y = Cu_difference, size = Cu_PXRF)) +
  geom_point(shape=21, stroke=1) +
  labs(x = "Total_Weight", y = "Cu_difference", title="_2 point") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))

b3 <- ggplot(results3_3, aes(x = Total_Weight, y = Cu_difference, size = Cu_PXRF)) +
  geom_point(shape=21, stroke=1) +
  labs(x = "Total_Weight", y = "Cu_difference", title="_3 point") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))


c1 <- ggplot(results3_1, aes(x = Substrate_RT, y = Cu_difference, size = Cu_PXRF)) +
  geom_point(shape=21, stroke=1) +
  labs(x = "Substrate_RT", y = "Cu_difference", title="_1 point") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))

c2 <- ggplot(results3_2, aes(x = Substrate_RT, y = Cu_difference, size = Cu_PXRF)) +
  geom_point(shape=21, stroke=1) +
  labs(x = "Substrate_RT", y = "Cu_difference", title="_2 point") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))

c3 <- ggplot(results3_3, aes(x = Substrate_RT, y = Cu_difference, size = Cu_PXRF)) +
  geom_point(shape=21, stroke=1) +  # Plot points
  labs(x = "Substrate_RT", y = "Cu_difference", title="_3 point") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))

#
cor(results3$Cu_difference, results3$Cu_PXRF, method = "spearman", use = "complete.obs")
cor(results3$Cu_difference, results3$Cu_ICP, method = "spearman", use = "complete.obs")

cor(results3_1$Cu_difference, results3_1$Cu_PXRF, method = "spearman", use = "complete.obs")
cor(results3_2$Cu_difference, results3_2$Cu_PXRF, method = "spearman", use = "complete.obs")
cor(results3_3$Cu_difference, results3_3$Cu_PXRF, method = "spearman", use = "complete.obs")
cor(results3_1$Cu_difference, results3_1$Cu_ICP, method = "spearman", use = "complete.obs")
cor(results3_2$Cu_difference, results3_2$Cu_ICP, method = "spearman", use = "complete.obs")
cor(results3_3$Cu_difference, results3_3$Cu_ICP, method = "spearman", use = "complete.obs")

d <- ggplot(results3, aes(x = Cu_PXRF, y = Cu_difference, size = Total_Weight)) +
  geom_point(shape=21, stroke=1) +  # Plot points
  labs(x = "Cu_PXRF", y = "Cu_difference", title="All") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))

dd <- ggplot(results3, aes(x = Cu_ICP, y = Cu_difference, size = Total_Weight)) +
  geom_point(shape=21, stroke=1) +  # Plot points
  labs(x = "Cu_ICP", y = "Cu_difference", title="All") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))

ddd <- ggplot(results3, aes(x = Cu_ICP, y = Cu_difference, size = Total_Weight)) +
  geom_point(shape=21, stroke=1) +  # Plot points
  labs(x = "Cu_ICP", y = "Cu_difference", title="All") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))

d1 <- ggplot(results3_1, aes(x = Cu_PXRF, y = Cu_difference, size = Total_Weight)) +
  geom_point(shape=21, stroke=1) +  # Plot points
  labs(x = "Cu_PXRF", y = "Cu_difference", title="_1") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))

d2 <- ggplot(results3_2, aes(x = Cu_PXRF, y = Cu_difference, size = Total_Weight)) +
  geom_point(shape=21, stroke=1) +  # Plot points
  labs(x = "Cu_PXRF", y = "Cu_difference", title="_2") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))


d3 <- ggplot(results3_3, aes(x = Cu_PXRF, y = Cu_difference, size = Total_Weight)) +
  geom_point(shape=21, stroke=1) +  # Plot points
  labs(x = "Cu_PXRF", y = "Cu_difference", title="_3") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))

e1 <- ggplot(results3_1, aes(x = Cu_ICP, y = Cu_difference, size = Total_Weight)) +
  geom_point(shape=21, stroke=1) +  # Plot points
  labs(x = "Cu_ICP", y = "Cu_difference", title="_1") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))

e2 <- ggplot(results3_2, aes(x = Cu_ICP, y = Cu_difference, size = Total_Weight)) +
  geom_point(shape=21, stroke=1) +  # Plot points
  labs(x = "Cu_ICP", y = "Cu_difference", title="_2") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))


e3 <- ggplot(results3_3, aes(x = Cu_ICP, y = Cu_difference, size = Total_Weight)) +
  geom_point(shape=21, stroke=1) +  # Plot points
  labs(x = "Cu_ICP", y = "Cu_difference", title="_3") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))


ggarrange(a,aa,aaa,b1,b2,b3,c1,c2,c3,
          ncol = 3, nrow = 3, 
          common.legend = FALSE, legend = "bottom")

ggarrange(d,dd,ddd,d1,d2,d3,e1,e2,e3,
          
          ncol = 3, nrow = 3, 
          common.legend = FALSE, legend = "bottom")





# Assuming results3 already has the 'Category' column created from the previous step
results3 <- results3 %>%
  group_by(SampleID) %>%
  mutate(
    Unique_Category_Count = n_distinct(Category),  # Count unique categories per SampleID
    Single_Category = if_else(Unique_Category_Count == 1, "Y", "N")  # Mark 'Y' if only one unique category
  ) %>%
  ungroup() %>%
  select(-Unique_Category_Count)  # Optionally, remove the temporary count column

# View the updated dataset
print(results3)



results3_single <- subset(results3, Single_Category=="Y")


cor(results3_single$Cu_difference, results3_single$Cu_PXRF, method = "spearman", use = "complete.obs")
cor(results3_single$Cu_difference, results3_single$Cu_ICP, method = "spearman", use = "complete.obs")

cor(results3_single$Cu_difference, results3_single$Total_Weight, method = "spearman", use = "complete.obs")
cor(results3_single$Cu_difference, results3_single$Substrate_RT, method = "spearman", use = "complete.obs")

singl1 <- ggplot(results3_single, aes(x = Cu_PXRF, y = Cu_difference, size = Total_Weight)) +
  geom_point(shape=21, stroke=1) +  # Plot points
  labs(x = "Cu_PXRF", y = "Cu_difference", title="_3") +
  scale_y_continuous(limits = c(-300, 0), breaks = seq(-300, 0, by = 50)) +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1))

singl2 <- ggplot(results3_single, aes(x = Cu_ICP, y = Cu_difference, size = Total_Weight)) +
  geom_point(shape=21, stroke=1) +  # Plot points
  labs(x = "Cu_ICP", y = "Cu_difference", title="_3") +
  scale_y_continuous(limits = c(-300, 0), breaks = seq(-300, 0, by = 50)) +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1))


singl3 <- ggplot(results3_single, aes(x = Total_Weight, y = Cu_difference, size = Cu_PXRF)) +
  geom_point(shape=21, stroke=1) +  # Plot points
  labs(x = "Total_Weight", y = "Cu_difference", title="1to1") +
  scale_y_continuous(limits = c(-300, 0), breaks = seq(-300, 0, by = 50)) +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1))

singl4 <- ggplot(results3_single, aes(x = Substrate_RT, y = Cu_difference, size = Cu_PXRF)) +
  geom_point(shape=21, stroke=1) +  # Plot points
  labs(x = "Substrate_RT", y = "Cu_difference", title="1to1") +
  scale_y_continuous(limits = c(-300, 0), breaks = seq(-300, 0, by = 50)) +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1))


ggarrange(singl1, singl2,singl3,singl4,
          ncol = 2, nrow = 2, 
          common.legend = FALSE, legend = "bottom")


















################# after filtered data!!!

{
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Cu <- glm(Cu_ICP ~ Cu_PXRF, data = results2_mean, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Cu <- glm(Cu_ICP ~ Cu_PXRF + Total_Weight, data = results2_mean, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Cu <- glm(Cu_ICP ~ Cu_PXRF + Substrate_RT, data = results2_mean, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M3Cu)
  
  dt_M1Cu <- results2_mean
  cooks_distance <- cooks.distance(M1Cu)
  threshold <- 4 / nrow(results2_mean)
  outliers <- which(cooks_distance > threshold)
  dt_M1Cu[outliers, "Cu_ICP"] <- NA
  
  M1Cu_adjusted <- glm(Cu_ICP ~ Cu_PXRF, data = dt_M1Cu, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Cu_adjusted)
  #
  results2_mean$Predicted_Cu_M1 <- predict(M1Cu_adjusted, newdata = results2_mean, type = "response")
  #
  dt_M2Cu <- results2_mean
  cooks_distance <- cooks.distance(M2Cu)
  threshold <- 4 / nrow(results2_mean)
  outliers <- which(cooks_distance > threshold)
  dt_M2Cu[outliers, "Cu_ICP"] <- NA
  
  M2Cu_adjusted <- glm(Cu_ICP ~ Cu_PXRF + Total_Weight, data = dt_M2Cu, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M2Cu_adjusted)
  #
  results2_mean$Predicted_Cu_M2 <- predict(M2Cu_adjusted, newdata = results2_mean, type = "response")
  #
  dt_M3Cu <- results2_mean
  cooks_distance <- cooks.distance(M3Cu)
  threshold <- 4 / nrow(results2_mean)
  outliers <- which(cooks_distance > threshold)
  dt_M3Cu[outliers, "Cu_ICP"] <- NA
  
  M3Cu_adjusted <- glm(Cu_ICP ~ Cu_PXRF + Substrate_RT, data = dt_M3Cu, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M3Cu_adjusted)
  
  #
  results2_mean$Predicted_Cu_M3 <- predict(M3Cu_adjusted, newdata = results2_mean, type = "response")
  #
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  #NEW CODE HERE
  # Ensuring Se_ICP is strictly greater than 0
  results2_mean <- results2_mean %>% 
    filter(Se_ICP > 0) 
  
  start_intercept <- mean(results2_mean$Se_ICP)  # Basic starting value for intercept
  start_predictors <- c(0, 0)  # Small starting values for predictors
  
  # Fit the model with starting values
  M2Se <- glm(Se_ICP ~ Se_PXRF + Total_Weight, data = results2_mean, 
              family = Gamma(link = "identity"), 
              control = glm.control(maxit = 50),
              start = c(start_intercept, start_predictors))
  
  start_Se_PXRF <- 0  # Example starting value for Se_PXRF
  
  # Fit the model with adjusted starting values
  M1Se <- glm(Se_ICP ~ Se_PXRF, data = results2_mean, 
              family = Gamma(link = "identity"), 
              control = glm.control(maxit = 50),
              start = c(intercept = start_intercept, Se_PXRF = start_Se_PXRF))
  
  M3Se <- glm(Se_ICP ~ Se_PXRF + Total_Weight, data = results2_mean, 
              family = Gamma(link = "identity"), 
              control = glm.control(maxit = 50),
              start = c(start_intercept, start_predictors))
  
  
  
  dt_M1Se <- results2_mean
  cooks_distance <- cooks.distance(M1Se)
  threshold <- 4 / nrow(results2_mean)
  outliers <- which(cooks_distance > threshold)
  dt_M1Se[outliers, "Se_ICP"] <- NA
  
  M1Se_adjusted <- glm(Se_ICP ~ Se_PXRF, data = dt_M1Se %>% filter(Se_ICP > 0), family = Gamma(link = "identity"), control = glm.control(maxit = 50),start = c(intercept = start_intercept, Se_PXRF = start_Se_PXRF))
  
  dt_M1Se$Predicted_Se_M1 <- predict(M1Se_adjusted, newdata = dt_M1Se, type = "response")
  #
  results2_mean$Predicted_Se_M1 <- predict(M1Se_adjusted, newdata = results2_mean, type = "response")
  #
  dt_M2Se <- results2_mean
  cooks_distance <- cooks.distance(M2Se)
  threshold <- 4 / nrow(results2_mean)
  outliers <- which(cooks_distance > threshold)
  dt_M2Se[outliers, "Se_ICP"] <- NA
  
  M2Se_adjusted <- glm(Se_ICP ~ Se_PXRF + Total_Weight, data = dt_M2Se%>% filter(Se_ICP > 0), family = Gamma(link = "identity"), control = glm.control(maxit = 50),start = c(start_intercept, start_predictors))
  
  dt_M2Se$Predicted_Se_M2 <- predict(M2Se_adjusted, newdata = dt_M2Se, type = "response")
  #
  results2_mean$Predicted_Se_M2 <- predict(M2Se_adjusted, newdata = results2_mean, type = "response")
  #
  dt_M3Se <- results2_mean
  cooks_distance <- cooks.distance(M3Se)
  threshold <- 4 / nrow(results2_mean)
  outliers <- which(cooks_distance > threshold)
  dt_M3Se[outliers, "Se_ICP"] <- NA
  
  M3Se_adjusted <- glm(Se_ICP ~ Se_PXRF + Substrate_RT, data = dt_M3Se%>% filter(Se_ICP > 0), family = Gamma(link = "identity"), control = glm.control(maxit = 50),start = c(start_intercept, start_predictors))
  
  dt_M3Se$Predicted_Se_M3 <- predict(M3Se_adjusted, newdata = dt_M3Se, type = "response")
  
  #
  results2_mean$Predicted_Se_M3 <- predict(M3Se_adjusted, newdata = results2_mean, type = "response")
  #
  
  
  
  
  
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Re <- glm(Re_ICP ~ Re_PXRF, data = results2_mean, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Re <- glm(Re_ICP ~ Re_PXRF + Total_Weight, data = results2_mean, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Re <- glm(Re_ICP ~ Re_PXRF + Substrate_RT, data = results2_mean, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Re)
  summary(M3Re)
  
  dt_M1Re <- results2_mean
  cooks_distance <- cooks.distance(M1Re)
  threshold <- 4 / nrow(results2_mean)
  outliers <- which(cooks_distance > threshold)
  dt_M1Re[outliers, "Re_ICP"] <- NA
  
  M1Re_adjusted <- glm(Re_ICP ~ Re_PXRF, data = dt_M1Re, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Re_adjusted)
  #
  results2_mean$Predicted_Re_M1 <- predict(M1Re_adjusted, newdata = results2_mean, type = "response")
  #
  dt_M2Re <- results2_mean
  cooks_distance <- cooks.distance(M2Re)
  threshold <- 4 / nrow(results2_mean)
  outliers <- which(cooks_distance > threshold)
  dt_M2Re[outliers, "Re_ICP"] <- NA
  
  M2Re_adjusted <- glm(Re_ICP ~ Re_PXRF + Total_Weight, data = dt_M2Re, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  #
  results2_mean$Predicted_Re_M2 <- predict(M2Re_adjusted, newdata = results2_mean, type = "response")
  #
  dt_M3Re <- results2_mean
  cooks_distance <- cooks.distance(M3Re)
  threshold <- 4 / nrow(results2_mean)
  outliers <- which(cooks_distance > threshold)
  dt_M3Re[outliers, "Re_ICP"] <- NA
  
  M3Re_adjusted <- glm(Re_ICP ~ Re_PXRF + Substrate_RT, data = dt_M3Re, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M3Re_adjusted)
  #
  results2_mean$Predicted_Re_M3 <- predict(M3Re_adjusted, newdata = results2_mean, type = "response")
  #
  
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Zn <- glm(Zn_ICP ~ Zn_PXRF, data = results2_mean, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Zn <- glm(Zn_ICP ~ Zn_PXRF + Total_Weight, data = results2_mean, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Zn <- glm(Zn_ICP ~ Zn_PXRF + Substrate_RT, data = results2_mean, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M4Zn <- glm(Zn_ICP ~ Zn_PXRF + Substrate_RT + Total_Weight, data = results2_mean, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M3Zn)
  
  dt_M1Zn <- results2_mean
  cooks_distance <- cooks.distance(M1Zn)
  threshold <- 4 / nrow(results2_mean)
  outliers <- which(cooks_distance > threshold)
  dt_M1Zn[outliers, "Zn_ICP"] <- NA
  
  M1Zn_adjusted <- glm(Zn_ICP ~ Zn_PXRF, data = dt_M1Zn, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  #
  results2_mean$Predicted_Zn_M1 <- predict(M1Zn_adjusted, newdata = results2_mean, type = "response")
  #
  dt_M2Zn <- results2_mean
  cooks_distance <- cooks.distance(M2Zn)
  threshold <- 4 / nrow(results2_mean)
  outliers <- which(cooks_distance > threshold)
  dt_M2Zn[outliers, "Zn_ICP"] <- NA
  
  M2Zn_adjusted <- glm(Zn_ICP ~ Zn_PXRF + Total_Weight, data = dt_M2Zn, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  #
  results2_mean$Predicted_Zn_M2 <- predict(M2Zn_adjusted, newdata = results2_mean, type = "response")
  #
  dt_M3Zn <- results2_mean
  cooks_distance <- cooks.distance(M3Zn)
  threshold <- 4 / nrow(results2_mean)
  outliers <- which(cooks_distance > threshold)
  dt_M3Zn[outliers, "Zn_ICP"] <- NA
  
  M3Zn_adjusted <- glm(Zn_ICP ~ Zn_PXRF + Substrate_RT, data = dt_M3Zn, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M3Zn_adjusted)
  #
  results2_mean$Predicted_Zn_M3 <- predict(M3Zn_adjusted, newdata = results2_mean, type = "response")
  #
  
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Mn <- glm(Mn_ICP ~ Mn_PXRF, data = results2_mean, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Mn <- glm(Mn_ICP ~ Mn_PXRF + Total_Weight, data = results2_mean, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Mn <- glm(Mn_ICP ~ Mn_PXRF + Substrate_RT, data = results2_mean, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Mn)
  
  dt_M1Mn <- results2_mean
  cooks_distance <- cooks.distance(M1Mn)
  threshold <- 4 / nrow(results2_mean)
  outliers <- which(cooks_distance > threshold)
  dt_M1Mn[outliers, "Mn_ICP"] <- NA
  
  M1Mn_adjusted <- glm(Mn_ICP ~ Mn_PXRF, data = dt_M1Mn, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  #
  results2_mean$Predicted_Mn_M1 <- predict(M1Mn_adjusted, newdata = results2_mean, type = "response")
  #
  dt_M2Mn <- results2_mean
  cooks_distance <- cooks.distance(M2Mn)
  threshold <- 4 / nrow(results2_mean)
  outliers <- which(cooks_distance > threshold)
  dt_M2Mn[outliers, "Mn_ICP"] <- NA
  
  M2Mn_adjusted <- glm(Mn_ICP ~ Mn_PXRF + Total_Weight, data = dt_M2Mn, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  #
  results2_mean$Predicted_Mn_M2 <- predict(M2Mn_adjusted, newdata = results2_mean, type = "response")
  #
  dt_M3Mn <- results2_mean
  cooks_distance <- cooks.distance(M3Mn)
  threshold <- 4 / nrow(results2_mean)
  outliers <- which(cooks_distance > threshold)
  dt_M3Mn[outliers, "Mn_ICP"] <- NA
  
  M3Mn_adjusted <- glm(Mn_ICP ~ Mn_PXRF + Substrate_RT, data = dt_M3Mn, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  
  #
  results2_mean$Predicted_Mn_M3 <- predict(M3Mn_adjusted, newdata = results2_mean, type = "response")
  #
  
  
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Fe <- glm(Fe_ICP ~ Fe_PXRF, data = results2_mean, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Fe <- glm(Fe_ICP ~ Fe_PXRF + Total_Weight, data = results2_mean, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Fe <- glm(Fe_ICP ~ Fe_PXRF + Substrate_RT, data = results2_mean, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  
  summary(M3Fe)
  dt_M1Fe <- results2_mean
  cooks_distance <- cooks.distance(M1Fe)
  threshold <- 4 / nrow(results2_mean)
  outliers <- which(cooks_distance > threshold)
  dt_M1Fe[outliers, "Fe_ICP"] <- NA
  
  M1Fe_adjusted <- glm(Fe_ICP ~ Fe_PXRF, data = dt_M1Fe, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Fe_adjusted)
  #
  results2_mean$Predicted_Fe_M1 <- predict(M1Fe_adjusted, newdata = results2_mean, type = "response")
  #
  dt_M2Fe <- results2_mean
  cooks_distance <- cooks.distance(M2Fe)
  threshold <- 4 / nrow(results2_mean)
  outliers <- which(cooks_distance > threshold)
  dt_M2Fe[outliers, "Fe_ICP"] <- NA
  
  M2Fe_adjusted <- glm(Fe_ICP ~ Fe_PXRF + Total_Weight, data = dt_M2Fe, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  #
  results2_mean$Predicted_Fe_M2 <- predict(M2Fe_adjusted, newdata = results2_mean, type = "response")
  #
  dt_M3Fe <- results2_mean
  cooks_distance <- cooks.distance(M3Fe)
  threshold <- 4 / nrow(results2_mean)
  outliers <- which(cooks_distance > threshold)
  dt_M3Fe[outliers, "Fe_ICP"] <- NA
  
  M3Fe_adjusted <- glm(Fe_ICP ~ Fe_PXRF + Substrate_RT, data = dt_M3Fe, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  
  #
  results2_mean$Predicted_Fe_M3 <- predict(M3Fe_adjusted, newdata = results2_mean, type = "response")
  #
  
  #write.xlsx(dt, "Solitude2022_Predicted_Cooks-Final2.xlsx")
  #write.table(dt, file='C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/Solitude2022_Predicted_Cooks-Final.csv', sep=",", row.names = F)
  
}
# SAVING MODEL PERFORMANCE METRICS - MEDIAN
{
  #RMSE with col names and save to excel
  {
    elements <- c("Cu", "Se", "Re", "Zn", "Mn", "Fe")
    raw_cols <- paste0(elements, "_PXRF")
    icp_cols <- paste0(elements, "_ICP")
    predicted_cols <- lapply(elements, function(el) paste0("Predicted_", el, "_M", 1:3))
    
    # Flattening the list of predicted columns for easier access
    predicted_cols_flat <- unlist(predicted_cols)
    
    library(openxlsx)
    
    # Initialize a data frame to store RMSE results
    rmse_results <- data.frame(Element = character(), Model = character(), RMSE = numeric(), stringsAsFactors = FALSE)
    
    for (i in 1:length(elements)) {
      # RMSE for RAW
      rmse_raw <- sqrt(mean((results2_mean[[icp_cols[i]]] - results2_mean[[raw_cols[i]]])^2, na.rm = TRUE))
      temp_df <- data.frame(Element = elements[i], Model = "RAW", RMSE = rmse_raw, stringsAsFactors = FALSE)
      rmse_results <- rbind(rmse_results, temp_df)
      
      # RMSE for Predicted models
      for (j in 1:3) {
        pred_col_name <- predicted_cols[[i]][j]
        rmse_pred <- sqrt(mean((results2_mean[[icp_cols[i]]] - results2_mean[[pred_col_name]])^2, na.rm = TRUE))
        temp_df <- data.frame(Element = elements[i], Model = paste("M", j, sep=""), RMSE = rmse_pred, stringsAsFactors = FALSE)
        rmse_results <- rbind(rmse_results, temp_df)
      }
    }
    
    rmse_results$RMSE <- as.numeric(rmse_results$RMSE)
    colnames(rmse_results) <- c("Element", "Model", "RMSE")
    wb <- createWorkbook()
    addWorksheet(wb, "RMSE Results")
    writeData(wb, "RMSE Results", rmse_results)
    
    # Save the workbook
    saveWorkbook(wb, "RMSE_Cook.xlsx", overwrite = TRUE)
    
  }
  
  
  #NRMSE with col names and save to excel
  
  # Initialize a data frame to store NRMSE results
  nrmse_results <- data.frame(Element = character(), Model = character(), NRMSE = numeric(), stringsAsFactors = FALSE)
  
  for (i in 1:length(elements)) {
    mean_icp <- mean(results2_mean[[icp_cols[i]]], na.rm = TRUE)  # Calculate mean of ICP values for the element
    
    # NRMSE for RAW
    rmse_raw <- sqrt(mean((results2_mean[[icp_cols[i]]] - results2_mean[[raw_cols[i]]])^2, na.rm = TRUE))
    nrmse_raw <- rmse_raw / mean_icp
    temp_df <- data.frame(Element = elements[i], Model = "RAW", NRMSE = nrmse_raw, stringsAsFactors = FALSE)
    nrmse_results <- rbind(nrmse_results, temp_df)
    
    # NRMSE for Predicted models
    for (j in 1:3) {
      pred_col_name <- predicted_cols[[i]][j]
      rmse_pred <- sqrt(mean((results2_mean[[icp_cols[i]]] - results2_mean[[pred_col_name]])^2, na.rm = TRUE))
      nrmse_pred <- rmse_pred / mean_icp
      temp_df <- data.frame(Element = elements[i], Model = paste("M", j, sep=""), NRMSE = nrmse_pred, stringsAsFactors = FALSE)
      nrmse_results <- rbind(nrmse_results, temp_df)
    }
  }
  
  
  wb_nrmse <- createWorkbook()
  addWorksheet(wb_nrmse, "NRMSE Results")
  writeData(wb_nrmse, "NRMSE Results", nrmse_results)
  saveWorkbook(wb_nrmse, "NRMSE_Cook.xlsx", overwrite = TRUE)
  
  
  
  #MAE
  ####MAE with col names and save to excel######
  
  # Initialize a data frame to store MAE results
  mae_results <- data.frame(Element = character(), Model = character(), MAE = numeric(), stringsAsFactors = FALSE)
  
  for (i in 1:length(elements)) {
    # MAE for RAW
    mae_raw <- mean(abs(results2_mean[[icp_cols[i]]] - results2_mean[[raw_cols[i]]]), na.rm = TRUE)
    temp_df <- data.frame(Element = elements[i], Model = "RAW", MAE = mae_raw, stringsAsFactors = FALSE)
    mae_results <- rbind(mae_results, temp_df)
    
    # MAE for Predicted models
    for (j in 1:3) {
      pred_col_name <- predicted_cols[[i]][j]
      mae_pred <- mean(abs(results2_mean[[icp_cols[i]]] - results2_mean[[pred_col_name]]), na.rm = TRUE)
      temp_df <- data.frame(Element = elements[i], Model = paste("M", j, sep=""), MAE = mae_pred, stringsAsFactors = FALSE)
      mae_results <- rbind(mae_results, temp_df)
    }
  }
  
  library(openxlsx)
  
  wb_mae <- createWorkbook()
  addWorksheet(wb_mae, "MAE Results")
  writeData(wb_mae, "MAE Results", mae_results)
  saveWorkbook(wb_mae, "MAE_Cook.xlsx", overwrite = TRUE)
  
  
  
  #R-Squared
  ####R-squared with col names and save to excel######
  
  # Initialize a data frame to store R-squared results
  r_squared_results <- data.frame(Element = character(), Model = character(), R_squared = numeric(), stringsAsFactors = FALSE)
  
  for (i in 1:length(elements)) {
    # R-squared for RAW
    lm_model_raw <- lm(results2_mean[[icp_cols[i]]] ~ results2_mean[[raw_cols[i]]], data = results2_mean, na.action = na.exclude)
    r_squared_raw <- summary(lm_model_raw)$r.squared
    temp_df <- data.frame(Element = elements[i], Model = "RAW", R_squared = r_squared_raw, stringsAsFactors = FALSE)
    r_squared_results <- rbind(r_squared_results, temp_df)
    
    # R-squared for Predicted models
    for (j in 1:3) {
      pred_col_name <- predicted_cols[[i]][j]
      lm_model_pred <- lm(results2_mean[[icp_cols[i]]] ~ results2_mean[[pred_col_name]], data = results2_mean, na.action = na.exclude)
      r_squared_pred <- summary(lm_model_pred)$r.squared
      temp_df <- data.frame(Element = elements[i], Model = paste("M", j, sep=""), R_squared = r_squared_pred, stringsAsFactors = FALSE)
      r_squared_results <- rbind(r_squared_results, temp_df)
    }
  }
  
  
  wb_r_squared <- createWorkbook()
  addWorksheet(wb_r_squared, "R-squared Results")
  writeData(wb_r_squared, "R-squared Results", r_squared_results)
  saveWorkbook(wb_r_squared, "R_Cook.xlsx", overwrite = TRUE)
  
  
  
  #RPD
  #RPD with col names and save to excel
  
  # Initialize a data frame to store RPD results
  rpd_results <- data.frame(Element = character(), Model = character(), RPD = numeric(), stringsAsFactors = FALSE)
  
  for (i in 1:length(elements)) {
    # Standard deviation for ICP data
    sdr <- sd(results2_mean[[icp_cols[i]]], na.rm = TRUE)
    
    # RPD for RAW
    rmse_raw <- sqrt(mean((results2_mean[[icp_cols[i]]] - results2_mean[[raw_cols[i]]])^2, na.rm = TRUE))
    rpd_raw <- sdr / rmse_raw
    temp_df <- data.frame(Element = elements[i], Model = "RAW", RPD = rpd_raw, stringsAsFactors = FALSE)
    rpd_results <- rbind(rpd_results, temp_df)
    
    # RPD for Predicted models
    for (j in 1:3) {
      pred_col_name <- predicted_cols[[i]][j]
      rmse_pred <- sqrt(mean((results2_mean[[icp_cols[i]]] - results2_mean[[pred_col_name]])^2, na.rm = TRUE))
      rpd_pred <- sdr / rmse_pred
      temp_df <- data.frame(Element = elements[i], Model = paste("M", j, sep=""), RPD = rpd_pred, stringsAsFactors = FALSE)
      rpd_results <- rbind(rpd_results, temp_df)
    }
  }
  
  wb_rpd <- createWorkbook()
  addWorksheet(wb_rpd, "RPD Results")
  writeData(wb_rpd, "RPD Results", rpd_results)
  saveWorkbook(wb_rpd, "RPD_Cook.xlsx", overwrite = TRUE)
  
  
  #ICC
  #ICC with col names, only ICC2 for single random raster value
  
  
  # Initialize a data frame to store ICC results
  icc_results <- data.frame(Element = character(), Model = character(), ICC_Value = numeric(), stringsAsFactors = FALSE)
  
  for (i in 1:length(elements)) {
    # ICC for RAW
    dt_icc_raw <- results2_mean[, c(icp_cols[i], raw_cols[i])]
    dt_icc_raw <- na.omit(dt_icc_raw)
    icc_raw <- ICC(dt_icc_raw, missing=TRUE, alpha=.05, lmer=TRUE, check.keys=FALSE)
    icc_value_raw <- icc_raw$results["Single_random_raters", "ICC"]
    temp_df <- data.frame(Element = elements[i], Model = "RAW", ICC_Value = icc_value_raw, stringsAsFactors = FALSE)
    icc_results <- rbind(icc_results, temp_df)
    
    # ICC for Predicted models
    for (j in 1:3) {
      pred_col_name <- predicted_cols[[i]][j]
      dt_icc_pred <- results2_mean[, c(icp_cols[i], pred_col_name)]
      dt_icc_pred <- na.omit(dt_icc_pred)
      icc_pred <- ICC(dt_icc_pred, missing=TRUE, alpha=.05, lmer=TRUE, check.keys=FALSE)
      icc_value_pred <- icc_pred$results["Single_random_raters", "ICC"]
      temp_df <- data.frame(Element = elements[i], Model = paste("M", j, sep=""), ICC_Value = icc_value_pred, stringsAsFactors = FALSE)
      icc_results <- rbind(icc_results, temp_df)
    }
  }
  
  wb_icc <- createWorkbook()
  addWorksheet(wb_icc, "ICC Results")
  writeData(wb_icc, "ICC Results", icc_results)
  saveWorkbook(wb_icc, "ICC_Cook.xlsx", overwrite = TRUE)
  
}





#### correlacja Error to error

cor(results2$Cu_error, results2$Zn_error, method = "spearman", use = "complete.obs")
cor(results2$Cu_error, results2$Fe_error, method = "spearman", use = "complete.obs")
cor(results2$Cu_error, results2$Fe_error, method = "spearman", use = "complete.obs")




# Error is not sample related! but element specific. 
library(corrplot)
library(Hmisc)
library(dplyr)

error_data <- results2 %>% 
  select(Cu_error, Re_error, Se_error, Zn_error, Mn_error, Fe_error)

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

