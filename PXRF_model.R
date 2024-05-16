# PXRF METHOD PAPER



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

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/Models_pxrf")
dt <-read.delim("PXRF_models.txt")


# Apply filter conditions based on your criteria
dt_Cu_best <- dt %>%
  filter(
    # Include criteria indicated with '-'
    (
      TubeTW == "oneTW.SMALL" |
        Qpercent_mass.TW == "80to100TW.SMALL" |
        Tube_No == "one" |
        ####TWCuTube == "TW.SMALLCu.LARGEone" |
        TW_Q == "TW.SMALL" |
        TubeTW == "oneTW.MEDIUM" |
        Scientific_Name == "Xanthisma gracile" |
        ####TWCu == "TW.SMALLCu.LARGE" |
        Form == "Shrub" |
        ####TWCu == "TW.VSMALLCu.MEDIUM" |
        ScieNamePlot == "P5Xanthisma gracile" |
        ScieNamePlot == "P5Gutierrezia sarothrae" |
        ####TWCuTube == "TW.SMALLCu.VSMALLone" |
        ####TWCuTube == "TW.SMALLCu.SMALLone" |
        TubeTW == "oneTW.LARGE" |
        ScieNamePlot == "P2Xanthisma gracile" |
        ####TWCuTube == "TW.SMALLCu.MEDIUMone" |
        ####TWCuTube == "TW.LARGECu.SMALLone" |
        Scientific_Name == "Ceanothus greggii" |
        ScieNamePlot == "P6Xanthisma gracile" |
        ScieNamePlot == "P1Euphorbia melanadenia" |
        ####TWCuTube == "TW.MEDIUMCu.LARGEone" |
        ####TWCuTube == "TW.MEDIUMCu.SMALLone" |
        Scientific_Name == "Mimosa biuncifera (=aculeaticarpa)" |
        Plot == "P5" |
        Scientific_Name == "Pectis papposa" |
        ScieNamePlot == "P5Pectis papposa" |
        Qpercent_mass.TW == "40to60TW.SMALL" |
        Scientific_Name == "Gutierrezia sarothrae" |
        Scientific_Name == "Bouteloua aristidoides" |
        ScieNamePlot == "P5Bouteloua aristidoides" |
        Type_of_Sample == "leaf-stem" |
        ####TWCu == "TW.SMALLCu.MEDIUM" |
        ####TWCu == "TW.MEDIUMCu.LARGE" |
        Mn_ICP_Q == "Mn.MEDIUM" |
        ScieNamePlot == "P5Ceanothus greggii" |
        ScieNamePlot == "P2Baccharis sarothroides" |
        Scientific_Name == "Euphorbia melanadenia" |
        Mn_ICP_Q == "Mn.SMALL" |
        Scientific_Name == "Boechera perennans" |
        ScieNamePlot == "P6Boechera perennans" |
        ScieNamePlot == "P1Mimosa biuncifera (=aculeaticarpa)" 

        
    ) &
      # Exclude criteria indicated with '+'
      !(
        TW_Q == "TW.VSMALL" |
        Qpercent_mass.TW == "80to100TW.VSMALL" |
        TubeTW == "oneTW.VSMALL" |
        Scientific_Name == "Allionia incarnata" |
       # Tube_No == "two" |
        ####TWCu == "TW.VSMALLCu.LARGE" |        
        ####TWCuTube == "TW.MEDIUMCu.SMALLtwo" |
        ####TWCuTube == "TW.VSMALLCu.LARGEone" |
        Scientific_Name == "Solanum elaeagnifolium" |
        ####TWCuTube == "TW.VSMALLCu.MEDIUMone" |
        ####TWCu == "TW.VSMALLCu.MEDIUM" |    
        Scientific_Name == "Populus fremontii" |
        ScieNamePlot == "P2Populus fremontii" |
        Scientific_Name == "Fraxinus velutina" |
        ScieNamePlot == "P2Fraxinus velutina" |
        ScieNamePlot == "P5Solanum elaeagnifolium" |  
        TubeTW == "twoTW.MEDIUM" |
        Scientific_Name == "Phyla nodiflora" |
        ScieNamePlot == "P2Phyla nodiflora" |
        ScieNamePlot == "P1Allionia incarnata" |  
        ScieNamePlot == "P5Allionia incarnata" |  
        Mn_ICP_Q == "Mn.VSMALL" |
        ScieNamePlot == "P6Amaranthus palmeri" |  
        ScieNamePlot == "P1Eragrostis lehmanniana" |  
        TubeTW == "twoTW.SMALL" |
        ####TWCuTube == "TW.SMALLCu.SMALLtwo" |
        ####TWCuTube == "TW.SMALLCu.VSMALLtwo" |
        Scientific_Name == "Eragrostis lehmanniana" |
        ScieNamePlot == "P1Xanthisma gracile" |   
        #Type_of_Sample == "leaf" |
       # Form == "Forb" |
        ScieNamePlot == "P2Portulaca suffrutescens" |   
        TubeTW == "twoTW.VSMALL" |
        ####TWCuTube == "TW.VSMALLCu.LARGEtwo" |
        #Plot == "P2" |
        Scientific_Name == "Mentzelia longiloba" |
        ScieNamePlot == "P6Mentzelia longiloba" 
      )
  )




# Find the remaining data points not in `dt_Cu_best`!!!!!!!!!!!!!!!!!!!!!!
dt_Cu_worst <- anti_join(dt, dt_Cu_best, by = "SampleID")

#Remove NAs
dt_Cu_worst <- dt_Cu_worst %>% 
  filter(!is.na(Cu_PXRF))

dt_Cu_best <- dt_Cu_best %>% 
  filter(!is.na(Cu_PXRF))

#Remove errorous plants from dt_Cu_worst - YES

dt_Cu_worst <- dt_Cu_worst %>% 
  filter(
    SampleID!="57" &
    SampleID!="86" &
    SampleID!="27" &
    SampleID!="18" &
    SampleID!="87" &
    SampleID!="53" &
    SampleID!="21" &
    SampleID!="17" &
    SampleID!="116" &
    SampleID!="20" &
    SampleID!="110" &
    SampleID!="56" &
    SampleID!="115" 
  )



#Training model 
{


  # Initialize lists to store RMSE results and model data
  results <- list(M1 = numeric(), M2 = numeric(), M3 = numeric())
  iterations <- 100  # Adjust the number of iterations as desired
  top_models <- list()  # Store top models with high ICP values
  top_n <- 5  # Number of top models to retain
  rmse_threshold <- Inf  # Initialize with infinity
  
  # Loop over multiple repetitions
  for (i in 1:iterations) {
    set.seed(123 + i)  # Ensure reproducibility
    
    # Randomly split the data into training and testing sets
    train_indices <- sample(1:nrow(dt_Cu_worst), 0.75 * nrow(dt_Cu_worst))
    train_data <- dt_Cu_worst[train_indices, ]
    test_data <- dt_Cu_worst[-train_indices, ]
    
    # Train the models
    M1Cu_train <- glm(Cu_ICP ~ Cu_PXRF, data = train_data, family = Gamma(link = "identity"))
    M2Cu_train <- glm(Cu_ICP ~ Cu_PXRF + Total_Weight, data = train_data, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
    M3Cu_train <- glm(Cu_ICP ~ Cu_PXRF + Substrate_RT, data = train_data, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
    M4Cu_train <- glm(Cu_ICP ~ Cu_PXRF + Substrate_RT + Mn_ICP, data = train_data, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
    
    # Make predictions on the test set
    test_data$Predicted_Cu_M1 <- predict(M1Cu_train, newdata = test_data, type = "response")
    test_data$Predicted_Cu_M2 <- predict(M2Cu_train, newdata = test_data, type = "response")
    test_data$Predicted_Cu_M3 <- predict(M3Cu_train, newdata = test_data, type = "response")
    test_data$Predicted_Cu_M4 <- predict(M4Cu_train, newdata = test_data, type = "response")
    
    # Calculate RMSE for each model
    rmse_M1 <- sqrt(mean((test_data$Cu_ICP - test_data$Predicted_Cu_M1)^2))
    rmse_M2 <- sqrt(mean((test_data$Cu_ICP - test_data$Predicted_Cu_M2)^2))
    rmse_M3 <- sqrt(mean((test_data$Cu_ICP - test_data$Predicted_Cu_M3)^2))
    rmse_M4 <- sqrt(mean((test_data$Cu_ICP - test_data$Predicted_Cu_M4)^2))
    # Store RMSE scores for each iteration
    results$M1 <- c(results$M1, rmse_M1)
    results$M2 <- c(results$M2, rmse_M2)
    results$M3 <- c(results$M3, rmse_M3)
    results$M4 <- c(results$M4, rmse_M4)
    # Check if the model includes points above the ICP threshold (e.g., 200) and update the top models
    if (rmse_M3 < rmse_threshold && any(test_data$Cu_ICP > 200)) {
      top_models[[length(top_models) + 1]] <- list(train = train_data, test = test_data, rmse = rmse_M3)
      # Keep only the top N models sorted by RMSE
      top_models <- top_models[order(sapply(top_models, function(x) x$rmse))]
      if (length(top_models) > top_n) {
        top_models <- top_models[1:top_n]
      }
      # Update the RMSE threshold to the current highest RMSE among top models
      rmse_threshold <- max(sapply(top_models, function(x) x$rmse))
    }
  }
  
  # Save RMSE results to an Excel file
  rmse_df <- data.frame(
    Iteration = 1:iterations,
    RMSE_M1 = results$M1,
    RMSE_M2 = results$M2,
    RMSE_M3 = results$M3,
    RMSE_M4 = results$M4
  )
  
  #write_xlsx(rmse_df, "RMSE_Results.xlsx")
  
  # Extract the best set of models for plotting or further analysis
  best_models <- top_models  # List of top models
  
  # You can now select any model in `best_models` for plotting purposes
  # For example, to use the first model in the list:
  best_train <- best_models[[1]]$train
  best_test <- best_models[[1]]$test
  
 #write_xlsx(best_train, "Cu_best_train.xlsx") 
#  write_xlsx(best_test, "Cu_best_test.xlsx")
  
  }


cor.test(dt_Cu_best$Cu_ICP, dt_Cu_best$Cu_PXRF, method="spearman") # 0.97158, p-val < 2.2e-16
cor.test(best_test$Cu_ICP, best_test$Predicted_Cu_M1, method="spearman") # 0.7942799, p-value = 0.0006912
cor.test(best_test$Cu_ICP, best_test$Predicted_Cu_M2, method="spearman") # 0.7934066 , p-value = 0.001151
cor.test(best_test$Cu_ICP, best_test$Predicted_Cu_M3, method="spearman") # 0.8769231 , p-value = < 2.2e-16
cor.test(best_test$Cu_ICP, best_test$Predicted_Cu_M4, method="spearman") # 0.8725275  ,  p-value < 2.2e-16

Cu <- ggplot(data=dt_Cu_best, aes(x = Cu_PXRF, y = Cu_ICP)) +
  geom_point(data=dt_Cu_best, color = "#4793AF", size=2.5, stroke=0.6, shape=1) +
  geom_point(data=best_test, aes(x = Predicted_Cu_M3, y = Cu_ICP), color = "#FEB941", size=2.5, stroke=0.6, shape=3) + # New points
  geom_smooth(data=dt_Cu_best, aes(x = Cu_PXRF, y = Cu_ICP), method = "lm", se = FALSE, color = "#4793AF", linetype = "solid", size=0.65) +   # Regression line for the first model
  geom_smooth(data=best_test, aes(x = Predicted_Cu_M3, y = Cu_ICP), method = "lm", se = FALSE, color = "#FEB941", linetype = "solid", size=0.65) +  # Regression line for the second model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  geom_point(data=dt_Cu_worst, aes(x = Cu_PXRF, y = Cu_ICP), color = "#8B322C", size=2.5, stroke=0.6, shape=4) + # New points
  geom_smooth(data=dt_Cu_worst, aes(x = Cu_PXRF, y = Cu_ICP), method = "lm", se = FALSE, color = "#8B322C", linetype = "solid", size=0.65) +  
  labs(x = "pXRF Cu", y = "ICP concentration Cu") +
  scale_y_continuous(limits = c(0, 750), breaks = seq(0, 750, by = 150)) +
  scale_x_continuous(limits = c(0, 750), breaks = seq(0, 750, by = 150)) +
  theme_classic() +  # Using theme_classic as theme_classic2 is not part of base ggplot2
  theme(panel.grid.major = element_blank(), # Removing major grid lines
        panel.grid.minor = element_blank(), # Removing minor grid lines
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5), # Adding border around the plot using updated argument
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.title = element_text(size = 16, face = "bold"),  # Customize plot title
        axis.title = element_text(size = 20),  # Customize axis labels
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 16, face = "bold"),
        legend.position = "top")

print(Cu)


pzoom <- ggplot(data=dt_Cu_best, aes(x = Cu_PXRF, y = Cu_ICP)) +
  geom_point(data=dt_Cu_best, color = "#4793AF", size=2.5, stroke=0.6, shape=1) +
  geom_point(data=best_test, aes(x = Predicted_Cu_M3, y = Cu_ICP), color = "#FEB941", size=2.5, stroke=0.6, shape=3) + # New points
  geom_smooth(data=dt_Cu_best, aes(x = Cu_PXRF, y = Cu_ICP), method = "lm", se = FALSE, color = "#4793AF", linetype = "solid", size=0.65) +   # Regression line for the first model
  geom_smooth(data=best_test, aes(x = Predicted_Cu_M3, y = Cu_ICP), method = "lm", se = FALSE, color = "#FEB941", linetype = "solid", size=0.65) +  # Regression line for the second model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  geom_point(data=dt_Cu_worst, aes(x = Cu_PXRF, y = Cu_ICP), color = "#8B322C", size=2.5, stroke=0.6, shape=4) + # New points
  geom_smooth(data=dt_Cu_worst, aes(x = Cu_PXRF, y = Cu_ICP), method = "lm", se = FALSE, color = "#8B322C", linetype = "solid", size=0.65) +  
  labs(x = "pXRF Cu", y = "ICP concentration Cu") +
  coord_cartesian(xlim = c(0, 150), ylim = c(0, 150)) +
  scale_x_continuous(breaks = seq(0, 150, by = 25)) +  # Set x-axis breaks
  scale_y_continuous(breaks = seq(0, 150, by = 25)) +
  theme_classic() +  # Using theme_classic as theme_classic2 is not part of base ggplot2
  theme(panel.grid.major = element_blank(), # Removing major grid lines
        panel.grid.minor = element_blank(), # Removing minor grid lines
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5), # Adding border around the plot using updated argument
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.title = element_text(size = 16, face = "bold"),  # Customize plot title
        axis.title = element_text(size = 20),  # Customize axis labels
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 16, face = "bold"),
        legend.position = "top")

print(pzoom)

#################SAVE BEST MODEL DATASETS AND SUMMARY BECAUSE IT WILL BE GONE.


# Load the writexl package for exporting to Excel
library(writexl)

# Select a specific model from `best_models`
selected_model <- 2  # Adjust this index to select a different model
best_train <- best_models[[selected_model]]$train
best_test <- best_models[[selected_model]]$test

# Save the best train and test data to separate sheets in an Excel file
write_xlsx(
  list(
    Best_Train = best_train,
    Best_Test = best_test
  ),
  "Best_Train_Test.xlsx"
)

# Fit the selected model for coefficients analysis
M1Cu_train <- glm(Cu_ICP ~ Cu_PXRF, data = best_train, family = Gamma(link = "identity"))
M2Cu_train <- glm(Cu_ICP ~ Cu_PXRF + Total_Weight, data = best_train, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
M3Cu_train <- glm(Cu_ICP ~ Cu_PXRF + Substrate_RT, data = best_train, family = Gamma(link = "identity"), control = glm.control(maxit = 50))

# Print model summaries to obtain coefficients
cat("Summary of M1 Model:\n")
print(summary(M1Cu_train))
cat("\nSummary of M2 Model:\n")
print(summary(M2Cu_train))
cat("\nSummary of M3 Model:\n")
print(summary(M3Cu_train))


##### Se modele
{
  dt <-read.delim("PXRF_models.txt")
# Define the filter criteria for conditions with '-' sign (included) and '+' sign (excluded)
dt_Se_best <- dt %>% 
  filter(
    # Include only those with '-' sign
    (
      Type_of_Sample == "leaf" |
        Mn_ICP_Q == "Mn.LARGE" |
        ####Se_ICP_Q == "Se.LARGE" |
        ####Se_ICP_Q == "Se.MEDIUM" |
        Form == "Tree" |
        Plot == "P6" |
        ####TWSe == "TW.LARGESe.MEDIUM" |
        ####TWSeTube == "TW.LARGESe.MEDIUMtwo" |
        Plot == "P2" |
        ####TWSeTube == "TW.LARGESe.LARGEtwo" |
        ####TWSeTube == "TW.SMALLSe.LARGEone" |
        ####TWSeTube == "TW.SMALLSe.SMALLone" |
        ####TWSe == "TW.LARGESe.LARGE" |
        ####TWSe == "TW.SMALLSe.LARGE" |
        ScieNamePlot == "P6Xanthisma gracile" 

    ) &
      # Exclude those with '+' sign
      !(
        ####Se_ICP_Q == "Se.VSMALL" |
          Scientific_Name == "Bouteloua curtipendula" |
          ScieNamePlot == "P1Bouteloua curtipendula" |
          ####TWSe == "TW.MEDIUMSe.VSMALL" |
          Scientific_Name == "Amaranthus palmeri" |
          ScieNamePlot == "P5Amaranthus palmeri" |
          ####TWSeTube == "TW.MEDIUMSe.VSMALLtwo" |
          ####TWSeTube == "TW.LARGESe.VSMALLtwo" |
          ####TWSe == "TW.LARGESe.VSMALL" |
          Mn_ICP_Q == "Mn.VSMALL" |
          ####TWSeTube == "TW.SMALLSe.VSMALLone" |
          Type_of_Sample == "leaf-stem" |
          ScieNamePlot == "P1Euphorbia melanadenia" |
          ####TWSe == "TW.SMALLSe.VSMALL" |
          Scientific_Name == "Pectis papposa" |
          ScieNamePlot == "P5Pectis papposa" |
          Scientific_Name == "Euphorbia melanadenia" |
          ####TWSeTube == "TW.MEDIUMSe.VSMALLone" |
          Plot == "P1" |
          ####TWSeTube == "TW.LARGESe.SMALLone" |
          Form == "Grass" |
          Plot == "P5" 
      )
  )


dt_Se_worst <- anti_join(dt, dt_Se_best, by = "SampleID")

dt_Se_best <- dt_Se_best %>% 
  filter(!is.na(Se_PXRF))

dt_Se_worst <- dt_Se_worst %>% 
  filter(!is.na(Se_PXRF))






# Initialize lists to store RMSE results and model data
results <- list(M1 = numeric(), M2 = numeric(), M3 = numeric())
iterations <- 100  # Adjust the number of iterations as desired
top_models <- list()  # Store top models with high ICP values
top_n <- 5  # Number of top models to retain
rmse_threshold <- Inf  # Initialize with infinity

# Loop over multiple repetitions
for (i in 1:iterations) {
  set.seed(123 + i)  # Ensure reproducibility
  
  # Randomly split the data into training and testing sets
  train_indices <- sample(1:nrow(dt_Se_worst), 0.8 * nrow(dt_Se_worst))
  train_data <- dt_Se_worst[train_indices, ]
  test_data <- dt_Se_worst[-train_indices, ]
  
  # Train the models
  start_vals <- c("(Intercept)" = 1, "Se_PXRF" = 0.1)
  start_vals2 <- c("(Intercept)" = 1, "Se_PXRF" = 0.1, "Total_Weight" = 0.1)
  start_vals3 <- c("(Intercept)" = 1, "Se_PXRF" = 0.1, "Substrate_RT" = 0.1)
  start_vals4 <- c("(Intercept)" = 1, "Se_PXRF" = 0.1, "Substrate_RT" = 0.1, "Mn_ICP" = 0.1)
  


  M1Se_train <- glm(Se_ICP ~ Se_PXRF, data = train_data, family = Gamma(link = "identity"), start = start_vals)
  M2Se_train <- glm(Se_ICP ~ Se_PXRF + Total_Weight, data = train_data, family = Gamma(link = "identity"),  start = start_vals2, control = glm.control(maxit = 50))
  M3Se_train <- glm(Se_ICP ~ Se_PXRF + Substrate_RT, data = train_data, family = Gamma(link = "identity"), start = start_vals3,  control = glm.control(maxit = 50))
  M4Se_train <- glm(Se_ICP ~ Se_PXRF + Substrate_RT + Mn_ICP, data = train_data, family = Gamma(link = "identity"), start = start_vals4,  control = glm.control(maxit = 50))
  
  
  # Make predictions on the test set
  test_data$Predicted_Se_M1 <- predict(M1Se_train, newdata = test_data, type = "response")
  test_data$Predicted_Se_M2 <- predict(M2Se_train, newdata = test_data, type = "response")
  test_data$Predicted_Se_M3 <- predict(M3Se_train, newdata = test_data, type = "response")
  test_data$Predicted_Se_M4 <- predict(M4Se_train, newdata = test_data, type = "response")
  
  # Calculate RMSE for each model
  rmse_M1 <- sqrt(mean((test_data$Se_ICP - test_data$Predicted_Se_M1)^2))
  rmse_M2 <- sqrt(mean((test_data$Se_ICP - test_data$Predicted_Se_M2)^2))
  rmse_M3 <- sqrt(mean((test_data$Se_ICP - test_data$Predicted_Se_M3)^2))
  rmse_M4 <- sqrt(mean((test_data$Se_ICP - test_data$Predicted_Se_M4)^2))
  
  # Store RMSE scores for each iteration
  results$M1 <- c(results$M1, rmse_M1)
  results$M2 <- c(results$M2, rmse_M2)
  results$M3 <- c(results$M3, rmse_M3)
  results$M4 <- c(results$M4, rmse_M4)
  
  # Check if the model includes points above the ICP threshold (e.g., 200) and update the top models
  if (rmse_M3 < rmse_threshold) {
    top_models[[length(top_models) + 1]] <- list(train = train_data, test = test_data, rmse = rmse_M3)
    # Keep only the top N models sorted by RMSE
    top_models <- top_models[order(sapply(top_models, function(x) x$rmse))]
    if (length(top_models) > top_n) {
      top_models <- top_models[1:top_n]
    }
    # Update the RMSE threshold to the current highest RMSE among top models
    rmse_threshold <- max(sapply(top_models, function(x) x$rmse))
  }
}

# Save RMSE results to an Excel file
rmse_df <- data.frame(
  Iteration = 1:iterations,
  RMSE_M1 = results$M1,
  RMSE_M2 = results$M2,
  RMSE_M3 = results$M3,
  RMSE_M4 = results$M4
)

#write_xlsx(rmse_df, "RMSE_Results_Se.xlsx")

# Extract the best set of models for plotting or further analysis
best_models <- top_models  # List of top models

# You can now select any model in `best_models` for plotting purposes
# For example, to use the first model in the list:
best_train <- best_models[[3]]$train
best_test <- best_models[[3]]$test


#write_xlsx(best_train, "Se_best_train.xlsx")
#write_xlsx(best_test, "Se_best_test.xlsx")


cor.test(dt_Se_best$Se_ICP, dt_Se_best$Se_PXRF, method="spearman") # 0.9010794 ,  p-value = 1.139e-11
cor.test(best_test$Se_ICP, best_test$Predicted_Se_M1, method="spearman") # 0.8376766, p-value = 9.708e-05
cor.test(best_test$Se_ICP, best_test$Predicted_Se_M2, method="spearman") # 0.8964286  , p-value < 2.2e-16
cor.test(best_test$Se_ICP, best_test$Predicted_Se_M3, method="spearman") # 0.9071429  , p-value = < 2.2e-16
cor.test(best_test$Se_ICP, best_test$Predicted_Se_M4, method="spearman") # 0.9178571  ,  p-value < 2.2e-16

Se <- ggplot(data=dt_Se_best, aes(x = Se_PXRF, y = Se_ICP)) +
  geom_point(data=dt_Se_best, color = "#4793AF", size=2.5, stroke=0.6, shape=1) +
  geom_point(data=best_test, aes(x = Predicted_Se_M3, y = Se_ICP), color = "#FEB941", size=2.5, stroke=0.6, shape=3) + # New points
  geom_smooth(data=dt_Se_best, aes(x = Se_PXRF, y = Se_ICP), method = "lm", se = FALSE, color = "#4793AF", linetype = "solid", size=0.65) +   # Regression line for the first model
  geom_smooth(data=best_test, aes(x = Predicted_Se_M3, y = Se_ICP), method = "lm", se = FALSE, color = "#FEB941", linetype = "solid", size=0.65) +  # Regression line for the second model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  geom_point(data=dt_Se_worst, aes(x = Se_PXRF, y = Se_ICP), color = "#8B322C", size=2.5, stroke=0.6, shape=4) + # New points
  geom_smooth(data=dt_Se_worst, aes(x = Se_PXRF, y = Se_ICP), method = "lm", se = FALSE, color = "#8B322C", linetype = "solid", size=0.65) +  
  labs(x = "pXRF Cu", y = "ICP concentration Cu") +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10)) +
  scale_x_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10)) +
  theme_classic() +  # Using theme_classic as theme_classic2 is not part of base ggplot2
  theme(panel.grid.major = element_blank(), # Removing major grid lines
        panel.grid.minor = element_blank(), # Removing minor grid lines
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5), # Adding border around the plot using updated argument
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.title = element_text(size = 16, face = "bold"),  # Customize plot title
        axis.title = element_text(size = 20),  # Customize axis labels
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 16, face = "bold"),
        legend.position = "top")

print(Se)




Sezoom <- ggplot(data=dt_Se_best, aes(x = Se_PXRF, y = Se_ICP)) +
  geom_point(data=dt_Se_best, color = "#4793AF", size=2.5, stroke=0.6, shape=1) +
  geom_point(data=best_test, aes(x = Predicted_Se_M3, y = Se_ICP), color = "#FEB941", size=2.5, stroke=0.6, shape=3) + # New points
  geom_smooth(data=dt_Se_best, aes(x = Se_PXRF, y = Se_ICP), method = "lm", se = FALSE, color = "#4793AF", linetype = "solid", size=0.65) +   # Regression line for the first model
  geom_smooth(data=best_test, aes(x = Predicted_Se_M3, y = Se_ICP), method = "lm", se = FALSE, color = "#FEB941", linetype = "solid", size=0.65) +  # Regression line for the second model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  geom_point(data=dt_Se_worst, aes(x = Se_PXRF, y = Se_ICP), color = "#8B322C", size=2.5, stroke=0.6, shape=4) + # New points
  geom_smooth(data=dt_Se_worst, aes(x = Se_PXRF, y = Se_ICP), method = "lm", se = FALSE, color = "#8B322C", linetype = "solid", size=0.65) +  
  labs(x = "pXRF Cu", y = "ICP concentration Cu") +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  scale_x_continuous(breaks = seq(0, 10, by = 2)) +  # Set x-axis breaks
  scale_y_continuous(breaks = seq(0, 10, by = 2)) +
  theme_classic() +  # Using theme_classic as theme_classic2 is not part of base ggplot2
  theme(panel.grid.major = element_blank(), # Removing major grid lines
        panel.grid.minor = element_blank(), # Removing minor grid lines
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5), # Adding border around the plot using updated argument
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.title = element_text(size = 16, face = "bold"),  # Customize plot title
        axis.title = element_text(size = 20),  # Customize axis labels
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 16, face = "bold"),
        legend.position = "top")

print(Sezoom)




}


##### Re modele
{
dt <-read.delim("PXRF_models.txt")

dt_Re_best <- dt %>%
  filter(
    # Include criteria indicated with '-'
    (
      Re_ICP_Q == "Re.LARGE" |
      Tube_No =="two"  
        ) &
      # Exclude criteria indicated with '+'
      !(
        Scientific_Name == "Xanthisma gracile" |
          ScieNamePlot == "P2Xanthisma gracile" |
          Re_ICP_Q == "Re.VSMALL" |
          Scientific_Name == "Mentzelia longiloba" | 
          ScieNamePlot == "P1Mentzelia longiloba)" |
          Zn_ICP_Q == "Zn.LARGE" |
          Tube_No =="one"
      )
  )



dt_Re_worst <- anti_join(dt, dt_Re_best, by = "SampleID")

dt_Re_worst <- dt_Re_worst %>% 
  filter(!is.na(Re_PXRF))

dt_Re_best <- dt_Re_best %>% 
  filter(!is.na(Re_PXRF))


# Model Performance - RMSE
rmse_best_RAW <- sqrt(mean((dt_Re_best$Re_ICP - dt_Re_best$Re_PXRF)^2)) # not much difference between those two, 2 values of RMSE
rmse_worst_RAW <- sqrt(mean((dt_Re_worst$Re_ICP - dt_Re_worst$Re_PXRF)^2))




Re <- ggplot(data=dt_Re_best, aes(x = Re_PXRF, y = Re_ICP)) +
  geom_point(data=dt_Re_best, color = "#4793AF", size=2.5, stroke=0.6, shape=1) +
 # geom_point(data=best_test, aes(x = Predicted_Cu_M3, y = Cu_ICP), color = "#AD0B0B", size=2.5, stroke=0.6, shape=3) + # New points
  geom_smooth(data=dt_Re_best, aes(x = Re_PXRF, y = Re_ICP), method = "lm", se = FALSE, color = "#4793AF", linetype = "solid", size=0.65) +   # Regression line for the first model
 # geom_smooth(data=best_test, aes(x = Predicted_Cu_M3, y = Cu_ICP), method = "lm", se = FALSE, color = "#AD0B0B", linetype = "solid", size=0.65) +  # Regression line for the second model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  geom_point(data=dt_Re_worst, aes(x = Re_PXRF, y = Re_ICP), color = "#8B322C", size=2.5, stroke=0.6, shape=4) + # New points
  geom_smooth(data=dt_Re_worst, aes(x = Re_PXRF, y = Re_ICP), method = "lm", se = FALSE, color = "#8B322C", linetype = "solid", size=0.65) +  
  labs(x = "pXRF Re", y = "ICP concentration Re") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25)) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25)) +
  theme_classic() +  # Using theme_classic as theme_classic2 is not part of base ggplot2
  theme(panel.grid.major = element_blank(), # Removing major grid lines
        panel.grid.minor = element_blank(), # Removing minor grid lines
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5), # Adding border around the plot using updated argument
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.title = element_text(size = 16, face = "bold"),  # Customize plot title
        axis.title = element_text(size = 20),  # Customize axis labels
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 16, face = "bold"),
        legend.position = "top")

print(Re)


Re <- ggplot(data=dt_Re_best, aes(x = Re_PXRF, y = Re_ICP)) +
  geom_point(data=dt_Re_best, color = "#4793AF", size=2.5, stroke=0.6, shape=1) +
  # geom_point(data=best_test, aes(x = Predicted_Cu_M3, y = Cu_ICP), color = "#AD0B0B", size=2.5, stroke=0.6, shape=3) + # New points
  geom_smooth(data=dt_Re_best, aes(x = Re_PXRF, y = Re_ICP), method = "lm", se = FALSE, color = "#4793AF", linetype = "solid", size=0.65) +   # Regression line for the first model
  # geom_smooth(data=best_test, aes(x = Predicted_Cu_M3, y = Cu_ICP), method = "lm", se = FALSE, color = "#AD0B0B", linetype = "solid", size=0.65) +  # Regression line for the second model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  geom_point(data=dt_Re_worst, aes(x = Re_PXRF, y = Re_ICP), color = "#8B322C", size=2.5, stroke=0.6, shape=4) + # New points
  geom_smooth(data=dt_Re_worst, aes(x = Re_PXRF, y = Re_ICP), method = "lm", se = FALSE, color = "#8B322C", linetype = "solid", size=0.65) +  
  labs(x = "pXRF Re", y = "ICP concentration Re") +
  coord_cartesian(xlim = c(0, 25), ylim = c(0, 25)) +
  scale_x_continuous(breaks = seq(0, 25, by = 5)) +  # Set x-axis breaks
  scale_y_continuous(breaks = seq(0, 25, by = 5)) +
  theme_classic() +  # Using theme_classic as theme_classic2 is not part of base ggplot2
  theme(panel.grid.major = element_blank(), # Removing major grid lines
        panel.grid.minor = element_blank(), # Removing minor grid lines
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5), # Adding border around the plot using updated argument
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.title = element_text(size = 16, face = "bold"),  # Customize plot title
        axis.title = element_text(size = 20),  # Customize axis labels
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 16, face = "bold"),
        legend.position = "top")

print(Re)

}


##################Zn Models



dt_Zn_best <- dt %>%
  filter(
    # Include criteria indicated with '-'
    (
      TubeTW == "oneTW.MEDIUM" |
      Fe_ICP_Q == "Fe.SMALL" |
      Form == "Shrub" | 
      TubeTW == "oneTW.LARGE" |
      TWZnTube == "TW.LARGEZn.VSMALLone" |
      Form == "Tree" |  
      Type_of_Sample == "leaf" | 
      Scientific_Name == "Xanthisma gracile" |
      Scientific_Name == "Ceanothus greggii" |
        TW_Q == "TW.LARGE" |
        Scientific_Name == "Mimosa biuncifera (=aculeaticarpa)" |
        TWZnTube == "TW.MEDIUMZn.MEDIUMone" |
        Scientific_Name == "Gutierrezia sarothrae" |
        ScieNamePlot == "P1Ceanothus greggii" |
        Scientific_Name == "Baccharis sarothroides" |
        Scientific_Name == "Senegalia (Acacia) greggii" |
        ScieNamePlot == "P2Senegalia (Acacia) greggii" |
        ScieNamePlot == "P5Gutierrezia sarothrae" |
        ScieNamePlot == "P6Xanthisma gracile" |
        TubeTW == "oneTW.SMALL" |
        Tube_No == "one" |
        Scientific_Name == "Datura wrightii" |
        ScieNamePlot == "P1Datura wrightii" |
        TWZnTube == "TW.MEDIUMZn.LARGEone" |
        TWZnTube == "TW.SMALLZn.MEDIUMone" |
        ScieNamePlot == "P2Xanthisma gracile" |
        ScieNamePlot == "P2Baccharis sarothroides" |
        ScieNamePlot == "P5Mimosa biuncifera (=aculeaticarpa)" |
        TWZnTube == "TW.MEDIUMZn.VSMALLone" |
        Scientific_Name == "Isocoma acradenia" |
        ScieNamePlot == "P2Isocoma acradenia" |
        Zn_ICP_Q == "Zn.VSMALL" |
        ScieNamePlot == "P1Mimosa biuncifera (=aculeaticarpa)" |
        TubeTW == "twoTW.LARGE" |
        Scientific_Name == "Juniperus arizonica" |
        TWZnTube == "TW.LARGEZn.SMALLtwo" |
        TWZn == "TW.LARGEZn.SMALL" |
        TWZnTube == "TW.LARGEZn.LARGEtwo" |
        TWZn == "TW.LARGEZn.LARGE" 
        
    ) &
      # Exclude criteria indicated with '+'
      !(
        #Form == "Grass" |
          TW_Q == "TW.VSMALL" |
          TWZnTube == "TW.VSMALLZn.LARGEone" |
          TWZn == "TW.VSMALLZn.LARGE" |
          TWZn == "oneTW.VSMALL" |
          Scientific_Name == "Mentzelia longiloba" |
          Scientific_Name == "Bothriochloa barbinodis" |
          ScieNamePlot == "P6Mentzelia longiloba" |
          ScieNamePlot == "P2Portulaca suffrutescens" |
          #Type_of_Sample == "leaf-stem" |
          TWZnTube == "TW.MEDIUMZn.SMALLtwo" |
          TubeTW == "twoTW.MEDIUM" |
          ScieNamePlot == "P1Mentzelia longiloba" |
          TWZnTube == "TW.SMALLZn.SMALLtwo" |
          TWZnTube == "TW.VSMALLZn.MEDIUMone" |
          TWZn == "VSMALLZn.MEDIUM" |
          Scientific_Name == "Portulaca oleracea" |
          ScieNamePlot == "P5Portulaca oleracea" |
          Scientific_Name == "Portulaca suffrutescens" |
          Scientific_Name == "Phyla nodiflora" |
          ScieNamePlot == "P2Phyla nodiflora" |
          ScieNamePlot == "P1Bothriochloa barbinodis" |
          Scientific_Name == "Cynodon dactylon" |
          Scientific_Name == "Amaranthus palmeri" |
          Scientific_Name == "Eragrostis lehmanniana" |
          TWZnTube == "TW.MEDIUMZn.MEDIUMtwo" |
          Fe_ICP_Q == "Fe.MEDIUM" |
          Fe_ICP_Q == "Fe.VSMALL" |
          ScieNamePlot == "P5Bothriochloa barbinodis" |
          Scientific_Name == "Allionia incarnata" |
          ScieNamePlot == "P1Allionia incarnata" |
          Scientific_Name == "Solanum elaeagnifolium" |
          ScieNamePlot == "P2Solanum elaeagnifolium" |
          TWZn == "TW.MEDIUMZn.SMALL" |
          ScieNamePlot == "P5Amaranthus palmeri" |
          #Tube_No == "two" |
          Scientific_Name == "Pseudognaphalium canescens" |
          ScieNamePlot == "P6Pseudognaphalium canescens" |
          ScieNamePlot == "P5Dasyochloa pulchella" |
          #Form == "Forb" |
         # Plot == "P6" |
          TubeTW == "twoTW.SMALL" |
          ScieNamePlot == "P5Cynodon dactylon" |
          Scientific_Name == "Eragrostis curvula" |
          ScieNamePlot == "P6Eragrostis curvula" |
          ScieNamePlot == "P1Pectis papposa" |
          TWZnTube == "TW.SMALLZn.MEDIUMtwo" |
          ScieNamePlot == "P6Eragrostis lehmanniana" |
          ScieNamePlot == "P6Portulaca suffrutescens" |
          ScieNamePlot == "P1Portulaca suffrutescens" 
          
          
      )
  )



dt_Zn_worst <- anti_join(dt, dt_Zn_best, by = "SampleID")

dt_Zn_worst <- dt_Zn_worst %>% 
  filter(!is.na(Zn_PXRF))

dt_Zn_best <- dt_Zn_best %>% 
  filter(!is.na(Zn_PXRF))

ggplot(data=dt_Zn_best, aes(x = Zn_PXRF, y = Zn_ICP)) +
  geom_point(data=dt_Zn_best, color = "#003f5c", size=2.5, stroke=0.6, shape=1) +
  geom_smooth(data=dt_Zn_best, aes(x = Zn_PXRF, y =Zn_ICP), method = "lm", se = FALSE, color = "#003f5c", linetype = "solid", size=0.65) +   # Regression line for the first model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  geom_point(data=dt_Zn_worst, aes(x = Zn_PXRF, y = Zn_ICP), color = "green", size=2.5, stroke=0.6, shape=4) + # New points
  geom_smooth(data=dt_Zn_worst, aes(x = Zn_PXRF, y = Zn_ICP), method = "lm", se = FALSE, color = "green", linetype = "solid", size=0.65) +  
  labs(x = "pXRF ", y = "ICP") +
  theme_classic()  


rmse_best_RAW <- sqrt(mean((dt_Zn_best$Zn_ICP - dt_Zn_best$Zn_PXRF)^2)) # not much difference between those two, 2 values of RMSE
rmse_worst_RAW <- sqrt(mean((dt_Zn_worst$Zn_ICP - dt_Zn_worst$Zn_PXRF)^2))

lm1 <- lm(dt_Zn_best$Zn_ICP~dt_Zn_best$Zn_PXRF)
summary(lm1)

lm1 <- lm(dt_Zn_worst$Zn_ICP~dt_Zn_worst$Zn_PXRF)
summary(lm1)




# Initialize lists to store RMSE results and model data
results <- list(M1 = numeric(), M2 = numeric(), M3 = numeric())
iterations <- 100  # Adjust the number of iterations as desired
top_models <- list()  # Store top models with high ICP values
top_n <- 5  # Number of top models to retain
rmse_threshold <- Inf  # Initialize with infinity

# Loop over multiple repetitions
for (i in 1:iterations) {
  set.seed(123 + i)  # Ensure reproducibility
  
  # Randomly split the data into training and testing sets
  train_indices <- sample(1:nrow(dt_Zn_worst), 0.8 * nrow(dt_Zn_worst))
  train_data <- dt_Zn_worst[train_indices, ]
  test_data <- dt_Zn_worst[-train_indices, ]
  
  # Train the models
  M1Cu_train <- glm(Zn_ICP ~ Zn_PXRF, data = train_data, family = Gamma(link = "identity"))
  M2Cu_train <- glm(Zn_ICP ~ Zn_PXRF + Total_Weight, data = train_data, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Cu_train <- glm(Zn_ICP ~ Zn_PXRF + Substrate_RT, data = train_data, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  
  # Make predictions on the test set
  test_data$Predicted_Zn_M1 <- predict(M1Cu_train, newdata = test_data, type = "response")
  test_data$Predicted_Zn_M2 <- predict(M2Cu_train, newdata = test_data, type = "response")
  test_data$Predicted_Zn_M3 <- predict(M3Cu_train, newdata = test_data, type = "response")
  
  # Calculate RMSE for each model
  rmse_M1 <- sqrt(mean((test_data$Zn_ICP - test_data$Predicted_Zn_M1)^2))
  rmse_M2 <- sqrt(mean((test_data$Zn_ICP - test_data$Predicted_Zn_M2)^2))
  rmse_M3 <- sqrt(mean((test_data$Zn_ICP - test_data$Predicted_Zn_M3)^2))
  
  # Store RMSE scores for each iteration
  results$M1 <- c(results$M1, rmse_M1)
  results$M2 <- c(results$M2, rmse_M2)
  results$M3 <- c(results$M3, rmse_M3)
  
  # Check if the model includes points above the ICP threshold (e.g., 200) and update the top models
  if (rmse_M3 < rmse_threshold && any(test_data$Zn_ICP > 200)) {
    top_models[[length(top_models) + 1]] <- list(train = train_data, test = test_data, rmse = rmse_M3)
    # Keep only the top N models sorted by RMSE
    top_models <- top_models[order(sapply(top_models, function(x) x$rmse))]
    if (length(top_models) > top_n) {
      top_models <- top_models[1:top_n]
    }
    # Update the RMSE threshold to the current highest RMSE among top models
    rmse_threshold <- max(sapply(top_models, function(x) x$rmse))
  }
}

# Save RMSE results to an Excel file
rmse_df <- data.frame(
  Iteration = 1:iterations,
  RMSE_M1 = results$M1,
  RMSE_M2 = results$M2,
  RMSE_M3 = results$M3
)

#write_xlsx(rmse_df, "RMSE_Results.xlsx")

# Extract the best set of models for plotting or further analysis
best_models <- top_models  # List of top models

# You can now select any model in `best_models` for plotting purposes
# For example, to use the first model in the list:
best_train <- best_models[[2]]$train
best_test <- best_models[[2]]$test





#### Szybki wykres NA 
{
dt_NA_Cu <- dt %>% 
  filter(is.na(Cu_PXRF))


ggplot(data=dt_NA_Cu, aes(x = Total_Weight, y = Cu_ICP)) +
  geom_point(color = "#003f5c", size=2.5, stroke=1, shape=2) +
  labs(x = "TW", y = "ICP concentration Cu") +
  theme_classic() 


dt_NA_Zn <- dt %>% 
  filter(is.na(Zn_PXRF))


ggplot(data=dt_NA_Zn, aes(x = Total_Weight, y = Zn_ICP)) +
  geom_point(color = "#003f5c", size=2.5, stroke=1, shape=2) +
  labs(x = "TW", y = "ICP concentration Zn") +
  theme_classic() 


dt_NA_Mn <- dt %>% 
  filter(is.na(Mn_PXRF))


ggplot(data=dt_NA_Mn, aes(x = Total_Weight, y = Mn_ICP)) +
  geom_point(color = "#003f5c", size=2.5, stroke=1, shape=2) +
  labs(x = "TW", y = "ICP concentration Mn") +
  theme_classic() 


dt_NA_Se <- dt %>% 
  filter(is.na(Se_PXRF))


ggplot(data=dt_NA_Se, aes(x = Total_Weight, y = Se_ICP)) +
  geom_point(color = "#003f5c", size=2.5, stroke=1, shape=2) +
  labs(x = "TW", y = "ICP concentration Se") +
  theme_classic() 

  }




#####Se wykres


Se <- ggplot(data=dt, aes(x = Se_PXRF, y = Se_ICP)) +
  geom_point(data=dt, color = "#4793AF", size=2.5, stroke=0.6, shape=1) +
  #geom_point(data=best_test, aes(x = Predicted_Se_M3, y = Se_ICP), color = "#FEB941", size=2.5, stroke=0.6, shape=3) + # New points
  geom_smooth(data=dt_Se_best, aes(x = Se_PXRF, y = Se_ICP), method = "lm", se = FALSE, color = "#4793AF", linetype = "solid", size=0.65) +   # Regression line for the first model
  #geom_smooth(data=best_test, aes(x = Predicted_Se_M3, y = Se_ICP), method = "lm", se = FALSE, color = "#FEB941", linetype = "solid", size=0.65) +  # Regression line for the second model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  #geom_point(data=dt_Se_worst, aes(x = Se_PXRF, y = Se_ICP), color = "#8B322C", size=2.5, stroke=0.6, shape=4) + # New points
  #geom_smooth(data=dt_Se_worst, aes(x = Se_PXRF, y = Se_ICP), method = "lm", se = FALSE, color = "#8B322C", linetype = "solid", size=0.65) +  
  labs(x = "pXRF Cu", y = "ICP concentration Cu") +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10)) +
  scale_x_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10)) +
  theme_classic() +  # Using theme_classic as theme_classic2 is not part of base ggplot2
  theme(panel.grid.major = element_blank(), # Removing major grid lines
        panel.grid.minor = element_blank(), # Removing minor grid lines
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5), # Adding border around the plot using updated argument
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.title = element_text(size = 16, face = "bold"),  # Customize plot title
        axis.title = element_text(size = 20),  # Customize axis labels
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 16, face = "bold"),
        legend.position = "top")

print(Se)
