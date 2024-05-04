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
  
}

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/Models_pxrf")
dt <-read.delim("PXRF_models.txt")


#Best Cu

dt_Cu_best <- dt %>% 
  filter(
    !(TW_Q == "TW.VSMALL" |
        TubeTW == "oneTW.VSMALL" |
        Scientific_Name == "Allionia incarnata" |
        Tube_No == "two" |
        TWCu == "TW.VSMALLCu.LARGE" |
        TWCuTube == "TW.MEDIUMCu.SMALLtwo" |
        Scientific_Name == "Solanum elaeagnifolium" |
        Scientific_Name == "Populus fremontii" |
        ScieNamePlot == "P2Populus fremontii" |
        Scientific_Name == "Fraxinus velutina" |
        ScieNamePlot == "P2Fraxinus velutina" |
        Scientific_Name == "Phyla nodiflora" |
        ScieNamePlot == "P2Phyla nodiflora" |
        ScieNamePlot == "P1Allionia incarnata" |
        ScieNamePlot == "P5Allionia incarnata" |
        Mn_ICP_Q == "Mn.VSMALL" |
        ScieNamePlot == "P6Amaranthus palmeri" |
        TubeTW == "twoTW.MEDIUM" |
        TWCuTube == "TW.SMALLCu.SMALLtwo" |
        TWCuTube == "TW.SMALLCu.VSMALLtwo" |
        Scientific_Name == "Eragrostis lehmanniana") &
      (TubeTW == "oneTW.SMALL" |
         TW_Q == "TW.SMALL" |
         TubeTW == "oneTW.MEDIUM" |
         Scientific_Name == "Xanthisma gracile" |
         TWCu == "TW.SMALLCu.LARGE" |
         ScieNamePlot == "P5Xanthisma gracile" |
         ScieNamePlot == "P5Gutierrezia sarothrae" |
         TWCuTube == "TW.SMALLCu.VSMALLone" |
         TWCuTube == "TW.SMALLCu.SMALLone" |
         Scientific_Name == "Ceanothus greggii" |
         TWCuTube == "TW.MEDIUMCu.LARGEone" |
         Scientific_Name == "Mimosa biuncifera (=aculeaticarpa)" |
         ScieNamePlot == "P1Xanthisma gracile" |
         TWCuTube == "TW.MEDIUMCu.SMALLone" |
         ScieNamePlot == "P1Euphorbia melanadenia" |
         ScieNamePlot == "P6Xanthisma gracile")
  )


dt_Cu_worst <- dt %>% 
  filter(
    (TW_Q == "TW.VSMALL" |
        TubeTW == "oneTW.VSMALL" |
        Scientific_Name == "Allionia incarnata" |
        Tube_No == "two" |
        TWCu == "TW.VSMALLCu.LARGE" |
        TWCuTube == "TW.MEDIUMCu.SMALLtwo" |
        Scientific_Name == "Solanum elaeagnifolium" |
        Scientific_Name == "Populus fremontii" |
        ScieNamePlot == "P2Populus fremontii" |
        Scientific_Name == "Fraxinus velutina" |
        ScieNamePlot == "P2Fraxinus velutina" |
        Scientific_Name == "Phyla nodiflora" |
        ScieNamePlot == "P2Phyla nodiflora" |
        ScieNamePlot == "P1Allionia incarnata" |
        ScieNamePlot == "P5Allionia incarnata" |
        Mn_ICP_Q == "Mn.VSMALL" |
        ScieNamePlot == "P6Amaranthus palmeri" |
        TubeTW == "twoTW.MEDIUM" |
        TWCuTube == "TW.SMALLCu.SMALLtwo" |
        TWCuTube == "TW.SMALLCu.VSMALLtwo" |
        Scientific_Name == "Eragrostis lehmanniana") &
      !(TubeTW == "oneTW.SMALL" |
         TW_Q == "TW.SMALL" |
         TubeTW == "oneTW.MEDIUM" |
         Scientific_Name == "Xanthisma gracile" |
         TWCu == "TW.SMALLCu.LARGE" |
         ScieNamePlot == "P5Xanthisma gracile" |
         ScieNamePlot == "P5Gutierrezia sarothrae" |
         TWCuTube == "TW.SMALLCu.VSMALLone" |
         TWCuTube == "TW.SMALLCu.SMALLone" |
         Scientific_Name == "Ceanothus greggii" |
         TWCuTube == "TW.MEDIUMCu.LARGEone" |
         Scientific_Name == "Mimosa biuncifera (=aculeaticarpa)" |
         ScieNamePlot == "P1Xanthisma gracile" |
         TWCuTube == "TW.MEDIUMCu.SMALLone" |
         ScieNamePlot == "P1Euphorbia melanadenia" |
         ScieNamePlot == "P6Xanthisma gracile")
  )

#worst
start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
M1Cu <- glm(Cu_ICP ~ Cu_PXRF, data = dt_Cu_worst, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
M2Cu <- glm(Cu_ICP ~ Cu_PXRF + Total_Weight, data = dt_Cu_worst, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
M3Cu <- glm(Cu_ICP ~ Cu_PXRF + Substrate_RT, data = dt_Cu_worst, family = Gamma(link = "identity"), control = glm.control(maxit = 50))

dt_Cu_worst$Predicted_Cu_M1 <- predict(M1Cu, newdata = dt_Cu_worst, type = "response")
dt_Cu_worst$Predicted_Cu_M2 <- predict(M2Cu, newdata = dt_Cu_worst, type = "response")
dt_Cu_worst$Predicted_Cu_M3 <- predict(M3Cu, newdata = dt_Cu_worst, type = "response")

#best
start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
M1Cu <- glm(Cu_ICP ~ Cu_PXRF, data = dt_Cu_best, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
M2Cu <- glm(Cu_ICP ~ Cu_PXRF + Total_Weight, data = dt_Cu_best, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
M3Cu <- glm(Cu_ICP ~ Cu_PXRF + Substrate_RT, data = dt_Cu_best, family = Gamma(link = "identity"), control = glm.control(maxit = 50))

dt_Cu_best$Predicted_Cu_M1 <- predict(M1Cu, newdata = dt_Cu_best, type = "response")
dt_Cu_best$Predicted_Cu_M2 <- predict(M2Cu, newdata = dt_Cu_best, type = "response")
dt_Cu_best$Predicted_Cu_M3 <- predict(M3Cu, newdata = dt_Cu_best, type = "response")

#RMSE Cu worst
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
    rmse_raw <- sqrt(mean((dt_Cu_worst[[icp_cols[i]]] - dt_Cu_worst[[raw_cols[i]]])^2, na.rm = TRUE))
    temp_df <- data.frame(Element = elements[i], Model = "RAW", RMSE = rmse_raw, stringsAsFactors = FALSE)
    rmse_results <- rbind(rmse_results, temp_df)
    
    # RMSE for Predicted models
    for (j in 1:3) {
      pred_col_name <- predicted_cols[[i]][j]
      rmse_pred <- sqrt(mean((dt_Cu_worst[[icp_cols[i]]] - dt_Cu_worst[[pred_col_name]])^2, na.rm = TRUE))
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

#RMSE Cu best
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
    rmse_raw <- sqrt(mean((dt_Cu_best[[icp_cols[i]]] - dt_Cu_best[[raw_cols[i]]])^2, na.rm = TRUE))
    temp_df <- data.frame(Element = elements[i], Model = "RAW", RMSE = rmse_raw, stringsAsFactors = FALSE)
    rmse_results <- rbind(rmse_results, temp_df)
    
    # RMSE for Predicted models
    for (j in 1:3) {
      pred_col_name <- predicted_cols[[i]][j]
      rmse_pred <- sqrt(mean((dt_Cu_best[[icp_cols[i]]] - dt_Cu_best[[pred_col_name]])^2, na.rm = TRUE))
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
  saveWorkbook(wb, "RMSE_Cook_best.xlsx", overwrite = TRUE)
  
}


# Filtering out rows where Cu_PXRF is NA
dt_Cu_best <- dt_Cu_best %>% 
  filter(!is.na(Cu_PXRF))

dt_Cu_worst <- dt_Cu_worst %>% 
  filter(!is.na(Cu_PXRF))

# Splitting the data into training (80%) and testing (20%) sets
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(dt_Cu_worst), 0.8 * nrow(dt_Cu_worst))
train_data <- dt_Cu_worst[train_indices, ]
test_data <- dt_Cu_worst[-train_indices, ]

# Fit the model on the training data
start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
M1Cu_train <- glm(Cu_ICP ~ Cu_PXRF, data = train_data, family = Gamma(link = "identity"), start = start_vals)
test_data$Predicted_Cu_M1 <- predict(M1Cu_train, newdata = test_data, type = "response")

M2Cu_train <- glm(Cu_ICP ~ Cu_PXRF + Total_Weight, data = train_data, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
test_data$Predicted_Cu_M2 <- predict(M2Cu_train, newdata = test_data, type = "response")

M3Cu_train <- glm(Cu_ICP ~ Cu_PXRF + Substrate_RT, data = train_data, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
test_data$Predicted_Cu_M3 <- predict(M3Cu_train, newdata = test_data, type = "response")

# Model Performance - RMSE
rmse_best_RAW <- sqrt(mean((dt_Cu_best$Cu_ICP - dt_Cu_best$Cu_PXRF)^2))
rmse_worst_RAW <- sqrt(mean((dt_Cu_worst$Cu_ICP - dt_Cu_worst$Cu_PXRF)^2))
rmse_M1 <- sqrt(mean((test_data$Cu_ICP - test_data$Predicted_Cu_M1)^2))
rmse_M2 <- sqrt(mean((test_data$Cu_ICP - test_data$Predicted_Cu_M2)^2))
rmse_M3 <- sqrt(mean((test_data$Cu_ICP - test_data$Predicted_Cu_M3)^2))


