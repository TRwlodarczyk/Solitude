# Solitude Plant Cooks and 
# Tomasz Wlodarczyk
# twlodarczyk@arizona.edu
# 2024 - 17 - 03

{
  library(ggplot2)
  library(dplyr)
  library(data.table)
  library(reshape2)
  library(reshape)
  library("readxl")
  library(ggpubr)
  library(agricolae)
  library(tidyverse)
  library (readr) #to read URL
  library(ggstatsplot)
  library(gplots)
  library(psych)
  library(car)
  library(openxlsx)
}

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/")
dt <-read.delim("Solitude_pXRF_ICP.txt")

dt <- dt[dt$Type_of_Sample != "root", ] # tego nie dawaj
dt <- dt[dt$Site != "NATURAL", ]        # tegp nie dawaj
dt <- dt[dt$Type_of_Sample != "stem", ] # tego nie dawaj

dt[,18:87] <- sapply(dt[,18:87],as.numeric)
dt[,12] <- sapply(dt[,12],as.numeric)




# Creating Predicting values
{
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Cu <- glm(Cu_ICP ~ Cu_PXRF, data = dt, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Cu <- glm(Cu_ICP ~ Cu_PXRF + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Cu <- glm(Cu_ICP ~ Cu_PXRF + Substrate_RT, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Cu)
  
  dt_M1Cu <- dt
  cooks_distance <- cooks.distance(M1Cu)
  threshold <- 4 / nrow(dt)
  outliers <- which(cooks_distance > threshold)
  dt_M1Cu[outliers, "Cu_ICP"] <- NA
  
  M1Cu_adjusted <- glm(Cu_ICP ~ Cu_PXRF, data = dt_M1Cu, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Cu_adjusted)
  #
  dt$Predicted_Cu_M1 <- predict(M1Cu_adjusted, newdata = dt, type = "response")
  #
  dt_M2Cu <- dt
  cooks_distance <- cooks.distance(M2Cu)
  threshold <- 4 / nrow(dt)
  outliers <- which(cooks_distance > threshold)
  dt_M2Cu[outliers, "Cu_ICP"] <- NA
  
  M2Cu_adjusted <- glm(Cu_ICP ~ Cu_PXRF + Total_Weight, data = dt_M2Cu, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M2Cu_adjusted)
  #
  dt$Predicted_Cu_M2 <- predict(M2Cu_adjusted, newdata = dt, type = "response")
  #
  dt_M3Cu <- dt
  cooks_distance <- cooks.distance(M3Cu)
  threshold <- 4 / nrow(dt)
  outliers <- which(cooks_distance > threshold)
  dt_M3Cu[outliers, "Cu_ICP"] <- NA
  
  M3Cu_adjusted <- glm(Cu_ICP ~ Cu_PXRF + Substrate_RT, data = dt_M3Cu, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M3Cu_adjusted)
  
  #
  dt$Predicted_Cu_M3 <- predict(M3Cu_adjusted, newdata = dt, type = "response")
  #
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Se <- glm(Se_ICP ~ Se_PXRF, data = dt, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Se <- glm(Se_ICP ~ Se_PXRF + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Se <- glm(Se_ICP ~ Se_PXRF + Substrate_RT, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  #M4Se <- glm(Se_ICP ~ Se_PXRF + Substrate_RT + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Se)
  
  
  dt_M1Se <- dt
  cooks_distance <- cooks.distance(M1Se)
  threshold <- 4 / nrow(dt)
  outliers <- which(cooks_distance > threshold)
  dt_M1Se[outliers, "Se_ICP"] <- NA
  
  M1Se_adjusted <- glm(Se_ICP ~ Se_PXRF, data = dt_M1Se, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Se_adjusted)
  dt_M1Se$Predicted_Se_M1 <- predict(M1Se_adjusted, newdata = dt_M1Se, type = "response")
  #
  dt$Predicted_Se_M1 <- predict(M1Se_adjusted, newdata = dt, type = "response")
  #
  dt_M2Se <- dt
  cooks_distance <- cooks.distance(M2Se)
  threshold <- 4 / nrow(dt)
  outliers <- which(cooks_distance > threshold)
  dt_M2Se[outliers, "Se_ICP"] <- NA
  
  M2Se_adjusted <- glm(Se_ICP ~ Se_PXRF + Total_Weight, data = dt_M2Se, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M2Se_adjusted)
  dt_M2Se$Predicted_Se_M2 <- predict(M2Se_adjusted, newdata = dt_M2Se, type = "response")
  #
  dt$Predicted_Se_M2 <- predict(M2Se_adjusted, newdata = dt, type = "response")
  #
  dt_M3Se <- dt
  cooks_distance <- cooks.distance(M3Se)
  threshold <- 4 / nrow(dt)
  outliers <- which(cooks_distance > threshold)
  dt_M3Se[outliers, "Se_ICP"] <- NA
  
  M3Se_adjusted <- glm(Se_ICP ~ Se_PXRF + Substrate_RT, data = dt_M3Se, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M3Se_adjusted)
  dt_M3Se$Predicted_Se_M3 <- predict(M3Se_adjusted, newdata = dt_M3Se, type = "response")
  
  #
  dt$Predicted_Se_M3 <- predict(M3Se_adjusted, newdata = dt, type = "response")
  #
  
  
  
  
  
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Re <- glm(Re_ICP ~ Re_PXRF, data = dt, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Re <- glm(Re_ICP ~ Re_PXRF + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Re <- glm(Re_ICP ~ Re_PXRF + Substrate_RT, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Re)
  summary(M3Re)
  
  dt_M1Re <- dt
  cooks_distance <- cooks.distance(M1Re)
  threshold <- 4 / nrow(dt)
  outliers <- which(cooks_distance > threshold)
  dt_M1Re[outliers, "Re_ICP"] <- NA
  
  M1Re_adjusted <- glm(Re_ICP ~ Re_PXRF, data = dt_M1Re, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Re_adjusted)
  #
  dt$Predicted_Re_M1 <- predict(M1Re_adjusted, newdata = dt, type = "response")
  #
  dt_M2Re <- dt
  cooks_distance <- cooks.distance(M2Re)
  threshold <- 4 / nrow(dt)
  outliers <- which(cooks_distance > threshold)
  dt_M2Re[outliers, "Re_ICP"] <- NA
  
  M2Re_adjusted <- glm(Re_ICP ~ Re_PXRF + Total_Weight, data = dt_M2Re, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  #
  dt$Predicted_Re_M2 <- predict(M2Re_adjusted, newdata = dt, type = "response")
  #
  dt_M3Re <- dt
  cooks_distance <- cooks.distance(M3Re)
  threshold <- 4 / nrow(dt)
  outliers <- which(cooks_distance > threshold)
  dt_M3Re[outliers, "Re_ICP"] <- NA
  
  M3Re_adjusted <- glm(Re_ICP ~ Re_PXRF + Substrate_RT, data = dt_M3Re, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M3Re_adjusted)
  #
  dt$Predicted_Re_M3 <- predict(M3Re_adjusted, newdata = dt, type = "response")
  #
  
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Zn <- glm(Zn_ICP ~ Zn_PXRF, data = dt, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Zn <- glm(Zn_ICP ~ Zn_PXRF + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Zn <- glm(Zn_ICP ~ Zn_PXRF + Substrate_RT, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M4Zn <- glm(Zn_ICP ~ Zn_PXRF + Substrate_RT + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M3Zn)
  
  dt_M1Zn <- dt
  cooks_distance <- cooks.distance(M1Zn)
  threshold <- 4 / nrow(dt)
  outliers <- which(cooks_distance > threshold)
  dt_M1Zn[outliers, "Zn_ICP"] <- NA
  
  M1Zn_adjusted <- glm(Zn_ICP ~ Zn_PXRF, data = dt_M1Zn, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  #
  dt$Predicted_Zn_M1 <- predict(M1Zn_adjusted, newdata = dt, type = "response")
  #
  dt_M2Zn <- dt
  cooks_distance <- cooks.distance(M2Zn)
  threshold <- 4 / nrow(dt)
  outliers <- which(cooks_distance > threshold)
  dt_M2Zn[outliers, "Zn_ICP"] <- NA
  
  M2Zn_adjusted <- glm(Zn_ICP ~ Zn_PXRF + Total_Weight, data = dt_M2Zn, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  #
  dt$Predicted_Zn_M2 <- predict(M2Zn_adjusted, newdata = dt, type = "response")
  #
  dt_M3Zn <- dt
  cooks_distance <- cooks.distance(M3Zn)
  threshold <- 4 / nrow(dt)
  outliers <- which(cooks_distance > threshold)
  dt_M3Zn[outliers, "Zn_ICP"] <- NA
  
  M3Zn_adjusted <- glm(Zn_ICP ~ Zn_PXRF + Substrate_RT, data = dt_M3Zn, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M3Zn_adjusted)
  #
  dt$Predicted_Zn_M3 <- predict(M3Zn_adjusted, newdata = dt, type = "response")
  #
  
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Mn <- glm(Mn_ICP ~ Mn_PXRF, data = dt, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Mn <- glm(Mn_ICP ~ Mn_PXRF + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Mn <- glm(Mn_ICP ~ Mn_PXRF + Substrate_RT, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Mn)
  
  dt_M1Mn <- dt
  cooks_distance <- cooks.distance(M1Mn)
  threshold <- 4 / nrow(dt)
  outliers <- which(cooks_distance > threshold)
  dt_M1Mn[outliers, "Mn_ICP"] <- NA
  
  M1Mn_adjusted <- glm(Mn_ICP ~ Mn_PXRF, data = dt_M1Mn, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  #
  dt$Predicted_Mn_M1 <- predict(M1Mn_adjusted, newdata = dt, type = "response")
  #
  dt_M2Mn <- dt
  cooks_distance <- cooks.distance(M2Mn)
  threshold <- 4 / nrow(dt)
  outliers <- which(cooks_distance > threshold)
  dt_M2Mn[outliers, "Mn_ICP"] <- NA
  
  M2Mn_adjusted <- glm(Mn_ICP ~ Mn_PXRF + Total_Weight, data = dt_M2Mn, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  #
  dt$Predicted_Mn_M2 <- predict(M2Mn_adjusted, newdata = dt, type = "response")
  #
  dt_M3Mn <- dt
  cooks_distance <- cooks.distance(M3Mn)
  threshold <- 4 / nrow(dt)
  outliers <- which(cooks_distance > threshold)
  dt_M3Mn[outliers, "Mn_ICP"] <- NA
  
  M3Mn_adjusted <- glm(Mn_ICP ~ Mn_PXRF + Substrate_RT, data = dt_M3Mn, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  
  #
  dt$Predicted_Mn_M3 <- predict(M3Mn_adjusted, newdata = dt, type = "response")
  #
  
  
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Fe <- glm(Fe_ICP ~ Fe_PXRF, data = dt, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Fe <- glm(Fe_ICP ~ Fe_PXRF + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Fe <- glm(Fe_ICP ~ Fe_PXRF + Substrate_RT, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  
  summary(M3Fe)
  dt_M1Fe <- dt
  cooks_distance <- cooks.distance(M1Fe)
  threshold <- 4 / nrow(dt)
  outliers <- which(cooks_distance > threshold)
  dt_M1Fe[outliers, "Fe_ICP"] <- NA
  
  M1Fe_adjusted <- glm(Fe_ICP ~ Fe_PXRF, data = dt_M1Fe, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Fe_adjusted)
  #
  dt$Predicted_Fe_M1 <- predict(M1Fe_adjusted, newdata = dt, type = "response")
  #
  dt_M2Fe <- dt
  cooks_distance <- cooks.distance(M2Fe)
  threshold <- 4 / nrow(dt)
  outliers <- which(cooks_distance > threshold)
  dt_M2Fe[outliers, "Fe_ICP"] <- NA
  
  M2Fe_adjusted <- glm(Fe_ICP ~ Fe_PXRF + Total_Weight, data = dt_M2Fe, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  #
  dt$Predicted_Fe_M2 <- predict(M2Fe_adjusted, newdata = dt, type = "response")
  #
  dt_M3Fe <- dt
  cooks_distance <- cooks.distance(M3Fe)
  threshold <- 4 / nrow(dt)
  outliers <- which(cooks_distance > threshold)
  dt_M3Fe[outliers, "Fe_ICP"] <- NA
  
  M3Fe_adjusted <- glm(Fe_ICP ~ Fe_PXRF + Substrate_RT, data = dt_M3Fe, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  
  #
  dt$Predicted_Fe_M3 <- predict(M3Fe_adjusted, newdata = dt, type = "response")
  #
  
  #write.xlsx(dt, "Solitude2022_Predicted_Cooks-Final2.xlsx")
  #write.table(dt, file='C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/Solitude2022_Predicted_Cooks-Final.csv', sep=",", row.names = F)
  
}


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
    rmse_raw <- sqrt(mean((dt[[icp_cols[i]]] - dt[[raw_cols[i]]])^2, na.rm = TRUE))
    temp_df <- data.frame(Element = elements[i], Model = "RAW", RMSE = rmse_raw, stringsAsFactors = FALSE)
    rmse_results <- rbind(rmse_results, temp_df)
    
    # RMSE for Predicted models
    for (j in 1:3) {
      pred_col_name <- predicted_cols[[i]][j]
      rmse_pred <- sqrt(mean((dt[[icp_cols[i]]] - dt[[pred_col_name]])^2, na.rm = TRUE))
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
  mean_icp <- mean(dt[[icp_cols[i]]], na.rm = TRUE)  # Calculate mean of ICP values for the element
  
  # NRMSE for RAW
  rmse_raw <- sqrt(mean((dt[[icp_cols[i]]] - dt[[raw_cols[i]]])^2, na.rm = TRUE))
  nrmse_raw <- rmse_raw / mean_icp
  temp_df <- data.frame(Element = elements[i], Model = "RAW", NRMSE = nrmse_raw, stringsAsFactors = FALSE)
  nrmse_results <- rbind(nrmse_results, temp_df)
  
  # NRMSE for Predicted models
  for (j in 1:3) {
    pred_col_name <- predicted_cols[[i]][j]
    rmse_pred <- sqrt(mean((dt[[icp_cols[i]]] - dt[[pred_col_name]])^2, na.rm = TRUE))
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
  mae_raw <- mean(abs(dt[[icp_cols[i]]] - dt[[raw_cols[i]]]), na.rm = TRUE)
  temp_df <- data.frame(Element = elements[i], Model = "RAW", MAE = mae_raw, stringsAsFactors = FALSE)
  mae_results <- rbind(mae_results, temp_df)
  
  # MAE for Predicted models
  for (j in 1:3) {
    pred_col_name <- predicted_cols[[i]][j]
    mae_pred <- mean(abs(dt[[icp_cols[i]]] - dt[[pred_col_name]]), na.rm = TRUE)
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
  lm_model_raw <- lm(dt[[icp_cols[i]]] ~ dt[[raw_cols[i]]], data = dt, na.action = na.exclude)
  r_squared_raw <- summary(lm_model_raw)$r.squared
  temp_df <- data.frame(Element = elements[i], Model = "RAW", R_squared = r_squared_raw, stringsAsFactors = FALSE)
  r_squared_results <- rbind(r_squared_results, temp_df)
  
  # R-squared for Predicted models
  for (j in 1:3) {
    pred_col_name <- predicted_cols[[i]][j]
    lm_model_pred <- lm(dt[[icp_cols[i]]] ~ dt[[pred_col_name]], data = dt, na.action = na.exclude)
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
  sdr <- sd(dt[[icp_cols[i]]], na.rm = TRUE)
  
  # RPD for RAW
  rmse_raw <- sqrt(mean((dt[[icp_cols[i]]] - dt[[raw_cols[i]]])^2, na.rm = TRUE))
  rpd_raw <- sdr / rmse_raw
  temp_df <- data.frame(Element = elements[i], Model = "RAW", RPD = rpd_raw, stringsAsFactors = FALSE)
  rpd_results <- rbind(rpd_results, temp_df)
  
  # RPD for Predicted models
  for (j in 1:3) {
    pred_col_name <- predicted_cols[[i]][j]
    rmse_pred <- sqrt(mean((dt[[icp_cols[i]]] - dt[[pred_col_name]])^2, na.rm = TRUE))
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
  dt_icc_raw <- dt[, c(icp_cols[i], raw_cols[i])]
  dt_icc_raw <- na.omit(dt_icc_raw)
  icc_raw <- ICC(dt_icc_raw, missing=TRUE, alpha=.05, lmer=TRUE, check.keys=FALSE)
  icc_value_raw <- icc_raw$results["Single_random_raters", "ICC"]
  temp_df <- data.frame(Element = elements[i], Model = "RAW", ICC_Value = icc_value_raw, stringsAsFactors = FALSE)
  icc_results <- rbind(icc_results, temp_df)
  
  # ICC for Predicted models
  for (j in 1:3) {
    pred_col_name <- predicted_cols[[i]][j]
    dt_icc_pred <- dt[, c(icp_cols[i], pred_col_name)]
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


