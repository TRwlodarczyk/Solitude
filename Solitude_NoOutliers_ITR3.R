# Solitude Manuscript 
# Tomasz Wlodarczyk
# twlodarczyk@arizona.edu
# 2023-15-11

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

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final/Manuscript")
dt <- read.delim("Solitude2022_RAW.txt")

dt <- dt[dt$Type_of_Sample != "root", ]
dt <- dt[dt$Site != "CONTROL", ]
dt <- dt[dt$Type_of_Sample != "stem", ]

dt[,19:87] <- sapply(dt[,19:87],as.numeric)
dt[,13] <- sapply(dt[,13],as.numeric)



#Alicja funciton for ITQ
{
  ############################################
  #DETECTION OF MILD AND/OR ECTREME OUTLIERS WITH BOXPLOTS (Q1=QUANTILE 25%, Q3 = 75%) 
  
  #choose level 1.5 for MILD outliers
  ## lower inner fence: Q1 - 1.5*IQ
  ## upper inner fence: Q2 + 1.5*IQ
  
  #choose level 3 for ectreme-outliers
  ## lower outer fence: Q1 - 3*IQ
  ## upper outer fence: Q2 + 3*IQ
  
  ### length of "position" potentialy to extend
  
  #############################################
  replace_outliers <- function(mymatrix, level){
    
    quant <- matrix(data = NA, ncol = ncol(mymatrix), nrow = 5)
    colnames(quant) <- colnames(mymatrix)
    rownames(quant) <- c("0%","25%","50%","75%","100%")
    for(i in 1:ncol(mymatrix)){quant[,i] <- quantile(mymatrix[,i], probs = seq(0,1,0.25), na.rm=T)}
    
    diff <- (quant[4,] - quant[2,])*level
    lowerborder <- quant[2,]-diff
    upperborder <- quant[4,]+diff
    
    position <- matrix(data = NA, ncol = ncol(mymatrix), nrow = 12000)
    
    for(a in 1:ncol(mymatrix)){
      mymatrix[,a][mymatrix[,a] < lowerborder[a]] <- NA
      mymatrix[,a][mymatrix[,a] > upperborder[a]] <- NA
      
      bla <- grep("TRUE", is.na(mymatrix[,a]))
      if(length(bla) > 0){position[1:length(bla),a] <- bla}
    }
    
    output <- vector(mode = "list", length = 2)
    output[[1]] <- mymatrix
    output[[2]] <- position
    names(output) <- c("matrix_without_outliers", "lines_with_outliers")
    
    return(output)
  }
  
  
  #without_outliers <- replace_outliers(tol_medians[,6:12],1.5)
  #tol_medians_without_outl <- cbind(tol_medians[,1:5],without_outliers[[1]])
  #write.table(tol_medians_without_outl, file = "tol_medians_without_outl.txt", sep = "\t", col.names = T, row.names = F, quote = F, na = "NA", dec = ".")
  
}


without_outliers_19_29 <- replace_outliers(dt[, 19:29], 3)
dt_without_outliers_19_29 <- cbind(dt[, 1:18], without_outliers_19_29[[1]], dt[, 30:ncol(dt)])
without_outliers_58_68 <- replace_outliers(dt[, 58:68], 3)
dt_without_outliers_final <- cbind(dt_without_outliers_19_29[, 1:57], without_outliers_58_68[[1]], dt_without_outliers_19_29[, 69:ncol(dt)])
write.xlsx(dt_without_outliers_final, "Solitude2022_RAW_No_outliers_ITR3.xlsx", overwrite = TRUE)


dt <- dt_without_outliers_final


{
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Cu <- glm(Cu_ICP ~ Cu_PXRF, data = dt, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Cu <- glm(Cu_ICP ~ Cu_PXRF + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Cu <- glm(Cu_ICP ~ Cu_PXRF + Substrate_RT, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M4Cu <- glm(Cu_ICP ~ Cu_PXRF + Substrate_RT + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Cu)
  summary(M2Cu)
  summary(M3Cu)
  dt$Predicted_Cu_M1 = 8.3412 + (1.5127* dt$Cu_PXRF) 
  dt$Predicted_Cu_M2 = 16.8502 + (1.4671* dt$Cu_PXRF) + (-11.0945 * dt$Total_Weight) 
  dt$Predicted_Cu_M3 = 28.63373 + (1.42440* dt$Cu_PXRF) + (-314.59753 * dt$Substrate_RT) 
  cooks_distances <- cooks.distance(M1Cu)
  plot(cooks_distances, type="h", ylab="Cook's Distance", xlab="Index")
  abline(h=4/(nrow(dt)), col="red")  # Add a threshold line
  
  cooks_distances2 <- cooks.distance(M3Cu)
  plot(cooks_distances2, type="h", ylab="Cook's Distance", xlab="Index")
  abline(h=4/(nrow(dt)), col="red")  # Add a threshold line
  
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Se <- glm(Se_ICP ~ Se_PXRF, data = dt, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Se <- glm(Se_ICP ~ Se_PXRF + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Se <- glm(Se_ICP ~ Se_PXRF + Substrate_RT, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M4Se <- glm(Se_ICP ~ Se_PXRF + Substrate_RT + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Se)
  summary(M2Se)
  summary(M3Se)
  dt$Predicted_Se_M1 = -0.18698 + (1.60642* dt$Se_PXRF) 
  dt$Predicted_Se_M2 = 0.07854 + (1.58342* dt$Se_PXRF) + (-0.32523 * dt$Total_Weight) 
  dt$Predicted_Se_M3 = 0.44740 + (1.56333* dt$Se_PXRF) + (-8.83631 * dt$Substrate_RT) 
  
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Re <- glm(Re_ICP ~ Re_PXRF, data = dt, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Re <- glm(Re_ICP ~ Re_PXRF + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Re <- glm(Re_ICP ~ Re_PXRF + Substrate_RT, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Re)
  summary(M2Re)
  summary(M3Re)
  dt$Predicted_Re_M1 = 2.25363  + (0.86889* dt$Re_PXRF) 
  dt$Predicted_Re_M2 = 4.21351 + (0.95966* dt$Re_PXRF) + (-3.75341 * dt$Total_Weight) 
  dt$Predicted_Re_M3 = 3.8246 + (0.9195* dt$Re_PXRF) + (-33.8513 * dt$Substrate_RT) 
  
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Zn <- glm(Zn_ICP ~ Zn_PXRF, data = dt, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Zn <- glm(Zn_ICP ~ Zn_PXRF + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Zn <- glm(Zn_ICP ~ Zn_PXRF + Substrate_RT, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M4Zn <- glm(Zn_ICP ~ Zn_PXRF + Substrate_RT + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Zn)
  summary(M2Zn)
  summary(M3Zn)
  dt$Predicted_Zn_M1 = 22.6387 + (0.8533* dt$Zn_PXRF) 
  dt$Predicted_Zn_M2 = 34.5681 + (0.8565* dt$Zn_PXRF) + (-16.8979 * dt$Total_Weight) 
  dt$Predicted_Zn_M3 = 51.9525 + (0.8772* dt$Zn_PXRF) + (-478.2824 * dt$Substrate_RT) 
  
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Mn <- glm(Mn_ICP ~ Mn_PXRF, data = dt, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Mn <- glm(Mn_ICP ~ Mn_PXRF + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Mn <- glm(Mn_ICP ~ Mn_PXRF + Substrate_RT, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Mn)
  summary(M2Mn)
  summary(M3Mn)
  dt$Predicted_Mn_M1 = 26.783 + (1.030* dt$Mn_PXRF) 
  dt$Predicted_Mn_M2 = 40.6027 + (1.0494* dt$Mn_PXRF) + (-20.5045 * dt$Total_Weight) 
  dt$Predicted_Mn_M3 = 51.4943 + (1.0760* dt$Mn_PXRF) + (-431.8509 * dt$Substrate_RT) 

  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Fe <- glm(Fe_ICP ~ Fe_PXRF, data = dt, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Fe <- glm(Fe_ICP ~ Fe_PXRF + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Fe <- glm(Fe_ICP ~ Fe_PXRF + Substrate_RT, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(M1Fe)
  summary(M2Fe)
  summary(M3Fe)
  dt$Predicted_Fe_M1 = -0.09906 + (1.07153* dt$Fe_PXRF) 
  dt$Predicted_Fe_M2 = 4.4046 + (1.0678* dt$Fe_PXRF) + (-5.4440 * dt$Total_Weight) 
  dt$Predicted_Fe_M3 = 11.40189 + (1.05251* dt$Fe_PXRF) + (-151.86555 * dt$Substrate_RT) 
  
  
  write.table(dt, file='C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New//Final/Modified Final/Manuscript/Solitude2022_Predicted_No_outliers_ITR3.csv', sep=",", row.names = F)
  
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
  saveWorkbook(wb, "RMSE_no_outliers.xlsx", overwrite = TRUE)
  
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
saveWorkbook(wb_nrmse, "NRMSE_No_outliers.xlsx", overwrite = TRUE)



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
saveWorkbook(wb_mae, "MAE_No_outliers.xlsx", overwrite = TRUE)



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
saveWorkbook(wb_r_squared, "R_squared_No_outliers.xlsx", overwrite = TRUE)



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
saveWorkbook(wb_rpd, "RPD_No_outliers.xlsx", overwrite = TRUE)


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
saveWorkbook(wb_icc, "ICC_No_outliers.xlsx", overwrite = TRUE)

