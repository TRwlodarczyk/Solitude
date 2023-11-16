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
}

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final/Manuscript")
dt <- read.delim("Solitude2022_RAW.txt")

dt <- dt[dt$Type_of_Sample != "root", ]
dt <- dt[dt$Site != "CONTROL", ]
dt <- dt[dt$Type_of_Sample != "stem", ]

dt[,19:87] <- sapply(dt[,19:87],as.numeric)
dt[,13] <- sapply(dt[,13],as.numeric)

# Creating Predicting values
{
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Cu <- glm(Cu_ICP ~ Cu_PXRF, data = dt, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Cu <- glm(Cu_ICP ~ Cu_PXRF + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Cu <- glm(Cu_ICP ~ Cu_PXRF + Substrate_RT, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M4Cu <- glm(Cu_ICP ~ Cu_PXRF + Substrate_RT + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  dt$Predicted_Cu_M1 = 8.5563 + (1.4929* dt$Cu_PXRF) 
  dt$Predicted_Cu_M2 = 17.03270 + (1.45362* dt$Cu_PXRF) + (-11.13508 * dt$Total_Weight) 
  dt$Predicted_Cu_M3 = 28.88747 + (1.41673* dt$Cu_PXRF) + (-316.95475 * dt$Substrate_RT) 
 
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Se <- glm(Se_ICP ~ Se_PXRF, data = dt, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Se <- glm(Se_ICP ~ Se_PXRF + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Se <- glm(Se_ICP ~ Se_PXRF + Substrate_RT, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M4Se <- glm(Se_ICP ~ Se_PXRF + Substrate_RT + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  dt$Predicted_Se_M1 = -0.18545 + (1.60411* dt$Se_PXRF) 
  dt$Predicted_Se_M2 = 0.07610 + (1.58532* dt$Se_PXRF) + (-0.32381 * dt$Total_Weight) 
  dt$Predicted_Se_M3 = 0.4417 + (1.5683* dt$Se_PXRF) + (-8.8017 * dt$Substrate_RT) 
  
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Re <- glm(Re_ICP ~ Re_PXRF, data = dt, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Re <- glm(Re_ICP ~ Re_PXRF + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Re <- glm(Re_ICP ~ Re_PXRF + Substrate_RT, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  dt$Predicted_Re_M1 = 2.17727 + (0.88060* dt$Re_PXRF) 
  dt$Predicted_Re_M2 = 4.29996 + (0.93425* dt$Re_PXRF) + (-3.64517 * dt$Total_Weight) 
  dt$Predicted_Re_M3 = 3.84146 + (0.91141* dt$Re_PXRF) + (-33.18455 * dt$Substrate_RT) 
  
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Zn <- glm(Zn_ICP ~ Zn_PXRF, data = dt, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Zn <- glm(Zn_ICP ~ Zn_PXRF + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Zn <- glm(Zn_ICP ~ Zn_PXRF + Substrate_RT, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M4Zn <- glm(Zn_ICP ~ Zn_PXRF + Substrate_RT + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  dt$Predicted_Zn_M1 = 21.7247 + (0.9342* dt$Zn_PXRF) 
  dt$Predicted_Zn_M2 = 33.6939 + (0.9314* dt$Zn_PXRF) + (-16.8131 * dt$Total_Weight) 
  dt$Predicted_Zn_M3 = 50.8422 + (0.9560* dt$Zn_PXRF) + (-473.9784 * dt$Substrate_RT) 
  
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Mn <- glm(Mn_ICP ~ Mn_PXRF, data = dt, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Mn <- glm(Mn_ICP ~ Mn_PXRF + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Mn <- glm(Mn_ICP ~ Mn_PXRF + Substrate_RT, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  dt$Predicted_Mn_M1 = 26.783 + (1.030* dt$Mn_PXRF) 
  dt$Predicted_Mn_M2 = 40.6027 + (1.0494* dt$Mn_PXRF) + (-20.5045 * dt$Total_Weight) 
  dt$Predicted_Mn_M3 = 51.4943 + (1.0760* dt$Mn_PXRF) + (-431.8509 * dt$Substrate_RT) 
  
  start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
  M1Fe <- glm(Fe_ICP ~ Fe_PXRF, data = dt, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  M2Fe <- glm(Fe_ICP ~ Fe_PXRF + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Fe <- glm(Fe_ICP ~ Fe_PXRF + Substrate_RT, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  dt$Predicted_Fe_M1 = -1.00099 + (1.10113* dt$Fe_PXRF) 
  dt$Predicted_Fe_M2 = 3.31281 + (1.09861* dt$Fe_PXRF) + (-5.30449 * dt$Total_Weight) 
  dt$Predicted_Fe_M3 = 9.25556 + (1.08668* dt$Fe_PXRF) + (-137.37547 * dt$Substrate_RT) 
  
  
  write.table(dt, file='C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New//Final/Modified Final/Manuscript/Solitude2022_RAW_outliers.csv', sep=",", row.names = F)
  
  
}

#RMSE
RMSE_RAW <- sqrt(mean((dt$Fe_ICP - dt$Fe_PXRF)^2, na.rm = TRUE))
RMSE_M1 <- sqrt(mean((dt$Fe_ICP - dt$Predicted_Fe_M1)^2, na.rm = TRUE)) # 
RMSE_M2 <- sqrt(mean((dt$Fe_ICP - dt$Predicted_Fe_M2)^2, na.rm = TRUE)) # 
RMSE_M3 <- sqrt(mean((dt$Fe_ICP - dt$Predicted_Fe_M3)^2, na.rm = TRUE)) # 

#NRMSE
RMSE_RAW / mean(dt$Fe_ICP, na.rm = TRUE)
RMSE_M1 / mean(dt$Fe_ICP, na.rm = TRUE)
RMSE_M2 / mean(dt$Fe_ICP, na.rm = TRUE)
RMSE_M3 / mean(dt$Fe_ICP, na.rm = TRUE)

#RPD
sdr <- sd(dt$Fe_ICP, na.rm = TRUE)
sdr / RMSE_RAW
sdr / RMSE_M1
sdr / RMSE_M2
sdr / RMSE_M3

#R squared
lm_model_RAW <- lm(Fe_ICP ~ Fe_PXRF, data = dt)
summary(lm_model_RAW)
lm_model_M1 <- lm(Fe_ICP ~ Predicted_Fe_M1, data = dt)
summary(lm_model_M1)
lm_model_M2 <- lm(Fe_ICP ~ Predicted_Fe_M2, data = dt)
summary(lm_model_M2)
lm_model_M3 <- lm(Fe_ICP ~ Predicted_Fe_M3, data = dt)
summary(lm_model_M3)


#MAE
mean(abs(dt$Fe_ICP - dt$Fe_PXRF), na.rm = TRUE) # pXRF 
mean(abs(dt$Fe_ICP - dt$Predicted_Fe_M1), na.rm = TRUE) 
mean(abs(dt$Fe_ICP - dt$Predicted_Fe_M2), na.rm = TRUE) 
mean(abs(dt$Fe_ICP - dt$Predicted_Fe_M3), na.rm = TRUE) 

dt_ICC_RAW<- dt[, c("Fe_ICP", "Fe_PXRF")]
dt_ICC_RAW <- na.omit(dt_ICC_RAW)
ICC(dt_ICC_RAW, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE)

dt_ICC_M1<- dt[, c("Fe_ICP", "Predicted_Fe_M1")]
dt_ICC_M1 <- na.omit(dt_ICC_M1)
ICC(dt_ICC_M1, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE)

dt_ICC_M2<- dt[, c("Fe_ICP", "Predicted_Fe_M2")]
dt_ICC_M2 <- na.omit(dt_ICC_M2)
ICC(dt_ICC_M2, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE)

dt_ICC_M3<- dt[, c("Fe_ICP", "Predicted_Fe_M3")]
dt_ICC_M3 <- na.omit(dt_ICC_M3)
ICC(dt_ICC_M3, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE)

  



#####RMSE######

for (i in 1:6) {
  raw_col <- 20 + i
  icp_col <- 59 + i  # Corresponding ICP column
  
  for (j in 0:3) {
    if (j == 0) {
      # RMSE for RAW
      rmse_raw <- sqrt(mean((dt[, icp_col] - dt[, raw_col])^2, na.rm = TRUE))
      print(paste("RMSE_RAW for element", raw_col, ":", rmse_raw))
    } else {
      # Calculating the index for Predicted columns
      pred_col <- 88 + (j - 1) + (i - 1) * 3
      # RMSE for Predicted models
      rmse_m <- sqrt(mean((dt[, icp_col] - dt[, pred_col])^2, na.rm = TRUE))
      print(paste("RMSE_M", j, "for element", raw_col, ":", rmse_m))
    }
  }
}


####NRMSE#####  
for (i in 21:26) {
  icp_col <- i + 39  # Corresponding ICP column
  
  for (j in 0:3) {
    pred_col <- i + 68 + j * 6  # Corresponding Predicted column
    rmse <- sqrt(mean((dt[, icp_col] - dt[, pred_col])^2, na.rm = TRUE))
    nrmse <- rmse / mean(dt[, icp_col], na.rm = TRUE)
    print(paste("NRMSE for element", i, "model", j, ":", nrmse))
  }
}


#####RPD#####

for (i in 21:26) {
  icp_col <- i + 39  # Corresponding ICP column
  sdr <- sd(dt[, icp_col], na.rm = TRUE)
  
  for (j in 0:3) {
    pred_col <- i + 68 + j * 6  # Corresponding Predicted column
    rmse <- sqrt(mean((dt[, icp_col] - dt[, pred_col])^2, na.rm = TRUE))
    rpd <- sdr / rmse
    print(paste("RPD for element", i, "model", j, ":", rpd))
  }
}



######R-SQUARED#####
for (i in 21:26) {
  icp_col <- i + 39  # Corresponding ICP column
  
  for (j in 0:3) {
    pred_col <- i + 68 + j * 6  # Corresponding Predicted column
    lm_model <- lm(dt[, icp_col] ~ dt[, pred_col], data = dt)
    r_squared <- summary(lm_model)$r.squared
    print(paste("R-squared for element", i, "model", j, ":", r_squared))
  }
}


######MAE#####

for (i in 21:26) {
  icp_col <- i + 39  # Corresponding ICP column
  
  for (j in 0:3) {
    pred_col <- i + 68 + j * 6  # Corresponding Predicted column
    mae <- mean(abs(dt[, icp_col] - dt[, pred_col]), na.rm = TRUE)
    print(paste("MAE for element", i, "model", j, ":", mae))
  }
}

#####ICP###
for (i in 21:26) {
  icp_col <- i + 39  # Corresponding ICP column
  
  for (j in 0:3) {
    pred_col <- i + 68 + j * 6  # Corresponding Predicted column
    dt_icc <- na.omit(dt[, c(icp_col, pred_col)])
    icc_result <- ICC(dt_icc, missing=TRUE, alpha=.05, lmer=TRUE, check.keys=FALSE)
    icc_value <- icc_result$results$ICC[1]  # Assuming you need the first ICC value
    print(paste("ICC for element", i, "model", j, ":", icc_value))
  }
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

# No need to convert RMSE to numeric now, as it should already be numeric


# Ensuring RMSE is numeric
rmse_results$RMSE <- as.numeric(rmse_results$RMSE)

# Set appropriate column names
colnames(rmse_results) <- c("Element", "Model", "RMSE")


# Create a new Excel workbook
wb <- createWorkbook()

# Add a sheet and write data for RMSE
addWorksheet(wb, "RMSE Results")
writeData(wb, "RMSE Results", rmse_results)

# Save the workbook
saveWorkbook(wb, "Statistical_Results.xlsx", overwrite = TRUE)

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

# Create a new Excel workbook or add to the existing one
wb <- loadWorkbook("Statistical_Results.xlsx")  # Load the existing workbook
addWorksheet(wb, "NRMSE Results")
writeData(wb, "NRMSE Results", nrmse_results)

# Save the workbook
saveWorkbook(wb, "Statistical_Results.xlsx", overwrite = TRUE)

