# SLT Manuscript with Masterfile
# Tomasz Wlodarczyk
# twlodarczyk@arizona.edu
# 2023-07-11

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

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final")
dt <- read.delim("SLT_MASTER_TABLE_11_23_v2.txt")

dt <- dt[dt$Type_of_Sample != "root", ]
dt <- dt[dt$Site != "CONTROL", ]
dt <- dt[dt$Type_of_Sample != "stem", ]

dt[,17:95] <- sapply(dt[,17:95],as.numeric)
dt[,12] <- sapply(dt[,12],as.numeric)

#Check skeweness

skewness(dt$Cu_ICP) # right skewed
skewness(dt$Cu_PXRF) # right skewed
skewness(dt$Se_ICP) # right skewed
skewness(dt$Se_PXRF) # right skewed
skewness(dt$Re_ICP) # right skewed
skewness(dt$Re_PXRF) # right skewed
skewness(dt$Zn_ICP) # right skewed
skewness(dt$Zn_PXRF) # right skewed
skewness(dt$Mn_ICP) # right skewed
skewness(dt$Mn_PXRF) # right skewed
skewness(dt$Fe_ICP) # right skewed
skewness(dt$Fe_PXRF) # right skewed
skewness(dt$As_ICP) # right skewed
skewness(dt$As_PXRF) # right skewed
skewness(dt$Cr_ICP) # right skewed
skewness(dt$Cr_PXRF) # right skewed

#sum(dt$Re_PXRF != "NA", na.rm = TRUE) # 
#Assumptions for LM model for Cu, Se, Re, Zn, Mn, Fe, Cr, As, Ti
##Assumptions for LM model ICP~pXRF + RT - Cu
{
{
  # Normality of residuals
  lm_model <- lm(Cu_ICP~Cu_PXRF, data=dt)
  shapiro_test <- shapiro.test(resid(lm_model))
  qqnorm(resid(lm_model))
  qqline(resid(lm_model))
  
  # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
  library(lmtest)
  bptest(lm_model)
  plot(fitted(lm_model), resid(lm_model))
  abline(h = 0, col = "red")
  
  # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
  dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
  
  #Absence of Multicollinearity - Variance Inflation Factors (VIF)
  library(car)
  vif(lm_model) #No multicollinearity
}
{
  # Normality of residuals
  lm_model <- lm(Cu_ICP~Cu_PXRF + Substrate_RT, data=dt)
  shapiro_test <- shapiro.test(resid(lm_model))
  qqnorm(resid(lm_model))
  qqline(resid(lm_model))
  
  # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
  library(lmtest)
  bptest(lm_model)
  plot(fitted(lm_model), resid(lm_model))
  abline(h = 0, col = "red")
  
  # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
  dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
  
  #Absence of Multicollinearity - Variance Inflation Factors (VIF)
  library(car)
  vif(lm_model) #No multicollinearity
}
    
##Assumptions for LM model ICP~pXRF + RT
{
  # Normality of residuals
  lm_model <- lm(Cu_ICP~Cu_PXRF + Substrate_RT + Total_Weight, data=dt)
  shapiro_test <- shapiro.test(resid(lm_model))
  qqnorm(resid(lm_model))
  qqline(resid(lm_model))
  
  # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
  library(lmtest)
  bptest(lm_model)
  plot(fitted(lm_model), resid(lm_model))
  abline(h = 0, col = "red")
  
  # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
  dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
  
  #Absence of Multicollinearity - Variance Inflation Factors (VIF)
  library(car)
  vif(lm_model) #No multicollinearity
}
  
  
  
  
  
  {
    # Normality of residuals
    lm_model <- lm(Se_ICP~Se_PXRF, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  {
    # Normality of residuals
    lm_model <- lm(Se_ICP~Se_PXRF + Substrate_RT, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  ##Assumptions for LM model ICP~pXRF + RT
  {
    # Normality of residuals
    lm_model <- lm(Se_ICP~Se_PXRF + Substrate_RT + Total_Weight, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  
  
  
  
  
  
  {
    # Normality of residuals
    lm_model <- lm(Re_ICP~Re_PXRF, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  {
    # Normality of residuals
    lm_model <- lm(Re_ICP~Re_PXRF + Substrate_RT, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  ##Assumptions for LM model ICP~pXRF + RT
  {
    # Normality of residuals
    lm_model <- lm(Re_ICP~Re_PXRF + Substrate_RT + Total_Weight, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  
  
  
  
  
  {
    # Normality of residuals
    lm_model <- lm(Zn_ICP~Zn_PXRF, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  {
    # Normality of residuals
    lm_model <- lm(Zn_ICP~Zn_PXRF + Substrate_RT, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  ##Assumptions for LM model ICP~pXRF + RT
  {
    # Normality of residuals
    lm_model <- lm(Zn_ICP~Zn_PXRF + Substrate_RT + Total_Weight, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  
  
  
  
  {
    # Normality of residuals
    lm_model <- lm(Mn_ICP~Mn_PXRF, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  {
    # Normality of residuals
    lm_model <- lm(Mn_ICP~Mn_PXRF + Substrate_RT, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  ##Assumptions for LM model ICP~pXRF + RT
  {
    # Normality of residuals
    lm_model <- lm(Mn_ICP~Mn_PXRF + Substrate_RT + Total_Weight, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  
  
  
  
  
  
  
  {
    # Normality of residuals
    lm_model <- lm(Fe_ICP~Fe_PXRF, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  {
    # Normality of residuals
    lm_model <- lm(Fe_ICP~Fe_PXRF + Substrate_RT, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  ##Assumptions for LM model ICP~pXRF + RT
  {
    # Normality of residuals
    lm_model <- lm(Fe_ICP~Fe_PXRF + Substrate_RT + Total_Weight, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  
  
  
  
  
  {
    # Normality of residuals
    lm_model <- lm(Cr_ICP~Cr_PXRF, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  {
    # Normality of residuals
    lm_model <- lm(Cr_ICP~Cr_PXRF + Substrate_RT, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  ##Assumptions for LM model ICP~pXRF + RT
  {
    # Normality of residuals
    lm_model <- lm(Cr_ICP~Cr_PXRF + Substrate_RT + Total_Weight, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  
  
  
  
  {
    # Normality of residuals
    lm_model <- lm(As_ICP~As_PXRF, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  {
    # Normality of residuals
    lm_model <- lm(As_ICP~As_PXRF + Substrate_RT, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  ##Assumptions for LM model ICP~pXRF + RT
  {
    # Normality of residuals
    lm_model <- lm(As_ICP~As_PXRF + Substrate_RT + Total_Weight, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  
  {
    # Normality of residuals
    lm_model <- lm(As_ICP~As_PXRF, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  {
    # Normality of residuals
    lm_model <- lm(As_ICP~As_PXRF + Substrate_RT, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  ##Assumptions for LM model ICP~pXRF + RT
  {
    # Normality of residuals
    lm_model <- lm(As_ICP~As_PXRF + Substrate_RT + Total_Weight, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
}

#Best Normalize
{
library(bestNormalize)
result_Cu_PXRF <- bestNormalize(dt$Cu_PXRF)
dt$Cu_PXRF2 <- result_Cu_PXRF$x.t
result_Cu_PXRF 

result_Cu_ICP <- bestNormalize(dt$Cu_ICP)
dt$Cu_ICP2 <- result_Cu_ICP$x.t
result_Cu_ICP 

result_Se_ICP <- bestNormalize(dt$Se_ICP)
dt$Se_ICP2 <- result_Se_ICP$x.t
result_Re_ICP <- bestNormalize(dt$Re_ICP)
dt$Re_ICP2 <- result_Re_ICP$x.t
result_Zn_ICP <- bestNormalize(dt$Zn_ICP)
dt$Zn_ICP2 <- result_Zn_ICP$x.t
result_Mn_ICP <- bestNormalize(dt$Mn_ICP)
dt$Mn_ICP2 <- result_Mn_ICP$x.t
result_Fe_ICP <- bestNormalize(dt$Fe_ICP)
dt$Fe_ICP2 <- result_Fe_ICP$x.t

result_Se_PXRF <- bestNormalize(dt$Se_PXRF)
dt$Se_PXRF2 <- result_Se_PXRF$x.t
result_Re_PXRF <- bestNormalize(dt$Re_PXRF)
dt$Re_PXRF2 <- result_Re_PXRF$x.t
result_Zn_PXRF <- bestNormalize(dt$Zn_PXRF)
dt$Zn_PXRF2 <- result_Zn_PXRF$x.t
result_Mn_PXRF <- bestNormalize(dt$Mn_PXRF)
dt$Mn_PXRF2 <- result_Mn_PXRF$x.t
result_Fe_PXRF <- bestNormalize(dt$Fe_PXRF)
dt$Fe_PXRF2 <- result_Fe_PXRF$x.t

}
##Assumptions for LM model ICP~pXRF + RT - Cu
{
  {
    # Normality of residuals
    lm_model <- lm(Cu_ICP2~Cu_PXRF2, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  {
    # Normality of residuals
    lm_model <- lm(Cu_ICP2~Cu_PXRF2 + Substrate_RT, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  ##Assumptions for LM model ICP~pXRF + RT
  {
    # Normality of residuals
    lm_model <- lm(Cu_ICP2~Cu_PXRF2 + Substrate_RT + Total_Weight, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  
  
  
  
  {
    # Normality of residuals
    lm_model <- lm(Se_ICP2~Se_PXRF2, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  {
    # Normality of residuals
    lm_model <- lm(Se_ICP2~Se_PXRF2 + Substrate_RT, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  ##Assumptions for LM model ICP~pXRF + RT
  {
    # Normality of residuals
    lm_model <- lm(Se_ICP2~Se_PXRF2 + Substrate_RT + Total_Weight, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  
  
  
  
  
  
  {
    # Normality of residuals
    lm_model <- lm(Re_ICP2~Re_PXRF2, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  {
    # Normality of residuals
    lm_model <- lm(Re_ICP2~Re_PXRF2 + Substrate_RT, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  ##Assumptions for LM model ICP~pXRF + RT
  {
    # Normality of residuals
    lm_model <- lm(Re_ICP2~Re_PXRF2 + Substrate_RT + Total_Weight, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  
  
  
  
  
  {
    # Normality of residuals
    lm_model <- lm(Zn_ICP2~Zn_PXRF2, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  {
    # Normality of residuals
    lm_model <- lm(Zn_ICP2~Zn_PXRF2 + Substrate_RT, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  ##Assumptions for LM model ICP~pXRF + RT
  {
    # Normality of residuals
    lm_model <- lm(Zn_ICP2~Zn_PXRF2 + Substrate_RT + Total_Weight, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  
  
  
  
  {
    # Normality of residuals
    lm_model <- lm(Mn_ICP2~Mn_PXRF2, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  {
    # Normality of residuals
    lm_model <- lm(Mn_ICP2~Mn_PXRF2 + Substrate_RT, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  ##Assumptions for LM model ICP~pXRF + RT
  {
    # Normality of residuals
    lm_model <- lm(Mn_ICP2~Mn_PXRF2 + Substrate_RT + Total_Weight, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  
  
  
  
  
  
  
  {
    # Normality of residuals
    lm_model <- lm(Fe_ICP2~Fe_PXRF2, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  {
    # Normality of residuals
    lm_model <- lm(Fe_ICP2~Fe_PXRF2 + Substrate_RT, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  ##Assumptions for LM model ICP~pXRF + RT
  {
    # Normality of residuals
    lm_model <- lm(Fe_ICP2~Fe_PXRF2 + Substrate_RT + Total_Weight, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  
  
  
  
  
  {
    # Normality of residuals
    lm_model <- lm(Cr_ICP2~Cr_PXRF2, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  {
    # Normality of residuals
    lm_model <- lm(Cr_ICP2~Cr_PXRF2 + Substrate_RT, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  ##Assumptions for LM model ICP~pXRF + RT
  {
    # Normality of residuals
    lm_model <- lm(Cr_ICP2~Cr_PXRF2 + Substrate_RT + Total_Weight, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  
  
  
  
  {
    # Normality of residuals
    lm_model <- lm(As_ICP2~As_PXRF2, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  {
    # Normality of residuals
    lm_model <- lm(As_ICP2~As_PXRF2 + Substrate_RT, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  ##Assumptions for LM model ICP~pXRF + RT
  {
    # Normality of residuals
    lm_model <- lm(As_ICP2~As_PXRF2 + Substrate_RT + Total_Weight, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  
  {
    # Normality of residuals
    lm_model <- lm(As_ICP2~As_PXRF2, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  {
    # Normality of residuals
    lm_model <- lm(As_ICP2~As_PXRF2 + Substrate_RT, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
  ##Assumptions for LM model ICP~pXRF + RT
  {
    # Normality of residuals
    lm_model <- lm(As_ICP2~As_PXRF2 + Substrate_RT + Total_Weight, data=dt)
    shapiro_test <- shapiro.test(resid(lm_model))
    qqnorm(resid(lm_model))
    qqline(resid(lm_model))
    
    # Homoscedasticity (Constant Variance of Errors) - Breusch-Pagan test
    library(lmtest)
    bptest(lm_model)
    plot(fitted(lm_model), resid(lm_model))
    abline(h = 0, col = "red")
    
    # Independence of Errors - to check autocorrelation of error - Durbin-Watson test
    dwtest(lm_model) # p<0.05 strong autocorrelation of the residuals, violation of lm
    
    #Absence of Multicollinearity - Variance Inflation Factors (VIF)
    library(car)
    vif(lm_model) #No multicollinearity
  }
  
}

#GLM

start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)

M1Cu <- glm(Cu_ICP ~ Cu_PXRF, data = dt, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
M2Cu <- glm(Cu_ICP ~ Cu_PXRF + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
M3Cu <- glm(Cu_ICP ~ Cu_PXRF + Substrate_RT, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
M4Cu <- glm(Cu_ICP ~ Cu_PXRF + Substrate_RT + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))

AIC(M1Cu)
AIC(M2Cu)
AIC(M3Cu)
AIC(M4Cu)

summary(M1Cu)
summary(M2Cu)
summary(M3Cu)
summary(M4Cu)

##Outliers
dt$z_scores_Cu_ICP <- (dt$Cu_ICP - mean(dt$Cu_ICP, na.rm = TRUE)) / sd(dt$Cu_ICP, na.rm = TRUE)
outlier_indices_Cu_ICP <- which(abs(dt$z_scores_Cu_ICP) > 3 & !is.na(dt$z_scores_Cu_ICP))
dt$z_scores_Cu_PXRF <- (dt$Cu_PXRF - mean(dt$Cu_PXRF, na.rm = TRUE)) / sd(dt$Cu_PXRF, na.rm = TRUE)
outlier_indices_Cu_PXRF <- which(abs(dt$z_scores_Cu_PXRF) > 3 & !is.na(dt$z_scores_Cu_PXRF))
combined_outlier_indices <- union(outlier_indices_Cu_ICP, outlier_indices_Cu_PXRF)
outliers_combined <- dt[combined_outlier_indices,]
dt_cleaned <- dt[-combined_outlier_indices,]
dt_cleaned$z_scores_Cu_ICP <- NULL
dt_cleaned$z_scores_Cu_PXRF <- NULL



M1Cu <- glm(Cu_ICP ~ Cu_PXRF, data = dt_cleaned, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
M2Cu <- glm(Cu_ICP ~ Cu_PXRF + Total_Weight, data = dt_cleaned, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
M3Cu <- glm(Cu_ICP ~ Cu_PXRF + Substrate_RT, data = dt_cleaned, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
M4Cu <- glm(Cu_ICP ~ Cu_PXRF + Substrate_RT + Total_Weight, data = dt_cleaned, family = Gamma(link = "identity"), control = glm.control(maxit = 50))

AIC(M1Cu)
AIC(M2Cu)
AIC(M3Cu)
AIC(M4Cu)
summary(M1Cu)
summary(M2Cu)
summary(M3Cu)
summary(M4Cu)

dt$z_scores_Fe_ICP <- (dt$Fe_ICP - mean(dt$Fe_ICP, na.rm = TRUE)) / sd(dt$Fe_ICP, na.rm = TRUE)
outlier_indices_Fe_ICP <- which(abs(dt$z_scores_Fe_ICP) > 3 & !is.na(dt$z_scores_Fe_ICP))
dt$z_scores_Fe_PXRF <- (dt$Fe_PXRF - mean(dt$Fe_PXRF, na.rm = TRUE)) / sd(dt$Fe_PXRF, na.rm = TRUE)
outlier_indices_Fe_PXRF <- which(abs(dt$z_scores_Fe_PXRF) > 3 & !is.na(dt$z_scores_Fe_PXRF))
combined_outlier_indices <- union(outlier_indices_Fe_ICP, outlier_indices_Fe_PXRF)
outliers_combined <- dt[combined_outlier_indices,]
dt_cleaned <- dt[-combined_outlier_indices,]
dt_cleaned$z_scores_Fe_ICP <- NULL
dt_cleaned$z_scores_Fe_PXRF <- NULL






describe(dt)
res_aov <- aov(Cu_PXRF~Form, data=dt)
res_aov <- aov(Cu_ICP~Form, data=dt)
QQ <- qqPlot(res_aov$residuals, id=FALSE)


#shapiro.test(dt$Cu_ICP)
#test_result <- shapiro.test(na.omit(dt$Cu_ICP))

#Normality loop with Shapiro
{
library(writexl)


  results <- data.frame(Column = character(0), W_Statistic = numeric(0), P_Value = numeric(0), Sample_Size = numeric(0), Valid = logical(0))
  
  for (i in 17:95) {

    column_data <- na.omit(dt[[i]])
    sample_size <- length(column_data)
    
    if (sample_size >= 3 && sample_size <= 5000) {
      test_result <- shapiro.test(column_data)
      results <- rbind(results, data.frame(
        Column = names(dt)[i],
        W_Statistic = test_result$statistic,
        P_Value = test_result$p.value,
        Sample_Size = sample_size,
        Valid = TRUE
      ))
    } else {

      results <- rbind(results, data.frame(
        Column = names(dt)[i],
        W_Statistic = NA,
        P_Value = NA,
        Sample_Size = sample_size,
        Valid = FALSE
      ))
    }
  }
  
  # Write the results to an Excel file
  write_xlsx(results, "ShapiroTestResults.xlsx")
  
  
}

















# ANOVA DOPIERO PO MODELU!!!!!!!!!!!!!!!!!!!!!!!!
#Homogenity of variance across groups (Scientific_Name and Plot)
lev <- leveneTest(Cu_PXRF ~ Form, data = dt)
print(lev) # variance is heterogenous for form

lev <- leveneTest(Cu_PXRF ~ Form*Plot, data = dt)
print(lev) 








library(lmtest)
lm_model <- lm(Cu_ICP~Cu_concentration, data=dt)
breusch_pagan_test <- bptest(lm_model) # jest hetero
breusch_pagan_test
library(bestNormalize)
result <- bestNormalize(dt$Cu_concentration)
dt$Cu_concentration2 <- result$x.t
result # log_b (x+a) was chosen because it has the lowest value of 1.1173

result2 <- bestNormalize(dt$Cu_ICP)
dt$Cu_ICP2 <- result2$x.t
result2

lm_model <- lm(Cu_concentration2~Cu_ICP2, data=dt)
bptest(lm_model) # homo


