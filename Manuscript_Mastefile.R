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
}

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final")
dt <- read.delim("SLT_MASTER_TABLE_11_23_v2.txt")

dt <- dt[dt$Type_of_Sample != "root", ]
dt <- dt[dt$Site != "CONTROL", ]
dt <- dt[dt$Type_of_Sample != "stem", ]

dt[,17:95] <- sapply(dt[,17:95],as.numeric)

#shapiro.test(dt$Cu_ICP)
#test_result <- shapiro.test(na.omit(dt$Cu_ICP))

#Normality loop
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



#Homogenity of variance across groups (Scientific_Name and Plot)
levene_result_species <- leveneTest(Cu_PXRF ~ Scientific_Name, data = dt)
print(levene_result_species) # variance is homogenous across species



boxplot(data=dt, dt$Cu_PXRF)


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


