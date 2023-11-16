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
  
}



without_outliers <- replace_outliers(tol_medians[,6:12],1.5)
tol_medians_without_outl <- cbind(tol_medians[,1:5],without_outliers[[1]])
write.table(tol_medians_without_outl, file = "tol_medians_without_outl.txt", sep = "\t", col.names = T, row.names = F, quote = F, na = "NA", dec = ".")


