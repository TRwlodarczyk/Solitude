## SLT Manuscript data change
# Tomasz Wlodarczyk
# twlodarczyk@arizona.edu
# 2023-07-11 Nov 23

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
  library(car)
}

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final")
dt <- read.delim("SLT_MASTER_TABLE_11_23.txt")

#dt <- dt[dt$Type_of_Sample != "root", ]
#dt <- dt[dt$Site != "CONTROL", ]
#dt <- dt[dt$Type_of_Sample != "stem", ]

#remove ND
{
  tr <- matrix(data = NA, ncol = ncol(dt[,c(1:85)]), nrow=nrow(dt)) # select all columns 1:46
  colnames(tr) <- colnames(dt[,c(1:85)])
  for (i in 18:85) # select when the concentrations start
  {
    tr[,c(i)] <- gsub(".*ND.*", 0, dt[,i])
  }
  
  for(i in c(1:17)) # select columns that need to stay the same 1:11 include character and double (weight)
  {
    tr[,c(i)] <- dt[,c(i)]
  }
  tr   
  
  #transform to dataframe
  tr <- as.data.frame.matrix(tr) #A correct command to change the dataset to dataframe after transformations
  tr[,18:85] <- sapply(tr[,18:85],as.numeric) # Change a character to numeric (double)
  typeof(tr$Cu_PXRF) # confirm the value is no longer a character
  dt <- tr
}


#apply LODs
{
  dt$Cl_PXRF[dt$Cl_PXRF == 0] <- NA
  dt$Ca_PXRF[dt$Ca_PXRF == 0] <- NA
  dt$Ti_PXRF[dt$Ti_PXRF == 0] <- NA
  dt$Cr_PXRF[dt$Cr_PXRF == 0] <- NA
  dt$Mn_PXRF[dt$Mn_PXRF == 0] <- NA
  dt$Fe_PXRF[dt$Fe_PXRF == 0] <- NA
  dt$Co_PXRF[dt$Co_PXRF == 0] <- NA
  dt$Ni_PXRF[dt$Ni_PXRF == 0] <- NA
  dt$Cu_PXRF[dt$Cu_PXRF == 0] <- NA
  dt$Zn_PXRF[dt$Zn_PXRF == 0] <- NA
  dt$As_PXRF[dt$As_PXRF == 0] <- NA
  dt$Se_PXRF[dt$Se_PXRF == 0] <- NA
  dt$Cd_PXRF[dt$Cd_PXRF == 0] <- NA
  dt$Re_PXRF[dt$Re_PXRF == 0] <- NA
  dt$Hg_PXRF[dt$Hg_PXRF == 0] <- NA
  dt$Tl_PXRF[dt$Tl_PXRF == 0] <- NA
  dt$Pb_PXRF[dt$Pb_PXRF == 0] <- NA
  dt$Ga_PXRF[dt$Ga_PXRF == 0] <- NA
  dt$Ge_PXRF[dt$Ge_PXRF == 0] <- NA
  dt$Y_PXRF[dt$Y_PXRF == 0] <- NA
  dt$Al_PXRF[dt$Al_PXRF == 0] <- NA
  dt$Si_PXRF[dt$Si_PXRF == 0] <- NA
  dt$S_PXRF[dt$S_PXRF == 0] <- NA
  dt$Br_PXRF[dt$Br_PXRF == 0] <- NA
  dt$K_PXRF[dt$K_PXRF == 0] <- NA
  dt$P_PXRF[dt$P_PXRF == 0] <- NA
  dt$Rb_PXRF[dt$Rb_PXRF == 0] <- NA
  dt$Sr_PXRF[dt$Sr_PXRF == 0] <- NA
  dt$Zr_PXRF[dt$Zr_PXRF == 0] <- NA
  dt$Pd_PXRF[dt$Pd_PXRF == 0] <- NA
  dt$Ag_PXRF[dt$Ag_PXRF == 0] <- NA
  dt$In_PXRF[dt$In_PXRF == 0] <- NA
  dt$Sn_PXRF[dt$Sn_PXRF == 0] <- NA
  dt$Sb_PXRF[dt$Sb_PXRF == 0] <- NA
  dt$Ba_PXRF[dt$Ba_PXRF == 0] <- NA
  dt$Hf_PXRF[dt$Hf_PXRF == 0] <- NA
  dt$Bi_PXRF[dt$Bi_PXRF == 0] <- NA
  dt$W_PXRF[dt$W_PXRF == 0] <- NA

  dt$Predicted_Cu[dt$Predicted_Cu == 0.25] <- NA
  dt$Predicted_Zn[dt$Predicted_Zn == 0.3] <- NA
  dt$Predicted_Se[dt$Predicted_Se == 0.05] <- NA
  dt$Predicted_Mn[dt$Predicted_Mn == 0.5] <- NA
  dt$Predicted_As[dt$Predicted_As == 0.05] <- NA
  dt$Predicted_Cr[dt$Predicted_Cr == 1] <- NA
  dt$Predicted_Re[dt$Predicted_Re == 0.25] <- NA
  
}

write.table(dt, file='C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final/SLT_MASTER_TABLE_11_23_v2.csv', sep=",", row.names = F)

