# Solitude Plant ICP-analysis
# Tomasz Wlodarczyk
# twlodarczyk@arizona.edu
# 2024 - 17 - 03
#Using dataset with all ICP points and pXRF points 
#Testing homogenity of ICP results for single tube samples etc.
#testing anova

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


setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/")
dt <-read.delim("Solitude_pXRF_ICP_AllPoints_metadata.txt")








