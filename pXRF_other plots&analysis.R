# Other plots

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


#### Szybki wykres NA 
{
  dt_NA_Cu <- dt %>% 
    filter(is.na(Cu_PXRF))
  
  fit_2x <- nls(Total_Weight ~ a * 2^Cu_ICP, data = dt_NA_Cu, start = list(a = 2))
  
  # Add fitted values to the data frame
  dt_NA_Cu$fit_2x <- predict(fit_2x)
  
  ggplot(data=dt_NA_Cu, aes(x = Total_Weight, y = Cu_ICP)) +
    geom_point(color = "#003f5c", size=2.5, stroke=1, shape=1) +
    geom_line(aes(y = fit_2x), color = "blue") +
    labs(x = "TW", y = "ICP concentration Cu") +
    theme_classic() 
  
  
  
  

  
  fit_model <- nls(Cu_ICP ~ a * exp(b * Total_Weight), data = dt_NA_Cu, start = list(a = 1, b = 0.1))
  
  # Add fitted values to the data frame
  dt_NA_Cu$fit_values <- predict(fit_model)
  
  # Plot the data and the fitted model
  ggplot(data = dt_NA_Cu, aes(x = Total_Weight, y = Cu_ICP)) +
    geom_point(color = "#003f5c", size = 2.5, stroke = 1, shape = 1) +
    geom_line(aes(y = fit_values), color = "red") +
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