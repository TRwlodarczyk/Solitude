#### figure std coeff



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


setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/2_Manuscript_Figures&Tables")
dt <-read.delim("STDcoeff-fig.txt")


category_colors <- c(
  "Total Weight" = "#F3EEE8",
  "Species" = "#CCC2B5",
  "Form" = "#82715A",
  "Tube number[mass]" = "#AEA4A8",
  "Tube number" = "#BBBEC5",
  "biomass-percent[mass]" = "#425D79",
  "Qualitative mass" = "#353844",
  "Sample type" = "#C29BD3"
)



library(ggplot2)
library(ggthemes)
library(scales)

# Fixed bar width
bar_width <- 0.8

# Determine the maximum number of variables
max_vars <- max(length(unique(dt_cu$variable)), length(unique(dt_zn$variable)), length(unique(dt_mn$variable)))

# Define a function to create consistent plots
create_plot <- function(data, title, max_vars) {
  ggplot(data, aes(x = reorder(variable, -coefficient), 
                   y = coefficient, fill = category)) +
    geom_bar(stat = "identity", size = 0.22, color = "black", width = bar_width) +
    geom_errorbar(aes(ymin = coefficient - SD, ymax = coefficient + SD),
                  width = 0.25,
                  size = 0.2) +
    geom_abline(intercept = 0, slope = 0, color = "black", linetype = 1, linewidth = 0.45) +
    labs(title = title, x = "Variable", y = "Coefficient") +
    scale_fill_manual(values = category_colors) +
    scale_x_discrete(limits = levels(reorder(data$variable, -data$coefficient))[1:max_vars]) +
    scale_y_continuous(limits = c(-0.17, 0.15), breaks = seq(-0.15, 0.15, by = 0.05), expand = expansion(mult = c(0, 0.05)), labels = number_format(accuracy = 0.01)) +
    theme_classic2() +
    theme(axis.text.x = element_blank(),
          axis.line.x = element_blank())
}

# Cu plot
STD_Cu <- create_plot(dt_cu, "Bar Plot of Coefficients", max_vars)

# Zn plot
dt_zn <- subset(dt, element == "Zn")
STD_Zn <- create_plot(dt_zn, "Zn percent error STD", max_vars)

# Mn plot
dt_mn <- subset(dt, element == "Mn")
STD_Mn <- create_plot(dt_mn, "Mn percent error STD", max_vars)

# Display the plots
print(STD_Cu)
print(STD_Zn)
print(STD_Mn)

