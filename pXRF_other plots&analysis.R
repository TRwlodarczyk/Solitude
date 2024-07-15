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


#### Checking what function fits NA plot
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
  
  
  
  

  
  fit_model <- nls(Cu_ICP ~ a * exp(b * Total_Weight), data = dt_NA_Cu, 
                   start = list(a = 2, b = 0.1))
  
  # Add fitted values to the data frame
  dt_NA_Cu$fit_values <- predict(fit_model)
  
  # Plot the data and the fitted model
  ggplot(data = dt_NA_Cu, aes(x = Total_Weight, y = Cu_ICP)) +
    geom_point(color = "#003f5c", size = 2.5, stroke = 1, shape = 1) +
    geom_line(aes(y = fit_values), color = "red") +
    labs(x = "TW", y = "ICP concentration Cu") +
    theme_classic()
  
  
  
  
  
  # Fit a reciprocal function
  fit_reciprocal <- nls(Cu_ICP ~ a / (Total_Weight + b), data = dt_NA_Cu, start = list(a = 1, b = 1))
  
  # Add fitted values to the data frame
  dt_NA_Cu$fit_values <- predict(fit_reciprocal)
  
  # Plot the data and the fitted reciprocal function
  ggplot(data = dt_NA_Cu, aes(x = Total_Weight, y = Cu_ICP)) +
    geom_point(color = "#003f5c", size = 2.5, stroke = 1, shape = 1) +
    geom_line(aes(y = fit_values), color = "red") +
    labs(x = "TW", y = "ICP concentration Cu") +
    theme_classic()
  
  summary(fit_reciprocal)
  
  rss <- sum((dt_NA_Cu$Cu_ICP - dt_NA_Cu$fit_values)^2)  # Residual sum of squares
  tss <- sum((dt_NA_Cu$Cu_ICP - mean(dt_NA_Cu$Cu_ICP))^2)  # Total sum of squares
  r_squared <- 1 - (rss / tss)
  
  
}
  
  

  
  
#NA plots
  
{
  
  dt_NA_Cu <- dt %>% 
    filter(is.na(Cu_PXRF))
  
  ggplot(data=dt_NA_Cu, aes(x = Total_Weight, y = Cu_ICP)) +
    geom_point(color = "#003f5c", size=1.6, stroke=1, shape=1) +
    labs(x = "Total Weight (g)", y = "CuICP-MS") +
    theme_classic() 
  
  
  
  dt_NA_Zn <- dt %>% 
    filter(is.na(Zn_PXRF))
  
  
  ggplot(data=dt_NA_Zn, aes(x = Total_Weight, y = Zn_ICP)) +
    geom_point(color = "#003f5c", size=1.6, stroke=1, shape=1) +
    labs(x = "Total Weight (g)", y = "ZnICP-MS") +
    theme_classic() 
  
  
  dt_NA_Mn <- dt %>% 
    filter(is.na(Mn_PXRF))
  
  
  ggplot(data=dt_NA_Mn, aes(x = Total_Weight, y = Mn_ICP)) +
    geom_point(color = "#003f5c", size=1.6, stroke=1, shape=1) +
    labs(x = "Total Weight (g)", y = "MnICP-MS") +
    theme_classic() 
  
  
  dt_NA_Se <- dt %>% 
    filter(is.na(Se_PXRF))
  
  
  ggplot(data=dt_NA_Se, aes(x = Total_Weight, y = Se_ICP)) +
    geom_point(color = "#003f5c", size=1.6, stroke=1, shape=1) +
    labs(x = "Total Weight (g)", y = "SeICP-MS") +
    theme_classic() 
  
  
  dt_NA_Re <- dt %>% 
    filter(is.na(Re_PXRF))
  
  
  ggplot(data=dt_NA_Re, aes(x = Total_Weight, y = Re_ICP)) +
    geom_point(color = "#003f5c", size=1.6, stroke=1, shape=1) +
    labs(x = "Total Weight (g)", y = "ReICP-MS") +
    theme_classic() 
  
  
  dt_NA_Fe <- dt %>% 
    filter(is.na(Fe_PXRF))
  
  
  ggplot(data=dt_NA_Fe, aes(x = Total_Weight, y =Fe_ICP)) +
    geom_point(color = "#003f5c", size=1.6, stroke=1, shape=1) +
    labs(x = "Total Weight (g)", y = "FeICP-MS") +
    theme_classic() 
  
}



#TW RT Plot

# Fit a quadratic model predicting Total_Weight based on Substrate_RT
fit_quadratic <- lm(Total_Weight ~ poly(Substrate_RT, 2, raw=TRUE), data = dt)

# Add fitted values to the data frame
dt$fit_values <- predict(fit_quadratic)

# Calculate R-squared value
r_squared <- summary(fit_quadratic)$r.squared

# Print the R-squared value
print(paste("R-squared: ", r_squared))

# Extract coefficients
coefficients <- coef(fit_quadratic)

# Format the equation
equation <- paste0("Total_Weight = ",
                   round(coefficients[1], 3), " + ",
                   round(coefficients[2], 3), "*Substrate_RT + ",
                   round(coefficients[3], 3), "*Substrate_RT^2")

# Print the equation
print(paste("Equation: ", equation))

# Plot the data and the fitted quadratic curve with a shaded area representing the confidence interval
ggplot(data = dt, aes(x = Substrate_RT, y = Total_Weight)) +
  geom_point(color = "#003f5c", size = 1.6, stroke = 1, shape = 1) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), color = "#8B322C", fill = "#8B322C", alpha = 0.2) +
  geom_line(aes(y = fit_values), color = "#8B322C") +
  labs(x = "RT", y = "Total Weight (g)") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

summary(fit_quadratic)
cor.test(dt$Substrate_RT, dt$Total_Weight, method="spearman") 
