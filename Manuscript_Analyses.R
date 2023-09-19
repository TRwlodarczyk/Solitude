# Solitude Final worksheet - Manuscript analyses
# Tomasz Wlodarczyk
# twlodarczyk@arizona.edu
# 2023-09-13

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
  library(car)
}


#Correlation between elements in plants
{
  
  setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final")
  dt <- read.delim("SLT_heatmap_plants_2.txt")
  dt <- subset(dt, Site != 'CONTROL')
  dt <- dt[dt$Type_of_Sample != "stem", ]
  dt <- dt[dt$Type_of_Sample != "root", ]
  
  shapiro.test(dt$Fe) # non normal
  shapiro.test(dt$Zn) # non normal
  shapiro.test(dt$Mn) # non normal
  shapiro.test(dt$Se) # non normal
  shapiro.test(dt$Cr) # non normal
  shapiro.test(dt$Re) # non normal
  cor.test(dt$Cu, dt$Fe, method="spearman") #0.55
  cor.test(dt$Cu, dt$Zn, method="spearman") #0.3
  cor.test(dt$Cu, dt$Mn, method="spearman") #0.38
  cor.test(dt$Cu, dt$Se, method="spearman") #0.34
  cor.test(dt$Cu, dt$Re, method="spearman") #0.22 pval 0.00124
  
  leveneTest(dt$Cu, dt$Fe) # <0.05, variances are heterogeneous (unequal) (violation of ANOVA). homogeneity of variances (also known as homoscedasticity)
  
  
  cor.test(dt$Fe, dt$Zn, method="spearman") # 0.0697 pval 0.304
  cor.test(dt$Fe, dt$Se, method="spearman") # 0.016 pval 0.8059
  cor.test(dt$Fe, dt$Mn, method="spearman") # 0.1631 pval 0.01567
  cor.test(dt$Fe, dt$Re, method="spearman") # 0.007339 pval 0.914
  
  
  cor.test(dt$Zn, dt$Se, method="spearman") # 0.21 pval 0.002177
  cor.test(dt$Zn, dt$Mn, method="spearman") # 0.289  pval < 0.00001
  cor.test(dt$Zn, dt$Re, method="spearman") # 0.222 pval 0.000895
  
  cor.test(dt$Se, dt$Mn, method="spearman") # 0.4137
  cor.test(dt$Se, dt$Re, method="spearman") # 0.37759
  cor.test(dt$Re, dt$Mn, method="spearman") # 0.4982833
  
  
  
  dt <- dt[dt$Scientific_Name == "Allionia incarnata", ]
  
  cor.test(dt$Fe, dt$Zn, method="spearman") # 
  
  
  ##kombo
  
  
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  
  # Assuming dt is your dataset
  
  # Pivot the data to long format
  dt_long <- dt %>%
    pivot_longer(cols = c(Fe, Zn, Mn),
                 names_to = "Variable",
                 values_to = "Value")
  
  # Calculate mean and standard deviation for each combination
  mean_sd_data <- dt_long %>%
    group_by(Scientific_Name, Variable) %>%
    summarize(Mean = mean(Value),
              SD = sd(Value))
  
  # Merge the calculated data back with the original long data
  dt_long <- dt_long %>%
    left_join(mean_sd_data, by = c("Scientific_Name", "Variable"))
  
  # Create the plot
  Cu <- ggplot(dt_long, aes(x = Scientific_Name, 
                            y = Value, fill = Variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                  position = position_dodge(),
                  
                  size = 0.5) +
    scale_fill_manual(values = c("Fe" = "#6699CC", "Zn" = "#36454F", "Mn" = "#D3D3D3")) +
    coord_flip() +
    labs(x = "", y = "Zn Fe Mn (mg/kg)") +
    theme_bw() +
    theme(legend.key.size = unit(1, "lines"),
          legend.text = element_text(size = 13.5), 
          legend.title = element_text(size = 15, face = "bold"))
  
  print(Cu)
  
  
  
  
  
  
}

# R squared
setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New")
#As

dt <- read.delim("SLT_pXRF_ICP.txt")
dt <- dt[dt$As_concentration != 0.05, ] # To remove LODs
dt$Predicted_As_ICP <- 0.5213 + (0.0643* dt$As_concentration) + (-3.8479* dt$Substrate_RT)

lm_model1 <- lm(As_ICP ~ As_concentration, data = dt)
summary(lm_model1)
lm_model2 <- lm(As_ICP ~ Predicted_As_ICP, data = dt)
summary(lm_model2)

#Cr
dt <- read.delim("SLT_pXRF_ICP.txt")
dt <- dt[dt$Cr_concentration != 1, ] # To remove LODs
dt <- dt[dt$Cr_ICP != 0, ] # To remove LODs
dt$Predicted_Cr_ICP <- 0.60775 + (0.01228* dt$Cr_concentration) + (-5.78566* dt$Substrate_RT)

lm_model1 <- lm(Cr_ICP ~ Cr_concentration, data = dt)
summary(lm_model1)
lm_model2 <- lm(Cr_ICP ~ Predicted_Cr_ICP, data = dt)
summary(lm_model2)

#Ti
dt <-read.delim("Solitude_pXRF_ICP_correl_Re.txt")
{
  tr <- matrix(data = NA, ncol = ncol(dt[,c(1:59)]), nrow=nrow(dt)) # select all columns 1:46
  colnames(tr) <- colnames(dt[,c(1:59)])
  for (i in 14:48) # select when the concentrations start
  {
    tr[,c(i)] <- gsub(".*ND.*", 0, dt[,i])
  }
  
  for(i in c(1:13, 49:59)) # select columns that need to stay the same 1:11 include character and double (weight)
  {
    tr[,c(i)] <- dt[,c(i)]
  }
  
}
#transform to dataframe
tr <- as.data.frame.matrix(tr) #A correct command to change the dataset to dataframe after transformations
tr[,14:59] <- sapply(tr[,14:59],as.numeric)# Change a character to numeric (double)
tr[,9] <- sapply(tr[,9],as.numeric)
typeof(tr$Cu_concentration) # confirm the value is no longer a character

dt <- tr


dt$Predicted_Ti_ICP <- -8.80946 + (0.46543* dt$Ti_concentration) + (162.79067* dt$Substrate_RT)

lm_model1 <- lm(Ti_ICP ~ Ti_concentration, data = dt)
summary(lm_model1)
lm_model2 <- lm(Ti_ICP ~ Predicted_Ti_ICP, data = dt)
summary(lm_model2)
