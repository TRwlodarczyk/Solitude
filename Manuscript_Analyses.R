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
{
setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New")

  
#Cu
  
  dt <- read.delim("SLT_pXRF_ICP.txt")
  dt <- dt[dt$Cu_concentration != 0.25, ] # To remove LODs
  
  dt$Predicted_Cu_ICP = 100.1909 + (1.3438* dt$Cu_concentration) + (-1150.8942* dt$Substrate_RT) 
  dt$Predicted_Cu_ICP2 <- 61.1518 + (1.2887* dt$Cu_concentration) + (-41.9641* dt$Total_Weight)
  dt$Predicted_Cu_ICP3 <- 29.954 + (1.213* dt$Cu_concentration) 

  
  lm_model1 <- lm(Cu_ICP ~ Cu_concentration, data = dt)
  summary(lm_model1)
  lm_model2 <- lm(Cu_ICP ~ Predicted_Cu_ICP, data = dt)
  summary(lm_model2)
  lm_model3 <- lm(Cu_ICP ~ Predicted_Cu_ICP2, data = dt)
  summary(lm_model3)
  lm_model4 <- lm(Cu_ICP ~ Predicted_Cu_ICP3, data = dt)
  summary(lm_model4)
  
  
#Fe
  
  dt <- read.delim("SLT_pXRF_ICP.txt")
  
  dt$Predicted_Fe_ICP <- 28.88747 + (1.41673* dt$Fe_concentration) + (-316.95475* dt$Substrate_RT)
  dt$Predicted_Fe_ICP2 <- 17.03270 + (1.45362* dt$Fe_concentration) + (-11.13508* dt$Total_Weight)
  dt$Predicted_Fe_ICP3 <- -1.00099 + (1.10113* dt$Fe_concentration)
  
  lm_model1 <- lm(Fe_ICP ~ Fe_concentration, data = dt)
  summary(lm_model1)
  lm_model2 <- lm(Fe_ICP ~ Predicted_Fe_ICP, data = dt)
  summary(lm_model2)
  lm_model3 <- lm(Fe_ICP ~ Predicted_Fe_ICP2, data = dt)
  summary(lm_model3)
  lm_model4 <- lm(Fe_ICP ~ Predicted_Fe_ICP3, data = dt)
  summary(lm_model4)
 
  
#Zn
  
  dt <- read.delim("SLT_pXRF_ICP.txt")
  dt <- dt[dt$Zn_concentration != 0.3, ] # To remove LODs
  dt$Predicted_Zn_ICP <- 50.8422 + (0.9560* dt$Zn_concentration) + (-473.9784* dt$Substrate_RT)
  dt$Predicted_Zn_ICP2 <- 33.6939 + (0.9314* dt$Zn_concentration) + (-16.8131* dt$Total_Weight)
  dt$Predicted_Zn_ICP3 <- 21.7247 + (0.9342* dt$Zn_concentration) 
  
  
  lm_model1 <- lm(Zn_ICP ~ Zn_concentration, data = dt)
  summary(lm_model1)
  lm_model2 <- lm(Zn_ICP ~ Predicted_Zn_ICP, data = dt)
  summary(lm_model2)
  lm_model3 <- lm(Zn_ICP ~ Predicted_Zn_ICP2, data = dt)
  summary(lm_model3)
  lm_model4 <- lm(Zn_ICP ~ Predicted_Zn_ICP3, data = dt)
  summary(lm_model4)
  
#Se
  
  dt <- read.delim("SLT_pXRF_ICP.txt")
  dt <- dt[dt$Se_concentration != 0.05, ] # To remove LODs

  dt$Predicted_Se_ICP <- 0.4417 + (1.5683* dt$Se_concentration) + (-8.8017* dt$Substrate_RT)
  dt$Predicted_Se_ICP2 <- 0.07610 + (1.58532* dt$Se_concentration) + (-0.32381* dt$Total_Weight) # but intercept is not significant, I wouldnt use this
  dt$Predicted_Se_ICP3 <- -0.18545 + (1.60411* dt$Se_concentration) # 
  
  lm_model1 <- lm(Se_ICP ~ Se_concentration, data = dt)
  summary(lm_model1)
  lm_model2 <- lm(Se_ICP ~ Predicted_Se_ICP, data = dt)
  summary(lm_model2)
  lm_model3 <- lm(Se_ICP ~ Predicted_Se_ICP2, data = dt)
  summary(lm_model3)
  lm_model4 <- lm(Se_ICP ~ Predicted_Se_ICP3, data = dt)
  summary(lm_model4)
  

  #Mn
  
  dt <- read.delim("SLT_pXRF_ICP.txt")
  dt <- dt[dt$Mn_concentration != 0.5, ] # To remove LODs
  dt$Predicted_Mn_ICP <- 51.4943 + (1.0760* dt$Mn_concentration) + (-431.8509* dt$Substrate_RT)
  dt$Predicted_Mn_ICP2 <- 40.6027 + (1.0494* dt$Mn_concentration) + (-20.5045* dt$Total_Weight) # but intercept is not significant, I wouldnt use this
  dt$Predicted_Mn_ICP3 <- 26.783 + (1.030* dt$Mn_concentration) # but intercept is not significant, I wouldnt use this
  
  lm_model1 <- lm(Mn_ICP ~ Mn_concentration, data = dt)
  summary(lm_model1)
  lm_model2 <- lm(Mn_ICP ~ Predicted_Mn_ICP, data = dt)
  summary(lm_model2)
  lm_model3 <- lm(Mn_ICP ~ Predicted_Mn_ICP2, data = dt)
  summary(lm_model3)
  lm_model4 <- lm(Mn_ICP ~ Predicted_Mn_ICP3, data = dt)
  summary(lm_model4)
  
 
# Re
  
  
#As
  
dt <- read.delim("SLT_pXRF_ICP.txt")
dt <- dt[dt$As_concentration != 0.05, ] # To remove LODs
dt$Predicted_As_ICP <- 0.5213 + (0.0643* dt$As_concentration) + (-3.8479* dt$Substrate_RT)
dt$Predicted_As_ICP3 <- 0.19905 + (0.09543* dt$As_concentration) # From model 6 significant correlations OK

lm_model1 <- lm(As_ICP ~ As_concentration, data = dt)
summary(lm_model1)
lm_model2 <- lm(As_ICP ~ Predicted_As_ICP, data = dt)
summary(lm_model2)
lm_model4 <- lm(As_ICP ~ Predicted_As_ICP3, data = dt)
summary(lm_model4)

#Cr
dt <- read.delim("SLT_pXRF_ICP.txt")
dt <- dt[dt$Cr_concentration != 1, ] # To remove LODs
dt <- dt[dt$Cr_ICP != 0, ] # To remove LODs
dt$Predicted_Cr_ICP <- 0.60775 + (0.01228* dt$Cr_concentration) + (-5.78566* dt$Substrate_RT)
dt$Predicted_Cr_ICP2 <- 0.39856 + (0.02* dt$Cr_concentration) + (-0.26371* dt$Total_Weight)
dt$Predicted_Cr_ICP3 <- 0.11921 + (0.03795* dt$Cr_concentration)  # based on model 6

lm_model1 <- lm(Cr_ICP ~ Cr_concentration, data = dt)
summary(lm_model1)
lm_model2 <- lm(Cr_ICP ~ Predicted_Cr_ICP, data = dt)
summary(lm_model2)
lm_model3 <- lm(Cr_ICP ~ Predicted_Cr_ICP2, data = dt)
summary(lm_model3)
lm_model4 <- lm(Cr_ICP ~ Predicted_Cr_ICP3, data = dt)
summary(lm_model4)



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

}











# Check some values above toxicity
setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final")
dt <- read.delim("SLT_Final_3reps.09.06.23.txt")
dt <- dt[dt$Type_of_Sample != "root", ]
dt <- dt[dt$Site != "CONTROL", ]
dt <- dt[dt$Type_of_Sample != "stem", ]


mean(dt$Predicted_Cu_ICP[dt$Scientific_Name == 'Phyla nodiflora'], na.rm = TRUE)
sd(dt$Predicted_Cu_ICP[dt$Scientific_Name == 'Phyla nodiflora'], na.rm = TRUE)
mean(dt$Predicted_Cu_ICP[dt$Scientific_Name == 'Ceanothus greggii'], na.rm = TRUE)
sd(dt$Predicted_Cu_ICP[dt$Scientific_Name == 'Ceanothus greggii'], na.rm = TRUE)
mean(dt$Predicted_Cu_ICP[dt$Scientific_Name == 'Acmispon rigidus'], na.rm = TRUE)
sd(dt$Predicted_Cu_ICP[dt$Scientific_Name == 'Acmispon rigidus'], na.rm = TRUE)
mean(dt$Predicted_Cu_ICP[dt$Scientific_Name == 'Mentzelia multiflora'], na.rm = TRUE)
sd(dt$Predicted_Cu_ICP[dt$Scientific_Name == 'Mentzelia multiflora'], na.rm = TRUE)


median(dt$Predicted_Cu_ICP[dt$Scientific_Name == 'Berberis haematocarpa'], na.rm = TRUE)
median(dt$Predicted_Fe_ICP[dt$Scientific_Name == 'Berberis haematocarpa'], na.rm = TRUE)
median(dt$Predicted_Fe_ICP[dt$Scientific_Name == 'Aristida adscencionis'], na.rm = TRUE)
median(dt$Predicted_Fe_ICP[dt$Scientific_Name == 'Dasylirion wheeleri'], na.rm = TRUE)
median(dt$Predicted_Fe_ICP[dt$Scientific_Name == 'Dasylirion wheeleri'], na.rm = TRUE)