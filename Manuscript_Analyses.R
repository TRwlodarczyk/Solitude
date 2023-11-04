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


P1 <- subset(dt, Plot=='P1')
P2 <- subset(dt, Plot=='P2')
P3 <- subset(dt, Plot=='P5')
P123 <- subset(dt, Plot!='P6')
P4 <- subset(dt, Plot=='P6')

stems <- subset(dt, Type_of_Sample=='stem')
leaves <- subset(dt, Type_of_Sample!='stem')

mean(stems$Predicted_Cu_ICP[stems$Scientific_Name == 'Boechera perennans'], na.rm = TRUE)
sd(stems$Predicted_Cu_ICP[stems$Scientific_Name == 'Boechera perennans'], na.rm = TRUE)/sqrt(4)

mean(leaves$Predicted_Cu_ICP[leaves$Scientific_Name == 'Boechera perennans'], na.rm = TRUE)
sd(leaves$Predicted_Cu_ICP[leaves$Scientific_Name == 'Boechera perennans'], na.rm = TRUE)/sqrt(8)

mean(stems$Predicted_Cu_ICP[stems$Scientific_Name == 'Pseudognaphalium canescens'], na.rm = TRUE)
sd(stems$Predicted_Cu_ICP[stems$Scientific_Name == 'Pseudognaphalium canescens'], na.rm = TRUE)/sqrt(1)

mean(leaves$Predicted_Cu_ICP[leaves$Scientific_Name == 'Pseudognaphalium canescens'], na.rm = TRUE)
sd(leaves$Predicted_Cu_ICP[leaves$Scientific_Name == 'Pseudognaphalium canescens'], na.rm = TRUE)/sqrt(8)

#Forbs, grasses, etc

sum(leaves$Form == 'Forb', na.rm = TRUE)
sum(leaves$Form == 'Grass', na.rm = TRUE)
sum(leaves$Form == 'Tree', na.rm = TRUE)
sum(leaves$Form == 'Shrub', na.rm = TRUE)
mean(leaves$Predicted_Cu_ICP[leaves$Form == 'Forb'], na.rm = TRUE)
min(leaves$Predicted_Cu_ICP[leaves$Form == 'Forb'], na.rm = TRUE)
max(leaves$Predicted_Cu_ICP[leaves$Form == 'Forb'], na.rm = TRUE)
median(leaves$Predicted_Cu_ICP[leaves$Form == 'Forb'], na.rm = TRUE)
sd(leaves$Predicted_Cu_ICP[leaves$Form == 'Forb'], na.rm = TRUE)/sqrt(109)

mean(leaves$Predicted_Cu_ICP[leaves$Form == 'Grass'], na.rm = TRUE)
min(leaves$Predicted_Cu_ICP[leaves$Form == 'Grass'], na.rm = TRUE)
max(leaves$Predicted_Cu_ICP[leaves$Form == 'Grass'], na.rm = TRUE)
median(leaves$Predicted_Cu_ICP[leaves$Form == 'Grass'], na.rm = TRUE)
sd(leaves$Predicted_Cu_ICP[leaves$Form == 'Grass'], na.rm = TRUE)/sqrt(48)

mean(leaves$Predicted_Cu_ICP[leaves$Form == 'Shrub'], na.rm = TRUE)
min(leaves$Predicted_Cu_ICP[leaves$Form == 'Shrub'], na.rm = TRUE)
max(leaves$Predicted_Cu_ICP[leaves$Form == 'Shrub'], na.rm = TRUE)
median(leaves$Predicted_Cu_ICP[leaves$Form == 'Shrub'], na.rm = TRUE)
sd(leaves$Predicted_Cu_ICP[leaves$Form == 'Shrub'], na.rm = TRUE)/sqrt(31)

mean(leaves$Predicted_Cu_ICP[leaves$Form == 'Tree'], na.rm = TRUE)
min(leaves$Predicted_Cu_ICP[leaves$Form == 'Tree'], na.rm = TRUE)
max(leaves$Predicted_Cu_ICP[leaves$Form == 'Tree'], na.rm = TRUE)
median(leaves$Predicted_Cu_ICP[leaves$Form == 'Tree'], na.rm = TRUE)
sd(leaves$Predicted_Cu_ICP[leaves$Form == 'Tree'], na.rm = TRUE)/sqrt(36)

#Cu
mean(P123$Predicted_Cu_ICP[P123$Scientific_Name == 'Amaranthus palmeri'], na.rm = TRUE)
mean(P4$Predicted_Cu_ICP[P4$Scientific_Name == 'Amaranthus palmeri'], na.rm = TRUE)
mean(P123$Predicted_Cu_ICP[P123$Scientific_Name == 'Nultuma (Prosopis) velutina'], na.rm = TRUE)
mean(P4$Predicted_Cu_ICP[P4$Scientific_Name == 'Nultuma (Prosopis) velutina'], na.rm = TRUE)
mean(P123$Predicted_Cu_ICP[P123$Scientific_Name == 'Mentzelia multiflora'], na.rm = TRUE)
mean(P4$Predicted_Cu_ICP[P4$Scientific_Name == 'Mentzelia multiflora'], na.rm = TRUE)
mean(P123$Predicted_Cu_ICP[P123$Scientific_Name == 'Cynodon dactylon'], na.rm = TRUE)
mean(P4$Predicted_Cu_ICP[P4$Scientific_Name == 'Cynodon dactylon'], na.rm = TRUE)
mean(P123$Predicted_Cu_ICP[P123$Scientific_Name == 'Eragrostis lehmanniana'], na.rm = TRUE)
mean(P4$Predicted_Cu_ICP[P4$Scientific_Name == 'Eragrostis lehmanniana'], na.rm = TRUE)
mean(P123$Predicted_Cu_ICP[P123$Scientific_Name == 'Portulaca suffrutescens'], na.rm = TRUE)
mean(P4$Predicted_Cu_ICP[P4$Scientific_Name == 'Portulaca suffrutescens'], na.rm = TRUE)
mean(P123$Predicted_Cu_ICP[P123$Scientific_Name == 'Solanum elaeagnifolium'], na.rm = TRUE) # except
mean(P4$Predicted_Cu_ICP[P4$Scientific_Name == 'Solanum elaeagnifolium'], na.rm = TRUE)
mean(P123$Predicted_Cu_ICP[P123$Scientific_Name == 'Tamarix chinensis'], na.rm = TRUE) 
mean(P4$Predicted_Cu_ICP[P4$Scientific_Name == 'Tamarix chinensis'], na.rm = TRUE)


#Fe
mean(P123$Predicted_Fe_ICP[P123$Scientific_Name == 'Amaranthus palmeri'], na.rm = TRUE)
mean(P4$Predicted_Fe_ICP[P4$Scientific_Name == 'Amaranthus palmeri'], na.rm = TRUE)
mean(P123$Predicted_Fe_ICP[P123$Scientific_Name == 'Nultuma (Prosopis) velutina'], na.rm = TRUE)
mean(P4$Predicted_Fe_ICP[P4$Scientific_Name == 'Nultuma (Prosopis) velutina'], na.rm = TRUE)
mean(P123$Predicted_Fe_ICP[P123$Scientific_Name == 'Mentzelia multiflora'], na.rm = TRUE)
mean(P4$Predicted_Fe_ICP[P4$Scientific_Name == 'Mentzelia multiflora'], na.rm = TRUE)
mean(P123$Predicted_Fe_ICP[P123$Scientific_Name == 'Cynodon dactylon'], na.rm = TRUE)
mean(P4$Predicted_Fe_ICP[P4$Scientific_Name == 'Cynodon dactylon'], na.rm = TRUE)
mean(P123$Predicted_Fe_ICP[P123$Scientific_Name == 'Eragrostis lehmanniana'], na.rm = TRUE)
mean(P4$Predicted_Fe_ICP[P4$Scientific_Name == 'Eragrostis lehmanniana'], na.rm = TRUE)
mean(P123$Predicted_Fe_ICP[P123$Scientific_Name == 'Portulaca suffrutescens'], na.rm = TRUE)
mean(P4$Predicted_Fe_ICP[P4$Scientific_Name == 'Portulaca suffrutescens'], na.rm = TRUE)
mean(P123$Predicted_Fe_ICP[P123$Scientific_Name == 'Solanum elaeagnifolium'], na.rm = TRUE) # except
mean(P4$Predicted_Fe_ICP[P4$Scientific_Name == 'Solanum elaeagnifolium'], na.rm = TRUE)
mean(P123$Predicted_Fe_ICP[P123$Scientific_Name == 'Tamarix chinensis'], na.rm = TRUE) 
mean(P4$Predicted_Fe_ICP[P4$Scientific_Name == 'Tamarix chinensis'], na.rm = TRUE)


# Counts how many samples you have
{
setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final")
dt <- read.delim("SLT_Final_3reps.09.06.23.txt")
dt <- dt[dt$Type_of_Sample != "root", ]
dt <- dt[dt$Site != "CONTROL", ]
dt <- dt[dt$Type_of_Sample != "stem", ]

#total - 259 bez rootow

#tailings total - 224 samples
sum(dt$Type_of_Sample == 'leaf-stem', na.rm = TRUE) #  109 leaf-stem samples
sum(dt$Type_of_Sample == 'leaf', na.rm = TRUE) #  110 leaf samples
sum(dt$Type_of_Sample == 'stem', na.rm = TRUE) #  5 leaf samples

dt <- dt[dt$Site != "TAILINGS", ]


# Count how many LODs in As and Cr
sum(dt$Cr_concentration == 0, na.rm = TRUE) #  46
sum(dt$As_concentration == 0, na.rm = TRUE) #  3
}
#QA samples recovery values

{
  setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final")
  dt <- read.delim("Solitude_Plants_Final_08.23.txt")
  dt <- subset(dt, Scientific_Name == 'QA_Sample')
  dt <- subset(dt, Sample_Name != 'blank')
  
  
  dt[,17:51] <- sapply(dt[,17:51],as.numeric)
  
  dt1573 <- subset(dt, Sample_Name == 'NIST1573')
  dt1570 <- subset(dt, Sample_Name == 'NIST1570')
  #na.rm = TRUE - omit NAs
  Cu <- mean(dt1573$Cu_concentration, na.rm = TRUE) 
  Se <- mean(dt1573$Se_concentration, na.rm = TRUE) 
  Re <- mean(dt1573$Re_concentration, na.rm = TRUE)  
  Zn <- mean(dt1573$Zn_concentration, na.rm = TRUE) 
  Mn <- mean(dt1573$Mn_concentration, na.rm = TRUE) 
  Fe <- mean(dt1573$Fe_concentration, na.rm = TRUE) 
  As <- mean(dt1573$As_concentration, na.rm = TRUE) 
  Ca <- mean(dt1573$Ca_concentration, na.rm = TRUE) 
  Cr <- mean(dt1573$Cr_concentration, na.rm = TRUE) 
  
  (Cu/4.7)*100 #97.787
  (Se/0.0543)*100 # lub non detected
  (Re) # NA
  (Zn/30.94)*100 #90.32
  (Mn/246.3)*100 #88.038
  (Fe/367.5)*100 #83.8095
  (As/0.1126)*100
  (Ca/50450)*100 #91.657
  (Cr/1.988)*100 #180.52
  
  
  Cu2 <- mean(dt1570$Cu_concentration, na.rm = TRUE) 
  Se2 <- mean(dt1570$Se_concentration, na.rm = TRUE) 
  Re2 <- mean(dt1570$Re_concentration, na.rm = TRUE)  
  Zn2 <- mean(dt1570$Zn_concentration, na.rm = TRUE) 
  Mn2 <- mean(dt1570$Mn_concentration, na.rm = TRUE) 
  Fe2 <- mean(dt1570$Fe_concentration, na.rm = TRUE) 

  (Cu2/12.22)*100 #88.17512
  (Se2/0.1152)*100 #395.44
  (Zn2/82.3)*100 #90.709
  
  dt1570 <- dt1570 %>% filter(Mn_concentration != 236.0)
  (Mn2/76)*100 #96.091
  (Fe2)# NA
  
  
}


#BC calculation
{
setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final")
dt <- read.delim("SLT_Final_3reps.09.06.23.txt")
dt <- dt[dt$Type_of_Sample != "root", ]
dt <- dt[dt$Site != "CONTROL", ]
dt <- dt[dt$Type_of_Sample != "stem", ]


bp <- mean(dt$Predicted_Cu_ICP[dt$Scientific_Name == 'Boechera perennans'], na.rm = TRUE)

bp/343

pc <- mean(dt$Predicted_Cu_ICP[dt$Scientific_Name == 'Pseudognaphalium canescens'], na.rm = TRUE)

pc/343

dt <- dt[dt$Site != "TAILINGS", ]
bp <- mean(dt$Predicted_Cu_ICP[dt$Scientific_Name == 'Boechera perennans'], na.rm = TRUE)

bp/187

}


# Data transformation  

{
  setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New")
  dt <- read.delim("SLT_pXRF_ICP.txt")
  dt <- dt[dt$Cu_concentration != 0.25, ] # To remove LODs
  
  shapiro.test(dt$Cu_concentration)
  shapiro.test(dt$Cu_ICP)
  leveneTest(dt$Cu_concentration, dt$Cu_ICP)
  library(lmtest)
  lm_model <- lm(Cu_ICP~Cu_concentration, data=dt)
  breusch_pagan_test <- bptest(lm_model) # jest hetero
  breusch_pagan_test
  
  dt$Cu_new_pxrf <- log(dt$Cu_concentration)
  dt$Cu_new_ICP <- log(dt$Cu_ICP)
  shapiro.test(dt$Cu_new_pxrf)
  shapiro.test(dt$Cu_new_ICP)
  leveneTest(dt$Cu_new_pxrf, dt$Cu_new_ICP)
  lm_model <- lm(Cu_new_ICP~Cu_new_pxrf, data=dt)
  breusch_pagan_test <- bptest(lm_model) # jest hetero wciaz

    library(MASS)
  bc_transform <- boxcox(dt$Cu_concentration ~ 1, plotit = TRUE)
  lambda <- bc_transform$x[which.max(bc_transform$y)]
  
  if (lambda != 0) {
    dt$transformed_data <- (dt$Cu_concentration^lambda - 1)/lambda
  } else {
    dt$transformed_data <- log(dt$Cu_concentration)
  }
  
  bc_transform2 <- boxcox(dt$Cu_ICP ~ 1, plotit = TRUE)
  lambda2 <- bc_transform2$x[which.max(bc_transform2$y)]
  
  if (lambda2 != 0) {
    dt$transformed_data2 <- (dt$Cu_ICP^lambda - 1)/lambda2
  } else {
    dt$transformed_data2 <- log(dt$Cu_ICP)
  }
  
  
  shapiro.test(dt$transformed_data)
  shapiro.test(dt$transformed_data2)
  leveneTest(dt$transformed_data, dt$transformed_data2)
  lm_model <- lm(transformed_data~transformed_data2, data=dt)
  breusch_pagan_test <- bptest(lm_model) # nie jest hetero
  
  
  dt <- read.delim("SLT_pXRF_ICP.txt")
  dt <- dt[dt$Se_concentration != 0.05, ] # To remove LODs
  
  shapiro.test(dt$Se_concentration)
  shapiro.test(dt$Se_ICP)
  leveneTest(dt$Se_concentration, dt$Se_ICP)
  library(lmtest)
  lm_model <- lm(Se_ICP~Se_concentration, data=dt)
  breusch_pagan_test <- bptest(lm_model) # jest hetero
  breusch_pagan_test
  
  
 
  
  
  
  
  
  library(MASS)
  bc_transform <- boxcox(dt$Se_concentration ~ 1, plotit = TRUE)
  lambda <- bc_transform$x[which.max(bc_transform$y)]
  
  if (lambda != 0) {
    dt$Se-box-pxrf <- (dt$Cu_concentration^lambda - 1)/lambda
  } else {
    dt$Se-box-pxrf <- log(dt$Cu_concentration)
  }
  


 

  dt <- read.delim("SLT_pXRF_ICP.txt")
  dt <- dt[dt$Cu_concentration != 0.25, ] # To remove LODs  
  
  shapiro.test(dt$Cu_concentration)
  shapiro.test(dt$Cu_ICP)
  
library(bestNormalize)
result <- bestNormalize(dt$Cu_concentration)
dt$Cu_concentration2 <- result$x.t
result # log_b (x+a) was chosen because it has the lowest value of 1.1173
  
result2 <- bestNormalize(dt$Cu_ICP)
dt$Cu_ICP2 <- result2$x.t
result2

lm_model <- lm(Cu_concentration2~Cu_ICP2, data=dt)
bptest(lm_model) # homo


dt <- read.delim("SLT_pXRF_ICP.txt")
dt <- dt[dt$Se_concentration != 0.05, ] # To remove LODs

shapiro.test(dt$Se_concentration)
shapiro.test(dt$Se_ICP)

result3 <- bestNormalize(dt$Se_concentration)
dt$Se_concentration2 <- result3$x.t
result3
result4 <- bestNormalize(dt$Se_ICP)
dt$Se_ICP2 <- result4$x.t
result4
 
shapiro.test(dt$Se_concentration2)
shapiro.test(dt$Se_ICP2)

lm_model <- lm(Se_concentration2~Se_ICP2, data=dt)
bptest(lm_model)# hetero - uffff


dt <-read.delim("Solitude_pXRF_ICP_correl_Re.txt")
shapiro.test(dt$Re_concentration)
shapiro.test(dt$Re_ICP)

lm_model <- lm(Re_concentration~Re_ICP, data=dt)
bptest(lm_model)

result5 <- bestNormalize(dt$Re_concentration)
dt$Re_concentration2 <- result5$x.t
result5
result6 <- bestNormalize(dt$Re_ICP)
dt$Re_ICP2 <- result6$x.t
result6

dt$Re_concentration2 <- log(dt$Re_concentration)
dt$Re_ICP2 <- log(dt$Re_ICP)

shapiro.test(dt$Re_concentration2)
shapiro.test(dt$Re_ICP2)
lm_model <- lm(Re_concentration2~Re_ICP2, data=dt)
bptest(lm_model) # heter - ufff


dt <- read.delim("SLT_pXRF_ICP.txt")
dt <- dt[dt$Zn_concentration != 0.3, ] # To remove LODs

shapiro.test(dt$Zn_concentration)
shapiro.test(dt$Zn_ICP)

lm_model <- lm(Zn_concentration~Zn_ICP, data=dt)
bptest(lm_model)

result7 <- bestNormalize(dt$Zn_concentration)
dt$Zn_concentration2 <- result7$x.t
result7
result8 <- bestNormalize(dt$Zn_ICP)
dt$Zn_ICP2 <- result8$x.t
result8

shapiro.test(dt$Zn_concentration2)
shapiro.test(dt$Zn_ICP2)
   
lm_model <- lm(Zn_concentration2~Zn_ICP2, data=dt)
bptest(lm_model) # hetero


dt <- read.delim("SLT_pXRF_ICP.txt")
dt <- dt[dt$Mn_concentration != 0.5, ] # To remove LODs

shapiro.test(dt$Mn_concentration)
shapiro.test(dt$Mn_ICP)
lm_model <- lm(Mn_concentration~Mn_ICP, data=dt)
bptest(lm_model)

result9 <- bestNormalize(dt$Mn_concentration)
dt$Mn_concentration2 <- result9$x.t
result9
result10 <- bestNormalize(dt$Mn_ICP)
dt$Mn_ICP2 <- result10$x.t
result10

shapiro.test(dt$Mn_concentration2)
shapiro.test(dt$Mn_ICP2)
lm_model <- lm(Mn_concentration2~Mn_ICP2, data=dt)
bptest(lm_model)# homo


dt <- read.delim("SLT_pXRF_ICP.txt")

shapiro.test(dt$Fe_concentration)
shapiro.test(dt$Fe_ICP)
lm_model <- lm(Fe_concentration~Fe_ICP, data=dt)
bptest(lm_model)

result11 <- bestNormalize(dt$Fe_concentration)
dt$Fe_concentration2 <- result11$x.t
result11
result12 <- bestNormalize(dt$Fe_ICP)
dt$Fe_ICP2 <- result12$x.t
result12

shapiro.test(dt$Fe_concentration2)
shapiro.test(dt$Fe_ICP2)
lm_model <- lm(Fe_concentration2~Fe_ICP2, data=dt)
bptest(lm_model)# homo




dt <- read.delim("SLT_pXRF_ICP.txt")
dt <- dt[dt$As_concentration != 0.05, ] # To remove LODs

shapiro.test(dt$As_concentration)
shapiro.test(dt$As_ICP)
lm_model <- lm(As_concentration~As_ICP, data=dt)
bptest(lm_model) # homo

result13 <- bestNormalize(dt$As_concentration)
dt$As_concentration2 <- result13$x.t
result13
result14 <- bestNormalize(dt$As_ICP)
dt$As_ICP2 <- result14$x.t
result14

shapiro.test(dt$As_concentration2)
shapiro.test(dt$As_ICP2)



dt <- read.delim("SLT_pXRF_ICP.txt")
dt <- dt[dt$Cr_concentration != 1, ] # To remove LODs
dt <- dt[dt$Cr_ICP != 0, ] # To remove LODs

shapiro.test(dt$Cr_concentration)
shapiro.test(dt$Cr_ICP)

lm_model <- lm(Cr_concentration~Cr_ICP, data=dt)
bptest(lm_model) # homo

result15 <- bestNormalize(dt$Cr_concentration)
dt$Cr_concentration2 <- result15$x.t
result15
result16 <- bestNormalize(dt$Cr_ICP)
dt$Cr_ICP2 <- result16$x.t
result16

shapiro.test(dt$Cr_concentration2)
shapiro.test(dt$Cr_ICP2)


}
  
#test shapiro on new data
setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final")
dt <- read.delim("SLT_Final_3reps.09.06.23.txt")
dt <- dt[dt$Type_of_Sample != "root", ]
dt <- dt[dt$Site != "CONTROL", ]
dt <- dt[dt$Type_of_Sample != "stem", ]


shapiro.test(dt$Predicted_Cu_ICP)


