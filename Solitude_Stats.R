# Solutude Statistical analysis
# Tomasz Wlodarczyk
# twlodarczyk@arizona.edu
# 2023-04-19

{
  library(data.table)
  library(reshape2)
  library(reshape)
  library("readxl")
  library(ggpubr)
  library(agricolae)
  library(tidyverse)
  library (readr) #to read URL
}

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/CESM/Boxplots")
dt_plants <-read_csv("dt_plants.csv")
dt_plants_trimmed <- dt_plants[c(-7,-8,-11,-12,-13,-14,-11, -27,-43, -45, -47, -seq(14,48,by=2))] #removed uncertainities
dt_plants_trimmed[,3] <- sapply(dt_plants_trimmed[,3],as.numeric)




####### Correlation both plants on plot 6 (35 datapoints)
dt_plants_XG_PC_6 <- dt_plants_trimmed[dt_plants_trimmed$Scientific_Name %in% c("Xanthisma gracile", "Pseudognaphalium canescens") 
                                       & dt_plants_trimmed$Plot %in% c("P6"), ]
dt_plants_XG <- dt_plants_trimmed[dt_plants_trimmed$Scientific_Name %in% "Xanthisma gracile",]
dt_plants_XG_6 <- dt_plants_trimmed[dt_plants_trimmed$Scientific_Name %in% "Xanthisma gracile"
                                   & dt_plants_trimmed$Plot %in% c("P6"), ]
dt_plants_PC_6 <- dt_plants_trimmed[dt_plants_trimmed$Scientific_Name %in% "Pseudognaphalium canescens",]
dt_plants_BP_6 <- dt_plants_trimmed[dt_plants_trimmed$Scientific_Name %in% "cf. Boechera perennans",]
dt_plants_NV <- dt_plants_trimmed[dt_plants_trimmed$Scientific_Name %in% "Nultuma (Prosopis) velutina",]
dt_plants_AP <- dt_plants_trimmed[dt_plants_trimmed$Scientific_Name %in% "Amaranthus palmeri",]
dt_plants_SG <- dt_plants_trimmed[dt_plants_trimmed$Scientific_Name %in% "Senegalia (Acacia) greggii",]
dt_plants_MB <- dt_plants_trimmed[dt_plants_trimmed$Scientific_Name %in% "Mimosa biuncifera",]



#shapiro.test() # p val > 0.05 =  normally distributed
#shapiro.test() # p val <0.05 = non normally distributed
#cor.test() # p val < 0.05 = significant correlation

#Gracile and Canescens on plot 6 (n = 10) (this may not be true since species are joined) 
{
shapiro.test(dt_plants_XG_PC_6$Cu_concentration) #  norm
shapiro.test(dt_plants_XG_PC_6$Cr_concentration) #  norm
shapiro.test(dt_plants_XG_PC_6$Fe_concentration) #  norm
shapiro.test(dt_plants_XG_PC_6$Ti_concentration) #  norm
cor.test(dt_plants_XG_PC_6$Fe_concentration, dt_plants_XG_PC_6$Cu_concentration, method="pearson") # no significant 
cor.test(dt_plants_XG_PC_6$Cr_concentration, dt_plants_XG_PC_6$Cu_concentration, method="pearson") # no significant 
cor.test(dt_plants_XG_PC_6$Fe_concentration, dt_plants_XG_PC_6$Cr_concentration, method="pearson") # no significant 
cor.test(dt_plants_XG_PC_6$Ti_concentration, dt_plants_XG_PC_6$Cu_concentration, method="pearson") # no significant 
cor.test(dt_plants_XG_PC_6$Ti_concentration, dt_plants_XG_PC_6$Cr_concentration, method="pearson") # no significant 
cor.test(dt_plants_XG_PC_6$Ti_concentration, dt_plants_XG_PC_6$Fe_concentration, method="pearson") # significant 
}

#Gracile over all plots (n = 13)
{
shapiro.test(dt_plants_XG$Cu_concentration) #  non norm
shapiro.test(dt_plants_XG$Cr_concentration) #  non norm
shapiro.test(dt_plants_XG$Fe_concentration) #  non norm
shapiro.test(dt_plants_XG$Ti_concentration) #  norm
cor.test(dt_plants_XG$Fe_concentration, dt_plants_XG$Cu_concentration, method="spearman") # no significant 
cor.test(dt_plants_XG$Cr_concentration, dt_plants_XG$Cu_concentration, method="spearman") # no significant 
cor.test(dt_plants_XG$Fe_concentration, dt_plants_XG$Cr_concentration, method="spearman") # no significant 
cor.test(dt_plants_XG$Ti_concentration, dt_plants_XG$Cu_concentration, method="spearman") # no significant 
cor.test(dt_plants_XG$Ti_concentration, dt_plants_XG$Cr_concentration, method="spearman") # no significant 
cor.test(dt_plants_XG$Ti_concentration, dt_plants_XG$Fe_concentration, method="spearman") # no significant 
}

#Gracile in plot 6 (n = 5)
{
  shapiro.test(dt_plants_XG_6$Cu_concentration) #  norm
  shapiro.test(dt_plants_XG_6$Cr_concentration) #  norm
  shapiro.test(dt_plants_XG_6$Fe_concentration) #  norm
  shapiro.test(dt_plants_XG_6$Ti_concentration) #  norm
  cor.test(dt_plants_XG_6$Fe_concentration, dt_plants_XG_6$Cu_concentration, method="pearson") # significant 
  cor.test(dt_plants_XG_6$Cr_concentration, dt_plants_XG_6$Cu_concentration, method="pearson") # no significant 
  cor.test(dt_plants_XG_6$Fe_concentration, dt_plants_XG_6$Cr_concentration, method="pearson") # no significant 
  cor.test(dt_plants_XG_6$Ti_concentration, dt_plants_XG_6$Cu_concentration, method="pearson") # no significant 
  cor.test(dt_plants_XG_6$Ti_concentration, dt_plants_XG_6$Cr_concentration, method="pearson") # no significant 
  cor.test(dt_plants_XG_6$Ti_concentration, dt_plants_XG_6$Fe_concentration, method="pearson") # no significant 
}

#Canescens in plot 6 (n = 5)

{
  shapiro.test(dt_plants_PC_6$Cu_concentration) #  norm
  shapiro.test(dt_plants_PC_6$Cr_concentration) #  norm
  shapiro.test(dt_plants_PC_6$Fe_concentration) #  norm
  shapiro.test(dt_plants_PC_6$Ti_concentration) #  norm
  cor.test(dt_plants_PC_6$Fe_concentration, dt_plants_PC_6$Cu_concentration, method="pearson") # no significant 
  cor.test(dt_plants_PC_6$Cr_concentration, dt_plants_PC_6$Cu_concentration, method="pearson") # no significant 
  cor.test(dt_plants_PC_6$Fe_concentration, dt_plants_PC_6$Cr_concentration, method="pearson") # no significant 
  cor.test(dt_plants_PC_6$Ti_concentration, dt_plants_PC_6$Cu_concentration, method="pearson") # no significant 
  cor.test(dt_plants_PC_6$Ti_concentration, dt_plants_PC_6$Cr_concentration, method="pearson") # no significant 
  cor.test(dt_plants_PC_6$Ti_concentration, dt_plants_PC_6$Fe_concentration, method="pearson") # significant 
}


#Boechera perennans in plot 6 (n = 4)

{
  shapiro.test(dt_plants_BP_6$Cu_concentration) #  norm
  shapiro.test(dt_plants_BP_6$Cr_concentration) #  norm
  shapiro.test(dt_plants_BP_6$Fe_concentration) #  norm
  shapiro.test(dt_plants_BP_6$Ti_concentration) #  norm
  cor.test(dt_plants_BP_6$Fe_concentration, dt_plants_BP_6$Cu_concentration, method="pearson") # no significant 
  cor.test(dt_plants_BP_6$Cr_concentration, dt_plants_BP_6$Cu_concentration, method="pearson") # no significant 
  cor.test(dt_plants_BP_6$Fe_concentration, dt_plants_BP_6$Cr_concentration, method="pearson") # no significant 
  cor.test(dt_plants_BP_6$Ti_concentration, dt_plants_BP_6$Cu_concentration, method="pearson") # no significant 
  cor.test(dt_plants_BP_6$Ti_concentration, dt_plants_BP_6$Cr_concentration, method="pearson") # no significant 
  cor.test(dt_plants_BP_6$Ti_concentration, dt_plants_BP_6$Fe_concentration, method="pearson") # no significant 
}

# Nultuma Velutina (n=8)

{
  shapiro.test(dt_plants_NV$Cu_concentration) #  norm
  shapiro.test(dt_plants_NV$Cr_concentration) #  non norm
  shapiro.test(dt_plants_NV$Fe_concentration) #  norm
  shapiro.test(dt_plants_NV$Ti_concentration) #  norm
  cor.test(dt_plants_NV$Fe_concentration, dt_plants_NV$Cu_concentration, method="pearson") # no significant 
  cor.test(dt_plants_NV$Cr_concentration, dt_plants_NV$Cu_concentration, method="spearman") # no significant 
  cor.test(dt_plants_NV$Fe_concentration, dt_plants_NV$Cr_concentration, method="spearman") # no significant 
  cor.test(dt_plants_NV$Ti_concentration, dt_plants_NV$Cu_concentration, method="pearson") # no significant 
  cor.test(dt_plants_NV$Ti_concentration, dt_plants_NV$Cr_concentration, method="spearman") # no significant 
  cor.test(dt_plants_NV$Ti_concentration, dt_plants_NV$Fe_concentration, method="pearson") # no significant 
}


# Amaranthus Palmeri (n=10)

{
  shapiro.test(dt_plants_AP$Cu_concentration) #  non norm
  shapiro.test(dt_plants_AP$Cr_concentration) #  non norm
  shapiro.test(dt_plants_AP$Fe_concentration) #  norm
  shapiro.test(dt_plants_AP$Ti_concentration) #  norm
  cor.test(dt_plants_AP$Fe_concentration, dt_plants_AP$Cu_concentration, method="spearman") # significant 
  cor.test(dt_plants_AP$Cr_concentration, dt_plants_AP$Cu_concentration, method="spearman") # no significant 
  cor.test(dt_plants_AP$Fe_concentration, dt_plants_AP$Cr_concentration, method="spearman") # no significant 
  cor.test(dt_plants_AP$Ti_concentration, dt_plants_AP$Cu_concentration, method="spearman") #  significant 
  cor.test(dt_plants_AP$Ti_concentration, dt_plants_AP$Cr_concentration, method="spearman") # no significant 
  cor.test(dt_plants_AP$Ti_concentration, dt_plants_AP$Fe_concentration, method="pearson") #  significant 
}


# Senegalia Greggii (n=3)

{
  shapiro.test(dt_plants_SG$Cu_concentration) #  norm
  shapiro.test(dt_plants_SG$Cr_concentration) #  norm
  shapiro.test(dt_plants_SG$Fe_concentration) #  norm
  shapiro.test(dt_plants_SG$Ti_concentration) #  non norm
  cor.test(dt_plants_SG$Fe_concentration, dt_plants_SG$Cu_concentration, method="pearson") # significant 
  cor.test(dt_plants_SG$Cr_concentration, dt_plants_SG$Cu_concentration, method="pearson") # NA SD is 0
  cor.test(dt_plants_SG$Fe_concentration, dt_plants_SG$Cr_concentration, method="pearson") # NA SD is 0
  cor.test(dt_plants_SG$Ti_concentration, dt_plants_SG$Cu_concentration, method="spearman") #  no significant 
  cor.test(dt_plants_SG$Ti_concentration, dt_plants_SG$Cr_concentration, method="spearman") # NA SD is 0 
  cor.test(dt_plants_SG$Ti_concentration, dt_plants_SG$Fe_concentration, method="spearman") #  NA SD is 0
}


# Mimosa (n=6)

{
  shapiro.test(dt_plants_MB$Cu_concentration) # non norm
  shapiro.test(dt_plants_MB$Cr_concentration) # non norm
  shapiro.test(dt_plants_MB$Fe_concentration) # non norm
  shapiro.test(dt_plants_MB$Ti_concentration) #   norm
  cor.test(dt_plants_MB$Fe_concentration, dt_plants_MB$Cu_concentration, method="spearman") # no significant
  cor.test(dt_plants_MB$Cr_concentration, dt_plants_MB$Cu_concentration, method="spearman") # no significant
  cor.test(dt_plants_MB$Fe_concentration, dt_plants_MB$Cr_concentration, method="spearman") # no significant
  cor.test(dt_plants_MB$Ti_concentration, dt_plants_MB$Cu_concentration, method="spearman") # no significant 
  cor.test(dt_plants_MB$Ti_concentration, dt_plants_MB$Cr_concentration, method="spearman") # No significant 
  cor.test(dt_plants_MB$Ti_concentration, dt_plants_MB$Fe_concentration, method="spearman") # no significant
}



