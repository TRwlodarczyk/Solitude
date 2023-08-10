# Solitude Final worksheet for Soil
# Tomasz Wlodarczyk
# twlodarczyk@arizona.edu
# 2023-07-28

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
}

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final")
dt <-read.delim("Solitude_Complete_List_Soil_08.04.23_R.txt")

#remove NDs
{
  tr <- matrix(data = NA, ncol = ncol(dt[,c(1:75)]), nrow=nrow(dt)) # select all columns 1:46
  colnames(tr) <- colnames(dt[,c(1:75)])
  for (i in 12:75) # select when the concentrations start
  {
    tr[,c(i)] <- gsub(".*ND.*", 0, dt[,i])
  }
  
  for(i in c(1:11)) # select columns that need to stay the same 1:11 include character and double (weight)
  {
    tr[,c(i)] <- dt[,c(i)]
  }
  
  
  #transform to dataframe
  tr <- as.data.frame.matrix(tr) #A correct command to change the dataset to dataframe after transformations
  tr[,12:75] <- sapply(tr[,12:75],as.numeric) # Change a character to numeric (double)
  typeof(tr$Cu_pXRF) # confirm the value is no longer a character
  dt <- tr
}

dt_T <- subset(dt, Site == "TAILINGS")
dt_C <- subset(dt, Site == "CONTROL")

shapiro.test(dt_T$Cu_ICP) # > 0.05 =  normally distributed
shapiro.test(dt_C$Cu_ICP) # > 0.05 =  normally distributed
shapiro.test(dt$Cu_ICP)   # > 0.05 =  normally distributed

{
# Shapiro: Si, Ti, Cr, Mn, Ni, Cd, Sn, Control: Se, Ag, Sn, Sb
{
shapiro.test(dt_T$Be_ICP) # > 0.05 =  normally distributed
shapiro.test(dt_T$B_ICP) # > 0.05 =  normally distributed
shapiro.test(dt_T$Al_ICP) # > 0.05 =  normally distributed
shapiro.test(dt_T$Si_ICP) # < 0.05 =  NO normally distributed ---------
shapiro.test(dt_T$P_ICP) # > 0.05 =  normally distributed
shapiro.test(dt_T$S_ICP) # > 0.05 =  normally distributed
shapiro.test(dt_T$Ti_ICP) #< 0.05 =  NO normally distributed ------------
shapiro.test(dt_T$V_ICP) # > 0.05 =  normally distributed
shapiro.test(dt_T$Cr_ICP) # < 0.05 =  NO normally distributed ------------
shapiro.test(dt_T$Mn_ICP) # < 0.05 =  NO normally distributed -------------
shapiro.test(dt_T$Fe_ICP) # > 0.05 =  normally distributed
shapiro.test(dt_T$Co_ICP) # > 0.05 =  normally distributed
shapiro.test(dt_T$Ni_ICP) # < 0.05 =  NO normally distributed -------------
shapiro.test(dt_T$Zn_ICP) # > 0.05 =  normally distributed
shapiro.test(dt_T$Ge_ICP) # > 0.05 =  normally distributed
shapiro.test(dt_T$As_ICP) # > 0.05 =  normally distributed
shapiro.test(dt_T$Se_ICP) # > 0.05 =  normally distributed
shapiro.test(dt_T$Zr_ICP) # > 0.05 =  normally distributed
shapiro.test(dt_T$Nb_ICP) # > 0.05 =  normally distributed
shapiro.test(dt_T$Mo_ICP) # > 0.05 =  normally distributed
shapiro.test(dt_T$Ag_ICP) # > 0.05 =  normally distributed
shapiro.test(dt_T$Cd_ICP) # < 0.05 =  NO normally distributed -----------
shapiro.test(dt_T$Sn_ICP) # < 0.05 =  NO normally distributed -------------
shapiro.test(dt_T$Sb_ICP) # > 0.05 =  normally distributed
shapiro.test(dt_T$Ba_ICP) # > 0.05 =  normally distributed
shapiro.test(dt_T$W_ICP) # > 0.05 =  normally distributed
shapiro.test(dt_T$Pb_ICP) # > 0.05 =  normally distributed
# removed Ta and Re - they are all the same BDL

shapiro.test(dt_C$Be_ICP) # > 0.05 =  normally distributed
shapiro.test(dt_C$B_ICP) # > 0.05 =  normally distributed
shapiro.test(dt_C$Al_ICP) # > 0.05 =  normally distributed
shapiro.test(dt_C$Si_ICP) 
shapiro.test(dt_C$P_ICP) # > 0.05 =  normally distributed
shapiro.test(dt_C$S_ICP) # > 0.05 =  normally distributed
shapiro.test(dt_C$Ti_ICP) 
shapiro.test(dt_C$V_ICP) 
shapiro.test(dt_C$Cr_ICP) 
shapiro.test(dt_C$Mn_ICP) 
shapiro.test(dt_C$Fe_ICP) 
shapiro.test(dt_C$Co_ICP) 
shapiro.test(dt_C$Ni_ICP) 
shapiro.test(dt_C$Cu_ICP)
shapiro.test(dt_C$Zn_ICP) 
shapiro.test(dt_C$Ge_ICP) 
shapiro.test(dt_C$As_ICP) 
shapiro.test(dt_C$Se_ICP) #####################
shapiro.test(dt_C$Zr_ICP) # > 0.05 =  normally distributed
shapiro.test(dt_C$Nb_ICP) # > 0.05 =  normally distributed
shapiro.test(dt_C$Mo_ICP) # > 0.05 =  normally distributed
shapiro.test(dt_C$Ag_ICP) ####################
shapiro.test(dt_C$Cd_ICP) 
shapiro.test(dt_C$Sn_ICP) ###################
shapiro.test(dt_C$Sb_ICP) ###################
shapiro.test(dt_C$Ba_ICP) # > 0.05 =  normally distributed
shapiro.test(dt_C$W_ICP) # > 0.05 =  normally distributed
shapiro.test(dt_C$Pb_ICP) # > 0.05 =  normally distributed
# removed Ta and Re - they are all the same BDL
}

#DIFFERENCES BETWEEN PLOTS: Tailing: B, S, As, Control: Pb
{
  #TAILING
  {
# Perform one-way ANOVA Cu
anova_result <- aov(Cu_ICP ~ Plot, data = dt_T)
# Perform Tukey's HSD test for post-hoc comparisons
tukey_result <- TukeyHSD(anova_result)
print(summary(anova_result))
print(tukey_result)

# Perform pairwise Wilcoxon rank sum tests for post-hoc comparisons
wilcox_result <- pairwise.wilcox.test(dt_T$Cu_ICP, dt_T$Plot, p.adjust.method = "BH")
print(wilcox_result)

# Be
anova_result <- aov(Be_ICP ~ Plot, data = dt_T)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)
# B
anova_result <- aov(B_ICP ~ Plot, data = dt_T)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result) # P6-P1 significant
# Al
anova_result <- aov(Al_ICP ~ Plot, data = dt_T)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result) 
# Si
wilcox_result <- pairwise.wilcox.test(dt_T$Si_ICP, dt_T$Plot, p.adjust.method = "BH")
print(wilcox_result)
# P
anova_result <- aov(P_ICP ~ Plot, data = dt_T)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result) 
# S
anova_result <- aov(S_ICP ~ Plot, data = dt_T)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result) # P2-P1 and P6-P1 significant
# Ti
wilcox_result <- pairwise.wilcox.test(dt_T$Ti_ICP, dt_T$Plot, p.adjust.method = "BH")
print(wilcox_result)
# V
anova_result <- aov(V_ICP ~ Plot, data = dt_T)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result) 
# Cr
wilcox_result <- pairwise.wilcox.test(dt_T$Cr_ICP, dt_T$Plot, p.adjust.method = "BH")
print(wilcox_result) 
# Mn
wilcox_result <- pairwise.wilcox.test(dt_T$Mn_ICP, dt_T$Plot, p.adjust.method = "BH")
print(wilcox_result) 
# Fe
anova_result <- aov(Fe_ICP ~ Plot, data = dt_T)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)
# Co
anova_result <- aov(Co_ICP ~ Plot, data = dt_T)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)
# Ni
wilcox_result <- pairwise.wilcox.test(dt_T$Ni_ICP, dt_T$Plot, p.adjust.method = "BH")
print(wilcox_result)
# Cu
anova_result <- aov(Cu_ICP ~ Plot, data = dt_T)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)
# Zn
anova_result <- aov(Zn_ICP ~ Plot, data = dt_T)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)
# Ge
anova_result <- aov(Ge_ICP ~ Plot, data = dt_T)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)
# As
anova_result <- aov(As_ICP ~ Plot, data = dt_T)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result) # P2-P1 sig, P5-P1 sig P6-P1 sig
# Se
anova_result <- aov(Se_ICP ~ Plot, data = dt_T)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)
# Zr
anova_result <- aov(Zr_ICP ~ Plot, data = dt_T)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)
# Nb
anova_result <- aov(Nb_ICP ~ Plot, data = dt_T)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)
# Mo
anova_result <- aov(Mo_ICP ~ Plot, data = dt_T)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)
# Ag
anova_result <- aov(Ag_ICP ~ Plot, data = dt_T)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)
# Cd
wilcox_result <- pairwise.wilcox.test(dt_T$Cd_ICP, dt_T$Plot, p.adjust.method = "BH")
print(wilcox_result)
# Sn
wilcox_result <- pairwise.wilcox.test(dt_T$Sn_ICP, dt_T$Plot, p.adjust.method = "BH")
print(wilcox_result)
# Sb
anova_result <- aov(Sb_ICP ~ Plot, data = dt_T)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)
# Ba
anova_result <- aov(Ba_ICP ~ Plot, data = dt_T)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)
# Ag
anova_result <- aov(Ag_ICP ~ Plot, data = dt_T)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)
# W
anova_result <- aov(W_ICP ~ Plot, data = dt_T)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)
# Pb
anova_result <- aov(Pb_ICP ~ Plot, data = dt_T)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)
  }
  #CONTROL
  {
  # Be
  anova_result <- aov(Be_ICP ~ Plot, data = dt_C)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  # B
  anova_result <- aov(B_ICP ~ Plot, data = dt_C)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result) 
  # Al
  anova_result <- aov(Al_ICP ~ Plot, data = dt_C)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result) 
  # Si
  anova_result <- aov(Si_ICP ~ Plot, data = dt_C)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  # P
  anova_result <- aov(P_ICP ~ Plot, data = dt_C)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result) 
  # S
  anova_result <- aov(S_ICP ~ Plot, data = dt_C)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result) # P2-P1 and P6-P1 significant
  # Ti
  anova_result <- aov(Ti_ICP ~ Plot, data = dt_C)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  # V
  anova_result <- aov(V_ICP ~ Plot, data = dt_C)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result) 
  # Cr
  anova_result <- aov(Cr_ICP ~ Plot, data = dt_C)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  # Mn
  anova_result <- aov(Mn_ICP ~ Plot, data = dt_C)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  # Fe
  anova_result <- aov(Fe_ICP ~ Plot, data = dt_C)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  # Co
  anova_result <- aov(Co_ICP ~ Plot, data = dt_C)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  # Ni
  anova_result <- aov(Ni_ICP ~ Plot, data = dt_C)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  # Cu
  anova_result <- aov(Cu_ICP ~ Plot, data = dt_C)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  # Zn
  anova_result <- aov(Zn_ICP ~ Plot, data = dt_C)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  # Ge
  anova_result <- aov(Ge_ICP ~ Plot, data = dt_C)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  # As
  anova_result <- aov(As_ICP ~ Plot, data = dt_C)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result) # P2-P1 sig, P5-P1 sig P6-P1 sig
  # Se
  wilcox_result <- pairwise.wilcox.test(dt_C$Se_ICP, dt_C$Plot, p.adjust.method = "BH")
  print(wilcox_result)
  # Zr
  anova_result <- aov(Zr_ICP ~ Plot, data = dt_C)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  # Nb
  anova_result <- aov(Nb_ICP ~ Plot, data = dt_C)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  # Mo
  anova_result <- aov(Mo_ICP ~ Plot, data = dt_C)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  # Ag
  anova_result <- aov(Ag_ICP ~ Plot, data = dt_C)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  # Cd
  anova_result <- aov(Cd_ICP ~ Plot, data = dt_C)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  # Sn
  wilcox_result <- pairwise.wilcox.test(dt_C$Sn_ICP, dt_C$Plot, p.adjust.method = "BH")
  print(wilcox_result)
  # Sb
  wilcox_result <- pairwise.wilcox.test(dt_C$Sb_ICP, dt_C$Plot, p.adjust.method = "BH")
  print(wilcox_result)
  # Ba
  anova_result <- aov(Ba_ICP ~ Plot, data = dt_C)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  # Ag
  wilcox_result <- pairwise.wilcox.test(dt_C$Ag_ICP, dt_C$Plot, p.adjust.method = "BH")
  print(wilcox_result)
  # W
  anova_result <- aov(W_ICP ~ Plot, data = dt_C)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  # Pb
  anova_result <- aov(Pb_ICP ~ Plot, data = dt_C)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result) # C2-C1 sig

  }
}

#Differences between TAILINGS and CONTROL 
#Shapiro
{
shapiro.test(dt$Be_ICP) 
shapiro.test(dt$B_ICP) 
shapiro.test(dt$Al_ICP) # sig
shapiro.test(dt$Si_ICP) # sig
shapiro.test(dt$P_ICP)  #sig
shapiro.test(dt$S_ICP)  # sig
shapiro.test(dt$Ti_ICP) #sig
shapiro.test(dt$V_ICP)  #sig
shapiro.test(dt$Cr_ICP) #sig
shapiro.test(dt$Mn_ICP) #sig
shapiro.test(dt$Fe_ICP) 
shapiro.test(dt$Co_ICP) 
shapiro.test(dt$Ni_ICP) #sig
shapiro.test(dt$Cu_ICP)
shapiro.test(dt$Zn_ICP) #sig
shapiro.test(dt$Ge_ICP) 
shapiro.test(dt$As_ICP) #sig
shapiro.test(dt$Se_ICP) 
shapiro.test(dt$Zr_ICP) 
shapiro.test(dt$Nb_ICP) #sig
shapiro.test(dt$Mo_ICP)  
shapiro.test(dt$Ag_ICP) 
shapiro.test(dt$Cd_ICP) #sig
shapiro.test(dt$Sn_ICP) #sig
shapiro.test(dt$Sb_ICP) 
shapiro.test(dt$Ba_ICP) 
shapiro.test(dt$W_ICP)  #sig
shapiro.test(dt$Pb_ICP) #sig
}

{
  # Be---
  anova_result <- aov(Be_ICP ~ Plot, data = dt)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  # B---
  anova_result <- aov(B_ICP ~ Plot, data = dt)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result) 
  # Al 
  wilcox_result <- pairwise.wilcox.test(dt$Al_ICP, dt$Plot, p.adjust.method = "BH")
  print(wilcox_result) 
  # Si
  wilcox_result <- pairwise.wilcox.test(dt$Si_ICP, dt$Plot, p.adjust.method = "BH")
  print(wilcox_result)
  # P
  wilcox_result <- pairwise.wilcox.test(dt$P_ICP, dt$Plot, p.adjust.method = "BH")
  print(wilcox_result)
  # S
  wilcox_result <- pairwise.wilcox.test(dt$S_ICP, dt$Plot, p.adjust.method = "BH")
  print(wilcox_result)
  # Ti
  wilcox_result <- pairwise.wilcox.test(dt$Ti_ICP, dt$Plot, p.adjust.method = "BH")
  print(wilcox_result)
  # V
  wilcox_result <- pairwise.wilcox.test(dt$V_ICP, dt$Plot, p.adjust.method = "BH")
  print(wilcox_result)
  # Cr
  wilcox_result <- pairwise.wilcox.test(dt$Cr_ICP, dt$Plot, p.adjust.method = "BH")
  print(wilcox_result)
  # Mn
  wilcox_result <- pairwise.wilcox.test(dt$Mn_ICP, dt$Plot, p.adjust.method = "BH")
  print(wilcox_result)
  # Fe---
  anova_result <- aov(Fe_ICP ~ Plot, data = dt)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  # Co---
  anova_result <- aov(Co_ICP ~ Plot, data = dt)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  # Ni
  wilcox_result <- pairwise.wilcox.test(dt$Ni_ICP, dt$Plot, p.adjust.method = "BH")
  print(wilcox_result)
  # Cu---
  anova_result <- aov(Cu_ICP ~ Plot, data = dt)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  # Zn
  wilcox_result <- pairwise.wilcox.test(dt$Zn_ICP, dt$Plot, p.adjust.method = "BH")
  print(wilcox_result)
  # Ge---
  anova_result <- aov(Ge_ICP ~ Plot, data = dt)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  # As
  wilcox_result <- pairwise.wilcox.test(dt$As_ICP, dt$Plot, p.adjust.method = "BH")
  print(wilcox_result)
  # Se---
  anova_result <- aov(Se_ICP ~ Plot, data = dt)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  # Zr---
  anova_result <- aov(Zr_ICP ~ Plot, data = dt)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  # Nb
  wilcox_result <- pairwise.wilcox.test(dt$Nb_ICP, dt$Plot, p.adjust.method = "BH")
  print(wilcox_result)
  # Mo---
  anova_result <- aov(Mo_ICP ~ Plot, data = dt)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  # Ag---
  anova_result <- aov(Ag_ICP ~ Plot, data = dt)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  # Cd
  wilcox_result <- pairwise.wilcox.test(dt$Cd_ICP, dt$Plot, p.adjust.method = "BH")
  print(wilcox_result)
  # Sn
  wilcox_result <- pairwise.wilcox.test(dt$Sn_ICP, dt$Plot, p.adjust.method = "BH")
  print(wilcox_result)
  # Sb---
  anova_result <- aov(Sb_ICP ~ Plot, data = dt)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  # Ba---
  anova_result <- aov(Ba_ICP ~ Plot, data = dt)
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  # Ag
  wilcox_result <- pairwise.wilcox.test(dt$Ag_ICP, dt$Plot, p.adjust.method = "BH")
  print(wilcox_result)
  # W
  wilcox_result <- pairwise.wilcox.test(dt$W_ICP, dt$Plot, p.adjust.method = "BH")
  print(wilcox_result)
  # Pb
  wilcox_result <- pairwise.wilcox.test(dt$Pb_ICP, dt$Plot, p.adjust.method = "BH")
  print(wilcox_result)
  
  
}
}

# Zrob Kruskala dla wszystkich i liste wciagnij do tabelki csv
library(agricolae)
Cu <- kruskal.test(Cu_ICP ~ Plot, data = dt)
Cu1<-kruskal(dt$Cu_ICP, dt$Plot, group=TRUE,p.adj="bonferroni")

#46 - 75

library(agricolae)

dt1 <- dt[c(-72,-74, -42, -40, -38, -36)]

#p_adj_table <- data.frame(Test = character(), p_adj = numeric(), stringsAsFactors = FALSE)
#wilcox_result <- pairwise.wilcox.test(dt1$Si_ICP, dt1$Plot, p.adjust.method = "BH")
#p_adj_table <- rbind(p_adj_table, data.frame(Test = "Si", p_adj = wilcox_result$p.value))


#"Be_ICP", "B_ICP", "Al_ICP", "Si_ICP", "P_ICP", "S_ICP", "Ti_ICP", "V_ICP", 
#"Cr_ICP", "Mn_ICP", "Fe_ICP", "Co_ICP", "Ni_ICP", "Zn_ICP", "Ge_ICP", 
#"As_ICP", "Se_ICP", "Zr_ICP", "Nb_ICP", "Mo_ICP", "Ag_ICP", "Cd_ICP", 
#"Sn_ICP", "Sb_ICP", "Ba_ICP", "W_ICP", "Pb_ICP")


#Kruskal for all soils
{
library(agricolae)
print(kruskal(dt$Be_ICP, dt$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$B_ICP, dt$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Al_ICP, dt$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Si_ICP, dt$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$P_ICP, dt$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$S_ICP, dt$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Ti_ICP, dt$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$V_ICP, dt$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Cr_ICP, dt$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Mn_ICP, dt$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Fe_ICP, dt$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Co_ICP, dt$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Ni_ICP, dt$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Zn_ICP, dt$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Cu_ICP, dt$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Ge_ICP, dt$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$As_ICP, dt$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Se_ICP, dt$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Zr_ICP, dt$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Nb_ICP, dt$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Mo_ICP, dt$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Ag_ICP, dt$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Cd_ICP, dt$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Sn_ICP, dt$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Sb_ICP, dt$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Ba_ICP, dt$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$W_ICP, dt$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Pb_ICP, dt$Plot, group=TRUE,p.adj="bonferroni"))


print(kruskal(dt$Be_ICP, dt$Site, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$B_ICP, dt$Site, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Al_ICP, dt$Site, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Si_ICP, dt$Site, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$P_ICP, dt$Site, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$S_ICP, dt$Site, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Ti_ICP, dt$Site, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$V_ICP, dt$Site, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Cr_ICP, dt$Site, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Mn_ICP, dt$Site, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Fe_ICP, dt$Site, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Co_ICP, dt$Site, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Ni_ICP, dt$Site, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Zn_ICP, dt$Site, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Cu_ICP, dt$Site, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Ge_ICP, dt$Site, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$As_ICP, dt$Site, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Se_ICP, dt$Site, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Zr_ICP, dt$Site, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Nb_ICP, dt$Site, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Mo_ICP, dt$Site, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Ag_ICP, dt$Site, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Cd_ICP, dt$Site, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Sn_ICP, dt$Site, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Sb_ICP, dt$Site, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Ba_ICP, dt$Site, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$W_ICP, dt$Site, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt$Pb_ICP, dt$Site, group=TRUE,p.adj="bonferroni"))
}




dt_soil_summary <- dt1 %>%
  filter(Plot %in% c("P1", "P2", "P5", "P6", "C1", "C2")) %>%
  pivot_longer(cols = 42:69, names_to = "Concentration", values_to = "Value") %>%
  group_by(Concentration, Plot) %>%
  summarise(min = min(Value), 
            max = max(Value), 
            mean = mean(Value), 
            median = median(Value), 
            sd = sd(Value), 
            .groups = "drop") %>%
  pivot_wider(names_from = Plot, values_from = c(min, max, mean, median, sd), names_glue = "{Plot}_{.value}") %>%
  select(Concentration, contains("_")) %>%
  pivot_longer(cols = -Concentration, names_to = "Plot_Stat", values_to = "Value") %>%
  separate(Plot_Stat, into = c("Plot", "Stat"), sep = "_") %>%
  pivot_wider(names_from = Plot, values_from = Value) %>%
  arrange(Concentration)


# With SE instead of SD
library(dplyr)
library(tidyr)

dt_soil_summary1 <- dt1 %>%
  filter(Plot %in% c("P1", "P2", "P5", "P6", "C1", "C2")) %>%
  pivot_longer(cols = 42:69, names_to = "Concentration", values_to = "Value") %>%
  group_by(Concentration, Plot) %>%
  summarise(min = min(Value), 
            max = max(Value), 
            mean = mean(Value), 
            median = median(Value), 
            se = sd(Value) / sqrt(n()),  # Calculate standard error
            .groups = "drop") %>%
  pivot_wider(names_from = Plot, values_from = c(min, max, mean, median, se), names_glue = "{Plot}_{.value}") %>%
  select(Concentration, contains("_")) %>%
  pivot_longer(cols = -Concentration, names_to = "Plot_Stat", values_to = "Value") %>%
  separate(Plot_Stat, into = c("Plot", "Stat"), sep = "_") %>%
  pivot_wider(names_from = Plot, values_from = Value) %>%
  arrange(Concentration)


dt_soil_summary2 <- dt_soil_summary1 %>%
  mutate(across(where(is.numeric), 
                ~ ifelse(. > 10, round(., 0), round(., 1))))

#write.csv(dt_soil_summary2, "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Soil_Table.csv", row.names=FALSE)

dt_soil_summary_Re <- dt %>%
  filter(Plot %in% c("P1", "P2", "P5", "P6", "C1", "C2")) %>%
  pivot_longer(cols = 46:75, names_to = "Concentration", values_to = "Value") %>%
  group_by(Concentration, Plot) %>%
  summarise(min = min(Value), 
            max = max(Value), 
            mean = mean(Value), 
            median = median(Value), 
            se = sd(Value) / sqrt(n()),  # Calculate standard error
            .groups = "drop") %>%
  pivot_wider(names_from = Plot, values_from = c(min, max, mean, median, se), names_glue = "{Plot}_{.value}") %>%
  select(Concentration, contains("_")) %>%
  pivot_longer(cols = -Concentration, names_to = "Plot_Stat", values_to = "Value") %>%
  separate(Plot_Stat, into = c("Plot", "Stat"), sep = "_") %>%
  pivot_wider(names_from = Plot, values_from = Value) %>%
  arrange(Concentration)


dt_soil_summary3 <- dt_soil_summary_Re %>%
  mutate(across(where(is.numeric), 
                ~ ifelse(. > 10, round(., 0), round(., 1))))

write.csv(dt_soil_summary3, "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Soil_Table_Re.csv", row.names=FALSE)