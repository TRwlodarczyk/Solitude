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


# Kruskal Forb Tree Shrub 

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final")
dt <- read.delim("Solitude_For_Fig.txt")
dt <- dt[dt$Type_of_Sample != "root", ]
dt <- dt[dt$Type_of_Sample != "stem", ]
#dt <- dt[dt$Site != "CONTROL", ]

P1 <- subset(dt, Plot=="P1")
P2 <- subset(dt, Plot=="P2")
P5 <- subset(dt, Plot=="P5")
P6 <- subset(dt, Plot=="P6")
C <- subset(dt, Plot=="C")
AVG <- subset(dt, Site=="TAILINGS")


library(agricolae)
CuP1<-kruskal(P1$Predicted_Cu_ICP, P1$Form, group=TRUE,p.adj="bonferroni")
CuP1

CuP2<-kruskal(P2$Predicted_Cu_ICP, P2$Form, group=TRUE,p.adj="bonferroni")
CuP2

CuP5<-kruskal(P5$Predicted_Cu_ICP, P5$Form, group=TRUE,p.adj="bonferroni")
CuP5

CuP6<-kruskal(P6$Predicted_Cu_ICP, P6$Form, group=TRUE,p.adj="bonferroni")
CuP6

CuAVG<-kruskal(AVG$Predicted_Cu_ICP, AVG$Form, group=TRUE,p.adj="bonferroni")
CuAVG

uC<-kruskal(C$Predicted_Cu_ICP, C$Form, group=TRUE,p.adj="bonferroni")
CuC


#Se
SeP1<-kruskal(P1$Predicted_Se_ICP, P1$Form, group=TRUE,p.adj="bonferroni")
SeP1

SeP2<-kruskal(P2$Predicted_Se_ICP, P2$Form, group=TRUE,p.adj="bonferroni")
SeP2

SeP5<-kruskal(P5$Predicted_Se_ICP, P5$Form, group=TRUE,p.adj="bonferroni")
SeP5

SeP6<-kruskal(P6$Predicted_Se_ICP, P6$Form, group=TRUE,p.adj="bonferroni")
SeP6

SeAVG<-kruskal(AVG$Predicted_Se_ICP, AVG$Form, group=TRUE,p.adj="bonferroni")
SeAVG

SeC<-kruskal(C$Predicted_Se_ICP, C$Form, group=TRUE,p.adj="bonferroni")
SeC


#Re
ReP1<-kruskal(P1$Predicted_Re_ICP, P1$Form, group=TRUE,p.adj="bonferroni")
ReP1

ReP2<-kruskal(P2$Predicted_Re_ICP, P2$Form, group=TRUE,p.adj="bonferroni")
ReP2

ReP5<-kruskal(P5$Predicted_Re_ICP, P5$Form, group=TRUE,p.adj="bonferroni")
ReP5

ReP6<-kruskal(P6$Predicted_Re_ICP, P6$Form, group=TRUE,p.adj="bonferroni")
ReP6

ReAVG<-kruskal(AVG$Predicted_Re_ICP, AVG$Form, group=TRUE,p.adj="bonferroni")
ReAVG

ReC<-kruskal(C$Predicted_Re_ICP, C$Form, group=TRUE,p.adj="bonferroni")
ReC


#Zn
ZnP1<-kruskal(P1$Predicted_Zn_ICP, P1$Form, group=TRUE,p.adj="bonferroni")
ZnP1

ZnP2<-kruskal(P2$Predicted_Zn_ICP, P2$Form, group=TRUE,p.adj="bonferroni")
ZnP2

ZnP5<-kruskal(P5$Predicted_Zn_ICP, P5$Form, group=TRUE,p.adj="bonferroni")
ReP5

ZnP6<-kruskal(P6$Predicted_Zn_ICP, P6$Form, group=TRUE,p.adj="bonferroni")
ZnP6

ZnAVG<-kruskal(AVG$Predicted_Zn_ICP, AVG$Form, group=TRUE,p.adj="bonferroni")
ZnAVG

ZnC<-kruskal(C$Predicted_Zn_ICP, C$Form, group=TRUE,p.adj="bonferroni")
ZnC




#Mn
MnP1<-kruskal(P1$Predicted_Mn_ICP, P1$Form, group=TRUE,p.adj="bonferroni")
MnP1

MnP2<-kruskal(P2$Predicted_Mn_ICP, P2$Form, group=TRUE,p.adj="bonferroni")
MnP2

MnP5<-kruskal(P5$Predicted_Mn_ICP, P5$Form, group=TRUE,p.adj="bonferroni")
ReP5

MnP6<-kruskal(P6$Predicted_Mn_ICP, P6$Form, group=TRUE,p.adj="bonferroni")
MnP6

MnAVG<-kruskal(AVG$Predicted_Mn_ICP, AVG$Form, group=TRUE,p.adj="bonferroni")
MnAVG

MnC<-kruskal(C$Predicted_Mn_ICP, C$Form, group=TRUE,p.adj="bonferroni")
MnC


#Mn
FeP1<-kruskal(P1$Predicted_Fe_ICP, P1$Form, group=TRUE,p.adj="bonferroni")
FeP1

FeP2<-kruskal(P2$Predicted_Fe_ICP, P2$Form, group=TRUE,p.adj="bonferroni")
FeP2

FeP5<-kruskal(P5$Predicted_Fe_ICP, P5$Form, group=TRUE,p.adj="bonferroni")
FeP5

FeP6<-kruskal(P6$Predicted_Fe_ICP, P6$Form, group=TRUE,p.adj="bonferroni")
FeP6

FeAVG<-kruskal(AVG$Predicted_Fe_ICP, AVG$Form, group=TRUE,p.adj="bonferroni")
FeAVG

FeC<-kruskal(C$Predicted_Fe_ICP, C$Form, group=TRUE,p.adj="bonferroni")
FeC


# Between plots for each Form

Forb <- subset(dt, Form=="Forb")
Grass <- subset(dt, Form=="Grass")
Tree <- subset(dt, Form=="Tree")
Shrub <- subset(dt, Form=="Shrub")



CuForb<-kruskal(Forb$Predicted_Cu_ICP, Forb$Plot, group=TRUE,p.adj="bonferroni")
CuForb

CuGrass<-kruskal(Grass$Predicted_Cu_ICP, Grass$Plot, group=TRUE,p.adj="bonferroni")
CuGrass

CuShrub<-kruskal(Shrub$Predicted_Cu_ICP, Shrub$Plot, group=TRUE,p.adj="bonferroni")
CuShrub

CuTree<-kruskal(Tree$Predicted_Cu_ICP, Tree$Plot, group=TRUE,p.adj="bonferroni")
CuTree

SeForb<-kruskal(Forb$Predicted_Se_ICP, Forb$Plot, group=TRUE,p.adj="bonferroni")
SeForb

SeGrass<-kruskal(Grass$Predicted_Se_ICP, Grass$Plot, group=TRUE,p.adj="bonferroni")
SeGrass

SeShrub<-kruskal(Shrub$Predicted_Se_ICP, Shrub$Plot, group=TRUE,p.adj="bonferroni")
SeShrub

SeTree<-kruskal(Tree$Predicted_Se_ICP, Tree$Plot, group=TRUE,p.adj="bonferroni")
SeTree

