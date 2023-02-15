# Data Visualization for Solitude Project
# Tomasz Wlodarczyk
# twlodarczyk@arizona.edu
# 2023-02-15

library(ggplot2)
library(dplyr)
library(data.table)
library(reshape2)
library(reshape)
library("readxl")
library(ggpubr)
library(agricolae)
library(ggplot2)
library(dplyr)
library(data.table)
library(reshape2)
library(reshape)

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data")
dt <-read.delim("Solitude_2.15.23.txt")

#replace ND with 0

tr <- matrix(data = NA, ncol = ncol(dt[,c(1:46)]), nrow=nrow(dt))
colnames(tr) <- colnames(dt[,c(1:46)])
for (i in 12:46)
{
  tr[,c(i)] <- gsub(".*ND.*", 0, dt[,i])
}

for(i in 1:11)
{
  tr[,c(i)] <- dt[,c(i)]
}
tr   

#transform to dataframe
tr <- as.data.frame.matrix(tr) #A correct command to change the dataset to dataframe after transformations
tr[,12:46] <- sapply(tr[,12:46],as.numeric) # Change a character to numeric (double)
typeof(tr$Cu_concentration) # confirm the value is no longer a character

#subset data to remove quality control samples

dt_plants <- subset(tr, Scientific_Name != 'QA_Sample')





Height.plot<- ggplot(dt_plants, aes(x = Scientific_Name, y = Cu_concentration, fill=Scientific_Name)) +
  geom_boxplot()+theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.x=element_blank())+
  theme(legend.position = "none")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  #scale_fill_manual(values = c("#38A6A5", "#73AF48", "#EDAD08", "#CC503E"))
Height.plot

typeof(dt_plants$Cu_concentration)


All_species<-ggplot(data=dt_plants, aes(x=Scientific_Name, y=Cu_concentration, fill = Scientific_Name)) +
  geom_boxplot()+
  scale_x_discrete(guide = guide_axis(angle = 45))
  #geom_jitter(color="black", size=0.4, alpha=0.9) +
  #theme(panel.grid.major.x = element_blank())+
  #scale_fill_brewer(palette="Dark2")
All_species
 
