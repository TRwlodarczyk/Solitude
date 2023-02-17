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





Cu_All<- ggplot(dt_plants, aes(x = Scientific_Name, y = Cu_concentration, group=Scientific_Name)) +
  geom_boxplot()+theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.x=element_blank())+
  theme(legend.position = "none")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  geom_jitter(color="red", size=0.6, alpha=0.9) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  #scale_fill_manual(values = c("#38A6A5", "#73AF48", "#EDAD08", "#CC503E"))
Cu_All

#All with size of the points for plots

Cu_All<- ggplot(dt_plants, aes(x = Scientific_Name, y = Cu_concentration, fill=Scientific_Name)) +
  geom_boxplot()+theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.x=element_blank())+
  theme(legend.position = "none")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#scale_fill_manual(values = c("#38A6A5", "#73AF48", "#EDAD08", "#CC503E"))
Cu_All



P1 <- subset(dt_plants, Plot=='P1')
P2 <- subset(dt_plants, Plot=='P2')
P5 <- subset(dt_plants, Plot=='P5')
P6 <- subset(dt_plants, Plot=='P6')

Cu_All_P1<- ggplot(P1, aes(x = Scientific_Name, y = Cu_concentration, fill=Scientific_Name)) +
  geom_boxplot()+theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.x=element_blank())+
  theme(legend.position = "none")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#scale_fill_manual(values = c("#38A6A5", "#73AF48", "#EDAD08", "#CC503E"))
Cu_All_P1

Cu_All_P2<- ggplot(P2, aes(x = Scientific_Name, y = Cu_concentration, fill=Scientific_Name)) +
  geom_boxplot()+theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.x=element_blank())+
  theme(legend.position = "none")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#scale_fill_manual(values = c("#38A6A5", "#73AF48", "#EDAD08", "#CC503E"))
Cu_All_P2

Cu_All_P5<- ggplot(P5, aes(x = Scientific_Name, y = Cu_concentration, fill=Scientific_Name)) +
  geom_boxplot()+theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.x=element_blank())+
  theme(legend.position = "none")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#scale_fill_manual(values = c("#38A6A5", "#73AF48", "#EDAD08", "#CC503E"))
Cu_All_P5


Cu_All_P6<- ggplot(P6, aes(x = Scientific_Name, y = Cu_concentration, fill=Scientific_Name)) +
  geom_boxplot()+theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.x=element_blank())+
  theme(legend.position = "none")+
  geom_jitter(color="red", size=3, alpha=0.9) +
  scale_x_discrete(guide = guide_axis(angle = 45))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#scale_fill_manual(values = c("#38A6A5", "#73AF48", "#EDAD08", "#CC503E"))
Cu_All_P6

Cu_Arranged <- ggarrange(Cu_All_P1, Cu_All_P2, Cu_All_P5, Cu_All_P6, nrow=2, ncol=2, common.legend = TRUE)
Cu_Arranged

#
 
plot(density(dt_plants$Cu_concentration))
plot(density(dt_plants$Zn_concentration))
plot(density(dt_plants$Mn_concentration))
plot(density(dt_plants$Se_concentration))
plot(density(dt_plants$Ca_concentration))
plot(density(dt_plants$Cl_concentration))

plot(density(P1$Cu_concentration))
shapiro.test(P1$Cu_concentration) # < 0.05 = non normally distributed
plot(density(P1$Zn_concentration))
shapiro.test(P1$Zn_concentration) # < 0.05 = non normally distributed
plot(density(P1$Mn_concentration))
shapiro.test(P1$Mn_concentration) # < 0.05 = non normally distributed
plot(density(P2$Cu_concentration))
shapiro.test(P2$Cu_concentration) # < 0.05 = non normally distributed
plot(density(P2$Zn_concentration))
shapiro.test(P2$Zn_concentration) # < 0.05 = non normally distributed
plot(density(P2$Mn_concentration))
shapiro.test(P2$Mn_concentration) # < 0.05 = non normally distributed
plot(density(P5$Cu_concentration))
shapiro.test(P5$Cu_concentration) # < 0.05 = non normally distributed
plot(density(P5$Zn_concentration))
shapiro.test(P5$Zn_concentration) # < 0.05 = non normally distributed
plot(density(P5$Mn_concentration))
shapiro.test(P5$Mn_concentration) # < 0.05 = non normally distributed
plot(density(P6$Cu_concentration))
shapiro.test(P6$Cu_concentration) # < 0.05 = non normally distributed
plot(density(P6$Zn_concentration))
shapiro.test(P6$Zn_concentration) # < 0.05 = non normally distributed
plot(density(P6$Mn_concentration))
shapiro.test(P6$Mn_concentration) # < 0.05 = non normally distributed

# I need to drop unvertainity columns first

dt_plants_nounc = select(dt_plants, -Cl_uncertainty,-Ca_uncertainty, -Ti_uncertainty,
                         -Cr_uncertainty, -Mn_uncertainty, -Fe_uncertainty, -Co_uncertainty, -Ni_uncertainty, -Cu_uncertainty,
                         -Zn_uncertainty, -As_uncertainty, -Se_uncertainty, -Cd_uncertainty, -Re_uncertainty, -Hg_uncertainty,
                         -Tl_uncertainty, -Pb_uncertainty, -Substrate_RT)

dt_plants_nounc = select(dt_plants_nounc, -Co_concentration, -Hg_concentration, -Tl_concentration, -Pb_concentration, -Ni_concentration)
dt_plants_nounc = select(dt_plants_nounc, -Ni_concentration)

#By Plot

dt_plants_nounc1 <- subset(dt_plants_nounc, Plot=="P1")
dt_plants_nounc2 <- subset(dt_plants_nounc, Plot=="P2")
dt_plants_nounc5 <- subset(dt_plants_nounc, Plot=="P5")
dt_plants_nounc6 <- subset(dt_plants_nounc, Plot=="P6")
dt_plants_nounce15 <- subset(dt_plants_nounc, Plot=="P1" | Plot=="P5")

#PCA

myPr1 <- prcomp(dt_plants_nounc1[,12:23], scale=TRUE)
myPr2 <- prcomp(dt_plants_nounc2[,12:23], scale=TRUE)
myPr5 <- prcomp(dt_plants_nounc5[,12:23], scale=TRUE)
myPr6 <- prcomp(dt_plants_nounc6[,12:23], scale=TRUE)
myPr15 <- prcomp(dt_plants_nounce15[,12:23], scale=TRUE)


#myPr2 <- prcomp(~Cu_concentration + Zn_concentration, Mn_concentration, data = dt_plants)

summary(myPr1) # SD - variability across single principle component, 
              #Proportion of variance - e.g., 23 % of data is explained by principle component 1, 18% of data is explained by principle component 2 etc.
              #Cumulative Proportion - PC1 explains 23 % of the variability of data, PC1 + PC2 explains 41%, PC1+PC2+PC3 explains 52% etc
              #We can represent cumultative proportion by l plot
plot(myPr1, type="l") #shows variances across principle components (square of a standard deviation)

#To interpret PC we use function biplot

biplot(myPr1, scale=0)

#Extract PC scores 
str(myPr1)

myPr1$x 
# Bind dataframes with PC1 and PC2 for each plot




dt_plants1 <- cbind(dt_plants_nounc1, myPr1$x[,1:2])
dt_plants2 <- cbind(dt_plants_nounc2, myPr2$x[,1:2])
dt_plants5 <- cbind(dt_plants_nounc5, myPr5$x[,1:2])
dt_plants6 <- cbind(dt_plants_nounc6, myPr6$x[,1:2])
dt_plants15 <- cbind(dt_plants_nounce15, myPr15$x[,1:2])

head(dt_plants1)

#Plot with ggplot

ggplot(dt_plants1, aes(PC1, PC2, col=Scientific_Name, fill=Scientific_Name))+
  stat_ellipse(geom="polygon", col="black", alpha=0.5)+
  geom_point(shape=21, col="black")

ggplot(dt_plants2, aes(PC1, PC2, col=Scientific_Name, fill=Scientific_Name))+
  stat_ellipse(geom="polygon", col="black", alpha=0.5)+
  geom_point(shape=21, col="black")
ggplot(dt_plants5, aes(PC1, PC2, col=Scientific_Name, fill=Scientific_Name))+
  stat_ellipse(geom="polygon", col="black", alpha=0.5)+
  geom_point(shape=21, col="black")
ggplot(dt_plants6, aes(PC1, PC2, col=Scientific_Name, fill=Scientific_Name))+
  stat_ellipse(geom="polygon", col="black", alpha=0.5)+
  geom_point(shape=21, col="black")

# Plot for all plot
myPr_all <- prcomp(dt_plants_nounc[,12:23], scale=TRUE)
dt_plants_all <- cbind(dt_plants_nounc, myPr_all$x[,1:2])

ggplot(dt_plants_all, aes(PC1, PC2, col=Plot, fill=Plot))+
  stat_ellipse(geom="polygon", col="black", alpha=0.5)+
  geom_point(shape=21, col="black")

# Plot by species for plot 1 and 5
ggplot(dt_plants15, aes(PC1, PC2, col=Scientific_Name, fill=Scientific_Name))+
  stat_ellipse(geom="polygon", col="black", alpha=0.5)+
  geom_point(shape=21, col="black")

biplot(myPr15, scale=0)


#Correlations between variables and principal components 
cor(dt_plants_nounce15[,12:23], dt_plants15[,24:25])

