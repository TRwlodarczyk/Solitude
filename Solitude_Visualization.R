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


###
P1 <- subset(dt_plants, Plot=='P1')
P2 <- subset(dt_plants, Plot=='P2')
P5 <- subset(dt_plants, Plot=='P5')
P6 <- subset(dt_plants, Plot=='P6')
P125 <- subset(dt_plants, Plot!='P6')
P125


Cu_All<- ggplot(dt_plants, aes(x = Scientific_Name, y = Cu_concentration, group=Scientific_Name)) +
  geom_boxplot()+theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.x=element_blank())+
  #theme(legend.position = "none")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  geom_jitter(aes(colour = Plot)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  #scale_fill_manual(values = c("#38A6A5", "#73AF48", "#EDAD08", "#CC503E"))
Cu_All


#plot 1 2 5


Cu_All<- ggplot(dt_plants, aes(x = Scientific_Name, y = Cu_concentration, group=Scientific_Name)) +
  geom_boxplot()+theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.x=element_blank())+
  #theme(legend.position = "none")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  geom_jitter(aes(colour = Plot)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#scale_fill_manual(values = c("#38A6A5", "#73AF48", "#EDAD08", "#CC503E"))
Cu_All

########All with color of the points for plots####

Cu_125<- ggplot(P125, aes(x = reorder(Scientific_Name, Cu_concentration, FUN = median), y = Cu_concentration, group=Scientific_Name)) +
  geom_boxplot()+
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.x=element_blank())+
  #theme(legend.position = "none")+
  scale_x_discrete(guide = guide_axis(angle = 0))+
  geom_jitter(aes(colour = Plot)) +
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#scale_fill_manual(values = c("#38A6A5", "#73AF48", "#EDAD08", "#CC503E"))
Cu_125


Zn_125<- ggplot(P125, aes(x = reorder(Scientific_Name, Zn_concentration, FUN = median), y = Zn_concentration, group=Scientific_Name)) +
  geom_boxplot()+theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.x=element_blank())+
  #theme(legend.position = "none")+
  scale_x_discrete(guide = guide_axis(angle = 0))+
  geom_jitter(aes(colour = Plot)) +
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#scale_fill_manual(values = c("#38A6A5", "#73AF48", "#EDAD08", "#CC503E"))
Zn_125

Se_125<- ggplot(P125, aes(x = reorder(Scientific_Name, Se_concentration, FUN = median), y = Se_concentration, group=Scientific_Name)) +
  geom_boxplot()+theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.x=element_blank())+
  #theme(legend.position = "none")+
  scale_x_discrete(guide = guide_axis(angle = 0))+
  geom_jitter(aes(colour = Plot)) +
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#scale_fill_manual(values = c("#38A6A5", "#73AF48", "#EDAD08", "#CC503E"))
Se_125


Re_125<- ggplot(P125, aes(x = reorder(Scientific_Name, Re_concentration, FUN = median), y = Re_concentration, group=Scientific_Name)) +
  geom_boxplot()+theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.x=element_blank())+
  #theme(legend.position = "none")+
  scale_x_discrete(guide = guide_axis(angle = 0))+
  geom_jitter(aes(colour = Plot)) +
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#scale_fill_manual(values = c("#38A6A5", "#73AF48", "#EDAD08", "#CC503E"))
Re_125




Cr_125<- ggplot(P125, aes(x = reorder(Scientific_Name, Cr_concentration, FUN = median), y = Cr_concentration, group=Scientific_Name)) +
  geom_boxplot()+theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.x=element_blank())+
  #theme(legend.position = "none")+
  scale_x_discrete(guide = guide_axis(angle = 0))+
  geom_jitter(aes(colour = Plot)) +
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#scale_fill_manual(values = c("#38A6A5", "#73AF48", "#EDAD08", "#CC503E"))
Cr_125

# Plot 6


Cu_6<- ggplot(P6, aes(x = reorder(Scientific_Name, Cu_concentration, FUN = median), y = Cu_concentration, group=Scientific_Name)) +
  geom_boxplot()+theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.x=element_blank())+
  #theme(legend.position = "none")+
  scale_x_discrete(guide = guide_axis(angle = 0))+
  geom_jitter(aes(colour = Plot)) +
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#scale_fill_manual(values = c("#38A6A5", "#73AF48", "#EDAD08", "#CC503E"))
Cu_6







Cu_AllPlots<- ggplot(dt_plants, aes(x = reorder(Scientific_Name, Cu_concentration, FUN = median), y = Cu_concentration, group=Scientific_Name)) +
  geom_boxplot()+theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.x=element_blank())+
  #theme(legend.position = "none")+
  scale_x_discrete(guide = guide_axis(angle = 0))+
  geom_jitter(aes(colour = Plot), size=1) +
  ylim(0,600)+
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#scale_fill_manual(values = c("#38A6A5", "#73AF48", "#EDAD08", "#CC503E"))
Cu_AllPlots



Fe_AllPlots<- ggplot(dt_plants, aes(x = reorder(Scientific_Name, Fe_concentration, FUN = median), y = Fe_concentration, group=Scientific_Name)) +
  geom_boxplot()+theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.x=element_blank())+
  #theme(legend.position = "none")+
  scale_x_discrete(guide = guide_axis(angle = 0))+
  geom_jitter(aes(colour = Plot), size=1) +
  #ylim(0,600)+
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#scale_fill_manual(values = c("#38A6A5", "#73AF48", "#EDAD08", "#CC503E"))
Fe_AllPlots

#Chromium is too low to be considered
Re_AllPlots<- ggplot(dt_plants, aes(x = reorder(Scientific_Name, Re_concentration, FUN = median), y = Re_concentration, group=Scientific_Name)) +
  geom_boxplot()+theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.x=element_blank())+
  #theme(legend.position = "none")+
  scale_x_discrete(guide = guide_axis(angle = 0))+
  geom_jitter(aes(colour = Plot), size=1) +
  #ylim(0,600)+
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#scale_fill_manual(values = c("#38A6A5", "#73AF48", "#EDAD08", "#CC503E"))
Re_AllPlots






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
All_byplot<- ggplot(dt_plants, aes(x = Scientific_Name, y = Cu_concentration, fill=Plot)) +
  geom_boxplot()+theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.x=element_blank())+
  theme(legend.position = "none")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#scale_fill_manual(values = c("#38A6A5", "#73AF48", "#EDAD08", "#CC503E"))
All_byplot




# Selenium

Se <- subset(dt_plants, Scientific_Name == 'Isocoma cf. tenuisecta' | Scientific_Name == 'Populus fremontii' | Scientific_Name == 'Senegalia (Acacia) greggii' )

Se_box <- ggplot(Se, aes(x = reorder(Scientific_Name, Se_concentration, FUN=median), y = Se_concentration, fill=Scientific_Name)) +
  geom_boxplot()+theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.x=element_blank())+
  theme(legend.position = "none")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  geom_jitter(color="#85b8bc", size=3, alpha=0.9) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_fill_manual(values = c("#251433", "#c7abdd", "#8347b2"))
Se_box



#Rhenium


Re <- subset(dt_plants, Scientific_Name == 'Isocoma cf. tenuisecta' | Scientific_Name == 'Baccharis sarothroides' | Scientific_Name == 'Senegalia (Acacia) greggii'| Scientific_Name == 'Nultuma (Prosopis) velutina' | Scientific_Name == 'Mimosa biuncifera (=aculeaticarpa)' | Scientific_Name == 'Fraxinus velutina'| Scientific_Name == 'Datura wrightii' )

Re_box <- ggplot(Re, aes(x = reorder(Scientific_Name, Re_concentration, FUN = median), y = Re_concentration, fill=Scientific_Name)) +
  geom_boxplot()+theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.x=element_blank())+
  theme(legend.position = "none")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  geom_jitter(color="#85b8bc", size=2, alpha=0.9) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  #scale_fill_manual(values = c("", "", "", "", "", "","" ))
  scale_fill_manual(values = c("#4b2866", "#c7abdd", "#a578c9", "#381e4c", "#8347b2", "#5d327f","#251433" ))
  #scale_fill_brewer(palette = "Greens")

Re_box





# CLusters K-mean

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)

df_1[,1:11] <- sapply(df_1[,1:11],as.numeric) 
df[,1:11] <- sapply(df[,1:11],as.numeric) 

df_1 <- read.table("ByPlot.txt", header=T)
df <- read.table("ByPlot.txt", header=T)

df <- df %>% mutate_all(~(scale(.) %>% as.vector))
df
df <- na.omit(df)
k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)
k2 <- kmeans(df, centers = 3, nstart = 25)
str(k2)
k2 <- kmeans(df, centers = 4, nstart = 25)
str(k2)
fviz_cluster(k2, data = df)

df2 <- read.table("CLUSTER_noCd.txt", header=T)
df2 <- df2 %>% mutate_all(~(scale(.) %>% as.vector))
df2
k2k <- kmeans(df2, centers = 3, nstart = 25)
str(k2)
fviz_cluster(k2k, data = df2)

## MIN MAX MEAN

library(dplyr)
library(tidyr)

PlAll <- read.table("ByPlot.txt", header=T)
PlP1 <- read.table("ByPlot_1.txt", header=T)
PlP2 <- read.table("ByPlot_2.txt", header=T)
PlP5 <- read.table("ByPlot_5.txt", header=T)
PlP6 <- read.table("ByPlot_6.txt", header=T)
SoilAll <- read.table("SoilAll.txt", header=T)

PlP1  %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarise_at(vars(value), list(Min = min, Mean = mean, Max = max, Sd = sd))


#summarise(PlP1)

PlAll  %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarise_at(vars(value), list(Min = min, Mean = mean, Max = max, Sd = sd))

#Soil
SoilAll  %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarise_at(vars(value), list(Min = min, Mean = mean, Max = max, Sd = sd))


# Nice table

library(sjPlot)


test1 <- read.table("test1.txt", sep = "\t")

tab_df(test1, file="test1.doc")




subset


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
dt_plants_nounce125 <- subset(dt_plants_nounc, Plot=="P1" | Plot=="P5" | Plot=="P2")

# Changing from Cu_concentration to Cu etc.
colnames(dt_plants_nounce125)[12] <- "Cl"
colnames(dt_plants_nounce125)[13] <- "Ca"
colnames(dt_plants_nounce125)[14] <- "Ti"
colnames(dt_plants_nounce125)[15] <- "Cr"
colnames(dt_plants_nounce125)[16] <- "Mn"
colnames(dt_plants_nounce125)[17] <- "Fe"
colnames(dt_plants_nounce125)[18] <- "Cu"
colnames(dt_plants_nounce125)[19] <- "Zn"
colnames(dt_plants_nounce125)[20] <- "As"
colnames(dt_plants_nounce125)[21] <- "Se"
colnames(dt_plants_nounce125)[22] <- "Cd"
colnames(dt_plants_nounce125)[23] <- "Re"

colnames(dt_plants_nounc6)[12] <- "Cl"
colnames(dt_plants_nounc6)[13] <- "Ca"
colnames(dt_plants_nounc6)[14] <- "Ti"
colnames(dt_plants_nounc6)[15] <- "Cr"
colnames(dt_plants_nounc6)[16] <- "Mn"
colnames(dt_plants_nounc6)[17] <- "Fe"
colnames(dt_plants_nounc6)[18] <- "Cu"
colnames(dt_plants_nounc6)[19] <- "Zn"
colnames(dt_plants_nounc6)[20] <- "As"
colnames(dt_plants_nounc6)[21] <- "Se"
colnames(dt_plants_nounc6)[22] <- "Cd"
colnames(dt_plants_nounc6)[23] <- "Re"


#PCA
require(stats)
myPr1 <- prcomp(dt_plants_nounc1[,12:23], scale=TRUE)
myPr2 <- prcomp(dt_plants_nounc2[,12:23], scale=TRUE)
myPr5 <- prcomp(dt_plants_nounc5[,12:23], scale=TRUE)
myPr6 <- prcomp(dt_plants_nounc6[,12:23], scale=TRUE)
myPr15 <- prcomp(dt_plants_nounce15[,12:23], scale=TRUE)
myPr125 <- prcomp(dt_plants_nounce125[,12:23], scale=TRUE) # it was not working because the scale was FALSE


#myPr2 <- prcomp(~Cu_concentration + Zn_concentration, Mn_concentration, data = dt_plants)

summary(myPr1) # SD - variability across single principle component, 
              #Proportion of variance - e.g., 23 % of data is explained by principle component 1, 18% of data is explained by principle component 2 etc.
              #Cumulative Proportion - PC1 explains 23 % of the variability of data, PC1 + PC2 explains 41%, PC1+PC2+PC3 explains 52% etc
              #We can represent cumultative proportion by l plot
plot(myPr125, type="l") #shows variances across principle components (square of a standard deviation)

#To interpret PC we use function biplot. Because PC1 and PC2 account for the most variation in the data, we only draw those. 

biplot(myPr1, scale=0)
biplot(myPr125, scale=0)
summary(myPr125)#PC1 and PC2 accounts for 47% percent of the data so it might not create a very accurate representation of the data!

biplot125 <- biplot(myPr125,
             col=c('blue', 'red'),
             cex=c(0.8, 0.8),
             xlim=c(-.4, .4),
             main='PCA Results',
             expand=1.2)

biplot6 <-  biplot(myPr6,
            col=c('blue', 'red'),
            cex=c(0.8, 0.8),
            xlim=c(-.4, .4),
            main='PCA Results',
            expand=1.2)


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
  theme_classic()+
  geom_point(shape=21, col="black")

# Plot by species for plot 1 and 5
ggplot(dt_plants15, aes(PC1, PC2, col=Scientific_Name, fill=Scientific_Name))+
  stat_ellipse(geom="polygon", col="black", alpha=0.5)+
  geom_point(shape=21, col="black")

biplot(myPr15, scale=0)


#CHAOS!

ggplot(dt_plants15, aes(PC1, PC2, col=Scientific_Name, fill=Scientific_Name))+
  stat_ellipse(geom="polygon", col="black", alpha=0.5)+
  theme_classic()+
  geom_abline(intercept=2,color="red", 
              linetype="dashed", size=1 )+
  geom_abline(intercept=4,color="blue", 
              linetype="dashed", size=1 )+
  geom_boxplot()+
  geom_point(shape=21, col="black")



#Correlations between variables and principal components 
cor(dt_plants_nounce15[,12:23], dt_plants15[,24:25])

