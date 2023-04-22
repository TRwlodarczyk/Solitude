# Data Visualization for Solitude Project
# Tomasz Wlodarczyk
# twlodarczyk@arizona.edu
# 2023-02-15

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

#setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data")
#dt <-read.delim("Solitude_Complete_List_3.6.23.txt")
#urlfile="https://raw.githubusercontent.com/TRwlodarczyk/Solitude/master/Solitude_Complete_List_3.6.23.csv" # tr doesn't work on this dataset with csv file
#dt<-read_csv(url(urlfile))

urlfile="https://raw.githubusercontent.com/TRwlodarczyk/Solitude/master/Solitude_Complete_List_3.6.23.txt"
dt<-read.delim(url(urlfile))




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
dt_plants_trimmed <- dt_plants[c(-2,-4,-5,-6,-8,-10,-11, -24, -25, -40, -41, -42, -43, -45, -seq(11,45,by=2))] #a removed -44 as I want to keep Pb
dt_plants_trimmed[,3] <- sapply(dt_plants_trimmed[,3],as.numeric)

write.table(dt_plants_trimmed, file='C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/new.txt')
write.csv(dt_plants_trimmed, "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Tomasz.csv", row.names=FALSE)
write.csv(dt_plants, "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/dt_plants.csv", row.names=FALSE)


# Create a summary table by plots


#summary_dt <- dt_plants_trimmed %>% # and then
#  group_by(Plot) %>% # and then
#  summary() # creat a new column that does mean of sepal length


dt_plants_summary <- dt_plants_trimmed %>%
  filter(Plot %in% c("P1", "P2", "P5", "P6")) %>%
  pivot_longer(cols = 5:18, names_to = "Concentration", values_to = "Value") %>%
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

# Round values to one digit after the comma
#dt_plants_summary[, 3:6] <- round(dt_plants_summary[, 3:6], 1)

dt_plants_summary <- dt_plants_summary %>%
  mutate(across(where(is.numeric), 
                ~ ifelse(. > 10, round(., 0), round(., 1))))


write.csv(dt_plants_summary, "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/CESM/dt_plants_summary.csv")
dt_summary_new <- read_csv("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/CESM/dt_plants_summary.csv")
tab_df(dt_summary_new, file="dt_plants_summary_new.doc", digits = 1)

tab_df(dt_plants_summary, file="dt_plants_summary_04.16.doc", digits = 1)


dt_summary_new <- read_csv("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/CESM/Table/dt_plants_summary2.csv")

library(sjPlot)


dt_plants_summary3 <- read.table("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/CESM/Table/summary.txt", sep = "\t")

tab_df(dt_plants_summary3, file="test1.doc")



# Subset to see how many for diff weight I have

dt_plants_ICP <- filter(dt_plants, ICP == 'y')
dt_plants_ICP[,7] <- sapply(dt_plants_ICP[,7],as.numeric)

library(dplyr)
library(nortest)
library(car)

# create weight ranges
ranges <- c(0, 0.3, 0.5, 0.7, 0.9, Inf)

# filter dataset by weight ranges
# filter dataset by weight ranges


ICP_low <- dt_plants_ICP %>%
  mutate(weight_range = cut(Total_Weight, breaks = ranges)) %>%
  filter(weight_range == "(0,0.3]")

ICP_low2 <- dt_plants_ICP %>%
  mutate(weight_range = cut(Total_Weight, breaks = ranges)) %>%
  filter(weight_range == "(0.3,0.5]")

ICP_medium <- dt_plants_ICP %>%
  mutate(weight_range = cut(Total_Weight, breaks = ranges)) %>%
  filter(weight_range == "(0.5,0.7]")

ICP_medium2 <- dt_plants_ICP %>%
  mutate(weight_range = cut(Total_Weight, breaks = ranges)) %>%
  filter(weight_range == "(0.7,0.9]")

ICP_high <- dt_plants_ICP %>%
  mutate(weight_range = cut(Total_Weight, breaks = ranges)) %>%
  filter(weight_range == "(0.9,Inf]")

ICP_All <- dt_plants_ICP %>%
  mutate(weight_range = cut(Total_Weight, breaks = ranges)) %>%
  filter(weight_range == "(0,Inf]") 


# Fit linear model to ICP_low subset
model <- lm(Substrate_RT ~ Total_Weight, data = ICP_low)
model2 <- lm(Substrate_RT ~ Total_Weight, data = ICP_low2)
model3 <- lm(Substrate_RT ~ Total_Weight, data = ICP_medium)
model4 <- lm(Substrate_RT ~ Total_Weight, data = ICP_medium2)
model5 <- lm(Substrate_RT ~ Total_Weight, data = ICP_high)
model_all <- lm(Substrate_RT ~ Total_Weight, data = dt_plants_ICP)

# Compute R-squared value
rsq <- summary(model)$r.squared
cat("R-squared value:", rsq, "\n\n")



rsq <- summary(model2)$r.squared
cat("R-squared value:", rsq, "\n\n")

rsq <- summary(model3)$r.squared
cat("R-squared value:", rsq, "\n\n")

rsq <- summary(model4)$r.squared
cat("R-squared value:", rsq, "\n\n")

rsq <- summary(model5)$r.squared
cat("R-squared value:", rsq, "\n\n")

rsq <- summary(model_all)$r.squared
cat("R-squared value:", rsq, "\n\n")

# Test normality of residuals
residuals <- model$residuals
cat("Normality test of residuals:\n")
print(ad.test(residuals))

# Generate residual plot
ggplot(data = data.frame(Total_Weight = ICP_low$Total_Weight, Residuals = residuals),
       aes(x = Total_Weight, y = Residuals)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Residual plot for ICP_low subset", x = "Total weight", y = "Residuals")



# Redundancy Analysis (RDA)


library(vegan)

# Create a matrix of the environmental variables (columns 5 to 18)
env_mat <- as.matrix(dt_plants_trimmed[,5:18])

# Create a data frame of the response variables (weight and thickness)
resp_df <- data.frame(weight = dt_plants_trimmed[,3], thickness = dt_plants_trimmed[,18])

# Perform RDA
rda_result <- rda(env_mat, resp_df)

# Print the RDA results
summary(rda_result)

#Plot
# Plot the RDA biplot
plot(rda_result, scaling = 3)

# Add labels to the plot
text(rda_result, display = "species", col = "blue", cex = 0.8) #species - environmental variales
text(rda_result, display = "sites", col = "red", cex = 0.8) # the sites - response variables 

# Add a title to the plot
title(main = "Redundancy Analysis (RDA) Biplot")



##### When Cu is is zmienna wujasniajaca


# Create a matrix of the environmental variables (columns 5 to 18)
env_mat2 <- as.matrix(dt_plants_trimmed[,5:18])

# Create a data frame of the response variables (weight and thickness)
resp_df2 <- data.frame(Cu = dt_plants_trimmed[,12])

# Perform RDA
rda_result <- rda(env_mat2, resp_df2)
plot(rda_result, scaling = 3)
text(rda_result, display = "species", col = "blue", cex = 0.8) #species - environmental variales
text(rda_result, display = "sites", col = "red", cex = 0.8) # the sites - response variables 
title(main = "Redundancy Analysis (RDA) Biplot")





#
cor(dt_plants_trimmed$Total_Weight, dt_plants_trimmed$Substrate_RT) # Wow

#### PLS-DA

library(pls)
library(caret)
# extract the variables of interest from dt_plants_trimmed
X <- dt_plants_trimmed[, 5:17]
# extract the grouping variable (e.g. plot number)
Y <- as.factor(dt_plants_trimmed[, 2])

# perform PLS-DA with 2 components
plsda <- plsda(X, Y, ncomp = 2)

# plot the scores of the first two components
plot(plsda$scores[,1], plsda$scores[,2], col=Y)


###
P1 <- subset(dt_plants, Plot=='P1')
P2 <- subset(dt_plants, Plot=='P2')
P5 <- subset(dt_plants, Plot=='P5')
P6 <- subset(dt_plants, Plot=='P6')
P125 <- subset(dt_plants, Plot!='P6')
P125



Cu_AllPlots<- ggplot(dt_plants, aes(x = reorder(Scientific_Name, Cu_concentration, FUN = median),
                                    y = Cu_concentration, group=Scientific_Name)) +
  geom_boxplot()+
  geom_jitter(aes(colour = Plot), size=1.1) +
  coord_flip()+
  scale_color_manual(values = c("#38A6A5", "#73AF48", "#EDAD08", "#CC503E")) +
  scale_x_discrete(guide = guide_axis(angle = 0))+
  scale_y_continuous(limits = c(0, 600), breaks = seq(0, 800, by = 50)) +
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=10),
        axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size=10.5, face="italic"),
        axis.title.y = element_blank(),
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 10)) +
  guides(colour = guide_legend(override.aes = list(size = 3.5)))+
  ylab("Copper Concentration (ppm)")
Cu_AllPlots

## CESM
#png(file="figure_name.png", unit="in", width=10, height=6.5,res=600,type="cairo")
#Settings
#  theme(
#    axis.title.x = element_text(size= 19, margin = unit(c(3, 0, 0, 0), "mm")), #adjust distance between axis label and ticks. mm are units
#    axis.title.y = element_text(size = 19, margin = unit(c(0, 3, 0, 0), "mm"), angle = 90),
#    axis.text.x = element_text(size=14),
#    axis.text.y = element_text(size=14),
#    legend.position = c(.1, .70),
#    legend.title=element_text(size=12),
#    legend.text=element_text(size=12.5),
#    panel.grid.major = element_blank(),
#    panel.grid.minor = element_blank())
#dev.off()



Cu_AllPlots<- ggplot(dt_plants, aes(x = reorder(Scientific_Name, Cu_concentration, FUN = median),
                                    y = Cu_concentration, group=Scientific_Name)) +
  geom_boxplot()+
  geom_jitter(aes(colour = Plot), size=1.1) +
  geom_hline(yintercept = 40, linetype = "dashed", color = "#9a9a9a", size=1.2)+
  #geom_hline(yintercept = 250, linetype = "longdash", color = "#707070", size=1.2)+
  geom_hline(yintercept = 300, linetype = "dotdash", color = "#454545", size=1.2)+
  coord_flip()+
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_x_discrete(guide = guide_axis(angle = 0))+
  scale_y_continuous(limits = c(0, 600), breaks = seq(0, 800, by = 50)) +
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
        axis.title.x = element_text(size = 19),
        axis.text.y = element_text(size=14, face="italic"),
        axis.title.y = element_blank(),
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 12.5)) +
  guides(colour = guide_legend(override.aes = list(size = 3.5)))+
  ylab("Copper Concentration (ppm)")
Cu_AllPlots





As_AllPlots<- ggplot(dt_plants, aes(x = reorder(Scientific_Name, As_concentration, FUN = median),
                                    y = As_concentration, group=Scientific_Name)) +
  geom_boxplot()+
  geom_jitter(aes(colour = Plot), size=1.1) +
  geom_hline(yintercept = 12, linetype = "dashed", color = "#9a9a9a", size=1.2)+
  #geom_hline(yintercept = 250, linetype = "longdash", color = "#707070", size=1.2)+
  #geom_hline(yintercept = 300, linetype = "dotdash", color = "#454545", size=1.2)+
  coord_flip()+
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_x_discrete(guide = guide_axis(angle = 0))+
  scale_y_continuous(limits = c(0, 18), breaks = seq(0, 18, by = 2)) +
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
        axis.title.x = element_text(size = 19),
        axis.text.y = element_text(size=14, face="italic"),
        axis.title.y = element_blank(),
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 12.5)) +
  guides(colour = guide_legend(override.aes = list(size = 3.5)))+
  ylab("Arsenic Concentration (ppm)")
As_AllPlots


Ca_AllPlots<- ggplot(dt_plants, aes(x = reorder(Scientific_Name, Ca_concentration, FUN = median),
                                    y = Ca_concentration, group=Scientific_Name)) +
  geom_boxplot()+
  geom_jitter(aes(colour = Plot), size=1.1) +
  geom_hline(yintercept = 15000, linetype = "dashed", color = "#9a9a9a", size=1.2)+
  #geom_hline(yintercept = 250, linetype = "longdash", color = "#707070", size=1.2)+
  geom_hline(yintercept = 40000, linetype = "dotdash", color = "#454545", size=1.2)+
  coord_flip()+
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_x_discrete(guide = guide_axis(angle = 0))+
  scale_y_continuous(limits = c(0, 55000), breaks = seq(0, 55000, by = 5000)) +
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
        axis.title.x = element_text(size = 19),
        axis.text.y = element_text(size=14, face="italic"),
        axis.title.y = element_blank(),
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 12.5)) +
  guides(colour = guide_legend(override.aes = list(size = 3.5)))+
  ylab("Calcium Concentration (ppm)")
Ca_AllPlots



Cr_AllPlots<- ggplot(dt_plants, aes(x = reorder(Scientific_Name, Cr_concentration, FUN = median),
                                    y = Cr_concentration, group=Scientific_Name)) +
  geom_boxplot()+
  geom_jitter(aes(colour = Plot), size=1.1) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "#9a9a9a", size=1.2)+
  #geom_hline(yintercept = 250, linetype = "longdash", color = "#707070", size=1.2)+
  #geom_hline(yintercept = 300, linetype = "dotdash", color = "#454545", size=1.2)+
  coord_flip()+
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_x_discrete(guide = guide_axis(angle = 0))+
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 4)) +
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
        axis.title.x = element_text(size = 19),
        axis.text.y = element_text(size=14, face="italic"),
        axis.title.y = element_blank(),
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 12.5)) +
  guides(colour = guide_legend(override.aes = list(size = 3.5)))+
  ylab("Chromium Concentration (ppm)")
Cr_AllPlots


Fe_AllPlots<- ggplot(dt_plants, aes(x = reorder(Scientific_Name, Fe_concentration, FUN = median),
                                    y = Fe_concentration, group=Scientific_Name)) +
  geom_boxplot()+
  geom_jitter(aes(colour = Plot), size=1.1) +
  geom_hline(yintercept = 500, linetype = "dashed", color = "#9a9a9a", size=1.2)+
  #geom_hline(yintercept = 250, linetype = "longdash", color = "#707070", size=1.2)+
  #geom_hline(yintercept = 300, linetype = "dotdash", color = "#454545", size=1.2)+
  coord_flip()+
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_x_discrete(guide = guide_axis(angle = 0))+
  scale_y_continuous(limits = c(0, 3000), breaks = seq(0, 3000, by = 300)) +
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
        axis.title.x = element_text(size = 19),
        axis.text.y = element_text(size=14, face="italic"),
        axis.title.y = element_blank(),
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 12.5)) +
  guides(colour = guide_legend(override.aes = list(size = 3.5)))+
  ylab(expression(paste("Iron Concentration (", "ppm", ")")))
Fe_AllPlots

Mn_AllPlots<- ggplot(dt_plants, aes(x = reorder(Scientific_Name, Mn_concentration, FUN = median),
                                    y = Mn_concentration, group=Scientific_Name)) +
  geom_boxplot()+
  geom_jitter(aes(colour = Plot), size=1.1) +
  geom_hline(yintercept = 500, linetype = "dashed", color = "#9a9a9a", size=1.2)+
  #geom_hline(yintercept = 250, linetype = "longdash", color = "#707070", size=1.2)+
  #geom_hline(yintercept = 300, linetype = "dotdash", color = "#454545", size=1.2)+
  coord_flip()+
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_x_discrete(guide = guide_axis(angle = 0))+
  #scale_y_continuous(limits = c(0, 3000), breaks = seq(0, 3000, by = 300)) +
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
        axis.title.x = element_text(size = 19),
        axis.text.y = element_text(size=14, face="italic"),
        axis.title.y = element_blank(),
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 12.5)) +
  guides(colour = guide_legend(override.aes = list(size = 3.5)))+
  ylab(expression(paste("Manganese Concentration (", "ppm", ")")))
Mn_AllPlots # 10 - 20 ppm is a deficiency!


Ni_AllPlots<- ggplot(dt_plants, aes(x = reorder(Scientific_Name, Ni_concentration, FUN = median),
                                    y = Ni_concentration, group=Scientific_Name)) +
  geom_boxplot()+
  geom_jitter(aes(colour = Plot), size=1.1) +
  #geom_hline(yintercept = 100, linetype = "dashed", color = "#9a9a9a", size=1.2)+
  #geom_hline(yintercept = 250, linetype = "longdash", color = "#707070", size=1.2)+
  #geom_hline(yintercept = 300, linetype = "dotdash", color = "#454545", size=1.2)+
  coord_flip()+
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_x_discrete(guide = guide_axis(angle = 0))+
  #scale_y_continuous(limits = c(0, 3000), breaks = seq(0, 3000, by = 300)) +
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
        axis.title.x = element_text(size = 19),
        axis.text.y = element_text(size=14, face="italic"),
        axis.title.y = element_blank(),
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 12.5)) +
  guides(colour = guide_legend(override.aes = list(size = 3.5)))+
  ylab(expression(paste("Nickel Concentration (", "ppm", ")")))
Ni_AllPlots



Re_AllPlots<- ggplot(dt_plants, aes(x = reorder(Scientific_Name, Re_concentration, FUN = median),
                                    y = Re_concentration, group=Scientific_Name)) +
  geom_boxplot()+
  geom_jitter(aes(colour = Plot), size=1.1) +
  #geom_hline(yintercept = 300, linetype = "dashed", color = "#AFE1AF", size=1.2)+
  #geom_hline(yintercept = 1200, linetype = "dashed", color = "#83A3BE", size=1.2)+
  #geom_hline(yintercept = 2500, linetype = "dashed", color = "#003A6B", size=1.2)+
  coord_flip()+
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_x_discrete(guide = guide_axis(angle = 0))+
  #scale_y_continuous(limits = c(0, 3000), breaks = seq(0, 3000, by = 300)) +
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
        axis.title.x = element_text(size = 19),
        axis.text.y = element_text(size=14, face="italic"),
        axis.title.y = element_blank(),
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 12.5)) +
  guides(colour = guide_legend(override.aes = list(size = 3.5)))+
  #ylab(expression(paste("Rhenium Concentration (", "ppm", ")")))
  ylab('Rhenium concentration (ppm)')
Re_AllPlots


Se_AllPlots<- ggplot(dt_plants, aes(x = reorder(Scientific_Name, Se_concentration, FUN = median),
                                    y = Se_concentration, group=Scientific_Name)) +
  geom_boxplot()+
  geom_jitter(aes(colour = Plot), size=1.1) +
  geom_hline(yintercept = 5, linetype = "dashed", color = "#9a9a9a", size=1.2)+
  #geom_hline(yintercept = 250, linetype = "longdash", color = "#707070", size=1.2)+
  geom_hline(yintercept = 100, linetype = "dotdash", color = "#454545", size=1.2)+
  coord_flip()+
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_x_discrete(guide = guide_axis(angle = 0))+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
        axis.title.x = element_text(size = 19),
        axis.text.y = element_text(size=14, face="italic"),
        axis.title.y = element_blank(),
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 12.5)) +
  guides(colour = guide_legend(override.aes = list(size = 3.5)))+
  #ylab(expression(paste("Rhenium Concentration (", "ppm", ")")))
  ylab('Selenium concentration (ppm)')
Se_AllPlots


Zn_AllPlots<- ggplot(dt_plants, aes(x = reorder(Scientific_Name, Zn_concentration, FUN = median),
                                    y = Zn_concentration, group=Scientific_Name)) +
  geom_boxplot()+
  geom_jitter(aes(colour = Plot), size=1.1) +
  #geom_hline(yintercept = 40, linetype = "dashed", color = "#9a9a9a", size=1.2)+
  #geom_hline(yintercept = 250, linetype = "longdash", color = "#707070", size=1.2)+
  #geom_hline(yintercept = 300, linetype = "dotdash", color = "#454545", size=1.2)+
  coord_flip()+
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_x_discrete(guide = guide_axis(angle = 0))+
  #scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
        axis.title.x = element_text(size = 19),
        axis.text.y = element_text(size=14, face="italic"),
        axis.title.y = element_blank(),
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 12.5)) +
  guides(colour = guide_legend(override.aes = list(size = 3.5)))+
  #ylab(expression(paste("Rhenium Concentration (", "ppm", ")")))
  ylab('Zinc concentration (ppm)')
Zn_AllPlots


Ti_AllPlots<- ggplot(dt_plants, aes(x = reorder(Scientific_Name, Ti_concentration, FUN = median),
                                    y = Ti_concentration, group=Scientific_Name)) +
  geom_boxplot()+
  geom_jitter(aes(colour = Plot), size=1.1) +
  #geom_hline(yintercept = 40, linetype = "dashed", color = "#9a9a9a", size=1.2)+
  #geom_hline(yintercept = 250, linetype = "longdash", color = "#707070", size=1.2)+
  #geom_hline(yintercept = 300, linetype = "dotdash", color = "#454545", size=1.2)+
  coord_flip()+
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_x_discrete(guide = guide_axis(angle = 0))+
  #scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
        axis.title.x = element_text(size = 19),
        axis.text.y = element_text(size=14, face="italic"),
        axis.title.y = element_blank(),
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 12.5)) +
  guides(colour = guide_legend(override.aes = list(size = 3.5)))+
  #ylab(expression(paste("Rhenium Concentration (", "ppm", ")")))
  ylab('Titanium concentration (ppm)')
Ti_AllPlots


Cd_AllPlots<- ggplot(dt_plants, aes(x = reorder(Scientific_Name, Cd_concentration, FUN = median),
                                    y = Cd_concentration, group=Scientific_Name)) +
  geom_boxplot()+
  geom_jitter(aes(colour = Plot), size=1.1) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "#9a9a9a", size=1.2)+
  #geom_hline(yintercept = 250, linetype = "longdash", color = "#707070", size=1.2)+
  #geom_hline(yintercept = 100, linetype = "dotdash", color = "#454545", size=1.2)+
  coord_flip()+
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_x_discrete(guide = guide_axis(angle = 0))+
  #scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
        axis.title.x = element_text(size = 19),
        axis.text.y = element_text(size=14, face="italic"),
        axis.title.y = element_blank(),
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 12.5)) +
  guides(colour = guide_legend(override.aes = list(size = 3.5)))+
  #ylab(expression(paste("Rhenium Concentration (", "ppm", ")")))
  ylab('Cadmium concentration (ppm)')
Cd_AllPlots

####### COrrelation both plants on plot 6 (35 datapoints)
subset_plants <- dt_plants[dt_plants$Scientific_Name %in% c("Xanthisma gracile", "Pseudognaphalium canescens"),]
subset_plants <- dt_plants[dt_plants$Plot %in% c("P6"),]
shapiro.test(subset_plants$Cu_concentration) # non norm
shapiro.test(subset_plants$Cr_concentration) # non norm
shapiro.test(subset_plants$Fe_concentration) # non norm
cor.test(subset_plants$Fe_concentration, subset_plants$Cu_concentration, method="spearman") # significant cor but ties
cor.test(subset_plants$Cr_concentration, subset_plants$Cu_concentration, method="spearman") # no significant but ties
cor.test(subset_plants$Fe_concentration, subset_plants$Cr_concentration, method="spearman") # no significant but ties

# Subset Only Xanthisma plot 6 (only 5 datapoints)
subset_plants <- dt_plants[dt_plants$Scientific_Name %in% c("Xanthisma gracile", "Pseudognaphalium canescens"),]
subset_plants <- dt_plants[dt_plants$Plot %in% c("P6"),]
subset_plants2 <- subset_plants[subset_plants$Scientific_Name %in% c("Xanthisma gracile"),]
shapiro.test(subset_plants2$Cu_concentration) # normal distribution
shapiro.test(subset_plants2$Cr_concentration) # normal distribution
shapiro.test(subset_plants2$Fe_concentration) # normal distribution
cor.test(subset_plants2$Fe_concentration, subset_plants2$Cu_concentration, method="pearson") # significant cor
cor.test(subset_plants2$Cr_concentration, subset_plants2$Cu_concentration, method="pearson") # no signif
cor.test(subset_plants2$Fe_concentration, subset_plants2$Cr_concentration, method="pearson") # no signif

# Subset onlu xanthisma all  plots (13 datapooints)
subset_plants <- dt_plants[dt_plants$Scientific_Name %in% c("Xanthisma gracile", "Pseudognaphalium canescens"),]
subset_plants_allX <- subset_plants[subset_plants$Scientific_Name %in% c("Xanthisma gracile"),]
shapiro.test(subset_plants_allX$Cu_concentration) # non normal
shapiro.test(subset_plants_allX$Cr_concentration) # non normal
shapiro.test(subset_plants_allX$Fe_concentration) # non normal
cor.test(subset_plants_allX$Fe_concentration, subset_plants_allX$Cu_concentration, method="spearman") # no signif
cor.test(subset_plants_allX$Cr_concentration, subset_plants_allX$Cu_concentration, method="spearman") # no signif
cor.test(subset_plants_allX$Fe_concentration, subset_plants_allX$Cr_concentration, method="spearman") # no signif

#Subset only Pseudognaphalium plot 6 (5 observations)

subset_plants <- dt_plants[dt_plants$Scientific_Name %in% c("Xanthisma gracile", "Pseudognaphalium canescens"),]
subset_plants3 <- subset_plants[subset_plants$Scientific_Name %in% c("Pseudognaphalium canescens"),]
shapiro.test(subset_plants3$Cu_concentration) # normal distribution
shapiro.test(subset_plants3$Cr_concentration) # normal distribution
shapiro.test(subset_plants3$Fe_concentration) # normal distribution
cor.test(subset_plants3$Fe_concentration, subset_plants3$Cu_concentration, method="pearson") # no signif
cor.test(subset_plants3$Cr_concentration, subset_plants3$Cu_concentration, method="pearson") # no signif
cor.test(subset_plants3$Fe_concentration, subset_plants3$Cr_concentration, method="pearson") # no signif

#Subset only Biechera perennans plot 6 (4 datapoints)

subset_plants_BP <- dt_plants[dt_plants$Scientific_Name %in% c("cf. Boechera perennans"),]
shapiro.test(subset_plants_BP$Cu_concentration) # normal distribution
shapiro.test(subset_plants_BP$Cr_concentration) # normal distribution
shapiro.test(subset_plants_BP$Fe_concentration) # normal distribution
cor.test(subset_plants_BP$Fe_concentration, subset_plants_BP$Cu_concentration, method="pearson") # no signif p>0.05
cor.test(subset_plants_BP$Cr_concentration, subset_plants_BP$Cu_concentration, method="pearson") # no signif
cor.test(subset_plants_BP$Fe_concentration, subset_plants_BP$Cr_concentration, method="pearson") # no signif



shapiro.test(subset_plants3$Cu_concentration) # > 0.05 =  normally distributed
shapiro.test(subset_plants$Fe_concentration) # <0.05 non normally all Fe, Cr, Cu



sd(dt_plants$Cd_concentration)
#scale_fill_manual(values = c("#38A6A5", "#73AF48", "#EDAD08", "#CC503E"))
#theme(legend.position = "none")+
#ylim(0,600)+
#scale_y_discrete(breaks = seq(1, length(unique(dt_plants$Scientific_Name)), by = 100)) +





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



#PLS

library(readr)
library(dplyr)
library(tidyr)
library(ropls)

dt_plants_nounc


dt_plants_nounc_3 <- dt_plants_nounc |> select(-Scientific_Name, -Group, -Plot, -Sample_Name, -Tube_No, -Type_of_Sample, -Cup_No, -pXRF_measurement_ID, -File, -Material)

typeof(dt_plants_nounc_3$Total_Weight)
dt_plants_nounc_3[,1] <- sapply(dt_plants_nounc_3[,1],as.numeric)

dt_nounc_PCA <- opls(x=dt_plants_nounc_3)
plot(baked_pca)


plot(dt_nounc_PCA, typeVc ="x-score", parAsColFcVn=dt_plants_nounc$Plot)
dt_opls <-opls(dt_plants_nounc_3, dt_plants_nounc$Plot)


# Correlation Plot for SHiny

library(plotly)
library(heatmaply)
library(ggcorrplot)

dt_plants_trimmed
dt_plants_trimmed2 <- dt_plants_trimmed[c(-1,-2,-4)]


dt_cor <-  dt_plants_tr |> select(-Scientific_Name, -Group, -Plot, -Sample_Name, -Tube_No, -Type_of_Sample, -Cup_No, -pXRF_measurement_ID, -ICP, -Material)
typeof(dt_cor$Total_Weight)
dt_cor[,1] <- sapply(dt_cor[,1],as.numeric)


dt_plants_trimmed2[,1] <- sapply(dt_plants_trimmed2[,1], as.numeric)


heatmaply_cor(
  cor(dt_plants_trimmed2),
  xlab = "Features", 
  ylab = "Features",
  k_col = 2, 
  k_row = 2
)


#heatmap without Weight and RT

dt_plants_trimmed3 <- dt_plants_trimmed2 |> select(-Total_Weight, -Substrate_RT)


heatmaply_cor(
  cor(dt_plants_trimmed3),
  xlab = "Features", 
  ylab = "Features",
  k_col = 2, 
  k_row = 2
)

#write.table(dt_cor, file = "my_data.txt", sep = "\t", col.names=TRUE)




library(ggplot2)
library(reshape2)

# Melt data into long format
dt_plants_trimmed_cb <- dt_plants_trimmed[c(-2,-3, -4, -18)]
melted_data <- melt(dt_plants_trimmed_cb, id.vars = c("Scientific_Name"))

#write.table(dt_cor, file = "my_data.txt", sep = "\t", col.names=TRUE)

# Subset data to only include metal concentrations
metal_data <- subset(melted_data, variable %in% colnames(dt_plants_trimmed_cb)[5:17])

write.csv(melted_data, "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Melted.csv", row.names=FALSE)



# Create circular barplot
ggplot(metal_data, aes(x = Scientific_Name, y = value, fill = variable)) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_polar(theta = "x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Metal Concentrations for All Species")


ggplot(dt_plants_trimmed_cb, aes(x = Scientific_Name, y = Cu_concentration, fill = Scientific_Name)) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_polar(theta = "x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Metal Concentrations for All Species")


ggplot(dt_plants_trimmed_cb, aes(x = Scientific_Name, y = Cu_concentration, fill = Scientific_Name)) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_polar(theta = "x") +
  #scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(size = 18, face = "bold")) +
  geom_text(data = dt_plants_trimmed_cb, aes(x = Scientific_Name, y = Cu_concentration/2, label = Scientific_Name), 
            color = "white", size = 4, angle = -90, hjust = 1)


#### SOIL PLANT SMMARY

summary(dt_plants_nounc_3)
sd(dt_plants$Ti_concentration)
