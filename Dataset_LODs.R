# Soolitude New Summaries
# Tomasz Wlodarczyk
# twlodarczyk@arizona.edu
# 2023-04-23

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

urlfile="https://raw.githubusercontent.com/TRwlodarczyk/Solitude/master/Solitude_Complete_List_3.6.23_New.txt"
dt<-read.delim(url(urlfile))


setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/CESM/Boxplots")
soil_new_LOD <-read_csv("Soil_new_trim_LOD.csv")

#replace ND with 0
{
tr <- matrix(data = NA, ncol = ncol(dt[,c(1:49)]), nrow=nrow(dt)) # select all columns 1:46
colnames(tr) <- colnames(dt[,c(1:49)])
for (i in 15:49) # select when the concentrations start
{
  tr[,c(i)] <- gsub(".*ND.*", 0, dt[,i])
}

for(i in 1:14) # select columns that need to stay the same 1:11 include character and double (weight)
{
  tr[,c(i)] <- dt[,c(i)]
}
  

#transform to dataframe
tr <- as.data.frame.matrix(tr) #A correct command to change the dataset to dataframe after transformations
tr[,15:49] <- sapply(tr[,15:49],as.numeric) # Change a character to numeric (double)
typeof(tr$Cu_concentration) # confirm the value is no longer a character

}

# change the LOD values to LOD/2

# Replace 0 values in Cu_concentration with detection limit of 0.5

tr$Cl_concentration[tr$Cl_concentration == 0] <- 100/2
tr$Ca_concentration[tr$Ca_concentration == 0] <- 22/2
tr$Ti_concentration[tr$Ti_concentration == 0] <- 5/2
tr$Cr_concentration[tr$Cr_concentration == 0] <- 2/2
tr$Mn_concentration[tr$Mn_concentration == 0] <- 1/2
tr$Fe_concentration[tr$Fe_concentration == 0] <- 12/2
tr$Co_concentration[tr$Co_concentration == 0] <- 1
tr$Ni_concentration[tr$Ni_concentration == 0] <- 0.2/2
tr$Cu_concentration[tr$Cu_concentration == 0] <- 0.5/2
tr$Zn_concentration[tr$Zn_concentration == 0] <- 0.6/2
tr$As_concentration[tr$As_concentration == 0] <- 0.1/2
tr$Se_concentration[tr$Se_concentration == 0] <- 0.1/2
tr$Cd_concentration[tr$Cd_concentration == 0] <- 1/2
tr$Re_concentration[tr$Re_concentration == 0] <- 0.5/2
tr$Hg_concentration[tr$Hg_concentration == 0] <- 0.3/2
tr$Tl_concentration[tr$Tl_concentration == 0] <- 1/2
tr$Pb_concentration[tr$Pb_concentration == 0] <- 0.2/2



tr[,10] <- sapply(tr[,10],as.numeric) # Total Weight


#write.table(tr, file='C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Newest Data/Plants_new_LOD.csv', sep=",", row.names = F)

#subset data to remove quality control samples

dt_plants <- subset(tr, Scientific_Name != 'QA_Sample')
dt_plants_trimmed <- dt_plants[c(-27,-43,-45, -seq(14,48,by=2))] # concentrations start from 15 to 49

#write.table(dt_plants_trimmed, file='C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Newest Data/Plants_new_LOD_nounc.csv', sep=",", row.names = F)



# Summary table Plants

dt_plants_summary <- dt_plants_trimmed %>%
  filter(Plot %in% c("P1", "P2", "P5", "P6")) %>%
  pivot_longer(cols = 14:27, names_to = "Concentration", values_to = "Value") %>%
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
                ~ ifelse(. > 10, round(., 0), round(., 2))))


#write.table(dt_plants_summary, file='C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Newest Data/Plants_new_summary.csv', sep=",", row.names = F)


dt_plants_summary_all <- dt_plants_trimmed %>%
  pivot_longer(cols = 14:27, names_to = "Concentration", values_to = "Value") %>%
  group_by(Concentration) %>%
  summarise(min = min(Value), 
            max = max(Value), 
            mean = mean(Value), 
            median = median(Value), 
            sd = sd(Value), 
            .groups = "drop") %>%
  arrange(Concentration)

dt_plants_summary_all <- dt_plants_summary_all %>%
  mutate(across(where(is.numeric), 
                ~ ifelse(. > 10, round(., 0), round(., 2))))


#write.table(dt_plants_summary_all, file='C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Newest Data/Plants_new_summary_all.csv', sep=",", row.names = F)



# Summary Table Soil

dt_soil_summary <- soil_new_LOD %>%
  filter(Plot2 %in% c("P1", "P2", "P5", "P6")) %>%
  pivot_longer(cols = 5:22, names_to = "Concentration", values_to = "Value") %>%
  group_by(Concentration, Plot2) %>%
  summarise(min = min(Value), 
            max = max(Value), 
            mean = mean(Value), 
            median = median(Value), 
            sd = sd(Value), 
            .groups = "drop") %>%
  pivot_wider(names_from = Plot2, values_from = c(min, max, mean, median, sd), names_glue = "{Plot2}_{.value}") %>%
  select(Concentration, contains("_")) %>%
  pivot_longer(cols = -Concentration, names_to = "Plot_Stat", values_to = "Value") %>%
  separate(Plot_Stat, into = c("Plot", "Stat"), sep = "_") %>%
  pivot_wider(names_from = Plot, values_from = Value) %>%
  arrange(Concentration)


dt_soil_summary <- dt_soil_summary %>%
  mutate(across(where(is.numeric), 
                ~ ifelse(. > 10, round(., 0), round(., 2))))






#write.table(dt_soil_summary, file='C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Newest Data/Soil_new_summary.csv', sep=",", row.names = F)


#Summary of the whole dataset
dt_soil_summary_all <- soil_new_LOD %>%
  pivot_longer(cols = 5:22, names_to = "Concentration", values_to = "Value") %>%
  group_by(Concentration) %>%
  summarise(min = min(Value), 
            max = max(Value), 
            mean = mean(Value), 
            median = median(Value), 
            sd = sd(Value), 
            .groups = "drop") %>%
  arrange(Concentration)

dt_soil_summary_all <- dt_soil_summary_all %>%
  mutate(across(where(is.numeric), 
                ~ ifelse(. > 10, round(., 0), round(., 2))))

write.table(dt_soil_summary_all, file='C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Newest Data/Soil_new_summary_all.csv', sep=",", row.names = F)

