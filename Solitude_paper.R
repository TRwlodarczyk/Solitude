# Soolitude Final worksheet
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
dt <-read.delim("Solitude_Complete_List_6.2.23_Final.txt")

dt <- dt[dt$Type_of_Sample != "root", ]
dt <- dt[dt$Type_of_Sample != "QA", ]
dt <- dt[dt$Type_of_Sample != "NA", ]
dt <- dt[complete.cases(dt), ]

#remove ND
{
  tr <- matrix(data = NA, ncol = ncol(dt[,c(1:48)]), nrow=nrow(dt)) # select all columns 1:46
  colnames(tr) <- colnames(dt[,c(1:48)])
  for (i in 14:48) # select when the concentrations start
  {
    tr[,c(i)] <- gsub(".*ND.*", 0, dt[,i])
  }
  
  for(i in c(1:13)) # select columns that need to stay the same 1:11 include character and double (weight)
  {
    tr[,c(i)] <- dt[,c(i)]
  }
  
  
  #transform to dataframe
  tr <- as.data.frame.matrix(tr) #A correct command to change the dataset to dataframe after transformations
  tr[,14:48] <- sapply(tr[,14:48],as.numeric) # Change a character to numeric (double)
  typeof(tr$Cu_concentration) # confirm the value is no longer a character
}
#apply LODs
{
  tr$Cl_concentration[tr$Cl_concentration == 0] <- 50/2
  tr$Ca_concentration[tr$Ca_concentration == 0] <- 10/2
  tr$Ti_concentration[tr$Ti_concentration == 0] <- 5/2
  tr$Cr_concentration[tr$Cr_concentration == 0] <- 2/2
  tr$Mn_concentration[tr$Mn_concentration == 0] <- 1/2
  tr$Fe_concentration[tr$Fe_concentration == 0] <- 5/2
  tr$Co_concentration[tr$Co_concentration == 0] <- 3/2
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
}

dt <- tr

#geomtile

#dt <- dt[dt$Site == "TAILINGS" & dt$Scientific_Name == "Xanthisma gracile", ]


# Load required libraries
library(ggplot2)
library(reshape2)

cols_to_include <- c(6, seq(18, 47, by = 2))
dt_subset <- dt[, cols_to_include]

# Step 2: Remove columns 14, 16, 44, and 46
cols_to_remove <- c(14, 16, 44, 46)
dt_subset <- dt_subset[, -cols_to_remove]

dt_melted <- melt(dt_subset, id.vars = "Sample_Name")

# Step 8: Create the heatmap using geom_tile
ggplot(dt_melted, aes(x = variable, y = Sample_Name, fill = value)) +
  geom_tile(color = "white", width = 0.7, height = 0.7) +
  scale_fill_gradient2(midpoint = 0, name = "Standardized Concentration") +
  labs(title = "Element Concentrations Heatmap (Individual Samples and Standardized)",
       x = "Element",
       y = "Sample",
       fill = "Standardized Concentration") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_blank())







# Load required libraries
library(ggplot2)
library(reshape2)

# Step 1: Subset the data frame to include only relevant columns (element concentrations)
cols_to_include <- c(6, seq(18, 41, by = 2))
dt_subset <- dt[, cols_to_include]

# Step 2: Remove columns 14, 16, 28, 26, and 38
cols_to_remove <- c(6,7,12) # now it removes from dt_subset
dt_subset <- dt_subset[, -cols_to_remove]

# Step 2: Rescale the numeric columns to a range of 0 to 1
rescale_0_to_1 <- function(x) {
  if (is.numeric(x)) {
    return((x - min(x)) / (max(x) - min(x)))
  } else {
    return(x)
  }
}
dt_subset_rescaled <- as.data.frame(lapply(dt_subset, rescale_0_to_1))

# Step 3: Melt the data frame to long format
dt_melted <- melt(dt_subset_rescaled)
dt_melted$variable <- gsub("_concentration", "", dt_melted$variable)
dt_melted$Sample_Scientific_Name <- paste(dt$Sample_Name, dt$Scientific_Name, sep = " - ")

# Step 4: Create the heatmap using geom_tile
ggplot(dt_melted, aes(x = variable, y = Sample_Name, fill = value)) +
  geom_tile(color = "white", width = 0.7, height = 0.7) +
  scale_fill_gradient(low = "blue", high = "red", name = "Rescaled Concentration") +
  labs(title = "Element Concentrations Heatmap (Rescaled to 0-1 for Each Element)",
       x = "Element", 
       y = "Sample",
       fill = "Rescaled Concentration") +
  theme_minimal() +
  theme(axis.text.x = element_text(),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_blank())









# New code to include Scientific Names


# Load required libraries
library(ggplot2)
library(reshape2)

# Step 1: Subset the data frame to include only relevant columns (element concentrations)
cols_to_include <- c(6, seq(18, 41, by = 2))
dt_subset <- dt[, cols_to_include]

# Step 2: Remove columns 14, 16, 28, 26, and 38
cols_to_remove <- c(6, 7, 12) # these columns are removed from the new dataset dt_subset. They are concentrations here (Co, Ni, Cd removed)
dt_subset <- dt_subset[, -cols_to_remove]

# Step 2: Rescale the numeric columns to a range of 0 to 1
rescale_0_to_1 <- function(x) {
  if (is.numeric(x)) {
    return((x - min(x)) / (max(x) - min(x)))
  } else {
    return(x)
  }
}
dt_subset_rescaled <- as.data.frame(lapply(dt_subset, rescale_0_to_1))

# Step 3: Melt the data frame to long format
dt_melted <- melt(dt_subset_rescaled)
dt_melted$variable <- gsub("_concentration", "", dt_melted$variable)
dt_melted$Sample_Scientific_Name <- paste(dt$Sample_Name, dt$Scientific_Name, sep = " - ")

# Step 4: Create a new factor variable for Sample_Scientific_Name
dt_melted$Sample_Scientific_Factor <- factor(
  dt_melted$Sample_Scientific_Name,
  levels = unique(dt_melted$Sample_Scientific_Name)
)

# Step 5: Create the heatmap using geom_tile with the new factor variable
ggplot(dt_melted, aes(x = variable, y = Sample_Scientific_Factor, fill = value)) +
  geom_tile(color = "white", width = 0.7, height = 0.7) +
  scale_fill_gradient(low = "blue", high = "red", name = "Rescaled Concentration") +
  labs(
    title = "Element Concentrations Heatmap (Rescaled to 0-1 for Each Element)",
    x = "Element",
    y = "Sample and Scientific Name",
    fill = "Rescaled Concentration"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_blank()
  )








# Avaraged


















# Count how many samples per plot

species_to_modify <- c("Boechera perennans", "Pseudognaphalium canescens", "Isocoma pluriflora")

for (species in species_to_modify) {
  dt <- dt[!(dt$Scientific_Name == species & dt$Type_of_Sample == "stem"), ]
}




ggplot(dt, aes(x = Plot, y = reorder(Scientific_Name, table(Scientific_Name)[Scientific_Name]), group = Form)) +
  geom_point(shape = 4, size = 1.55, color = "black", stroke = 1.1)+  # specify shape argument as 4 for X symbol and customize size, color, and stroke
  facet_grid(Form ~ ., scales = "free_y", space = "free_y") +
  labs(x = "Plot", color = "Form") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size=14, face="italic"),
        strip.text = element_text(size = 14, face = "bold"))





library(dplyr)

# Group the data by Scientific_Name and Plot, and count the number of rows in each group
species_per_plot <- dt %>% 
  group_by(Scientific_Name, Plot) %>% 
  summarise(num_species = n())

# Group by plot only - i.e. how many plants collected from each plot
species_per_plot <- dt %>% 
  group_by(Plot) %>% 
  summarise(num_species = n())






# APPLY CORRECTION TO CU!!! FINAL 08.07.2023


setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final")
dt <- read.delim("Solitude_Plants_Final_08.23.txt")
dt <- subset(dt, Scientific_Name != 'QA_Sample')
dt <- subset(dt, Site != 'CONTROL')
#dt <- dt[dt$Type_of_Sample != "root", ]

#removing LODs
{
  tr <- matrix(data = NA, ncol = ncol(dt[,c(1:51)]), nrow=nrow(dt)) # select all columns 1:46
  colnames(tr) <- colnames(dt[,c(1:51)])
  for (i in 17:51) # select when the concentrations start
  {
    tr[,c(i)] <- gsub(".*ND.*", 0, dt[,i])
  }
  
  for(i in 1:16) # select columns that need to stay the same 1:11 include character and double (weight)
  {
    tr[,c(i)] <- dt[,c(i)]
  }
  tr   
  
  #transform to dataframe
  tr <- as.data.frame.matrix(tr) #A correct command to change the dataset to dataframe after transformations
  tr[,17:51] <- sapply(tr[,17:51],as.numeric) # Change a character to numeric (double)
  typeof(tr$Cu_concentration) # confirm the value is no longer a character
}
dt <- tr
#apply new LODs
{
  tr$Cl_concentration[tr$Cl_concentration == 0] <- 50/2
  tr$Ca_concentration[tr$Ca_concentration == 0] <- 10/2
  tr$Ti_concentration[tr$Ti_concentration == 0] <- 5/2
  tr$Cr_concentration[tr$Cr_concentration == 0] <- 2/2
  tr$Mn_concentration[tr$Mn_concentration == 0] <- 1/2
  tr$Fe_concentration[tr$Fe_concentration == 0] <- 5/2
  tr$Co_concentration[tr$Co_concentration == 0] <- 3/2
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
}

#Generating predicting variables
{
# Cu
apply_equation_without_zeros <- function(cu, substrate) {
  if (is.numeric(cu) && is.numeric(substrate) && cu != 0) {
    return(28.88747 + (1.41673 * cu) + (-316.95475 * substrate))
  } else {
    return(0)  # Set to NA to indicate exclusion
  }
}
  
dt$Predicted_Cu_ICP <- mapply(apply_equation_without_zeros, dt$Cu_concentration, dt$Substrate_RT)
# Zn
apply_equation_without_zeros <- function(zn, substrate) {
  if (is.numeric(zn) && is.numeric(substrate) && zn != 0) {
    return(50.8422 + (0.9560 * zn) + (-473.9784 * substrate))
  } else {
    return(0)  # Set to NA to indicate exclusion
  }
}

# Apply the modified equation function to create the new column
dt$Predicted_Zn_ICP <- mapply(apply_equation_without_zeros, dt$Zn_concentration, dt$Substrate_RT)


# Se
apply_equation_without_zeros <- function(zn, substrate) {
  if (is.numeric(zn) && is.numeric(substrate) && zn != 0) {
    return(0.4417 + (1.5683 * zn) + (-8.8017 * substrate))
  } else {
    return(0)  # Set to NA to indicate exclusion
  }
}

# Apply the modified equation function to create the new column
dt$Predicted_Se_ICP <- mapply(apply_equation_without_zeros, dt$Se_concentration, dt$Substrate_RT)

# Mn
apply_equation_without_zeros <- function(zn, substrate) {
  if (is.numeric(zn) && is.numeric(substrate) && zn != 0) {
    return(51.4943 + (1.0760 * zn) + (-431.8509 * substrate))
  } else {
    return(0)  # Set to NA to indicate exclusion
  }
}

# Apply the modified equation function to create the new column
dt$Predicted_Mn_ICP <- mapply(apply_equation_without_zeros, dt$Mn_concentration, dt$Substrate_RT)

# As
apply_equation_without_zeros <- function(zn, substrate) {
  if (is.numeric(zn) && is.numeric(substrate) && zn != 0) {
    return(0.5213 + (0.0643 * zn) + (-3.8479 * substrate))
  } else {
    return(0)  # Set to NA to indicate exclusion
  }
}

# Apply the modified equation function to create the new column
dt$Predicted_As_ICP <- mapply(apply_equation_without_zeros, dt$As_concentration, dt$Substrate_RT)

# Cr
apply_equation_without_zeros <- function(zn, substrate) {
  if (is.numeric(zn) && is.numeric(substrate) && zn != 0) {
    return(0.60775 + (0.01228 * zn) + (-5.78566 * substrate))
  } else {
    return(0)  # Set to NA to indicate exclusion
  }
}

# Apply the modified equation function to create the new column
dt$Predicted_Cr_ICP <- mapply(apply_equation_without_zeros, dt$Cr_concentration, dt$Substrate_RT)


# Re
apply_equation_without_zeros <- function(zn, substrate) {
  if (is.numeric(zn) && is.numeric(substrate) && zn != 0) {
    return(3.84146 + (0.91141 * zn) + (-33.18455 * substrate))
  } else {
    return(0)  # Set to NA to indicate exclusion
  }
}

# Apply the modified equation function to create the new column
dt$Predicted_Re_ICP <- mapply(apply_equation_without_zeros, dt$Re_concentration, dt$Substrate_RT)


# Ti
apply_equation_without_zeros <- function(zn, substrate) {
  if (is.numeric(zn) && is.numeric(substrate) && zn != 0) {
    return(-8.80946 + (0.46543 * zn) + (162.79067 * substrate))
  } else {
    return(0)  # Set to NA to indicate exclusion
  }
}

# Apply the modified equation function to create the new column
dt$Predicted_Ti_ICP <- mapply(apply_equation_without_zeros, dt$Ti_concentration, dt$Substrate_RT)




dt$Predicted_Ti_ICP[dt$Predicted_Ti_ICP == 0] <- 5/2
dt$Predicted_Cr_ICP[dt$Predicted_Cr_ICP == 0] <- 2/2
dt$Predicted_Mn_ICP[dt$Predicted_Mn_ICP == 0] <- 1/2
dt$Fe_concentration[dt$Fe_concentration == 0] <- 5/2
dt$Predicted_Cu_ICP[dt$Predicted_Cu_ICP == 0] <- 0.5/2
dt$Predicted_Zn_ICP[dt$Predicted_Zn_ICP == 0] <- 0.6/2
dt$Predicted_As_ICP[dt$Predicted_As_ICP == 0] <- 0.1/2
dt$Predicted_Se_ICP[dt$Predicted_Se_ICP == 0] <- 0.1/2
dt$Predicted_Re_ICP[dt$Predicted_Re_ICP == 0] <- 0.5/2

#write.table(dt, file='C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final/Solitude_Plants_Predicted_08.08.23.csv', sep=",", row.names = F)


}

##### Equations just in case:
#dt$Predicted_Cu_ICP <- 28.88747 + (1.41673* dt$Cu_concentration) + (-316.95475* dt$Substrate_RT)
#dt$Predicted_Zn_ICP <- 50.8422 + (0.9560* dt$Zn_concentration) + (-473.9784* dt$Substrate_RT)
#$Predicted_Se_ICP <- 0.4417 + (1.5683* dt$Se_concentration) + (-8.8017* dt$Substrate_RT)
#dt$Predicted_Mn_ICP <- 51.4943 + (1.0760* dt$Mn_concentration) + (-431.8509* dt$Substrate_RT)
#dt$Predicted_As_ICP <- 0.5213 + (0.0643* dt$As_concentration) + (-3.8479* dt$Substrate_RT)
#dt$Predicted_Cr_ICP <- 0.60775 + (0.01228* dt$Cr_concentration) + (-5.78566* dt$Substrate_RT)
#dt$Predicted_Re_ICP <- 3.84146 + (0.91141* dt$Re_concentration) + (-33.18455* dt$Substrate_RT)
#dt$Predicted_Ti_ICP <- -8.80946 + (0.46543* dt$Ti_concentration) + (162.79067* dt$Substrate_RT)

dt <- read.delim("Solitude_Plants_Predicted_08.09.23-3reps.txt")
dt <- subset(dt, Site != 'CONTROL')
dt <- dt[dt$Type_of_Sample != "root", ]

#removing LODs
{
  tr <- matrix(data = NA, ncol = ncol(dt[,c(1:47)]), nrow=nrow(dt)) # select all columns 1:46
  colnames(tr) <- colnames(dt[,c(1:47)])
  for (i in 17:47) # select when the concentrations start
  {
    tr[,c(i)] <- gsub(".*ND.*", 0, dt[,i])
  }
  
  for(i in 1:16) # select columns that need to stay the same 1:11 include character and double (weight)
  {
    tr[,c(i)] <- dt[,c(i)]
  }
  tr   
  
  #transform to dataframe
  tr <- as.data.frame.matrix(tr) #A correct command to change the dataset to dataframe after transformations
  tr[,17:47] <- sapply(tr[,17:47],as.numeric) # Change a character to numeric (double)
  typeof(tr$Cu_concentration) # confirm the value is no longer a character
}
dt <- tr

sum(dt$Zn_concentration == 0, na.rm = TRUE) # 67/224 = 0.32
sum(dt$Predicted_Zn_ICP < 10, na.rm = TRUE)  #72/224 = 0.321
sum(dt$Se_concentration == 0, na.rm = TRUE) # 49/224 = 0.218
sum(dt$Re_concentration == 0, na.rm = TRUE) # 176/224 = 0.785
sum(dt$Cu_concentration == 0, na.rm = TRUE) #58 /224 = 0.258
sum(dt$Predicted_Zn_ICP < 20, na.rm = TRUE)  #84 224 = 0.375

sum(dt$Predicted_Mn_ICP < 20, na.rm = TRUE) #115/224 = 0.513
sum(dt$Predicted_Mn_ICP < 10, na.rm = TRUE) #115/224 = 0.513
sum(dt$Fe_concentration < 30, na.rm = TRUE) #24/224 = 0.107
sum(dt$Fe_concentration > 500, na.rm = TRUE) #25/224 = 0.111
sum(dt$Predicted_Se_ICP > 30, na.rm = TRUE) #3/224 = 0.0133
sum(dt$Predicted_Se_ICP > 5, na.rm = TRUE) #32/224 = 0.14


#write.table(dt, file='C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final/Solitude_Plants_Final_08.23_Cu_Pred.csv', sep=",", row.names = F)




setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final")
dt <- read.delim("SLT_heatmap.txt")


library(dplyr)
dt_removed_cols <- dt %>%
  select(-c(2, 3, 4, 5, 6))
dt_grouped <- dt_removed_cols %>%
  group_by(Scientific_Name) %>%
  summarize(across(Ca:Ti, mean))





rescale_0_to_1 <- function(x) {
  if (is.numeric(x)) {
    return((x - min(x)) / (max(x) - min(x)))
  } else {
    return(x)
  }
}
dt_subset_rescaled <- as.data.frame(lapply(dt_grouped, rescale_0_to_1))

# Step 3: Melt the data frame to long format
dt_melted <- melt(dt_subset_rescaled)
dt_melted$variable <- gsub("_concentration", "", dt_melted$variable)
dt_melted$Sample_Scientific_Name <- paste(dt_grouped$Scientific_Name)

# Step 4: Create a new factor variable for Sample_Scientific_Name
dt_melted$Sample_Scientific_Factor <- factor(
  dt_melted$Sample_Scientific_Name,
  levels = unique(dt_melted$Sample_Scientific_Name)
)

ggplot(dt_melted, aes(x = variable, y = reorder(Sample_Scientific_Factor, -value), fill = value)) +
  geom_tile(color = "white", width = 0.7, height = 0.7) +
  scale_fill_gradient(low = "#C5DFF8", high = "#4A55A2", name = "Rescaled Concentration") +
  labs(
    title = "Element Concentrations Heatmap (Rescaled to 0-1 for Each Element)",
    x = "Element",
    y = "Sample and Scientific Name",
    fill = "Rescaled Concentration"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_blank()
  )








