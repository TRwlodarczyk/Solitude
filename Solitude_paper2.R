# Soolitude Final worksheet
# Tomasz Wlodarczyk
# twlodarczyk@arizona.edu
# 2023-07-28

#Libraries
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
}

{
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
  
  # Old code to include Scientific Names and Sample Names - Heatmap
  {
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
    
    
  }
  
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
  
  
  
}


# APPLY CORRECTION TO CU!!! FINAL 08.07.2023
{
  setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final")
  dt <- read.delim("Solitude_Plants_Final_08.23.txt")
  dt <- subset(dt, Scientific_Name != 'QA_Sample')
  #dt <- subset(dt, Site != 'CONTROL')
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
    
    # Fe NO, because no replicates were removed in this dataset.
    apply_equation_without_zeros <- function(zn, substrate) {
      if (is.numeric(zn) && is.numeric(substrate) && zn != 0) {
        return(28.88747 + (1.41673 * zn) + (-316.95475 * substrate))
      } else {
        return(0)  # Set to NA to indicate exclusion
      }
    }
    
    # Apply the modified equation function to create the new column
    dt$Predicted_Fe_ICP <- mapply(apply_equation_without_zeros, dt$Fe_concentration, dt$Substrate_RT)

    
    dt$Predicted_Ti_ICP[dt$Predicted_Ti_ICP == 0] <- 5/2
    dt$Predicted_Cr_ICP[dt$Predicted_Cr_ICP == 0] <- 2/2
    dt$Predicted_Mn_ICP[dt$Predicted_Mn_ICP == 0] <- 1/2
    dt$Fe_concentration[dt$Fe_concentration == 0] <- 5/2
    #dt$Predicted_Fe_ICP[dt$Predicted_Fe_ICP == 0] <- 5/2
    dt$Predicted_Cu_ICP[dt$Predicted_Cu_ICP == 0] <- 0.5/2
    dt$Predicted_Zn_ICP[dt$Predicted_Zn_ICP == 0] <- 0.6/2
    dt$Predicted_As_ICP[dt$Predicted_As_ICP == 0] <- 0.1/2
    dt$Predicted_Se_ICP[dt$Predicted_Se_ICP == 0] <- 0.1/2
    dt$Predicted_Re_ICP[dt$Predicted_Re_ICP == 0] <- 0.5/2
    
   # write.table(dt, file='C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final/Solitude_Plants_Predicted_08.08.23.csv', sep=",", row.names = F)
    
    
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
  
}


setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final")
dt <- read.delim("Solitude_Plants_Predicted_08.09.23-3reps.txt")
dt <- subset(dt, Site != 'CONTROL')
dt <- dt[dt$Type_of_Sample != "root", ]

#Outliers check:
{
  # Your dataset 'dt'
  # Assuming 'Predicted_Cu_ICP' is the column of interest
  data_to_test <- dt$Cd_concentration
  
  # Sort the data in ascending order
  sorted_data <- sort(data_to_test)
  
  # Calculate the range
  range_data <- max(sorted_data) - min(sorted_data)
  
  # Calculate ratios of gaps to the range
  ratios <- diff(sorted_data) / range_data
  
  # Dixon's Q-test critical values for various sample sizes and significance levels
  # For a 95% confidence level, use Q_crit = 0.704 for n = 5 and Q_crit = 0.425 for n = 10
  Q_crit <- c(0.704, 0.425)
  
  # Check if any ratios exceed the critical value
  outliers <- ratios > Q_crit[length(sorted_data)]
  
  # Print the results
  cat("Ratios:", ratios, "\n")
  cat("Outliers Detected:", sorted_data[outliers], "\n")
  
  
  #Cu, Fe, Zn, Mn, Se, Cr, As, Re no outliers
}


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
} # not necessary here
dt <- tr

#SUM
{
  sum(dt$Zn_concentration == 0, na.rm = TRUE) # 67/224 = 0.32
  sum(dt$Predicted_Zn_ICP < 10, na.rm = TRUE)  #72/224 = 0.321
  sum(dt$Se_concentration == 0, na.rm = TRUE) # 49/224 = 0.218
  sum(dt$Re_concentration == 0, na.rm = TRUE) # 176/224 = 0.785
  sum(dt$Cu_concentration == 0, na.rm = TRUE) #58 /224 = 0.258
  sum(dt$Predicted_Zn_ICP < 20, na.rm = TRUE)  #84 224 = 0.375
  sum(dt$Re_concentration == 0, na.rm = TRUE)  #
  sum(dt$Mn_concentration == 0, na.rm = TRUE)  #
  
  sum(dt$Predicted_Mn_ICP < 20, na.rm = TRUE) #115/224 = 0.513
  sum(dt$Predicted_Mn_ICP < 10, na.rm = TRUE) #115/224 = 0.513
  sum(dt$Fe_concentration < 30, na.rm = TRUE) #24/224 = 0.107
  sum(dt$Fe_concentration > 500, na.rm = TRUE) #25/224 = 0.111
  sum(dt$Predicted_Se_ICP > 30, na.rm = TRUE) #3/224 = 0.0133
  sum(dt$Predicted_Se_ICP > 5, na.rm = TRUE) #32/224 = 0.14
  
  dt$Predicted_Cr_ICP[dt$Predicted_Cr_ICP == 1] <- 0
  sum(dt$Predicted_Cr_ICP < 0.5, na.rm = TRUE) #183/224 = 0.82
  sum(dt$Predicted_Re_ICP > 5, na.rm = TRUE) #29/224 = 0.1294
  
}


#write.table(dt, file='C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final/Solitude_Plants_Final_08.23_Cu_Pred.csv', sep=",", row.names = F)



# Heatmap 
{
  setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final")
  dt <- read.delim("SLT_heatmap_plants.txt")
  dt <- dt[dt$Type_of_Sample != "stem", ]
  
  library(dplyr)
  library(reshape2)
  library(ggplot2)
  
  dt_removed_cols <- dt %>%
    select(-c(2, 3, 4, 5, 6, 11, 16))
  
  dt_grouped <- dt_removed_cols %>%
    group_by(Scientific_Name) %>%
    summarize(across(Cu:Se, median))
  
  rescale_0_to_1 <- function(x) {
    if (is.numeric(x)) {
      return((x - min(x)) / (max(x) - min(x)))
    } else {
      return(x)
    }
  }
  dt_subset_rescaled <- as.data.frame(lapply(dt_grouped, rescale_0_to_1))
  
  # Step 4: Melt the data frame to long format
  dt_melted <- melt(dt_subset_rescaled, id.vars = "Scientific_Name")
  dt_melted$variable <- gsub("_concentration", "", dt_melted$variable)
  
  # Step 5: Sort dt_grouped by the highest Cu values
  dt_grouped_sorted <- dt_grouped %>%
    arrange(desc(Cu))
  
  # Reorder levels of Scientific_Name based on Cu values
  dt_melted$Scientific_Name <- factor(
    dt_melted$Scientific_Name,
    levels = dt_grouped_sorted$Scientific_Name
  )
  
  # Define the desired order of elements
  element_order <- c("Cu", "Fe", "Mn", "Zn", "As", "Cr", "Re", "Se")
  
  # Factor the variable column based on element_order
  dt_melted$variable <- factor(dt_melted$variable, levels = element_order)
  
  # Create the heatmap using geom_tile with the sorted and melted data
  ggplot(dt_melted, aes(x = variable, y = reorder(Scientific_Name, match(Scientific_Name, dt_grouped_sorted$Scientific_Name)), fill = value)) +
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
  
  
  
  
  
  
  
  
  
  
  ##################### BEZ CR and AS
  
  
  library(dplyr)
  library(reshape2)
  library(ggplot2)
  
  dt_removed_cols <- dt %>%
    select(-c(2, 3, 4, 5, 6, 11,12,13, 16))
  
  dt_grouped <- dt_removed_cols %>%
    group_by(Scientific_Name) %>%
    summarize(across(Cu:Se, median))
  
  rescale_0_to_1 <- function(x) {
    if (is.numeric(x)) {
      return((x - min(x)) / (max(x) - min(x)))
    } else {
      return(x)
    }
  }
  dt_subset_rescaled <- as.data.frame(lapply(dt_grouped, rescale_0_to_1))
  
  # Step 4: Melt the data frame to long format
  dt_melted <- melt(dt_subset_rescaled, id.vars = "Scientific_Name")
  dt_melted$variable <- gsub("_concentration", "", dt_melted$variable)
  
  # Step 5: Sort dt_grouped by the highest Cu values
  dt_grouped_sorted <- dt_grouped %>%
    arrange(desc(Cu))
  
  # Reorder levels of Scientific_Name based on Cu values
  dt_melted$Scientific_Name <- factor(
    dt_melted$Scientific_Name,
    levels = dt_grouped_sorted$Scientific_Name
  )
  
  # Define the desired order of elements
  element_order <- c("Cu", "Fe", "Mn", "Zn", "Re", "Se")
  
  # Factor the variable column based on element_order
  dt_melted$variable <- factor(dt_melted$variable, levels = element_order)
  
  # Create the heatmap using geom_tile with the sorted and melted data
  ggplot(dt_melted, aes(x = variable, y = reorder(Scientific_Name, match(Scientific_Name, dt_grouped_sorted$Scientific_Name)), fill = value)) +
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
  
  
  ######################## FINAL
  library(dplyr)
  library(reshape2)
  library(ggplot2)
  library(gplots)
  
  setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final")
  dt <- read.delim("SLT_heatmap_plants.txt")
  dt <- dt[dt$Type_of_Sample != "stem", ]
  
  dt_removed_cols <- dt %>%
    select(-c(2, 3, 4, 5, 6, 11, 12, 13, 16))
  
  dt_grouped <- dt_removed_cols %>%
    group_by(Scientific_Name) %>%
    summarize(across(Cu:Se, median))
  
  rescale_0_to_1 <- function(x) {
    if (is.numeric(x)) {
      return((x - min(x)) / (max(x) - min(x)))
    } else {
      return(x)
    }
  }
  dt_subset_rescaled <- as.data.frame(lapply(dt_grouped, rescale_0_to_1))
  
  # Step 4: Melt the data frame to long format
  dt_melted <- melt(dt_subset_rescaled, id.vars = "Scientific_Name")
  dt_melted$variable <- gsub("_concentration", "", dt_melted$variable)
  
  # Step 5: Sort dt_grouped by the highest Cu values
  dt_grouped_sorted <- dt_grouped %>%
    arrange(desc(Cu))
  
  # Reorder levels of Scientific_Name based on Cu values
  dt_melted$Scientific_Name <- factor(
    dt_melted$Scientific_Name,
    levels = dt_grouped_sorted$Scientific_Name
  )
  
  # Define the desired order of elements
  element_order <- c("Cu", "Fe", "Mn", "Zn", "Re", "Se")
  
  # Factor the variable column based on element_order
  dt_melted$variable <- factor(dt_melted$variable, levels = element_order)
  
  # Create the heatmap using the heatmap.2 function with dendrograms
  heatmap.2(as.matrix(acast(dt_melted, Scientific_Name ~ variable, value.var = "value")),
            scale = "none", # Use "none" to keep the original values
            trace = "none", # Remove trace colors
            Rowv = TRUE, Colv = FALSE, # Add dendrograms
            col = colorRampPalette(c("#C5DFF8", "#4A55A2"))(256),
            key = TRUE, keysize = 1, key.title = NA, # Add color scale
            symkey = FALSE, density.info = "none",
            lwid = c(0.5, 0.5))
  
  
  
  #######
  
  dt <- read.delim("SLT_heatmap_plants.txt")
  library(dplyr)
  library(reshape2)
  library(ggplot2)
  
  dt_removed_cols <- dt %>%
    select(-c(2, 3, 4, 5, 6, 11, 16))
  
  dt_grouped <- dt_removed_cols %>%
    group_by(Scientific_Name) %>%
    summarize(across(Cu:Se, median))
  
  rescale_0_to_1 <- function(x) {
    if (is.numeric(x)) {
      return((x - min(x)) / (max(x) - min(x)))
    } else {
      return(x)
    }
  }
  dt_subset_rescaled <- as.data.frame(lapply(dt_grouped, rescale_0_to_1))
  
  # Step 4: Melt the data frame to long format
  dt_melted <- melt(dt_subset_rescaled, id.vars = "Scientific_Name")
  dt_melted$variable <- gsub("_concentration", "", dt_melted$variable)
  
  # Step 5: Sort dt_grouped by the highest Cu values
  dt_grouped_sorted <- dt_grouped %>%
    arrange(desc(Cu))
  
  # Reorder levels of Scientific_Name based on Cu values
  dt_melted$Scientific_Name <- factor(
    dt_melted$Scientific_Name,
    levels = dt_grouped_sorted$Scientific_Name
  )
  
  # Define the desired order of elements
  element_order <- c("Cu", "Fe", "Mn", "Zn", "As", "Cr", "Re", "Se")
  
  # Factor the variable column based on element_order
  dt_melted$variable <- factor(dt_melted$variable, levels = element_order)
  
  # Create the heatmap using geom_tile with the sorted and melted data
  ggplot(dt_melted, aes(x = variable, y = reorder(Scientific_Name, match(Scientific_Name, dt_grouped_sorted$Scientific_Name)), fill = value)) +
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
  
  
  
  library(dendextend)
  library(ggdendro)
  
  
  
  
  # Check for missing values and non-numeric values in dt_subset_rescaled
  # For columns Cu:Se, ensure they contain only numeric values
  # If necessary, perform appropriate data cleaning or transformation
  
  # Calculate distance matrix (assuming your data is clean)
  dist_matrix <- dist(dt_subset_rescaled, method = "euclidean")
  
  # Perform hierarchical clustering
  hclust_result <- hclust(dist_matrix, method = "ward.D2")
  
  # Convert hclust object to dendrogram
  dendro <- as.dendrogram(hclust_result)
  
  # Get the labels and heights
  leaf_labels <- labels(dendro)
  leaf_heights <- dendro$height
  
  # Create a data frame with leaf labels and heights
  leaf_data <- data.frame(label = leaf_labels, height = leaf_heights)
  
  # Create a ggplot2 dendrogram with labeled leaves
  dend_plot <- ggplot(leaf_data, aes(x = height, y = label)) +
    geom_line() +
    geom_text(aes(label = label), hjust = -0.1, size = 3) +
    labs(x = "Height", y = "Scientific Names") +
    theme_minimal() +
    theme(axis.title.x = element_blank())
  
  # Rotate y-axis labels for better readability
  dend_plot <- dend_plot + theme(axis.text.y = element_text(angle = 0, hjust = 1))
  
  # Show the plot
  print(dend_plot)
  
  
  
  
  
  
  
  
  # Create the heatmap using geom_tile with the sorted and melted data
  ggplot(dt_melted, aes(x = variable, y = reorder(Scientific_Name, -value), fill = value)) +
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
}


#Correlation between elements in plants
{
  library(car)
  
  dt <- read.delim("SLT_heatmap_plants.txt")
  dt <- dt[dt$Type_of_Sample != "stem", ] # wszystkie korelacje byly robione ze stemami..............
  
  shapiro.test(dt$Fe)
  shapiro.test(dt$Zn) # non normal
  shapiro.test(dt$Mn) # non normal
  shapiro.test(dt$Se) # non normal
  shapiro.test(dt$Cr) # non normal
  shapiro.test(dt$Re) # non normal
  cor.test(dt$Cu, dt$Fe, method="spearman") 
  cor.test(dt$Cu, dt$Zn, method="spearman") #0.3
  cor.test(dt$Cu, dt$Mn, method="spearman") #0.35
  cor.test(dt$Cu, dt$Cr, method="spearman") #0.18
  cor.test(dt$Cu, dt$Se, method="spearman") #0.33
  cor.test(dt$Cu, dt$Re, method="spearman") #0.19
  leveneTest(dt$Cu, dt$Fe) # <0.05, variances are heterogeneous (unequal) (violation of ANOVA). homogeneity of variances (also known as homoscedasticity)
  
  
  cor.test(dt$As, dt$Fe, method="spearman") #0.389
  cor.test(dt$As, dt$Cr, method="spearman") # 0.32435
  cor.test(dt$As, dt$Zn, method="spearman") # -0.27
  cor.test(dt$As, dt$Se, method="spearman") # -0.201
  cor.test(dt$As, dt$Mn, method="spearman") # -0.341
  cor.test(dt$As, dt$Re, method="spearman") #-0.273
  
  
  
  cor.test(dt$Fe, dt$Cr, method="spearman") # 0.3557
  cor.test(dt$Fe, dt$Zn, method="spearman") # 0.05 pval 0.37
  cor.test(dt$Fe, dt$Se, method="spearman") # 0.018 pval 0.7838
  cor.test(dt$Fe, dt$Mn, method="spearman") # 0.159 pval 0.016
  cor.test(dt$Fe, dt$Re, method="spearman") # 0.008 pval 0.899
  
  
  cor.test(dt$Cr, dt$Zn, method="spearman") # no correl
  cor.test(dt$Cr, dt$Se, method="spearman") # no
  cor.test(dt$Cr, dt$Mn, method="spearman") # no
  cor.test(dt$Cr, dt$Re, method="spearman") # no
  
  cor.test(dt$Zn, dt$Se, method="spearman") # 0.2 pval 0.0025
  cor.test(dt$Zn, dt$Mn, method="spearman") # 0.289 
  cor.test(dt$Zn, dt$Re, method="spearman") # 0.21 pval 0.00126
  
  cor.test(dt$Se, dt$Mn, method="spearman") # 0.407 
  cor.test(dt$Se, dt$Re, method="spearman") # 0.3731
  cor.test(dt$Re, dt$Mn, method="spearman") # 0.498
  
  
  
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



#Factor analysis
{
  
  
  setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final")
  dt <- read.delim("SLT_Combined_ICP_Soil_Plants_08.05.23.txt")
  
  library(factoextra)
  library(FactoMineR)
  
  # Select relevant columns from your dataset
  selected_cols <- dt[, 7:44]
  
  # Perform EFA
  efa_result <- factanal(selected_cols, factors = 5, rotation = "varimax")
  
  # Display factor loadings
  print(efa_result$loadings)
  
  library(lavaan)
  
  # Create a model specification
  model <- '
  # Define latent factors
  f1 =~ Cu_Plant + Cu_Soil + C
  f2 =~ pH + EC + Mo_Soil

  # Define relationships
  f1 ~ f2
'
  
  # Fit the model
  cfa_result <- sem(model, data = dt)
  
  # Display results
  summary(cfa_result)
  
  
  
  
  
  
  
}


#Boxplots and Barplots
{
  setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final")
  dt <- read.delim("Solitude_Plants_Predicted_08.09.23-3reps.txt")
  dt <- dt[dt$Type_of_Sample != "root", ]
  dt <- subset(dt, Site != 'CONTROL')
  dt <- dt[dt$Type_of_Sample != "stem", ]
  
  dt_selected <- dt[dt$Scientific_Name %in% c("Xanthisma gracile", "Pseudognaphalium canescens", "Boechera perennans",
                                              "Nultuma (Prosopis) velutina", "Tamarix chinensis", "Senegalia (Acacia) greggii","Isocoma acradenia"),]
  
  
  Cu <- ggplot(dt_selected, aes(x = reorder(Scientific_Name, Predicted_Cu_ICP, FUN = median),
                                y = Predicted_Cu_ICP, Sceintific_Name = Scientific_Name)) +
    geom_boxplot(linewidth=0.35) +
    geom_point(aes(shape = Plot), size = 2.5) +  # Adjust the size parameter here
    scale_shape_manual(values = c(21, 21, 21, 22)) +
    geom_hline(yintercept = 70, linetype = "dashed", color = "#9a9a9a", size = 0.4) +
    geom_hline(yintercept = 300, linetype = "dotdash", color = "#454545", size = 0.4) +
    scale_y_continuous(limits = c(0, 850), breaks = seq(0, 850, by = 75)) +
    coord_flip() +
    theme_classic()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=12),
          axis.title.x = element_text(size = 15),
          axis.text.y = element_text(size=12, face="italic"),
          axis.title.y = element_blank(),
          legend.key.size = unit(1, "lines"),
          legend.text = element_text(size = 13), 
          legend.title = element_text(size=14, face = "bold"))+
    guides(color = guide_legend(override.aes = list(size = 3.5)),
           shape = guide_legend(override.aes = list(size = 3.5))) +
    ylab("Cu (mg kg-1)")
  Cu
  Fe <- ggplot(dt_selected, aes(x = reorder(Scientific_Name, Fe_concentration, FUN = median),
                                y = Fe_concentration, Sceintific_Name = Scientific_Name)) +
    geom_boxplot(linewidth=0.35) +
    geom_point(aes(shape = Plot), size = 2.5) +  # Adjust the size parameter here
    scale_shape_manual(values = c(21, 21, 21, 22)) +
    scale_y_continuous(limits = c(0, 700), breaks = seq(0, 700, by = 75)) +
    coord_flip() +
    theme_classic()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=12),
          axis.title.x = element_text(size = 15),
          axis.text.y = element_text(size=12, face="italic"),
          axis.title.y = element_blank(),
          legend.key.size = unit(1, "lines"),
          legend.text = element_text(size = 13), 
          legend.title = element_text(size=14, face = "bold"))+
    guides(color = guide_legend(override.aes = list(size = 3.5)),
           shape = guide_legend(override.aes = list(size = 3.5))) +
    ylab("Fe (mg kg-1)")
  Fe
  
  Se <- ggplot(dt_selected, aes(x = reorder(Scientific_Name, Predicted_Se_ICP, FUN = median),
                                y = Predicted_Se_ICP, Sceintific_Name = Scientific_Name)) +
    geom_boxplot(linewidth=0.35) +
    geom_point(aes(shape = Plot), size = 2.5) +  # Adjust the size parameter here
    scale_shape_manual(values = c(21, 21, 21, 22)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
    coord_flip() +
    theme_classic()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=12),
          axis.title.x = element_text(size = 15),
          axis.text.y = element_text(size=12, face="italic"),
          axis.title.y = element_blank(),
          legend.key.size = unit(1, "lines"),
          legend.text = element_text(size = 13), 
          legend.title = element_text(size=14, face = "bold"))+
    guides(color = guide_legend(override.aes = list(size = 3.5)),
           shape = guide_legend(override.aes = list(size = 3.5))) +
    ylab("Se (mg kg-1)")
  Se
  
  Re <- ggplot(dt_selected, aes(x = reorder(Scientific_Name, Predicted_Re_ICP, FUN = median),
                                y = Predicted_Re_ICP, Sceintific_Name = Scientific_Name)) +
    geom_boxplot(linewidth=0.35) +
    geom_point(aes(shape = Plot), size = 2.5) +  # Adjust the size parameter here
    scale_shape_manual(values = c(21, 21, 21, 22)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
    coord_flip() +
    theme_classic()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=12),
          axis.title.x = element_text(size = 15),
          axis.text.y = element_text(size=12, face="italic"),
          axis.title.y = element_blank(),
          legend.key.size = unit(1, "lines"),
          legend.text = element_text(size = 13), 
          legend.title = element_text(size=14, face = "bold"))+
    guides(color = guide_legend(override.aes = list(size = 3.5)),
           shape = guide_legend(override.aes = list(size = 3.5))) +
    ylab("Re (mg kg-1)")
  Re
  
  
  Mn <- ggplot(dt_selected, aes(x = reorder(Scientific_Name, Predicted_Mn_ICP, FUN = median),
                                y = Predicted_Mn_ICP, Sceintific_Name = Scientific_Name)) +
    geom_boxplot(linewidth=0.35) +
    geom_point(aes(shape = Plot), size = 2.5) +  # Adjust the size parameter here
    scale_shape_manual(values = c(21, 21, 21, 22)) +
    scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, by = 20)) +
    coord_flip() +
    theme_classic()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=12),
          axis.title.x = element_text(size = 15),
          axis.text.y = element_text(size=12, face="italic"),
          axis.title.y = element_blank(),
          legend.key.size = unit(1, "lines"),
          legend.text = element_text(size = 13), 
          legend.title = element_text(size=14, face = "bold"))+
    guides(color = guide_legend(override.aes = list(size = 3.5)),
           shape = guide_legend(override.aes = list(size = 3.5))) +
    ylab("Mn (mg kg-1)")
  Mn
  
  
  Zn <- ggplot(dt_selected, aes(x = reorder(Scientific_Name, Predicted_Zn_ICP, FUN = median),
                                y = Predicted_Zn_ICP, Sceintific_Name = Scientific_Name)) +
    geom_boxplot(linewidth=0.35) +
    geom_point(aes(shape = Plot), size = 2.5) +  # Adjust the size parameter here
    scale_shape_manual(values = c(21, 21, 21, 22)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
    coord_flip() +
    theme_classic()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=12),
          axis.title.x = element_text(size = 15),
          axis.text.y = element_text(size=12, face="italic"),
          axis.title.y = element_blank(),
          legend.key.size = unit(1, "lines"),
          legend.text = element_text(size = 13), 
          legend.title = element_text(size=14, face = "bold"))+
    guides(color = guide_legend(override.aes = list(size = 3.5)),
           shape = guide_legend(override.aes = list(size = 3.5))) +
    ylab("Zn (mg kg-1)")
  Zn
  
  
  Cr <- ggplot(dt_selected, aes(x = reorder(Scientific_Name, Predicted_Cr_ICP, FUN = median),
                                y = Predicted_Cr_ICP, Sceintific_Name = Scientific_Name)) +
    geom_boxplot(linewidth=0.35) +
    geom_point(aes(shape = Plot), size = 2.5) +  # Adjust the size parameter here
    scale_shape_manual(values = c(21, 21, 21, 22)) +
    scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1.1, by = 0.25)) +
    coord_flip() +
    theme_classic()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=12),
          axis.title.x = element_text(size = 15),
          axis.text.y = element_text(size=12, face="italic"),
          axis.title.y = element_blank(),
          legend.key.size = unit(1, "lines"),
          legend.text = element_text(size = 13), 
          legend.title = element_text(size=14, face = "bold"))+
    guides(color = guide_legend(override.aes = list(size = 3.5)),
           shape = guide_legend(override.aes = list(size = 3.5))) +
    ylab("Cr (mg kg-1)")
  Cr
  
  As <- ggplot(dt_selected, aes(x = reorder(Scientific_Name, Predicted_As_ICP, FUN = median),
                                y = Predicted_As_ICP, Sceintific_Name = Scientific_Name)) +
    geom_boxplot(linewidth=0.35) +
    geom_point(aes(shape = Plot), size = 2.5) +  # Adjust the size parameter here
    scale_shape_manual(values = c(21, 21, 21, 22)) +
    scale_y_continuous(limits = c(0, 1.25), breaks = seq(0, 1.25, by = 0.25)) +
    coord_flip() +
    theme_classic()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=12),
          axis.title.x = element_text(size = 15),
          axis.text.y = element_text(size=12, face="italic"),
          axis.title.y = element_blank(),
          legend.key.size = unit(1, "lines"),
          legend.text = element_text(size = 13), 
          legend.title = element_text(size=14, face = "bold"))+
    guides(color = guide_legend(override.aes = list(size = 3.5)),
           shape = guide_legend(override.aes = list(size = 3.5))) +
    ylab("As (mg kg-1)")
  As
  
  
  
  
  
  ggarrange(Cu, Fe, Mn, Zn, As, Cr, Re, Se,
            ncol = 2, nrow = 4, 
            common.legend = TRUE, legend = "bottom")
  
  
  
  
  dt_Cu <- dt_selected %>%
    group_by(Plot, Scientific_Name, Form) %>%
    summarize(Median = median(Predicted_Cu_ICP), 
              Mean = mean(Predicted_Cu_ICP), 
              SD = sd(Predicted_Cu_ICP)/sqrt(n())) %>%
    arrange(Median) %>%
    ungroup()
  
  Cu <- ggplot(dt_Cu, aes(x = reorder(fct_rev(Scientific_Name), Median), 
                          y = Mean, fill = Form)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_grid(Plot~., scales = "free_y") +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                  position = position_dodge(width = 0.9),
                  width = 0.2) +
    coord_flip() +
    labs(x = "", y = "Cu (mg/kg)") +
    scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31", "#007555", "#007222","#EDFD00")) +
    theme_bw()
  Cu
  #######
  
  
  
  
}



# Statplot with ggbetweenstats
{
  # Use the custom color palette in the ggbetweenstats plot
  plt <- ggbetweenstats(
    data = dt,
    x = Form,
    y = Predicted_Cu_ICP
  ) +
    scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31", "#007555", "#007222")) 
  
  plt <- ggbetweenstats(
    data = dt,
    x = Duration,
    y = Predicted_Cu_ICP
  ) +
    scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31", "#007555", "#007222")) 
}



#Barplot Control vs Tailings plants only matching!

{
  
  setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final")
  dt <- read.delim("Solitude_Plants_Predicted_TLandCTRL.txt")
  dt <- dt[dt$Type_of_Sample != "root", ]
  dt <- dt[dt$Type_of_Sample != "stem", ]
  
  
  
  # dt_Cu <- dt %>%
  #  group_by(Scientific_Name, Site, Form) %>%
  # summarize(Median = median(Predicted_Cu_ICP), 
  #          Mean = mean(Predicted_Cu_ICP), 
  #         SD = sd(Predicted_Cu_ICP)/sqrt(n())) %>%
  #arrange(Median) %>%
  #ungroup()
  
  #  Cu <- ggplot(dt_Cu, aes(x = reorder(Scientific_Name, Median), 
  #                         y = Mean, fill = Site, color = Form)) +
  #  geom_bar(stat = "identity", position = "dodge") +
  # geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
  #              position = position_dodge(width = 0.9),
  #             width = 0.25,
  #            size = 0.5) +
  #scale_fill_manual(values = c("#643A6B", "#068DA9"), 
  #                 guide = guide_legend(override.aes = list(pattern = c(1, 1)))) +
  #scale_color_manual(values = c("#643A6B", "#068DA9", "#34495E", "#B0A4A4")) +
  #coord_flip() +
  #labs(x = "", y = "Cu (mg/kg)") +
  #theme_bw() +
  #theme(legend.key.size = unit(1, "lines"),
  #      legend.text = element_text(size = 13.5), 
  #      legend.title = element_text(size = 15, face = "bold"))
  #  
  # Cu
  
  #dt_Mn <- dt %>%
  # group_by(Scientific_Name, Site, Form) %>%
  #summarize(Median = median(Predicted_Mn_ICP), 
  #         Mean = mean(Predicted_Mn_ICP), 
  #        SD = sd(Predicted_Mn_ICP)/sqrt(n())) %>%
  #arrange(Median) %>%
  # ungroup()
  
  # Mn <- ggplot(dt_Mn, aes(x = reorder(Scientific_Name, Median), 
  ##                         y = Mean, fill = Site, color = Form)) +
  #   geom_bar(stat = "identity", position = "dodge") +
  #   geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
  #                 position = position_dodge(width = 0.9),
  #                 width = 0.25,
  #                 size = 0.5) +
  #  scale_fill_manual(values = c("#643A6B", "#068DA9"), 
  ##                     guide = guide_legend(override.aes = list(pattern = c(1, 1)))) +
  #   scale_color_manual(values = c("#643A6B", "#068DA9", "#34495E", "#B0A4A4")) +
  #   coord_flip() +
  #  labs(x = "", y = "Mn (mg/kg)") +
  #  theme_bw() +
  #  theme(legend.key.size = unit(1, "lines"),
  #       legend.text = element_text(size = 13.5), 
  #       legend.title = element_text(size = 15, face = "bold"))
  
  # Mn
  
  
  
  
  
  
  
  library(dplyr)
  library(ggplot2)
  
  
  # with empty bars
  dt_Cu <- dt %>%
    group_by(Scientific_Name, Site) %>%
    summarize(
      Median = median(Predicted_Cu_ICP),
      Mean = mean(Predicted_Cu_ICP),
      SD = sd(Predicted_Cu_ICP) / sqrt(n())
    ) %>%
    arrange(Median) %>%
    ungroup()
  
  Cu <- ggplot(dt_Cu, aes(x = reorder(Scientific_Name, Median), 
                          y = Mean, fill=Site)) +
    geom_bar(stat = "identity", position = "dodge", size=0.3, # Position bars next to each other
             color = "black",    # Border color of the bars
             # Setting fill to white for empty bars
    ) +
    
    geom_errorbar(
      aes(ymin = Mean - SD, ymax = Mean + SD),
      position = position_dodge(width = 0.9),
      width = 0.3,
      size = 0.3
    ) +
    scale_fill_manual(values = c("white", "white")) +
    coord_flip() +
    labs(x = "", y = "Cu (mg/kg)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.key.size = unit(1, "lines"),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8, face = "bold")
    )
  
  Cu
  
  
  # with empty bars
  dt_Fe <- dt %>%
    group_by(Scientific_Name, Site) %>%
    summarize(
      Median = median(Predicted_Cu_ICP),
      Mean = mean(Fe_concentration),
      SD = sd(Fe_concentration) / sqrt(n())
    ) %>%
    arrange(Median) %>%
    ungroup()
  
  Fe <- ggplot(dt_Fe, aes(x = reorder(Scientific_Name, Median), 
                          y = Mean, fill=Site)) +
    geom_bar(stat = "identity", position = "dodge", size=0.3, # Position bars next to each other
             color = "black",    # Border color of the bars
             # Setting fill to white for empty bars
    ) +
    
    geom_errorbar(
      aes(ymin = Mean - SD, ymax = Mean + SD),
      position = position_dodge(width = 0.9),
      width = 0.3,
      size = 0.3
    ) +
    scale_fill_manual(values = c("white", "white")) +
    coord_flip() +
    labs(x = "", y = "Fe (mg/kg)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.key.size = unit(1, "lines"),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8, face = "bold")
    )
  
  Fe
  
  
  
  
  # with empty bars
  dt_Mn <- dt %>%
    group_by(Scientific_Name, Site) %>%
    summarize(
      Median = median(Predicted_Cu_ICP),
      Mean = mean(Predicted_Mn_ICP),
      SD = sd(Predicted_Mn_ICP) / sqrt(n())
    ) %>%
    arrange(Median) %>%
    ungroup()
  
  Mn <- ggplot(dt_Mn, aes(x = reorder(Scientific_Name, Median), 
                          y = Mean, fill=Site)) +
    geom_bar(stat = "identity", position = "dodge", size=0.3, # Position bars next to each other
             color = "black",    # Border color of the bars
             # Setting fill to white for empty bars
    ) +
    
    geom_errorbar(
      aes(ymin = Mean - SD, ymax = Mean + SD),
      position = position_dodge(width = 0.9),
      width = 0.3,
      size = 0.3
    ) +
    scale_fill_manual(values = c("white", "white")) +
    coord_flip() +
    labs(x = "", y = "Mn (mg/kg)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.key.size = unit(1, "lines"),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8, face = "bold")
    )
  
  Mn
  
  # with empty bars
  dt_Zn <- dt %>%
    group_by(Scientific_Name, Site) %>%
    summarize(
      Median = median(Predicted_Cu_ICP),
      Mean = mean(Predicted_Zn_ICP),
      SD = sd(Predicted_Zn_ICP) / sqrt(n())
    ) %>%
    arrange(Median) %>%
    ungroup()
  
  Zn <- ggplot(dt_Zn, aes(x = reorder(Scientific_Name, Median), 
                          y = Mean, fill=Site)) +
    geom_bar(stat = "identity", position = "dodge", size=0.3, # Position bars next to each other
             color = "black",    # Border color of the bars
             # Setting fill to white for empty bars
    ) +
    
    geom_errorbar(
      aes(ymin = Mean - SD, ymax = Mean + SD),
      position = position_dodge(width = 0.9),
      width = 0.3,
      size = 0.3
    ) +
    scale_fill_manual(values = c("white", "white")) +
    coord_flip() +
    labs(x = "", y = "Zn (mg/kg)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.key.size = unit(1, "lines"),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8, face = "bold")
    )
  
  Zn
  
  
  
  # with empty bars
  dt_As <- dt %>%
    group_by(Scientific_Name, Site) %>%
    summarize(
      Median = median(Predicted_Cu_ICP),
      Mean = mean(Predicted_As_ICP),
      SD = sd(Predicted_As_ICP) / sqrt(n())
    ) %>%
    arrange(Median) %>%
    ungroup()
  
  As <- ggplot(dt_As, aes(x = reorder(Scientific_Name, Median), 
                          y = Mean, fill=Site)) +
    geom_bar(stat = "identity", position = "dodge", size=0.3, # Position bars next to each other
             color = "black",    # Border color of the bars
             # Setting fill to white for empty bars
    ) +
    
    geom_errorbar(
      aes(ymin = Mean - SD, ymax = Mean + SD),
      position = position_dodge(width = 0.9),
      width = 0.3,
      size = 0.3
    ) +
    scale_fill_manual(values = c("white", "white")) +
    coord_flip() +
    labs(x = "", y = "As (mg/kg)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.key.size = unit(1, "lines"),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8, face = "bold")
    )
  
  As
  
  
  
  # with empty bars
  dt_Cr <- dt %>%
    group_by(Scientific_Name, Site) %>%
    summarize(
      Median = median(Predicted_Cu_ICP),
      Mean = mean(Predicted_Cr_ICP),
      SD = sd(Predicted_Cr_ICP) / sqrt(n())
    ) %>%
    arrange(Median) %>%
    ungroup()
  
  Cr <- ggplot(dt_Cr, aes(x = reorder(Scientific_Name, Median), 
                          y = Mean, fill=Site)) +
    geom_bar(stat = "identity", position = "dodge", size=0.3, # Position bars next to each other
             color = "black",    # Border color of the bars
             # Setting fill to white for empty bars
    ) +
    
    geom_errorbar(
      aes(ymin = Mean - SD, ymax = Mean + SD),
      position = position_dodge(width = 0.9),
      width = 0.3,
      size = 0.3
    ) +
    scale_fill_manual(values = c("white", "white")) +
    coord_flip() +
    labs(x = "", y = "Cr (mg/kg)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.key.size = unit(1, "lines"),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8, face = "bold")
    )
  
  Cr
  
  
  
  # with empty bars
  dt_Se <- dt %>%
    group_by(Scientific_Name, Site) %>%
    summarize(
      Median = median(Predicted_Cu_ICP),
      Mean = mean(Predicted_Se_ICP),
      SD = sd(Predicted_Se_ICP) / sqrt(n())
    ) %>%
    arrange(Median) %>%
    ungroup()
  
  Se <- ggplot(dt_Se, aes(x = reorder(Scientific_Name, Median), 
                          y = Mean, fill=Site)) +
    geom_bar(stat = "identity", position = "dodge", size=0.3, # Position bars next to each other
             color = "black",    # Border color of the bars
             # Setting fill to white for empty bars
    ) +
    
    geom_errorbar(
      aes(ymin = Mean - SD, ymax = Mean + SD),
      position = position_dodge(width = 0.9),
      width = 0.3,
      size = 0.3
    ) +
    scale_fill_manual(values = c("white", "white")) +
    coord_flip() +
    labs(x = "", y = "Se (mg/kg)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.key.size = unit(1, "lines"),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8, face = "bold")
    )
  
  Se
  
  # with empty bars
  dt_Re <- dt %>%
    group_by(Scientific_Name, Site) %>%
    summarize(
      Median = median(Predicted_Cu_ICP),
      Mean = mean(Predicted_Re_ICP),
      SD = sd(Predicted_Re_ICP) / sqrt(n())
    ) %>%
    arrange(Median) %>%
    ungroup()
  
  Re <- ggplot(dt_Re, aes(x = reorder(Scientific_Name, Median), 
                          y = Mean, fill=Site)) +
    geom_bar(stat = "identity", position = "dodge", size=0.3, # Position bars next to each other
             color = "black",    # Border color of the bars
             # Setting fill to white for empty bars
    ) +
    
    geom_errorbar(
      aes(ymin = Mean - SD, ymax = Mean + SD),
      position = position_dodge(width = 0.9),
      width = 0.3,
      size = 0.3
    ) +
    scale_fill_manual(values = c("white", "white")) +
    coord_flip() +
    labs(x = "", y = "Se (mg/kg)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.key.size = unit(1, "lines"),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8, face = "bold")
    )
  
  Re
  
}



# All plants barplots with lines!!!

{
  
  setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final")
  dt <- read.delim("Solitude_Plants_Predicted_08.09.23-3reps.txt")
  dt <- dt[dt$Type_of_Sample != "root", ]
  dt <- dt[dt$Site != "CONTROL", ]
  dt <- dt[dt$Type_of_Sample != "stem", ]
  
  #Cu
  
  colors-to-use = c("#454545", "#9a9a9a", "#F9B4B4", "#003f5c", "#AD0B0B")
  
  dt_Cu <- dt %>%
    group_by(Scientific_Name, Site) %>%
    summarize(Median = median(Predicted_Cu_ICP), 
              Mean = mean(Predicted_Cu_ICP), 
              SD = sd(Predicted_Cu_ICP)/sqrt(n())) %>%
    arrange(Median) %>%
    ungroup()
  
  Cu <- ggplot(dt_Cu, aes(x = reorder(Scientific_Name, Median), 
                          y = Mean, fill = Site )) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.01), size=0.22, color = "black") +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                  position = position_dodge(width = 0.85),
                  width = 0.25,
                  size = 0.25) +
    geom_hline(yintercept = 5, linetype = "dashed", color = "#AD0B0B", size = 0.3) +
    geom_hline(yintercept = 20, linetype = "twodash", color = "#003f5c", size = 0.3) +
    scale_fill_manual(values = c("lightgrey")) +
    scale_y_continuous(limits = c(0, 405), breaks = seq(0, 405, by = 100), expand = c(0, 9)) +
    scale_x_discrete(expand = c(0, 0.8)) +  
    coord_flip() +
    labs(x = "", y = "Cu (mg/kg)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7),
          legend.key.size = unit(1, "lines"),
          legend.text = element_text(size = 4),
          legend.title = element_text(size = 4, face = "bold"))
  
  Cu
  
  #Mn
  dt_Mn <- dt %>%
    group_by(Scientific_Name, Site) %>%
    summarize(Median = median(Predicted_Cu_ICP), 
              Mean = mean(Predicted_Mn_ICP), 
              SD = sd(Predicted_Mn_ICP)/sqrt(n())) %>%
    arrange(Median) %>%
    ungroup()
  
  Mn <- ggplot(dt_Mn, aes(x = reorder(Scientific_Name, Median), 
                          y = Mean, fill = Site )) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.01), size=0.22, color = "black") +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                  position = position_dodge(width = 0.85),
                  width = 0.25,
                  size = 0.25) +
    geom_hline(yintercept = 20, linetype = "dashed", color = "#AD0B0B", size = 0.3) +
    scale_fill_manual(values = c("lightgrey")) +
    scale_y_continuous(limits = c(0, 180), breaks = seq(0, 180, by = 40), expand = c(0, 4)) +
    scale_x_discrete(expand = c(0, 0.8)) +  
    coord_flip() +
    labs(x = "", y = "Mn (mg/kg)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7),
          legend.key.size = unit(1, "lines"),
          legend.text = element_text(size = 4),
          legend.title = element_text(size = 4, face = "bold"))
  
  Mn
  
  #Zn
  dt_Zn <- dt %>%
    group_by(Scientific_Name, Site) %>%
    summarize(Median = median(Predicted_Cu_ICP), 
              Mean = mean(Predicted_Zn_ICP), 
              SD = sd(Predicted_Zn_ICP)/sqrt(n())) %>%
    arrange(Median) %>%
    ungroup()
  
  Zn <- ggplot(dt_Zn, aes(x = reorder(Scientific_Name, Median), 
                          y = Mean, fill = Site )) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.01), size=0.22, color = "black") +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                  position = position_dodge(width = 0.85),
                  width = 0.25,
                  size = 0.25) +
    geom_hline(yintercept = 20, linetype = "dashed", color = "#AD0B0B", size = 0.3) +
    geom_hline(yintercept = 100, linetype = "twodash", color = "#003f5c", size = 0.3) +
    scale_fill_manual(values = c("lightgrey")) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25), expand = c(0, 2.5)) +
    scale_x_discrete(expand = c(0, 0.8)) +  
    coord_flip() +
    labs(x = "", y = "Zn (mg/kg)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7),
          legend.key.size = unit(1, "lines"),
          legend.text = element_text(size = 4),
          legend.title = element_text(size = 4, face = "bold"))
  
  Zn
  
  #Fe
  dt_Fe <- dt %>%
    group_by(Scientific_Name, Site) %>%
    summarize(Median = median(Predicted_Cu_ICP), 
              Mean = mean(Fe_concentration), 
              SD = sd(Fe_concentration)/sqrt(n())) %>%
    arrange(Median) %>%
    ungroup()
  
  Fe <- ggplot(dt_Fe, aes(x = reorder(Scientific_Name, Median), 
                          y = Mean, fill = Site )) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.01), size=0.22, color = "black") +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                  position = position_dodge(width = 0.85),
                  width = 0.25,
                  size = 0.25) +
    geom_hline(yintercept = 30, linetype = "dashed", color = "#AD0B0B", size = 0.3) +
    geom_hline(yintercept = 500, linetype = "twodash", color = "#003f5c", size = 0.3) +
    scale_fill_manual(values = c("lightgrey")) +
    scale_y_continuous(limits = c(0, 1500), breaks = seq(0, 1500, by = 350), expand = c(0, 35)) +
    scale_x_discrete(expand = c(0, 0.8)) +  
    coord_flip() +
    labs(x = "", y = "Fe (mg/kg)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7),
          legend.key.size = unit(1, "lines"),
          legend.text = element_text(size = 4),
          legend.title = element_text(size = 4, face = "bold"))
  
  Fe
  
  
  #Se
  dt_Se <- dt %>%
    group_by(Scientific_Name, Site) %>%
    summarize(Median = median(Predicted_Cu_ICP), 
              Mean = mean(Predicted_Se_ICP), 
              SD = sd(Predicted_Se_ICP)/sqrt(n())) %>%
    arrange(Median) %>%
    ungroup()
  
  Se <- ggplot(dt_Se, aes(x = reorder(Scientific_Name, Median), 
                          y = Mean, fill = Site )) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.01), size=0.22, color = "black") +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                  position = position_dodge(width = 0.85),
                  width = 0.25,
                  size = 0.25) +
    #geom_hline(yintercept = 30, linetype = "dashed", color = "#AD0B0B", size = 0.3) +
    geom_hline(yintercept = 5, linetype = "twodash", color = "#003f5c", size = 0.3) +
    scale_fill_manual(values = c("lightgrey")) +
    scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 15), expand = c(0, 1.4)) +
    scale_x_discrete(expand = c(0, 0.8)) +  
    coord_flip() +
    labs(x = "", y = "Se (mg/kg)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7),
          legend.key.size = unit(1, "lines"),
          legend.text = element_text(size = 4),
          legend.title = element_text(size = 4, face = "bold"))
  
  Se
  
  #Re
  dt_Re <- dt %>%
    group_by(Scientific_Name, Site) %>%
    summarize(Median = median(Predicted_Cu_ICP), 
              Mean = mean(Predicted_Re_ICP), 
              SD = sd(Predicted_Re_ICP)/sqrt(n())) %>%
    arrange(Median) %>%
    ungroup()
  
  Re <- ggplot(dt_Re, aes(x = reorder(Scientific_Name, Median), 
                          y = Mean, fill = Site )) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.01), size=0.22, color = "black") +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                  position = position_dodge(width = 0.85),
                  width = 0.25,
                  size = 0.25) +
    #geom_hline(yintercept = 30, linetype = "dashed", color = "#AD0B0B", size = 0.3) +
    geom_hline(yintercept = 5, linetype = "twodash", color = "#003f5c", size = 0.3) +
    scale_fill_manual(values = c("lightgrey")) +
    scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 15), expand = c(0, 1.4)) +
    scale_x_discrete(expand = c(0, 0.8)) +  
    coord_flip() +
    labs(x = "", y = "Re (mg/kg)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7),
          legend.key.size = unit(1, "lines"),
          legend.text = element_text(size = 4),
          legend.title = element_text(size = 4, face = "bold"))
  
  Re
  
  
  
  
  #########################
  #######################
  ########################
  #####################
  ###############
  
  #Testujemy po medianie
  
  
  dt_Cu <- dt %>%
    group_by(Scientific_Name, Site) %>%
    summarize(Median = median(Predicted_Cu_ICP), 
              Mean = mean(Predicted_Cu_ICP), 
              SD = sd(Predicted_Cu_ICP)/sqrt(n())) %>%
    arrange(Median) %>%
    ungroup()
  
  Cu <- ggplot(dt_Cu, aes(x = reorder(Scientific_Name, Median), 
                          y = Median, fill = Site )) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.01), size=0.22, color = "black") +
    geom_errorbar(aes(ymin = Median - SD, ymax = Median + SD),
                  position = position_dodge(width = 0.85),
                  width = 0.25,
                  size = 0.25) +
    geom_hline(yintercept = 5, linetype = "dashed", color = "#AD0B0B", size = 0.3) +
    geom_hline(yintercept = 20, linetype = "twodash", color = "#003f5c", size = 0.3) +
    scale_fill_manual(values = c("lightgrey")) +
    scale_y_continuous(limits = c(0, 405), breaks = seq(0, 405, by = 100), expand = c(0, 9)) +
    scale_x_discrete(expand = c(0, 0.8)) +  
    coord_flip() +
    labs(x = "", y = "Cu (mg/kg)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7),
          legend.key.size = unit(1, "lines"),
          legend.text = element_text(size = 4),
          legend.title = element_text(size = 4, face = "bold"))
  
  Cu
  
  #Fe
  dt_Fe <- dt %>%
    group_by(Scientific_Name, Site) %>%
    summarize(Median = median(Fe_concentration), 
              Median2 = median(Predicted_Cu_ICP),
              Mean = mean(Fe_concentration), 
              SD = sd(Fe_concentration)/sqrt(n())) %>%
    arrange(Median) %>%
    ungroup()
  
  Fe <- ggplot(dt_Fe, aes(x = reorder(Scientific_Name, Median2), 
                          y = Median, fill = Site )) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.01), size=0.22, color = "black") +
    geom_errorbar(aes(ymin = Median - SD, ymax = Median + SD),
                  position = position_dodge(width = 0.85),
                  width = 0.25,
                  size = 0.25) +
    geom_hline(yintercept = 30, linetype = "dashed", color = "#AD0B0B", size = 0.3) +
    geom_hline(yintercept = 500, linetype = "twodash", color = "#003f5c", size = 0.3) +
    scale_fill_manual(values = c("lightgrey")) +
    scale_y_continuous(limits = c(0, 1500), breaks = seq(0, 1500, by = 350), expand = c(0, 35)) +
    scale_x_discrete(expand = c(0, 0.8)) +  
    coord_flip() +
    labs(x = "", y = "Fe (mg/kg)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7),
          legend.key.size = unit(1, "lines"),
          legend.text = element_text(size = 4),
          legend.title = element_text(size = 4, face = "bold"))
  
  Fe
  
  dt_Mn <- dt %>%
    group_by(Scientific_Name, Site) %>%
    summarize(Median = median(Predicted_Mn_ICP),
              Median2 = median(Predicted_Cu_ICP), 
              Mean = mean(Predicted_Mn_ICP), 
              SD = sd(Predicted_Mn_ICP)/sqrt(n())) %>%
    arrange(Median) %>%
    ungroup()
  
  Mn <- ggplot(dt_Mn, aes(x = reorder(Scientific_Name, Median2), 
                          y = Median, fill = Site )) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.01), size=0.22, color = "black") +
    geom_errorbar(aes(ymin = Median - SD, ymax = Median + SD),
                  position = position_dodge(width = 0.85),
                  width = 0.25,
                  size = 0.25) +
    geom_hline(yintercept = 20, linetype = "dashed", color = "#AD0B0B", size = 0.3) +
    scale_fill_manual(values = c("lightgrey")) +
    scale_y_continuous(limits = c(0, 180), breaks = seq(0, 180, by = 40), expand = c(0, 4)) +
    scale_x_discrete(expand = c(0, 0.8)) +  
    coord_flip() +
    labs(x = "", y = "Mn (mg/kg)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7),
          legend.key.size = unit(1, "lines"),
          legend.text = element_text(size = 4),
          legend.title = element_text(size = 4, face = "bold"))
  
  Mn
  
}


#Counts species across plots - Figure
{
  setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final")
  dt <- read.delim("Solitude_Plants_Predicted_08.09.23-3reps.txt")
  dt <- subset(dt, Site != 'CONTROL')
  
  
  #Species distr figure - Robic C1 i C2 jako 1?
  ggplot(dt, aes(x = Plot, y = reorder(Scientific_Name, table(Scientific_Name)[Scientific_Name]), group = Form)) +
    geom_point(shape = 4, size = 1.45, color = "black", stroke = 1)+  # specify shape argument as 4 for X symbol and customize size, color, and stroke
    facet_grid(Form ~ ., scales = "free_y", space = "free_y") +
    labs(x = "Plot", color = "Form") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_text(size=12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_text(size=12, face="italic"),
          strip.text = element_text(size = 14, face = "bold"))
  
}


# Mean of concentrations for Re - no control, root or stem
{
  
  setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final")
  dt <- read.delim("Solitude_Plants_Predicted_08.09.23-3reps.txt")
  dt <- dt[dt$Type_of_Sample != "root", ]
  dt <- subset(dt, Site != 'CONTROL')
  dt <- dt[dt$Type_of_Sample != "stem", ]
  dt_T <- subset(dt, Site == "TAILINGS")
  dt_C <- subset(dt, Site == "CONTROL")
  
  Re <- ggplot(dt, aes(x = reorder(Scientific_Name, Predicted_Se_ICP, FUN = mean),
                       y = Predicted_Se_ICP, Sceintific_Name = Scientific_Name)) +
    geom_boxplot(linewidth=0.35) +
    geom_point(aes(shape = Plot), size = 2.5) +  # Adjust the size parameter here
    scale_shape_manual(values = c(21, 21, 21, 22)) +
    #scale_y_continuous(limits = c(0, 1.25), breaks = seq(0, 1.25, by = 0.25)) +
    coord_flip() +
    theme_classic()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=12),
          axis.title.x = element_text(size = 15),
          axis.text.y = element_text(size=12, face="italic"),
          axis.title.y = element_blank(),
          legend.key.size = unit(1, "lines"),
          legend.text = element_text(size = 13), 
          legend.title = element_text(size=14, face = "bold"))+
    guides(color = guide_legend(override.aes = list(size = 3.5)),
           shape = guide_legend(override.aes = list(size = 3.5))) +
    ylab("Se (mg kg-1)")
  Re
  
  
  subset_data <- dt[dt$Scientific_Name == "Isocoma acradenia", ]
  mean(subset_data$Predicted_Re_ICP)
  sd(subset_data$Predicted_Re_ICP) / sqrt(nrow(subset_data))
  
  subset_data <- dt[dt$Scientific_Name == "Isocoma pluriflora", ]
  mean(subset_data$Predicted_Re_ICP)
  sd(subset_data$Predicted_Re_ICP) / sqrt(nrow(subset_data))
  
  subset_data <- dt[dt$Scientific_Name == "Senegalia (Acacia) greggii", ]
  mean(subset_data$Predicted_Re_ICP)
  sd(subset_data$Predicted_Re_ICP) / sqrt(nrow(subset_data))
  
  
  subset_data <- dt[dt$Scientific_Name == "Isocoma acradenia", ]
  mean(subset_data$Predicted_Se_ICP)
  sd(subset_data$Predicted_Se_ICP) / sqrt(nrow(subset_data))
  
  subset_data <- dt[dt$Scientific_Name == "Tamarix chinensis", ]
  mean(subset_data$Predicted_Se_ICP)
  sd(subset_data$Predicted_Se_ICP) / sqrt(nrow(subset_data))
  
  subset_data <- dt[dt$Scientific_Name == "Senegalia (Acacia) greggii", ]
  mean(subset_data$Predicted_Se_ICP)
  sd(subset_data$Predicted_Se_ICP) / sqrt(nrow(subset_data))
  
  subset_data <- dt[dt$Scientific_Name == "Populus fremontii", ]
  mean(subset_data$Predicted_Se_ICP)
  sd(subset_data$Predicted_Se_ICP) / sqrt(nrow(subset_data))
}


# RT ~ Total Wieght Plot

{
  
  setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final")
  dt <- read.delim("Solitude_Plants_Predicted_08.09.23-3reps.txt")
  
  a1 <- ggplot(dt, aes(y = Substrate_RT, x = Total_Weight)) +
    geom_point(size = 2.1, stroke = 1) +
    geom_smooth(method = "lm", se = TRUE, color = "#AD0B0B", fill = "lightblue") + 
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "#AD0B0B", fill = "lightblue") +
    labs(x = "Total Weight (TW)", y = "Relative thickness (RT)") +
    theme_classic2() +
    theme(panel.grid.major = element_line(), panel.grid.minor = element_line(),
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 20),
          axis.text.x = element_text(size = 16),
          axis.title.x = element_text(size = 20),
          axis.text.y = element_text(size = 16),
          axis.title.y = element_text(size = 20),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 16, face = "bold"),
          legend.position = "top")
  a1
  
  cor(dt$Total_Weight, dt$Substrate_RT, method = "spearman") # 0.8826
  
  shapiro.test(dt$Total_Weight) # non norm
  shapiro.test(dt$Substrate_RT) # non norm
  
  
  lm1<- lm(Substrate_RT ~ Total_Weight, data = dt)
  summary(lm1) # R squared 0.7451, p.val < 0.0001
  
  # Quadratic model
  lm2 <- lm(Substrate_RT ~ poly(Total_Weight, 2), data = dt)
  summary(lm2) # R squared 0.7952, p.val < 0.0001
  
  
}

# Add Fe as a predicted not RAW pXRF
{
setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final")
dt <- read.delim("Solitude_Plants_Predicted_08.09.23-3reps.txt")


dt$Fe_concentration[dt$Fe_concentration == 2.5] <- 0
apply_equation_without_zeros <- function(fe, substrate) {
  if (is.numeric(fe) && is.numeric(substrate) && fe != 0) {
    return(28.88747 + (1.41673 * fe) + (-316.95475 * substrate))
  } else {
    return(0)  # Set to NA to indicate exclusion
  }
}

dt$Predicted_Fe_ICP <- mapply(apply_equation_without_zeros, dt$Fe_concentration, dt$Substrate_RT)

dt$Predicted_Fe_ICP[dt$Predicted_Fe_ICP == 0] <- 2.5

write.table(dt, file='C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final/SLT_Final_3reps.09.06.23.csv', sep=",", row.names = F)

}

