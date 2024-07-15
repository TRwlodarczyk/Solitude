#NIST2





{
  library(ggplot2)
  library(ggpubr)
  library(dplyr)
  library(data.table)
  library(reshape2)
  library(reshape)
  library("readxl")
  library(ggpubr)
  library(agricolae)
  library(tidyverse)
  library (readr) #to read URL
  library(stringr) # For str_replace_all
  library(psych)
  library(car)
  library(openxlsx)
  
}


setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/NIST-test")


#Mean with SD and SE
{
  
  dt <-read.delim("NIST_FINAL_May24.txt")
  dt1 <- dt %>% 
    filter(Sample != "QA")
  
  
  # Remove NDs
  
  {
    # Replace "ND" with 0 in columns 9 to 32
    for (i in 9:32) {
      dt1[, i] <- gsub(".*ND.*", 0, dt1[, i])
    }
    
    # Preserve columns 1 to 9 and 33 to 57
    dt1_preserved <- dt1[, c(1:8, 33:57)]
    
    # Transform to dataframe
    dt1 <- as.data.frame(dt1)
    
    # Change character to numeric in columns 9 to 32
    dt1[, 9:32] <- sapply(dt1[, 9:32], as.numeric)
    
    # Combine preserved columns with modified columns
    dt1 <- cbind(dt1_preserved, dt1[, 9:32])
    }
  
  
  #apply LODs
  {
    
    dt1$Ca[dt1$Ca == 0] <- NA
    dt1$Ti[dt1$Ti == 0] <- NA
    dt1$Cr[dt1$Cr == 0] <- NA
    dt1$Mn[dt1$Mn == 0] <- NA
    dt1$Fe[dt1$Fe == 0] <- NA
    dt1$Co[dt1$Co == 0] <- NA
    dt1$Ni[dt1$Ni == 0] <- NA
    dt1$Cu[dt1$Cu == 0] <- NA
    dt1$Zn[dt1$Zn == 0] <- NA
    dt1$As[dt1$As == 0] <- NA
    dt1$Se[dt1$Se == 0] <- NA
    dt1$Cd[dt1$Cd == 0] <- NA
    dt1$Re[dt1$Re == 0] <- NA
  }
  
  
  library(dplyr)
  
  # Assuming dt3 is your dataset and columns are named appropriately
  element_columns <- c("P", "S", "K", "Ca", "Mn", "Fe", "Ni", "Cu", "Zn", "As", "Se", "Re")
  metadata_columns <- c("Sample", "Sample_ID", "Date", "File", "Total_Weight", "Method", "Material", "Optimization", "P_NIST", "S_NIST", "K_NIST", "Ca_NIST", "Mn_NIST", "Fe_NIST", "Ni_NIST", "Cu_NIST", "Zn_NIST", "As_NIST", "Se_NIST", "Re_NIST")
  
  # Function to calculate standard error
  se <- function(x) {
    sd(x, na.rm = TRUE) / sqrt(length(na.omit(x)))
  }
  
  # Compute the mean, SD, and SE for each element within the specified groups and add metadata
  dt_stats <- dt1 %>%
    group_by(Sample_ID, Total_Weight, Method, Optimization) %>%
    summarise(across(all_of(element_columns), list(mean = ~ mean(.x, na.rm = TRUE),
                                                   sd = ~ sd(.x, na.rm = TRUE),
                                                   se = ~ se(.x)), .names = "{.col}_{.fn}"), .groups = "drop") %>%
    # Ensure each group is represented once for the join to prevent duplicates
    distinct(Sample_ID, Total_Weight, Method, Optimization, .keep_all = TRUE) %>%
    # Join metadata based on the most frequent occurrence or a specific entry for each group
    left_join(dt1 %>%
                select(Sample_ID, Total_Weight, Method, Optimization, all_of(metadata_columns)) %>%
                group_by(Sample_ID, Total_Weight, Method, Optimization) %>%
                slice(1), # Assuming the first row is representative for the metadata
              by = c("Sample_ID", "Total_Weight", "Method", "Optimization"))
  
  # Checking if all 'Method' types are present
  print(unique(dt_stats$Method))
  
  # Check the resulting dataframe
  print(dt_stats)
  
  # Write the dataframe to an Excel file
  #write.xlsx(dt_stats, 'NISTS_May24-MEAN.xlsx')
  
  
  
}


#GRAPH FOR PAPER DIfference between nist reference and pxrf nist measurement

# FINAL PLOT - Cu
{
  # Calculate Cu_diff for the entire dataset
  dt_stats <- dt_stats %>%
    mutate(Cu_diff =  Cu_NIST - Cu_mean, ##########!!!!!!!!!!!!!!!podmienilame miejscami!!
           Total_Weight = as.factor(Total_Weight))  # Convert Total_Weight to factor
  
  # Aggregate data to ensure a single entry per Sample_ID, Total_Weight, Method, and Optimization
  agg_data <- dt_stats %>%
    group_by(Sample_ID, Total_Weight, Method, Optimization) %>%
    summarise(Cu_diff = mean(Cu_diff, na.rm = TRUE),
              Cu_se = mean(Cu_se, na.rm = TRUE),
              .groups = "drop")
  
  # Assign colors based on specific conditions
  agg_data <- agg_data %>%
    mutate(genecolour = case_when(
      Method == 'point' & Optimization == 'T1.5' ~ '#FEB941',
      Method == 'point' & Optimization == 'T6' ~ '#4793AF',
      Method == 'cup' & Optimization == 'T6' ~ '#8B322C',
      TRUE ~ 'black'
    ))
  
  # Add width based on the number of groups for each Total_Weight
  agg_data <- agg_data %>%
    group_by(Total_Weight) %>%
    mutate(width = 0.1 * n())
  
  # Sort the data to ensure T6 is plotted last
  agg_data <- agg_data %>%
    arrange(Total_Weight, desc(Optimization == 'T6'))
  
  # Manually set shapes to ensure a cross instead of a plus sign
  shape_values <- c(16, 17, 18, 4, 15, 8)  # 4 represents a cross
  names(shape_values) <- levels(agg_data$Sample_ID)
  
  # Create the plot with all points and color specific categories
  pos <- position_dodge(width = 0.75)
  diffplot <- ggplot(agg_data, aes(x = Total_Weight, y = Cu_diff, shape = Sample_ID)) +
    geom_point(aes(colour = genecolour), position = pos, size = 2.5) +  # Plot all points with specific colors
    geom_errorbar(aes(ymin = Cu_diff - Cu_se, ymax = Cu_diff + Cu_se, colour = genecolour, width = width),
                  position = pos, linewidth = 0.2) +  # Error bars with specific colors and dynamic width
    geom_hline(yintercept = 0, color = "darkgrey", size = 0.6) +  # Horizontal line at 0
    scale_y_continuous(limits = c(-3, 6), breaks = seq(-3, 6, by = 3)) +
    scale_shape_manual(values = shape_values) +  # Manually set shapes
    scale_colour_identity() +  # Use colors directly from the dataframe
    labs(x = "Total Weight", y = "Relative Difference (PXRF - NIST)", shape = "CRM:") +
    theme_classic2()
  
  # Display the plot
  print(diffplot)
  
  # Save the plot with specified dimensions
  ggsave("diffplot-Cu.pdf", plot = diffplot, width = 10, height = 2.5, units = "in")
  
  ggsave("diffplot.pdf", plot = diffplot, width = 10, height = 2, units = "in")
  
  ggsave("diffplot4-Cu.pdf", plot = diffplot, width = 10, height = 1.9, units = "in")
  
}

# FINAL PLOT - Zn
{
  # Calculate Cu_diff for the entire dataset
  dt_stats <- dt_stats %>%
    mutate(Zn_diff = Zn_NIST - Zn_mean,
           Total_Weight = as.factor(Total_Weight))  # Convert Total_Weight to factor
  
  # Aggregate data to ensure a single entry per Sample_ID, Total_Weight, Method, and Optimization
  agg_data <- dt_stats %>%
    group_by(Sample_ID, Total_Weight, Method, Optimization) %>%
    summarise(Zn_diff = mean(Zn_diff, na.rm = TRUE),
              Zn_se = mean(Zn_se, na.rm = TRUE),
              .groups = "drop")
  
  # Assign colors based on specific conditions
  agg_data <- agg_data %>%
    mutate(genecolour = case_when(
      Method == 'point' & Optimization == 'T1.5' ~ '#FEB941',
      Method == 'point' & Optimization == 'T6' ~ '#4793AF',
      Method == 'cup' & Optimization == 'T6' ~ '#8B322C',
      TRUE ~ 'black'
    ))
  
  # Add width based on the number of groups for each Total_Weight
  agg_data <- agg_data %>%
    group_by(Total_Weight) %>%
    mutate(width = 0.1 * n())
  
  # Sort the data to ensure T6 is plotted last
  agg_data <- agg_data %>%
    arrange(Total_Weight, desc(Optimization == 'T6'))
  
  # Manually set shapes to ensure a cross instead of a plus sign
  shape_values <- c(16, 17, 18, 4, 15, 8)  # 4 represents a cross
  names(shape_values) <- levels(agg_data$Sample_ID)
  
  # Create the plot with all points and color specific categories
  pos <- position_dodge(width = 0.75)
  diffplot <- ggplot(agg_data, aes(x = Total_Weight, y = Zn_diff, shape = Sample_ID)) +
    geom_point(aes(colour = genecolour), position = pos, size = 2.5) +  # Plot all points with specific colors
    geom_errorbar(aes(ymin = Zn_diff - Zn_se, ymax = Zn_diff + Zn_se, colour = genecolour, width = width),
                  position = pos, linewidth = 0.2) +  # Error bars with specific colors and dynamic width
    geom_hline(yintercept = 0, color = "darkgrey", size = 0.6) +  # Horizontal line at 0
    scale_y_continuous(limits = c(-16, 19), breaks = seq(-16, 19, by = 7)) +
    scale_shape_manual(values = shape_values) +  # Manually set shapes
    scale_colour_identity() +  # Use colors directly from the dataframe
    labs(x = "Total Weight", y = "Relative Difference (PXRF - NIST)", shape = "CRM:") +
    theme_classic2()
  
  # Display the plot
  print(diffplot)
  
  # Save the plot with specified dimensions
  ggsave("diffplot.pdf", plot = diffplot, width = 10, height = 2.5, units = "in")
  
  ggsave("diffplot4-Zn.pdf", plot = diffplot, width = 10, height = 1.9, units = "in")
}


# FINAL PLOT - Mn
{
  # Calculate Cu_diff for the entire dataset
  dt_stats <- dt_stats %>%
    mutate(Mn_diff = Mn_NIST - Mn_mean,
           Total_Weight = as.factor(Total_Weight))  # Convert Total_Weight to factor
  
  # Aggregate data to ensure a single entry per Sample_ID, Total_Weight, Method, and Optimization
  agg_data <- dt_stats %>%
    group_by(Sample_ID, Total_Weight, Method, Optimization) %>%
    summarise(Mn_diff = mean(Mn_diff, na.rm = TRUE),
              Mn_se = mean(Mn_se, na.rm = TRUE),
              .groups = "drop")
  
  # Assign colors based on specific conditions
  agg_data <- agg_data %>%
    mutate(genecolour = case_when(
      Method == 'point' & Optimization == 'T1.5' ~ '#FEB941',
      Method == 'point' & Optimization == 'T6' ~ '#4793AF',
      Method == 'cup' & Optimization == 'T6' ~ '#8B322C',
      TRUE ~ 'black'
    ))
  
  # Add width based on the number of groups for each Total_Weight
  agg_data <- agg_data %>%
    group_by(Total_Weight) %>%
    mutate(width = 0.1 * n())
  
  # Sort the data to ensure T6 is plotted last
  agg_data <- agg_data %>%
    arrange(Total_Weight, desc(Optimization == 'T6'))
  
  # Manually set shapes to ensure a cross instead of a plus sign
  shape_values <- c(16, 17, 18, 4, 15, 8)  # 4 represents a cross
  names(shape_values) <- levels(agg_data$Sample_ID)
  
  # Create the plot with all points and color specific categories
  pos <- position_dodge(width = 0.75)
  diffplot <- ggplot(agg_data, aes(x = Total_Weight, y = Mn_diff, shape = Sample_ID)) +
    geom_point(aes(colour = genecolour), position = pos, size = 2.5) +  # Plot all points with specific colors
    geom_errorbar(aes(ymin = Mn_diff - Mn_se, ymax = Mn_diff + Mn_se, colour = genecolour, width = width),
                  position = pos, linewidth = 0.2) +  # Error bars with specific colors and dynamic width
    geom_hline(yintercept = 0, color = "darkgrey", size = 0.6) +  # Horizontal line at 0
    #scale_y_continuous(limits = c(-20, 16), breaks = seq(-20, 15, by = 5)) +
    scale_shape_manual(values = shape_values) +  # Manually set shapes
    scale_colour_identity() +  # Use colors directly from the dataframe
    labs(x = "Total Weight", y = "Relative Difference (PXRF - NIST)", shape = "CRM:") +
    theme_classic2()
  
  # Display the plot
  print(diffplot)
  
  # Save the plot with specified dimensions
  ggsave("diffplot2.pdf", plot = diffplot, width = 10, height = 2.5, units = "in")
  
  ggsave("diffplot4-Mn.pdf", plot = diffplot, width = 10, height = 1.9, units = "in")
  
}

