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





#################### min NIST - min PXRF etc. plot. Run lines 1 to 76 first 
# Also Creating new columns for NIST - SD and NIST + SD 


# Load necessary libraries
library(dplyr)

# Define your element columns and metadata columns
element_columns <- c("Cu", "Zn", "Mn", "Se", "P", "Fe", "S", "K")
metadata_columns <- c("Substrate.RT", "P_NIST", "S_NIST", "K_NIST", "Ca_NIST", "Mn_NIST", "Fe_NIST", "Ni_NIST", "Cu_NIST", "Zn_NIST", "As_NIST", "Se_NIST", "Re_NIST",
                      "P_NIST_SD", "S_NIST_SD", "K_NIST_SD", "Ca_NIST_SD", "Mn_NIST_SD", "Fe_NIST_SD", "Ni_NIST_SD", "Cu_NIST_SD", "Zn_NIST_SD", "As_NIST_SD", "Se_NIST_SD", "Re_NIST_SD")

# Define the standard error function
se <- function(x) sd(x, na.rm = TRUE) / sqrt(length(na.omit(x)))

# Compute the mean, SD, SE, min, and max for each element within the specified groups and add metadata
dt_stats2 <- dt1 %>%
  group_by(Sample_ID, Total_Weight, Method, Optimization) %>%
  summarise(across(all_of(element_columns), list(
    mean = ~ mean(.x, na.rm = TRUE),
    sd = ~ sd(.x, na.rm = TRUE),
    se = ~ se(.x),
    min = ~ min(.x, na.rm = TRUE),
    max = ~ max(.x, na.rm = TRUE)
  ), .names = "{.col}_{.fn}"), .groups = "drop") %>%
  # Ensure each group is represented once for the join to prevent duplicates
  distinct(Sample_ID, Total_Weight, Method, Optimization, .keep_all = TRUE) %>%
  # Join metadata based on the most frequent occurrence or a specific entry for each group
  left_join(dt1 %>%
              select(Sample_ID, Total_Weight, Method, Optimization, all_of(metadata_columns)) %>%
              group_by(Sample_ID, Total_Weight, Method, Optimization) %>%
              slice(1), # Assuming the first row is representative for the metadata
            by = c("Sample_ID", "Total_Weight", "Method", "Optimization"))

# Add NIST_min and NIST_max columns
for (element in c("P", "S", "K", "Ca", "Mn", "Fe", "Ni", "Cu", "Zn", "As", "Se", "Re")) {
  dt_stats2 <- dt_stats2 %>%
    mutate(!!paste0(element, "_NIST_min") := .data[[paste0(element, "_NIST")]] - .data[[paste0(element, "_NIST_SD")]],
           !!paste0(element, "_NIST_max") := .data[[paste0(element, "_NIST")]] + .data[[paste0(element, "_NIST_SD")]])
}

# Display the resulting dataframe
print(dt_stats2)



# Calculate Cu_diff for the entire dataset
dt_stats2 <- dt_stats2 %>%
  mutate(Cu_diff_mean =  Cu_NIST - Cu_mean, 
         Cu_diff_min = Cu_NIST_min - Cu_min,
         Cu_diff_max = Cu_NIST_max - Cu_max,
         Total_Weight = as.factor(Total_Weight))  #




agg_data <- dt_stats2 %>%
  group_by(Sample_ID, Total_Weight, Method, Optimization) %>%
  summarise(Cu_diff_mean = mean(Cu_diff_mean, na.rm = TRUE),
            Cu_se = mean(Cu_se, na.rm = TRUE),
            Cu_diff_min = mean(Cu_diff_min, na.rm = TRUE),
            Cu_diff_max = mean(Cu_diff_max, na.rm = TRUE),
            .groups = "drop")


agg_data <- agg_data %>%
  mutate(genecolour = case_when(
    Method == 'point' & Optimization == 'T1.5' ~ '#FEB941',
    Method == 'point' & Optimization == 'T6' ~ '#4793AF',
    Method == 'cup' & Optimization == 'T6' ~ '#8B322C',
    TRUE ~ 'black'
  ))


agg_data <- agg_data %>%
  group_by(Total_Weight) %>%
  mutate(width = 0.1 * n())

# Sort the data to ensure T6 is plotted last
agg_data <- agg_data %>%
  arrange(Total_Weight, desc(Optimization == 'T6'))


shape_values <- c(16, 17, 18, 4, 15, 8)  # 4 represents a cross
names(shape_values) <- levels(agg_data$Sample_ID)


pos <- position_dodge(width = 0.75)
diffplot <- ggplot(agg_data, aes(x = Total_Weight, y = Cu_diff_min, shape = Sample_ID)) +
  geom_point(aes(colour = genecolour), position = pos, size = 2.5) +  # Plot all points with specific colors
  geom_errorbar(aes(ymin = Cu_diff_min - Cu_se, ymax = Cu_diff_min + Cu_se, colour = genecolour, width = width),
                position = pos, linewidth = 0.2) +  # Error bars with specific colors and dynamic width
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.6) +  # Horizontal line at 0
  scale_y_continuous(limits = c(-3.5, 7), breaks = seq(-3, 7, by = 3)) +
  scale_shape_manual(values = shape_values) +  # Manually set shapes
  scale_colour_identity() +  # Use colors directly from the dataframe
  labs(x = "Total Weight", y = "Relative Difference (PXRF - NIST)", shape = "CRM:") +
  theme_classic2()


print(diffplot)

# Save the plot with specified dimensions
ggsave("diffplot-Cu-min.pdf", plot = diffplot, width = 10, height = 1.9, units = "in")


diffplot <- ggplot(agg_data, aes(x = Total_Weight, y = Cu_diff_max, shape = Sample_ID)) +
  geom_point(aes(colour = genecolour), position = pos, size = 2.5) +  # Plot all points with specific colors
  geom_errorbar(aes(ymin = Cu_diff_max - Cu_se, ymax = Cu_diff_max + Cu_se, colour = genecolour, width = width),
                position = pos, linewidth = 0.2) +  # Error bars with specific colors and dynamic width
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.6) +  # Horizontal line at 0
  scale_y_continuous(limits = c(-3.5, 7), breaks = seq(-3, 7, by = 3)) +
  scale_shape_manual(values = shape_values) +  # Manually set shapes
  scale_colour_identity() +  # Use colors directly from the dataframe
  labs(x = "Total Weight", y = "Relative Difference (PXRF - NIST)", shape = "CRM:") +
  theme_classic2()


print(diffplot)

# Save the plot with specified dimensions
ggsave("diffplot-Cu-max.pdf", plot = diffplot, width = 10, height = 1.9, units = "in")



diffplot <- ggplot(agg_data, aes(x = Total_Weight, y = Cu_diff_mean, shape = Sample_ID)) +
  geom_point(aes(colour = genecolour), position = pos, size = 2.5) +  # Plot all points with specific colors
  geom_errorbar(aes(ymin = Cu_diff_mean - Cu_se, ymax = Cu_diff_mean + Cu_se, colour = genecolour, width = width),
                position = pos, linewidth = 0.2) +  # Error bars with specific colors and dynamic width
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.6) +  # Horizontal line at 0
  scale_y_continuous(limits = c(-3.5, 7), breaks = seq(-3, 7, by = 3)) +
  scale_shape_manual(values = shape_values) +  # Manually set shapes
  scale_colour_identity() +  # Use colors directly from the dataframe
  labs(x = "Total Weight", y = "Relative Difference (PXRF - NIST)", shape = "CRM:") +
  theme_classic2()


print(diffplot)

# Save the plot with specified dimensions
ggsave("diffplot-Cu-mean.pdf", plot = diffplot, width = 10, height = 1.9, units = "in")





###########   Zn   ########
###########        ########




# Calculate Cu_diff for the entire dataset
dt_stats2 <- dt_stats2 %>%
  mutate(Zn_diff_mean =  Zn_NIST - Zn_mean, 
         Zn_diff_min = Zn_NIST_min - Zn_min,
         Zn_diff_max = Zn_NIST_max - Zn_max,
         Total_Weight = as.factor(Total_Weight))  #




agg_data <- dt_stats2 %>%
  group_by(Sample_ID, Total_Weight, Method, Optimization) %>%
  summarise(Zn_diff_mean = mean(Zn_diff_mean, na.rm = TRUE),
            Zn_se = mean(Zn_se, na.rm = TRUE),
            Zn_diff_min = mean(Zn_diff_min, na.rm = TRUE),
            Zn_diff_max = mean(Zn_diff_max, na.rm = TRUE),
            .groups = "drop")


agg_data <- agg_data %>%
  mutate(genecolour = case_when(
    Method == 'point' & Optimization == 'T1.5' ~ '#FEB941',
    Method == 'point' & Optimization == 'T6' ~ '#4793AF',
    Method == 'cup' & Optimization == 'T6' ~ '#8B322C',
    TRUE ~ 'black'
  ))


agg_data <- agg_data %>%
  group_by(Total_Weight) %>%
  mutate(width = 0.1 * n())

# Sort the data to ensure T6 is plotted last
agg_data <- agg_data %>%
  arrange(Total_Weight, desc(Optimization == 'T6'))


shape_values <- c(16, 17, 18, 4, 15, 8)  # 4 represents a cross
names(shape_values) <- levels(agg_data$Sample_ID)


pos <- position_dodge(width = 0.75)
diffplot <- ggplot(agg_data, aes(x = Total_Weight, y = Zn_diff_min, shape = Sample_ID)) +
  geom_point(aes(colour = genecolour), position = pos, size = 2.5) +  # Plot all points with specific colors
  geom_errorbar(aes(ymin = Zn_diff_min - Zn_se, ymax = Zn_diff_min + Zn_se, colour = genecolour, width = width),
                position = pos, linewidth = 0.2) +  # Error bars with specific colors and dynamic width
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.6) +  # Horizontal line at 0
  scale_y_continuous(limits = c(-18, 26), breaks = seq(-18, 26, by = 6 )) +
  scale_shape_manual(values = shape_values) +  # Manually set shapes
  scale_colour_identity() +  # Use colors directly from the dataframe
  labs(x = "Total Weight", y = "Zn", shape = "CRM:") +
  theme_classic2()


print(diffplot)

# Save the plot with specified dimensions
ggsave("diffplot-Zn-min.pdf", plot = diffplot, width = 10, height = 1.9, units = "in")




diffplot <- ggplot(agg_data, aes(x = Total_Weight, y = Zn_diff_max, shape = Sample_ID)) +
  geom_point(aes(colour = genecolour), position = pos, size = 2.5) +  # Plot all points with specific colors
  geom_errorbar(aes(ymin = Zn_diff_max - Zn_se, ymax = Zn_diff_max + Zn_se, colour = genecolour, width = width),
                position = pos, linewidth = 0.2) +  # Error bars with specific colors and dynamic width
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.6) +  # Horizontal line at 0
  scale_y_continuous(limits = c(-18, 26), breaks = seq(-18, 26, by = 6 )) +
  scale_shape_manual(values = shape_values) +  # Manually set shapes
  scale_colour_identity() +  # Use colors directly from the dataframe
  labs(x = "Total Weight", y = "Zn", shape = "CRM:") +
  theme_classic2()


print(diffplot)

# Save the plot with specified dimensions
ggsave("diffplot-Zn-max.pdf", plot = diffplot, width = 10, height = 1.9, units = "in")


diffplot <- ggplot(agg_data, aes(x = Total_Weight, y = Zn_diff_mean, shape = Sample_ID)) +
  geom_point(aes(colour = genecolour), position = pos, size = 2.5) +  # Plot all points with specific colors
  geom_errorbar(aes(ymin = Zn_diff_mean - Zn_se, ymax = Zn_diff_mean + Zn_se, colour = genecolour, width = width),
                position = pos, linewidth = 0.2) +  # Error bars with specific colors and dynamic width
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.6) +  # Horizontal line at 0
  scale_y_continuous(limits = c(-18, 26), breaks = seq(-18, 26, by = 6 )) +
  scale_shape_manual(values = shape_values) +  # Manually set shapes
  scale_colour_identity() +  # Use colors directly from the dataframe
  labs(x = "Total Weight", y = "Zn", shape = "CRM:") +
  theme_classic2()


print(diffplot)

# Save the plot with specified dimensions
ggsave("diffplot-Zn-mean.pdf", plot = diffplot, width = 10, height = 1.9, units = "in")









###########   Mn  ########
###########        ########




# Calculate Cu_diff for the entire dataset
dt_stats2 <- dt_stats2 %>%
  mutate(Mn_diff_mean =  Mn_NIST - Mn_mean, 
         Mn_diff_min = Mn_NIST_min - Mn_min,
         Mn_diff_max = Mn_NIST_max - Mn_max,
         Total_Weight = as.factor(Total_Weight))  #




agg_data <- dt_stats2 %>%
  group_by(Sample_ID, Total_Weight, Method, Optimization) %>%
  summarise(Mn_diff_mean = mean(Mn_diff_mean, na.rm = TRUE),
            Mn_se = mean(Mn_se, na.rm = TRUE),
            Mn_diff_min = mean(Mn_diff_min, na.rm = TRUE),
            Mn_diff_max = mean(Mn_diff_max, na.rm = TRUE),
            .groups = "drop")


agg_data <- agg_data %>%
  mutate(genecolour = case_when(
    Method == 'point' & Optimization == 'T1.5' ~ '#FEB941',
    Method == 'point' & Optimization == 'T6' ~ '#4793AF',
    Method == 'cup' & Optimization == 'T6' ~ '#8B322C',
    TRUE ~ 'black'
  ))


agg_data <- agg_data %>%
  group_by(Total_Weight) %>%
  mutate(width = 0.1 * n())

# Sort the data to ensure T6 is plotted last
agg_data <- agg_data %>%
  arrange(Total_Weight, desc(Optimization == 'T6'))


shape_values <- c(16, 17, 18, 4, 15, 8)  # 4 represents a cross
names(shape_values) <- levels(agg_data$Sample_ID)

pos <- position_dodge(width = 0.75)
diffplot <- ggplot(agg_data, aes(x = Total_Weight, y = Mn_diff_min, shape = Sample_ID)) +
  geom_point(aes(colour = genecolour), position = pos, size = 2.5) +  # Plot all points with specific colors
  geom_errorbar(aes(ymin = Mn_diff_min - Mn_se, ymax = Mn_diff_min + Mn_se, colour = genecolour, width = width),
                position = pos, linewidth = 0.2) +  # Error bars with specific colors and dynamic width
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.6) +  # Horizontal line at 0
  scale_y_continuous(limits = c(-100, 300), breaks = seq(-100, 300, by = 100 )) +
  scale_shape_manual(values = shape_values) +  # Manually set shapes
  scale_colour_identity() +  # Use colors directly from the dataframe
  labs(x = "Total Weight", y = "Mn", shape = "CRM:") +
  theme_classic2()


print(diffplot)

# Save the plot with specified dimensions
ggsave("diffplot-Mn-min.pdf", plot = diffplot, width = 10, height = 1.9, units = "in")

diffplot <- ggplot(agg_data, aes(x = Total_Weight, y = Mn_diff_max, shape = Sample_ID)) +
  geom_point(aes(colour = genecolour), position = pos, size = 2.5) +  # Plot all points with specific colors
  geom_errorbar(aes(ymin = Mn_diff_max - Mn_se, ymax = Mn_diff_max + Mn_se, colour = genecolour, width = width),
                position = pos, linewidth = 0.2) +  # Error bars with specific colors and dynamic width
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.6) +  # Horizontal line at 0
  #scale_y_continuous(limits = c(-100, 300), breaks = seq(-100, 300, by = 100 )) +
  scale_shape_manual(values = shape_values) +  # Manually set shapes
  scale_colour_identity() +  # Use colors directly from the dataframe
  labs(x = "Total Weight", y = "Mn", shape = "CRM:") +
  theme_classic2()


print(diffplot)

# Save the plot with specified dimensions
ggsave("diffplot-Mn-max.pdf", plot = diffplot, width = 10, height = 1.9, units = "in")























#t test - Mean

# Filter the data for the desired conditions
dt_stats2 <- dt_stats2 %>% 
  filter(Optimization == "T1.5", Method == "cup")

# Ensure the relevant columns are numeric
dt_stats2[, 5:96] <- sapply(dt_stats2[, 5:96], as.numeric)

# List of elements to perform t-test on
elements <- c("P", "S", "K", "Ca", "Mn", "Fe", "Ni", "Cu", "Zn", "As", "Se", "Re")

# Function to perform one-sample t-test for a given element
perform_t_test <- function(data, element, weight) {
  # Subset data for the specific Total_Weight
  dt_weight <- subset(data, Total_Weight == weight)
  
  # Calculate the difference (correct column names)
  difference <- dt_weight[[paste0(element, "_NIST")]] - dt_weight[[paste0(element, "_mean")]]
  
  # Perform t-test if there are enough non-NA differences
  if(sum(!is.na(difference)) >= 3) {
    t_test_result <- t.test(difference, mu = 0, na.action = na.exclude)
    p_value <- t_test_result$p.value
  } else {
    p_value <- NA  # Not enough data to perform t-test
  }
  return(p_value)
}

# DataFrame to store results
results <- data.frame(Element = character(),
                      Weight = numeric(),
                      P_Value = numeric())

# Loop through each element and weight
for (element in elements) {
  for (weight in unique(dt_stats2$Total_Weight)) {
    p_value <- perform_t_test(dt_stats2, element, weight)
    results <- rbind(results, data.frame(Element = element, Weight = weight, P_Value = p_value))
  }
}

# View results
print(results)


write.xlsx(results, "T_Test_NIST_Mean.xlsx")



#t test - Min

# Filter the data for the desired conditions
dt_stats2 <- dt_stats2 %>% 
  filter(Optimization == "T1.5", Method == "cup")

# Ensure the relevant columns are numeric
dt_stats2[, 5:96] <- sapply(dt_stats2[, 5:96], as.numeric)

# List of elements to perform t-test on
elements <- c("P", "S", "K", "Ca", "Mn", "Fe", "Ni", "Cu", "Zn", "As", "Se", "Re")

# Function to perform one-sample t-test for a given element
perform_t_test <- function(data, element, weight) {
  # Subset data for the specific Total_Weight
  dt_weight <- subset(data, Total_Weight == weight)
  
  # Calculate the difference (correct column names)
  difference <- dt_weight[[paste0(element, "_NIST_min")]] - dt_weight[[paste0(element, "_min")]]
  
  # Perform t-test if there are enough non-NA differences
  if(sum(!is.na(difference)) >= 3) {
    t_test_result <- t.test(difference, mu = 0, na.action = na.exclude)
    p_value <- t_test_result$p.value
  } else {
    p_value <- NA  # Not enough data to perform t-test
  }
  return(p_value)
}

# DataFrame to store results
results <- data.frame(Element = character(),
                      Weight = numeric(),
                      P_Value = numeric())

# Loop through each element and weight
for (element in elements) {
  for (weight in unique(dt_stats2$Total_Weight)) {
    p_value <- perform_t_test(dt_stats2, element, weight)
    results <- rbind(results, data.frame(Element = element, Weight = weight, P_Value = p_value))
  }
}

# View results
print(results)


write.xlsx(results, "T_Test_NIST_Min.xlsx")



#t test - Max

# Filter the data for the desired conditions
dt_stats2 <- dt_stats2 %>% 
  filter(Optimization == "T1.5", Method == "cup")

# Ensure the relevant columns are numeric
dt_stats2[, 5:96] <- sapply(dt_stats2[, 5:96], as.numeric)

# List of elements to perform t-test on
elements <- c("P", "S", "K", "Ca", "Mn", "Fe", "Ni", "Cu", "Zn", "As", "Se", "Re")

# Function to perform one-sample t-test for a given element
perform_t_test <- function(data, element, weight) {
  # Subset data for the specific Total_Weight
  dt_weight <- subset(data, Total_Weight == weight)
  
  # Calculate the difference (correct column names)
  difference <- dt_weight[[paste0(element, "_NIST_max")]] - dt_weight[[paste0(element, "_max")]]
  
  # Perform t-test if there are enough non-NA differences
  if(sum(!is.na(difference)) >= 3) {
    t_test_result <- t.test(difference, mu = 0, na.action = na.exclude)
    p_value <- t_test_result$p.value
  } else {
    p_value <- NA  # Not enough data to perform t-test
  }
  return(p_value)
}

# DataFrame to store results
results <- data.frame(Element = character(),
                      Weight = numeric(),
                      P_Value = numeric())

# Loop through each element and weight
for (element in elements) {
  for (weight in unique(dt_stats2$Total_Weight)) {
    p_value <- perform_t_test(dt_stats2, element, weight)
    results <- rbind(results, data.frame(Element = element, Weight = weight, P_Value = p_value))
  }
}

# View results
print(results)


write.xlsx(results, "T_Test_NIST_Max.xlsx")
