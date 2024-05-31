#NISTS TEST



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
dt <-read.delim("NISTS_04.18.2024_trimmed.txt")
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
  
  dt1$Ca[dt1$Ca == 0] <- 10/2
  dt1$Ti[dt1$Ti == 0] <- 5/2
  dt1$Cr[dt1$Cr == 0] <- 2/2
  dt1$Mn[dt1$Mn == 0] <- 1/2
  dt1$Fe[dt1$Fe == 0] <- 5/2
  dt1$Co[dt1$Co == 0] <- 3/2
  dt1$Ni[dt1$Ni == 0] <- 0.2/2
  dt1$Cu[dt1$Cu == 0] <- 0.5/2
  dt1$Zn[dt1$Zn == 0] <- 0.6/2
  dt1$As[dt1$As == 0] <- 0.1/2
  dt1$Se[dt1$Se == 0] <- 0.1/2
  dt1$Cd[dt1$Cd == 0] <- 1/2
  dt1$Re[dt1$Re == 0] <- 0.5/2
}






dt2 <- dt1 %>% 
  filter(Optimization == "T1.5")
dt3 <- dt2 %>% 
  filter(Method == "cup")


str(dt3)

dt3[, 9:33] <- sapply(dt3[, 9:33], as.numeric)


dt3 <- dt3 %>%
  mutate(across(everything(), ~replace_na(., 0)))




library(ggplot2)
library(dplyr)

# Ensure Total_Weight is treated as a categorical variable
dt3 <- dt3 %>%
  mutate(Total_Weight = as.factor(Total_Weight))

# Create a boxplot
ggplot(dt3, aes(x = Total_Weight, y = Cu)) +
  geom_boxplot() +
  facet_wrap(~Sample_ID, scales = "free") +
  theme_minimal() +
  labs(title = "Boxplot of Cu by Total Weight for each Sample ID",
       x = "Total Weight",
       y = "Cu")


dt3_NIST1515 <- subset(dt3, Sample_ID=="NIST1515")

ggplot(dt3_NIST1515, aes(x = Total_Weight, y = Cu)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of Cu by Total Weight for each Sample ID",
       x = "Total Weight",
       y = "Cu")


library(tidyr)

dt1[, 9:33] <- sapply(dt1[, 9:33], as.numeric)


dt1 <- dt1 %>%
  mutate(across(everything(), ~replace_na(., 0)))

dt1 <- dt1 %>%
  mutate(Total_Weight = as.factor(Total_Weight))

#Test with ribbon
#NIST1573a <- ggplot(dt1_NIST1573a, aes(x = Total_Weight, y = Cu, color = Interaction)) +
#  geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
#  geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
#  geom_hline(yintercept = 5.69, color = "darkgrey", size = 1) + # Horizontal line at 5.69
#  geom_ribbon(aes(ymin = 5.69 - 3.13, ymax = 5.69 + 0.13), fill = "grey", alpha = 0.2) + # Shaded area for ±0.13
#  theme_minimal() +
#  labs(title = "Apple Leaves NIST 1573a",
#       x = "Total Weight",
#       y = "Cu [ppm]",
#       color = "Method & Optimization") +
#  scale_color_brewer(palette = "Set1") # Adjust the color palette

#Cu-NISTS
{
#NIST 1515 Cu
dt1_NIST1515 <- subset(dt1, Sample_ID=="NIST1515")
dt1_NIST1515 <- dt1_NIST1515 %>%
  mutate(Interaction = interaction(Method, Optimization))

NIST1515_Cu <- ggplot(dt1_NIST1515, aes(x = Total_Weight, y = Cu, color = Interaction)) +
  geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
  geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
  geom_hline(yintercept = 5.69, color = "darkgrey", size = 0.6) + # Horizontal line at 5.69
  geom_hline(yintercept = 5.56, color = "grey", size = 0.3) + # 
  geom_hline(yintercept = 5.82, color = "grey", size = 0.3) + #
  theme_classic() +
  scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, by = 1)) +
  labs(title = "Apple Leaves NIST 1515",
       x = "Total Weight",
       y = "Cu [ppm]",
       color = "Method & Optimization") +
  scale_color_brewer(palette = "Set1") # Adjust the color palette


#NIST 1573a Cu

dt1_NIST1573a <- subset(dt1, Sample_ID=="NIST1573a")
dt1_NIST1573a <- dt1_NIST1573a %>%
  mutate(Interaction = interaction(Method, Optimization))

NIST1573a_Cu <- ggplot(dt1_NIST1573a, aes(x = Total_Weight, y = Cu, color = Interaction)) +
  geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
  geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
  geom_hline(yintercept = 4.7, color = "darkgrey", size = 0.6) + # Horizontal line at 5.69
  geom_hline(yintercept = 4.84, color = "grey", size = 0.3) + # 
  geom_hline(yintercept = 4.56, color = "grey", size = 0.3) + #
  theme_classic() +
  scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, by = 1)) +
  labs(title = "Tomato Leaves NIST 1573a",
       x = "Total Weight",
       y = "Cu [ppm]",
       color = "Method & Optimization") +
  scale_color_brewer(palette = "Set1") # Adjust the color palette



#NIST 1568b Cu
dt1_NIST1568b <- subset(dt1, Sample_ID=="NIST1568b")
dt1_NIST1568b <- dt1_NIST1568b %>%
  mutate(Interaction = interaction(Method, Optimization))

NIST1568b_Cu <- ggplot(dt1_NIST1568b, aes(x = Total_Weight, y = Cu, color = Interaction)) +
  geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
  geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
  geom_hline(yintercept = 2.35, color = "darkgrey", size = 0.6) + # Horizontal line at 5.69
  geom_hline(yintercept = 2.51, color = "grey", size = 0.3) + # 
  geom_hline(yintercept = 2.19, color = "grey", size = 0.3) + #
  theme_classic() +
  scale_y_continuous(limits = c(0, 4.5), breaks = seq(0, 4.5, by = 0.5)) +
  labs(title = "Rice Flour NIST 1568b",
       x = "Total Weight",
       y = "Cu [ppm]",
       color = "Method & Optimization") +
  scale_color_brewer(palette = "Set1") # Adjust the color palette



#NIST 1570a Cu
dt1_NIST1570a <- subset(dt1, Sample_ID=="NIST1570")
dt1_NIST1570a <- dt1_NIST1570a %>%
  mutate(Interaction = interaction(Method, Optimization))

NIST1570a_Cu <- ggplot(dt1_NIST1570a, aes(x = Total_Weight, y = Cu, color = Interaction)) +
  geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
  geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
  geom_hline(yintercept = 12.22, color = "darkgrey", size = 0.6) + # Horizontal line at 5.69
  geom_hline(yintercept = 13.08, color = "grey", size = 0.3) + # 
  geom_hline(yintercept = 11.36, color = "grey", size = 0.3) + #
  theme_classic() +
  scale_y_continuous(limits = c(5, 16), breaks = seq(5, 16, by = 2)) +
  labs(title = "Spinach leaves NIST 1570",
       x = "Total Weight",
       y = "Cu [ppm]",
       color = "Method & Optimization") +
  scale_color_brewer(palette = "Set1") # Adjust the color palette


#NIST 1575a Cu
dt1_NIST1575a <- subset(dt1, Sample_ID=="NIST1575a")
dt1_NIST1575a <- dt1_NIST1575a %>%
  mutate(Interaction = interaction(Method, Optimization))

NIST1575a_Cu <- ggplot(dt1_NIST1575a, aes(x = Total_Weight, y = Cu, color = Interaction)) +
  geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
  geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
  geom_hline(yintercept = 2.8, color = "darkgrey", size = 0.6) + # Horizontal line at 5.69
  geom_hline(yintercept = 3.0, color = "grey", size = 0.3) + # 
  geom_hline(yintercept = 2.6, color = "grey", size = 0.3) + #
  theme_classic() +
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 0.5)) +
  labs(title = "Pine Needles NIST 1575a",
       x = "Total Weight",
       y = "Cu [ppm]",
       color = "Method & Optimization") +
  scale_color_brewer(palette = "Set1") # Adjust the color palette


#NIST 1575a Cu
dt1_NIST1547 <- subset(dt1, Sample_ID=="NIST1547")
dt1_NIST1547 <- dt1_NIST1547 %>%
  mutate(Interaction = interaction(Method, Optimization))

NIST1575a_Cu <- ggplot(dt1_NIST1547, aes(x = Total_Weight, y = Cu, color = Interaction)) +
  geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
  geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
  geom_hline(yintercept = 3.75, color = "darkgrey", size = 0.6) + # Horizontal line at 5.69
  geom_hline(yintercept = 4.12, color = "grey", size = 0.3) + # 
  geom_hline(yintercept = 3.38, color = "grey", size = 0.3) + #
  theme_classic() +
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 1)) +
  labs(title = "Pine Needles NIST 1547",
       x = "Total Weight",
       y = "Cu [ppm]",
       color = "Method & Optimization") +
  scale_color_brewer(palette = "Set1") # Adjust the color palette


nists <- ggarrange(NIST1515_Cu,NIST1573a_Cu,NIST1568b_Cu,NIST1570a_Cu,NIST1575a_Cu, NIST1575a_Cu, 
          ncol = 1, nrow = 6, 
          common.legend = TRUE, legend = "bottom")



ggsave(filename = "nists.pdf", plot = nists, width = 3, height = 10, units = "in")

  }

#Zn-NISTS
{
  #NIST 1515 Zn
  dt1_NIST1515 <- subset(dt1, Sample_ID=="NIST1515")
  dt1_NIST1515 <- dt1_NIST1515 %>%
    mutate(Interaction = interaction(Method, Optimization))
  
  NIST1515_Zn <- ggplot(dt1_NIST1515, aes(x = Total_Weight, y = Zn, color = Interaction)) +
    geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
    geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
    geom_hline(yintercept = 12.45, color = "darkgrey", size = 1) + # Horizontal line at 5.69
    geom_hline(yintercept = 12.88, color = "grey", size = 0.5) + # 
    geom_hline(yintercept = 12.02, color = "grey", size = 0.5) + #
    theme_classic() +
    scale_y_continuous(limits = c(5, 15), breaks = seq(5, 15, by = 1)) +
    labs(title = "Apple Leaves NIST 1515",
         x = "Total Weight",
         y = "Zn [ppm]",
         color = "Method & Optimization") +
    scale_color_brewer(palette = "Set1") # Adjust the color palette  
  
  
  #NIST 1573a Zn
  
  dt1_NIST1573a <- subset(dt1, Sample_ID=="NIST1573a")
  dt1_NIST1573a <- dt1_NIST1573a %>%
    mutate(Interaction = interaction(Method, Optimization))
  
  NIST1573a_Zn <- ggplot(dt1_NIST1573a, aes(x = Total_Weight, y = Zn, color = Interaction)) +
    geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
    geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
    geom_hline(yintercept = 30.94, color = "darkgrey", size = 1) + # Horizontal line at 5.69
    geom_hline(yintercept = 31.49, color = "grey", size = 0.5) + # 
    geom_hline(yintercept = 30.39, color = "grey", size = 0.5) + #
    theme_classic() +
    scale_y_continuous(limits = c(20, 35), breaks = seq(20, 35, by = 1)) +
    labs(title = "Tomato Leaves NIST 1573a",
         x = "Total Weight",
         y = "Zn [ppm]",
         color = "Method & Optimization") +
    scale_color_brewer(palette = "Set1") # Adjust the color palette
  
  
  
  #NIST 1568b Zn
  dt1_NIST1568b <- subset(dt1, Sample_ID=="NIST1568b")
  dt1_NIST1568b <- dt1_NIST1568b %>%
    mutate(Interaction = interaction(Method, Optimization))
  
  NIST1568b_Zn <- ggplot(dt1_NIST1568b, aes(x = Total_Weight, y = Zn, color = Interaction)) +
    geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
    geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
    geom_hline(yintercept = 19.42, color = "darkgrey", size = 1) + # Horizontal line at 5.69
    geom_hline(yintercept = 19.68, color = "grey", size = 0.5) + # 
    geom_hline(yintercept = 19.16, color = "grey", size = 0.5) + #
    theme_classic() +
   scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 2.5)) +
    labs(title = "Rice Flour NIST 1568b",
         x = "Total Weight",
         y = "Zn [ppm]",
         color = "Method & Optimization") +
    scale_color_brewer(palette = "Set1") # Adjust the color palette
  
  
  
  #NIST 1570a Zn
  dt1_NIST1570a <- subset(dt1, Sample_ID=="NIST1570")
  dt1_NIST1570a <- dt1_NIST1570a %>%
    mutate(Interaction = interaction(Method, Optimization))
  
  NIST1570a_Zn <- ggplot(dt1_NIST1570a, aes(x = Total_Weight, y = Zn, color = Interaction)) +
    geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
    geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
    geom_hline(yintercept = 82.3, color = "darkgrey", size = 1) + # Horizontal line at 5.69
    geom_hline(yintercept = 86.2, color = "grey", size = 0.5) + # 
    geom_hline(yintercept = 78.4, color = "grey", size = 0.5) + #
    theme_classic() +
    scale_y_continuous(limits = c(60, 100), breaks = seq(60, 100, by = 5)) +
    labs(title = "Spinach leaves NIST 1570",
         x = "Total Weight",
         y = "Zn [ppm]",
         color = "Method & Optimization") +
    scale_color_brewer(palette = "Set1") # Adjust the color palette
  
  
  #NIST 1575a Zn
  dt1_NIST1575a <- subset(dt1, Sample_ID=="NIST1575a")
  dt1_NIST1575a <- dt1_NIST1575a %>%
    mutate(Interaction = interaction(Method, Optimization))
  
  NIST1575a_Zn <- ggplot(dt1_NIST1575a, aes(x = Total_Weight, y = Zn, color = Interaction)) +
    geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
    geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
    geom_hline(yintercept = 38, color = "darkgrey", size = 1) + # Horizontal line at 5.69
    geom_hline(yintercept = 40, color = "grey", size = 0.5) + # 
    geom_hline(yintercept = 36, color = "grey", size = 0.5) + #
    theme_classic() +
    scale_y_continuous(limits = c(20, 45), breaks = seq(20, 45, by = 5)) +
    labs(title = "Pine Needles NIST 1575a",
         x = "Total Weight",
         y = "Zn [ppm]",
         color = "Method & Optimization") +
    scale_color_brewer(palette = "Set1") # Adjust the color palette
  
  
  ggarrange(NIST1515_Zn,NIST1573a_Zn,NIST1568b_Zn,NIST1570a_Zn,NIST1575a_Zn,
            ncol = 3, nrow = 2, 
            common.legend = TRUE, legend = "bottom")
  
}

#Mn-NISTS
{
  #NIST 1515 Mn
  dt1_NIST1515 <- subset(dt1, Sample_ID=="NIST1515")
  dt1_NIST1515 <- dt1_NIST1515 %>%
    mutate(Interaction = interaction(Method, Optimization))
  
  NIST1515_Mn <- ggplot(dt1_NIST1515, aes(x = Total_Weight, y = Mn, color = Interaction)) +
    geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
    geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
    geom_hline(yintercept = 54.1, color = "darkgrey", size = 1) + # Horizontal line at 5.69
    geom_hline(yintercept = 55.2, color = "grey", size = 0.5) + # 
    geom_hline(yintercept = 53, color = "grey", size = 0.5) + #
    theme_classic() +
    scale_y_continuous(limits = c(30, 65), breaks = seq(30, 65, by = 5)) +
    labs(title = "Apple Leaves NIST 1515",
         x = "Total Weight",
         y = "Mn [ppm]",
         color = "Method & Optimization") +
    scale_color_brewer(palette = "Set1") # Adjust the color palette  
  
  
  #NIST 1573a Mn
  
  dt1_NIST1573a <- subset(dt1, Sample_ID=="NIST1573a")
  dt1_NIST1573a <- dt1_NIST1573a %>%
    mutate(Interaction = interaction(Method, Optimization))
  
  NIST1573a_Mn <- ggplot(dt1_NIST1573a, aes(x = Total_Weight, y = Mn, color = Interaction)) +
    geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
    geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
    geom_hline(yintercept = 246.3, color = "darkgrey", size = 1) + # Horizontal line at 5.69
    geom_hline(yintercept = 253.4, color = "grey", size = 0.5) + # 
    geom_hline(yintercept = 239.2, color = "grey", size = 0.5) + #
    theme_classic() +
    scale_y_continuous(limits = c(180, 290), breaks = seq(180, 290, by = 10)) +
    labs(title = "Tomato Leaves NIST 1573a",
         x = "Total Weight",
         y = "Mn [ppm]",
         color = "Method & Optimization") +
    scale_color_brewer(palette = "Set1") # Adjust the color palette
  
  
  
  #NIST 1568b Mn
  dt1_NIST1568b <- subset(dt1, Sample_ID=="NIST1568b")
  dt1_NIST1568b <- dt1_NIST1568b %>%
    mutate(Interaction = interaction(Method, Optimization))
  
  NIST1568b_Mn <- ggplot(dt1_NIST1568b, aes(x = Total_Weight, y = Mn, color = Interaction)) +
    geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
    geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
    geom_hline(yintercept = 19.2, color = "darkgrey", size = 1) + # Horizontal line at 5.69
    geom_hline(yintercept = 21, color = "grey", size = 0.5) + # 
    geom_hline(yintercept = 17.4, color = "grey", size = 0.5) + #
    theme_classic() +
    scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 5)) +
    labs(title = "Rice Flour NIST 1568b",
         x = "Total Weight",
         y = "Mn [ppm]",
         color = "Method & Optimization") +
    scale_color_brewer(palette = "Set1") # Adjust the color palette
  
  
  
  #NIST 1570a Zn
  dt1_NIST1570a <- subset(dt1, Sample_ID=="NIST1570")
  dt1_NIST1570a <- dt1_NIST1570a %>%
    mutate(Interaction = interaction(Method, Optimization))
  
  NIST1570a_Mn <- ggplot(dt1_NIST1570a, aes(x = Total_Weight, y = Mn, color = Interaction)) +
    geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
    geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
    geom_hline(yintercept = 76, color = "darkgrey", size = 1) + # Horizontal line at 5.69
    geom_hline(yintercept = 77.2, color = "grey", size = 0.5) + # 
    geom_hline(yintercept = 74.8, color = "grey", size = 0.5) + #
    theme_classic() +
    scale_y_continuous(limits = c(55, 92), breaks = seq(55, 100, by = 5)) +
    labs(title = "Spinach leaves NIST 1570",
         x = "Total Weight",
         y = "Mn [ppm]",
         color = "Method & Optimization") +
    scale_color_brewer(palette = "Set1") # Adjust the color palette
  
  
  #NIST 1575a Zn
  dt1_NIST1575a <- subset(dt1, Sample_ID=="NIST1575a")
  dt1_NIST1575a <- dt1_NIST1575a %>%
    mutate(Interaction = interaction(Method, Optimization))
  
  NIST1575a_Mn <- ggplot(dt1_NIST1575a, aes(x = Total_Weight, y = Mn, color = Interaction)) +
    geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
    geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
    geom_hline(yintercept = 488, color = "darkgrey", size = 1) + # Horizontal line at 5.69
    geom_hline(yintercept = 500, color = "grey", size = 0.5) + # 
    geom_hline(yintercept = 476, color = "grey", size = 0.5) + #
    theme_classic() +
    scale_y_continuous(limits = c(250, 575), breaks = seq(250, 600, by = 50)) +
    labs(title = "Pine Needles NIST 1575a",
         x = "Total Weight",
         y = "Mn [ppm]",
         color = "Method & Optimization") +
    scale_color_brewer(palette = "Set1") # Adjust the color palette
  
  
  ggarrange(NIST1515_Mn,NIST1573a_Mn,NIST1568b_Mn,NIST1570a_Mn,NIST1575a_Mn,
            ncol = 3, nrow = 2, 
            common.legend = TRUE, legend = "bottom")
  
}

#Fe-NISTS
{
  #NIST 1515 Fe
  dt1_NIST1515 <- subset(dt1, Sample_ID=="NIST1515")
  dt1_NIST1515 <- dt1_NIST1515 %>%
    mutate(Interaction = interaction(Method, Optimization))
  
  NIST1515_Fe <- ggplot(dt1_NIST1515, aes(x = Total_Weight, y = Fe, color = Interaction)) +
    geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
    geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
    geom_hline(yintercept = 82.7, color = "darkgrey", size = 1) + # Horizontal line at 5.69
    geom_hline(yintercept = 85.3, color = "grey", size = 0.5) + # 
    geom_hline(yintercept = 80.1, color = "grey", size = 0.5) + #
    theme_classic() +
    scale_y_continuous(limits = c(20, 95), breaks = seq(20, 100, by = 10)) +
    labs(title = "Apple Leaves NIST 1515",
         x = "Total Weight",
         y = "Fe [ppm]",
         color = "Method & Optimization") +
    scale_color_brewer(palette = "Set1") # Adjust the color palette  
  
  
  #NIST 1573a Fe
  
  dt1_NIST1573a <- subset(dt1, Sample_ID=="NIST1573a")
  dt1_NIST1573a <- dt1_NIST1573a %>%
    mutate(Interaction = interaction(Method, Optimization))
  
  NIST1573a_Fe <- ggplot(dt1_NIST1573a, aes(x = Total_Weight, y = Fe, color = Interaction)) +
    geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
    geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
    geom_hline(yintercept = 367.5, color = "darkgrey", size = 1) + # Horizontal line at 5.69
    geom_hline(yintercept = 371.8, color = "grey", size = 0.5) + # 
    geom_hline(yintercept = 363.2, color = "grey", size = 0.5) + #
    theme_classic() +
    scale_y_continuous(limits = c(240, 420), breaks = seq(240, 440, by = 20)) +
    labs(title = "Tomato Leaves NIST 1573a",
         x = "Total Weight",
         y = "Fe [ppm]",
         color = "Method & Optimization") +
    scale_color_brewer(palette = "Set1") # Adjust the color palette
  
  
  
  #NIST 1568b Fe
  dt1_NIST1568b <- subset(dt1, Sample_ID=="NIST1568b")
  dt1_NIST1568b <- dt1_NIST1568b %>%
    mutate(Interaction = interaction(Method, Optimization))
  
  NIST1568b_Fe <- ggplot(dt1_NIST1568b, aes(x = Total_Weight, y = Fe, color = Interaction)) +
    geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
    geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
    geom_hline(yintercept = 7.42, color = "darkgrey", size = 1) + # Horizontal line at 5.69
    geom_hline(yintercept = 7.86, color = "grey", size = 0.5) + # 
    geom_hline(yintercept = 6.98, color = "grey", size = 0.5) + #
    theme_classic() +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
    labs(title = "Rice Flour NIST 1568b",
         x = "Total Weight",
         y = "Fe [ppm]",
         color = "Method & Optimization") +
    scale_color_brewer(palette = "Set1") # Adjust the color palette
  
  
  
  #NIST 1570a Fe - NA in NIST
  #dt1_NIST1570a <- subset(dt1, Sample_ID=="NIST1570")
  #dt1_NIST1570a <- dt1_NIST1570a %>%
  #  mutate(Interaction = interaction(Method, Optimization))
  
  #NIST1570a_Fe <- ggplot(dt1_NIST1570a, aes(x = Total_Weight, y = Fe, color = Interaction)) +
   # geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
    #geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
    #geom_hline(yintercept = 76, color = "darkgrey", size = 1) + # Horizontal line at 5.69
    #geom_hline(yintercept = 77.2, color = "grey", size = 0.5) + # 
    #geom_hline(yintercept = 74.8, color = "grey", size = 0.5) + #
    #theme_classic() +
    #scale_y_continuous(limits = c(55, 92), breaks = seq(55, 100, by = 5)) +
    #labs(title = "Spinach leaves NIST 1570",
     #    x = "Total Weight",
      #   y = "Fe [ppm]",
       #  color = "Method & Optimization") +
    #scale_color_brewer(palette = "Set1") # Adjust the color palette
  
  
  #NIST 1575a Fe
  dt1_NIST1575a <- subset(dt1, Sample_ID=="NIST1575a")
  dt1_NIST1575a <- dt1_NIST1575a %>%
    mutate(Interaction = interaction(Method, Optimization))
  
  NIST1575a_Fe <- ggplot(dt1_NIST1575a, aes(x = Total_Weight, y = Fe, color = Interaction)) +
    geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
    geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
    geom_hline(yintercept = 46, color = "darkgrey", size = 1) + # Horizontal line at 5.69
    geom_hline(yintercept = 48, color = "grey", size = 0.5) + # 
    geom_hline(yintercept = 44, color = "grey", size = 0.5) + #
    theme_classic() +
    scale_y_continuous(limits = c(10, 120), breaks = seq(10, 120, by = 20)) +
    labs(title = "Pine Needles NIST 1575a",
         x = "Total Weight",
         y = "Fe [ppm]",
         color = "Method & Optimization") +
    scale_color_brewer(palette = "Set1") # Adjust the color palette
  
  
  ggarrange(NIST1515_Fe,NIST1573a_Fe,NIST1568b_Fe,NIST1575a_Fe,
            ncol = 2, nrow = 2, 
            common.legend = TRUE, legend = "bottom")
  
}


#Mn-NISTS
{
  #NIST 1515 K
  dt1_NIST1515 <- subset(dt1, Sample_ID=="NIST1515")
  dt1_NIST1515 <- dt1_NIST1515 %>%
    mutate(Interaction = interaction(Method, Optimization))
  
  NIST1515_K <- ggplot(dt1_NIST1515, aes(x = Total_Weight, y = K, color = Interaction)) +
    geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
    geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
    #geom_hline(yintercept = 54.1, color = "darkgrey", size = 1) + # Horizontal line at 5.69
    #geom_hline(yintercept = 55.2, color = "grey", size = 0.5) + # 
    #geom_hline(yintercept = 53, color = "grey", size = 0.5) + #
    theme_classic() +
    #scale_y_continuous(limits = c(30, 65), breaks = seq(30, 65, by = 5)) +
    labs(title = "Apple Leaves NIST 1515",
         x = "Total Weight",
         y = "K [ppm]",
         color = "Method & Optimization") +
    scale_color_brewer(palette = "Set1") # Adjust the color palette  
  
  
  #NIST 1573a K
  
  dt1_NIST1573a <- subset(dt1, Sample_ID=="NIST1573a")
  dt1_NIST1573a <- dt1_NIST1573a %>%
    mutate(Interaction = interaction(Method, Optimization))
  
  NIST1573a_K <- ggplot(dt1_NIST1573a, aes(x = Total_Weight, y = K, color = Interaction)) +
    geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
    geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
    #geom_hline(yintercept = 246.3, color = "darkgrey", size = 1) + # Horizontal line at 5.69
    #geom_hline(yintercept = 253.4, color = "grey", size = 0.5) + # 
    #geom_hline(yintercept = 239.2, color = "grey", size = 0.5) + #
    theme_classic() +
    #scale_y_continuous(limits = c(180, 290), breaks = seq(180, 290, by = 10)) +
    labs(title = "Tomato Leaves NIST 1573a",
         x = "Total Weight",
         y = "K [ppm]",
         color = "Method & Optimization") +
    scale_color_brewer(palette = "Set1") # Adjust the color palette
  
  
  
  #NIST 1568b K
  dt1_NIST1568b <- subset(dt1, Sample_ID=="NIST1568b")
  dt1_NIST1568b <- dt1_NIST1568b %>%
    mutate(Interaction = interaction(Method, Optimization))
  
  NIST1568b_K <- ggplot(dt1_NIST1568b, aes(x = Total_Weight, y = K, color = Interaction)) +
    geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
    geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
    #geom_hline(yintercept = 19.2, color = "darkgrey", size = 1) + # Horizontal line at 5.69
    #geom_hline(yintercept = 21, color = "grey", size = 0.5) + # 
    #geom_hline(yintercept = 17.4, color = "grey", size = 0.5) + #
    theme_classic() +
    #scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 5)) +
    labs(title = "Rice Flour NIST 1568b",
         x = "Total Weight",
         y = "K [ppm]",
         color = "Method & Optimization") +
    scale_color_brewer(palette = "Set1") # Adjust the color palette
  
  
  
  #NIST 1570a K
  dt1_NIST1570a <- subset(dt1, Sample_ID=="NIST1570")
  dt1_NIST1570a <- dt1_NIST1570a %>%
    mutate(Interaction = interaction(Method, Optimization))
  
  NIST1570a_K <- ggplot(dt1_NIST1570a, aes(x = Total_Weight, y = K, color = Interaction)) +
    geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
    geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
    #geom_hline(yintercept = 76, color = "darkgrey", size = 1) + # Horizontal line at 5.69
    #geom_hline(yintercept = 77.2, color = "grey", size = 0.5) + # 
    #geom_hline(yintercept = 74.8, color = "grey", size = 0.5) + #
    theme_classic() +
    #scale_y_continuous(limits = c(55, 92), breaks = seq(55, 100, by = 5)) +
    labs(title = "Spinach leaves NIST 1570",
         x = "Total Weight",
         y = "K [ppm]",
         color = "Method & Optimization") +
    scale_color_brewer(palette = "Set1") # Adjust the color palette
  
  
  #NIST 1575a K
  dt1_NIST1575a <- subset(dt1, Sample_ID=="NIST1575a")
  dt1_NIST1575a <- dt1_NIST1575a %>%
    mutate(Interaction = interaction(Method, Optimization))
  
  NIST1575a_K <- ggplot(dt1_NIST1575a, aes(x = Total_Weight, y = K, color = Interaction)) +
    geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
    geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
    #geom_hline(yintercept = 488, color = "darkgrey", size = 1) + # Horizontal line at 5.69
    #geom_hline(yintercept = 500, color = "grey", size = 0.5) + # 
    #geom_hline(yintercept = 476, color = "grey", size = 0.5) + #
    theme_classic() +
    #scale_y_continuous(limits = c(250, 575), breaks = seq(250, 600, by = 50)) +
    labs(title = "Pine Needles NIST 1575a",
         x = "Total Weight",
         y = "K [ppm]",
         color = "Method & Optimization") +
    scale_color_brewer(palette = "Set1") # Adjust the color palette
  
  
  ggarrange(NIST1515_Mn,NIST1573a_Mn,NIST1568b_Mn,NIST1570a_Mn,NIST1575a_Mn,
            ncol = 3, nrow = 2, 
            common.legend = TRUE, legend = "bottom")
  
}


#P-NISTS
{
  #NIST 1515 P
  dt1_NIST1515 <- subset(dt1, Sample_ID=="NIST1515")
  dt1_NIST1515 <- dt1_NIST1515 %>%
    mutate(Interaction = interaction(Method, Optimization))
  
  NIST1515_P <- ggplot(dt1_NIST1515, aes(x = Total_Weight, y = P, color = Interaction)) +
    geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
    geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
    geom_hline(yintercept = 1593, color = "darkgrey", size = 1) + # Horizontal line at 5.69
    geom_hline(yintercept = 1661, color = "grey", size = 0.5) + # 
    geom_hline(yintercept = 1525, color = "grey", size = 0.5) + #
    theme_classic() +
    #scale_y_continuous(limits = c(30, 65), breaks = seq(30, 65, by = 5)) +
    labs(title = "Apple Leaves NIST 1515",
         x = "Total Weight",
         y = "P [ppm]",
         color = "Method & Optimization") +
    scale_color_brewer(palette = "Set1") # Adjust the color palette  
  
  
  #NIST 1573a P
  
  dt1_NIST1573a <- subset(dt1, Sample_ID=="NIST1573a")
  dt1_NIST1573a <- dt1_NIST1573a %>%
    mutate(Interaction = interaction(Method, Optimization))
  
  NIST1573a_P <- ggplot(dt1_NIST1573a, aes(x = Total_Weight, y = P, color = Interaction)) +
    geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
    geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
    geom_hline(yintercept = 2161, color = "darkgrey", size = 1) + # Horizontal line at 5.69
    geom_hline(yintercept = 2189, color = "grey", size = 0.5) + # 
    geom_hline(yintercept = 2133, color = "grey", size = 0.5) + #
    theme_classic() +
    #scale_y_continuous(limits = c(180, 290), breaks = seq(180, 290, by = 10)) +
    labs(title = "Tomato Leaves NIST 1573a",
         x = "Total Weight",
         y = "P [ppm]",
         color = "Method & Optimization") +
    scale_color_brewer(palette = "Set1") # Adjust the color palette
  
  
  
  #NIST 1568b P
  dt1_NIST1568b <- subset(dt1, Sample_ID=="NIST1568b")
  dt1_NIST1568b <- dt1_NIST1568b %>%
    mutate(Interaction = interaction(Method, Optimization))
  
  NIST1568b_P <- ggplot(dt1_NIST1568b, aes(x = Total_Weight, y = P, color = Interaction)) +
    geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
    geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
    geom_hline(yintercept = 1530, color = "darkgrey", size = 1) + # Horizontal line at 5.69
    geom_hline(yintercept = 1570, color = "grey", size = 0.5) + # 
    geom_hline(yintercept = 1490, color = "grey", size = 0.5) + #
    theme_classic() +
    #scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 5)) +
    labs(title = "Rice Flour NIST 1568b",
         x = "Total Weight",
         y = "P [ppm]",
         color = "Method & Optimization") +
    scale_color_brewer(palette = "Set1") # Adjust the color palette
  
  
  
  #NIST 1570a P
  dt1_NIST1570a <- subset(dt1, Sample_ID=="NIST1570")
  dt1_NIST1570a <- dt1_NIST1570a %>%
    mutate(Interaction = interaction(Method, Optimization))
  
  NIST1570a_P <- ggplot(dt1_NIST1570a, aes(x = Total_Weight, y = P, color = Interaction)) +
    geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
    geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
    geom_hline(yintercept = 5187, color = "darkgrey", size = 1) + # Horizontal line at 5.69
    geom_hline(yintercept = 5254, color = "grey", size = 0.5) + # 
    geom_hline(yintercept = 5120, color = "grey", size = 0.5) + #
    theme_classic() +
    #scale_y_continuous(limits = c(55, 92), breaks = seq(55, 100, by = 5)) +
    labs(title = "Spinach leaves NIST 1570",
         x = "Total Weight",
         y = "K [ppm]",
         color = "Method & Optimization") +
    scale_color_brewer(palette = "Set1") # Adjust the color palette
  
  
  #NIST 1575a P
  dt1_NIST1575a <- subset(dt1, Sample_ID=="NIST1575a")
  dt1_NIST1575a <- dt1_NIST1575a %>%
    mutate(Interaction = interaction(Method, Optimization))
  
  NIST1575a_P <- ggplot(dt1_NIST1575a, aes(x = Total_Weight, y = P, color = Interaction)) +
    geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
    geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
    geom_hline(yintercept = 1070, color = "darkgrey", size = 1) + # Horizontal line at 5.69
    geom_hline(yintercept = 990, color = "grey", size = 0.5) + # 
    geom_hline(yintercept = 1150, color = "grey", size = 0.5) + #
    theme_classic() +
    #scale_y_continuous(limits = c(250, 575), breaks = seq(250, 600, by = 50)) +
    labs(title = "Pine Needles NIST 1575a",
         x = "Total Weight",
         y = "P [ppm]",
         color = "Method & Optimization") +
    scale_color_brewer(palette = "Set1") # Adjust the color palette
  
  
  ggarrange(NIST1515_P,NIST1573a_P,NIST1568b_P,NIST1570a_P,NIST1575a_P,
            ncol = 3, nrow = 2, 
            common.legend = TRUE, legend = "bottom")
  
}

#Se-NISTS
{
  #NIST 1515 Se
  dt1_NIST1515 <- subset(dt1, Sample_ID=="NIST1515")
  dt1_NIST1515 <- dt1_NIST1515 %>%
    mutate(Interaction = interaction(Method, Optimization))
  
  NIST1515_Se <- ggplot(dt1_NIST1515, aes(x = Total_Weight, y = Se, color = Interaction)) +
    geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
    geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
    geom_hline(yintercept = 1593, color = "darkgrey", size = 1) + # Horizontal line at 5.69
    geom_hline(yintercept = 1661, color = "grey", size = 0.5) + # 
    geom_hline(yintercept = 1525, color = "grey", size = 0.5) + #
    theme_classic() +
    #scale_y_continuous(limits = c(30, 65), breaks = seq(30, 65, by = 5)) +
    labs(title = "Apple Leaves NIST 1515",
         x = "Total Weight",
         y = "Se [ppm]",
         color = "Method & Optimization") +
    scale_color_brewer(palette = "Set1") # Adjust the color palette  
  
  
  #NIST 1573a P
  
  dt1_NIST1573a <- subset(dt1, Sample_ID=="NIST1573a")
  dt1_NIST1573a <- dt1_NIST1573a %>%
    mutate(Interaction = interaction(Method, Optimization))
  
  NIST1573a_Se <- ggplot(dt1_NIST1573a, aes(x = Total_Weight, y = Se, color = Interaction)) +
    geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
    geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
    geom_hline(yintercept = 2161, color = "darkgrey", size = 1) + # Horizontal line at 5.69
    geom_hline(yintercept = 2189, color = "grey", size = 0.5) + # 
    geom_hline(yintercept = 2133, color = "grey", size = 0.5) + #
    theme_classic() +
    #scale_y_continuous(limits = c(180, 290), breaks = seq(180, 290, by = 10)) +
    labs(title = "Tomato Leaves NIST 1573a",
         x = "Total Weight",
         y = "Se [ppm]",
         color = "Method & Optimization") +
    scale_color_brewer(palette = "Set1") # Adjust the color palette
  
  
  
  #NIST 1568b P
  dt1_NIST1568b <- subset(dt1, Sample_ID=="NIST1568b")
  dt1_NIST1568b <- dt1_NIST1568b %>%
    mutate(Interaction = interaction(Method, Optimization))
  
  NIST1568b_Se <- ggplot(dt1_NIST1568b, aes(x = Total_Weight, y = Se, color = Interaction)) +
    geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
    geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
    geom_hline(yintercept = 1530, color = "darkgrey", size = 1) + # Horizontal line at 5.69
    geom_hline(yintercept = 1570, color = "grey", size = 0.5) + # 
    geom_hline(yintercept = 1490, color = "grey", size = 0.5) + #
    theme_classic() +
    #scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 5)) +
    labs(title = "Rice Flour NIST 1568b",
         x = "Total Weight",
         y = "Se [ppm]",
         color = "Method & Optimization") +
    scale_color_brewer(palette = "Set1") # Adjust the color palette
  
  
  
  #NIST 1570a P
  dt1_NIST1570a <- subset(dt1, Sample_ID=="NIST1570")
  dt1_NIST1570a <- dt1_NIST1570a %>%
    mutate(Interaction = interaction(Method, Optimization))
  
  NIST1570a_Se <- ggplot(dt1_NIST1570a, aes(x = Total_Weight, y = Se, color = Interaction)) +
    geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
    geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
    geom_hline(yintercept = 5187, color = "darkgrey", size = 1) + # Horizontal line at 5.69
    geom_hline(yintercept = 5254, color = "grey", size = 0.5) + # 
    geom_hline(yintercept = 5120, color = "grey", size = 0.5) + #
    theme_classic() +
    #scale_y_continuous(limits = c(55, 92), breaks = seq(55, 100, by = 5)) +
    labs(title = "Spinach leaves NIST 1570",
         x = "Total Weight",
         y = "Se [ppm]",
         color = "Method & Optimization") +
    scale_color_brewer(palette = "Set1") # Adjust the color palette
  
  
  #NIST 1575a P
  dt1_NIST1575a <- subset(dt1, Sample_ID=="NIST1575a")
  dt1_NIST1575a <- dt1_NIST1575a %>%
    mutate(Interaction = interaction(Method, Optimization))
  
  NIST1575a_Se <- ggplot(dt1_NIST1575a, aes(x = Total_Weight, y = Se, color = Interaction)) +
    geom_boxplot(position = position_dodge(0)) + # No dodging for boxplots
    geom_jitter(width = 0.1, size = 2, alpha = 0.5, shape = 16) + # Jitter points with specified shape
    geom_hline(yintercept = 1070, color = "darkgrey", size = 1) + # Horizontal line at 5.69
    geom_hline(yintercept = 990, color = "grey", size = 0.5) + # 
    geom_hline(yintercept = 1150, color = "grey", size = 0.5) + #
    theme_classic() +
    #scale_y_continuous(limits = c(250, 575), breaks = seq(250, 600, by = 50)) +
    labs(title = "Pine Needles NIST 1575a",
         x = "Total Weight",
         y = "Se [ppm]",
         color = "Method & Optimization") +
    scale_color_brewer(palette = "Set1") # Adjust the color palette
  
  
  ggarrange(NIST1515_Se,NIST1573a_Se,NIST1568b_Se,NIST1570a_Se,NIST1575a_Se,
            ncol = 3, nrow = 2, 
            common.legend = TRUE, legend = "bottom")
  
}





#Modified Bland altman - difference vs TW














#### Median
{
  
  library(dplyr)
  
  # Assuming dt3 is your dataset and columns are named appropriately
  element_columns <- c("P", "S", "K", "Ca", "Mn", "Fe", "Ni", "Cu", "Zn", "As", "Se", "Re")
  metadata_columns <- c("Sample", "Sample_ID", "Date", "File", "Total_Weight", "Method", "Material", "Optimization", "P_NIST", "S_NIST", "K_NIST", "Ca_NIST", "Mn_NIST", "Fe_NIST", "Ni_NIST", "Cu_NIST", "Zn_NIST", "As_NIST", "Se_NIST", "Re_NIST")
  
  # Compute the median for each element within the specified groups and add metadata
  dt_median <- dt1 %>%
    group_by(Sample_ID, Total_Weight, Method, Optimization) %>%
    summarise(across(all_of(element_columns), median, na.rm = TRUE), .groups = "drop") %>%
    # Ensure each group is represented once for the join to prevent duplicates
    distinct(Sample_ID, Total_Weight, Method, Optimization, .keep_all = TRUE) %>%
    # Join metadata based on the most frequent occurrence or a specific entry for each group
    left_join(dt1 %>%
                select(Sample_ID, Total_Weight, Method, Optimization, all_of(metadata_columns)) %>%
                group_by(Sample_ID, Total_Weight, Method, Optimization) %>%
                slice(1), # Assuming the first row is representative for the metadata
              by = c("Sample_ID", "Total_Weight", "Method", "Optimization"))
  
  # Checking if all 'Method' types are present
  print(unique(dt_median$Method))
  
  # Check the resulting dataframe
  print(dt_median)

  write.xlsx(dt_median, 'NISTS_May24-MEDIAN.xlsx')
  
}



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
    
    dt1$Ca[dt1$Ca == 0] <- 10/2
    dt1$Ti[dt1$Ti == 0] <- 5/2
    dt1$Cr[dt1$Cr == 0] <- 2/2
    dt1$Mn[dt1$Mn == 0] <- 1/2
    dt1$Fe[dt1$Fe == 0] <- 5/2
    dt1$Co[dt1$Co == 0] <- 3/2
    dt1$Ni[dt1$Ni == 0] <- 0.2/2
    dt1$Cu[dt1$Cu == 0] <- 0.5/2
    dt1$Zn[dt1$Zn == 0] <- 0.6/2
    dt1$As[dt1$As == 0] <- 0.1/2
    dt1$Se[dt1$Se == 0] <- 0.1/2
    dt1$Cd[dt1$Cd == 0] <- 1/2
    dt1$Re[dt1$Re == 0] <- 0.5/2
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
  mutate(Cu_diff = Cu_mean - Cu_NIST,
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
  scale_y_continuous(limits = c(-6, 3), breaks = seq(-6, 3, by = 3)) +
  scale_shape_manual(values = shape_values) +  # Manually set shapes
  scale_colour_identity() +  # Use colors directly from the dataframe
  labs(x = "Total Weight", y = "Relative Difference (PXRF - NIST)", shape = "CRM:") +
  theme_classic2()

# Display the plot
print(diffplot)

# Save the plot with specified dimensions
ggsave("diffplot.pdf", plot = diffplot, width = 10, height = 2.5, units = "in")

ggsave("diffplot.pdf", plot = diffplot, width = 10, height = 2, units = "in")

ggsave("diffplot4.pdf", plot = diffplot, width = 10, height = 1.9, units = "in")

}

# FINAL PLOT - Zn
{
  # Calculate Cu_diff for the entire dataset
  dt_stats <- dt_stats %>%
    mutate(Zn_diff = Zn_mean - Zn_NIST,
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
    scale_y_continuous(limits = c(-20, 16), breaks = seq(-20, 15, by = 7)) +
    scale_shape_manual(values = shape_values) +  # Manually set shapes
    scale_colour_identity() +  # Use colors directly from the dataframe
    labs(x = "Total Weight", y = "Relative Difference (PXRF - NIST)", shape = "CRM:") +
    theme_classic2()
  
  # Display the plot
  print(diffplot)
  
  # Save the plot with specified dimensions
  ggsave("diffplot.pdf", plot = diffplot, width = 10, height = 2.5, units = "in")
  
  ggsave("diffplot4.pdf", plot = diffplot, width = 10, height = 1.9, units = "in")
}


# FINAL PLOT - Mn
{
  # Calculate Cu_diff for the entire dataset
  dt_stats <- dt_stats %>%
    mutate(Mn_diff = Mn_mean - Mn_NIST,
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
  
  ggsave("diffplot4.pdf", plot = diffplot, width = 10, height = 1.9, units = "in")
  
}





# FINAL PLOT - Se
{
  # Calculate Cu_diff for the entire dataset
  dt_stats <- dt_stats %>%
    mutate(Se_diff = Se_mean - Se_NIST,
           Total_Weight = as.factor(Total_Weight))  # Convert Total_Weight to factor
  
  # Aggregate data to ensure a single entry per Sample_ID, Total_Weight, Method, and Optimization
  agg_data <- dt_stats %>%
    group_by(Sample_ID, Total_Weight, Method, Optimization) %>%
    summarise(Se_diff = mean(Se_diff, na.rm = TRUE),
              Se_se = mean(Se_se, na.rm = TRUE),
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
  diffplot <- ggplot(agg_data, aes(x = Total_Weight, y = Se_diff, shape = Sample_ID)) +
    geom_point(aes(colour = genecolour), position = pos, size = 2.5) +  # Plot all points with specific colors
    geom_errorbar(aes(ymin = Se_diff - Se_se, ymax = Se_diff + Se_se, colour = genecolour, width = width),
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
  ggsave("diffplot3.pdf", plot = diffplot, width = 10, height = 2.5, units = "in")
  
}



# FINAL PLOT - Fe
{
  # Calculate Cu_diff for the entire dataset
  dt_stats <- dt_stats %>%
    mutate(Fe_diff = Fe_mean - Fe_NIST,
           Total_Weight = as.factor(Total_Weight))  # Convert Total_Weight to factor
  
  # Aggregate data to ensure a single entry per Sample_ID, Total_Weight, Method, and Optimization
  agg_data <- dt_stats %>%
    group_by(Sample_ID, Total_Weight, Method, Optimization) %>%
    summarise(Fe_diff = mean(Fe_diff, na.rm = TRUE),
              Fe_se = mean(Fe_se, na.rm = TRUE),
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
  diffplot <- ggplot(agg_data, aes(x = Total_Weight, y = Fe_diff, shape = Sample_ID)) +
    geom_point(aes(colour = genecolour), position = pos, size = 2.5) +  # Plot all points with specific colors
    geom_errorbar(aes(ymin = Fe_diff - Fe_se, ymax = Fe_diff + Fe_se, colour = genecolour, width = width),
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
  ggsave("diffplot3.pdf", plot = diffplot, width = 10, height = 2.5, units = "in")
  
  ggsave("diffplot4.pdf", plot = diffplot, width = 10, height = 1.9, units = "in")
  
}

#t test - FINAL


library(dplyr)

# Filter the data for the desired conditions
dt_stats2 <- dt_stats %>% 
  filter(Optimization == "T1.5", Method == "cup")

# Ensure the relevant columns are numeric
dt_stats2[, 5:56] <- sapply(dt_stats2[, 5:56], as.numeric)

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


write.xlsx(results, "T_Test_Results_FINAL.xlsx")


# Additional t-tests for point and T

dt_0.05 <- subset(dt_stats, Total_Weight=="0.05")
dt_0.05_point <- subset(dt_0.05, Method=="point")
dt_0.05_point_T15 <- subset(dt_0.05_point, Optimization=="T1.5")
difference <- dt_0.05_point_T15$Cu_NIST - dt_0.05_point_T15$Cu_mean
t_test <- t.test(difference, mu = 0) # p val 0.5889

dt_0.05 <- subset(dt_stats, Total_Weight=="0.05")
dt_0.05_point <- subset(dt_0.05, Method=="point")
dt_0.05_point_T6 <- subset(dt_0.05_point, Optimization=="T6")
difference <- dt_0.05_point_T6$Cu_NIST - dt_0.05_point_T6$Cu_mean
t_test <- t.test(difference, mu = 0) # p val 0.4414


dt_2 <- subset(dt_stats, Total_Weight=="2")
dt_2_cup <- subset(dt_2, Method=="cup")
dt_2_cup_T6 <- subset(dt_2_cup, Optimization=="T6")
difference <- dt_2_cup_T6$Cu_NIST - dt_2_cup_T6$Cu_mean
t_test <- t.test(difference, mu = 0) # p val 0.03526

dt_2 <- subset(dt_stats, Total_Weight=="2")
dt_2_cup <- subset(dt_2, Method=="cup")
dt_2_cup_T15 <- subset(dt_2_cup, Optimization=="T1.5")
difference <- dt_2_cup_T15$Cu_NIST - dt_2_cup_T15$Cu_mean
t_test <- t.test(difference, mu = 0) # p val 0.09106





dt_0.05 <- subset(dt_stats, Total_Weight=="0.05")
dt_0.05_point <- subset(dt_0.05, Method=="point")
dt_0.05_point_T15 <- subset(dt_0.05_point, Optimization=="T1.5")
difference <- dt_0.05_point_T15$Zn_NIST - dt_0.05_point_T15$Zn_mean
t_test <- t.test(difference, mu = 0) # p val 0.03

dt_0.05 <- subset(dt_stats, Total_Weight=="0.05")
dt_0.05_point <- subset(dt_0.05, Method=="point")
dt_0.05_point_T6 <- subset(dt_0.05_point, Optimization=="T6")
difference <- dt_0.05_point_T6$Zn_NIST - dt_0.05_point_T6$Zn_mean
t_test <- t.test(difference, mu = 0) # p val 0.0508


dt_2 <- subset(dt_stats, Total_Weight=="2")
dt_2_cup <- subset(dt_2, Method=="cup")
dt_2_cup_T6 <- subset(dt_2_cup, Optimization=="T6")
difference <- dt_2_cup_T6$Zn_NIST - dt_2_cup_T6$Zn_mean
t_test <- t.test(difference, mu = 0) # p val 0.2







dt_0.05 <- subset(dt_stats, Total_Weight=="0.05")
dt_0.05_point <- subset(dt_0.05, Method=="point")
dt_0.05_point_T15 <- subset(dt_0.05_point, Optimization=="T1.5")
difference <- dt_0.05_point_T15$Mn_NIST - dt_0.05_point_T15$Mn_mean
t_test <- t.test(difference, mu = 0) # p val 0.037

dt_0.05 <- subset(dt_stats, Total_Weight=="0.05")
dt_0.05_point <- subset(dt_0.05, Method=="point")
dt_0.05_point_T6 <- subset(dt_0.05_point, Optimization=="T6")
difference <- dt_0.05_point_T6$Mn_NIST - dt_0.05_point_T6$Mn_mean
t_test <- t.test(difference, mu = 0) # p val 0.03085


dt_2 <- subset(dt_stats, Total_Weight=="2")
dt_2_cup <- subset(dt_2, Method=="cup")
dt_2_cup_T6 <- subset(dt_2_cup, Optimization=="T6")
difference <- dt_2_cup_T6$Mn_NIST - dt_2_cup_T6$Mn_mean
t_test <- t.test(difference, mu = 0) # p val 0.1716





dt_0.05 <- subset(dt_stats, Total_Weight=="0.05")
dt_0.05_point <- subset(dt_0.05, Method=="cup")
dt_0.05_point_T15 <- subset(dt_0.05_point, Optimization=="T1.5")
difference <- dt_0.05_point_T15$Mn_NIST - dt_0.05_point_T15$Mn_mean
t_test <- t.test(difference, mu = 0) # p val 0.1

glimpse(dt_stats)




dt_0.05 <- subset(dt_stats, Total_Weight=="0.05")
dt_0.05_point <- subset(dt_0.05, Method=="point")
dt_0.05_point_T15 <- subset(dt_0.05_point, Optimization=="T1.5")
difference <- dt_0.05_point_T15$Fe_NIST - dt_0.05_point_T15$Fe_mean
t_test <- t.test(difference, mu = 0) # p val 0.1577

dt_0.05 <- subset(dt_stats, Total_Weight=="0.05")
dt_0.05_point <- subset(dt_0.05, Method=="point")
dt_0.05_point_T6 <- subset(dt_0.05_point, Optimization=="T6")
difference <- dt_0.05_point_T6$Fe_NIST - dt_0.05_point_T6$Fe_mean
t_test <- t.test(difference, mu = 0) # p val 0.069


dt_2 <- subset(dt_stats, Total_Weight=="2")
dt_2_cup <- subset(dt_2, Method=="cup")
dt_2_cup_T6 <- subset(dt_2_cup, Optimization=="T6")
difference <- dt_2_cup_T6$Fe_NIST - dt_2_cup_T6$Fe_mean
t_test <- t.test(difference, mu = 0) # p val 0.1447


# Save results to Excel
#write.xlsx(results, "T_Test_Results.xlsx")


































glimpse(filtered_data)

#Caly pozostaly syf

{

####Error calculation
{
dt_error <-read.delim("NISTS_04.18.2024_trimmed_MEDIAN_NIST.txt")

dt_error[, 5:28] <- sapply(dt_error[, 5:28], as.numeric)

dt_error <- dt_error %>%
  mutate(across(where(is.numeric), ~na_if(., 0)))



dt_error <- dt_error %>%
  mutate(
    P_error = 100 * abs(P - P_NIST) / P_NIST,
    S_error = 100 * abs(S - S_NIST) / S_NIST,
    K_error = 100 * abs(K - K_NIST) / K_NIST,
    Ca_error = 100 * abs(Ca - Ca_NIST) / Ca_NIST,
    Mn_error = 100 * abs(Mn - Mn_NIST) / Mn_NIST,
    Fe_error = 100 * abs(Fe - Fe_NIST) / Fe_NIST,
    Ni_error = 100 * abs(Ni - Ni_NIST) / Ni_NIST,
    Cu_error = 100 * abs(Cu - Cu_NIST) / Cu_NIST,
    Zn_error = 100 * abs(Zn - Zn_NIST) / Zn_NIST,
    As_error = ifelse(!is.na(As_NIST), 100 * abs(As - As_NIST) / As_NIST, NA),  # Handle NAs
    Se_error = ifelse(!is.na(Se_NIST), 100 * abs(Se - Se_NIST) / Se_NIST, NA),  # Handle NAs
    Re_error = ifelse(!is.na(Re_NIST), 100 * abs(Re - Re_NIST) / Re_NIST, NA)   # Handle NAs
  )


#dt_error <- dt_error %>%
#  mutate(Total_Weight = as.factor(Total_Weight))

dt_error <- subset(dt_error, Optimization=="T1.5")

Cu_error <- ggplot(dt_error, aes(x = Total_Weight, y = Cu_error, color = Sample_ID, shape=Method)) +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, by = 5)) +
  labs(title = "Cu Error",
       x = "Total Weight",
       y = "Cu [ppm]",
       color = "Method & Optimization") +
  scale_color_brewer(palette = "Set1") # Adjust the color palette




Zn_error <- ggplot(dt_error, aes(x = Total_Weight, y = Zn_error, color = Sample_ID, shape=Method)) +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0, 7.5), breaks = seq(0, 7.5, by = 0.5)) +
  labs(title = "Zn Error",
       x = "Total Weight",
       y = "Zn [ppm]",
       color = "Method & Optimization") +
  scale_color_brewer(palette = "Set1") # Adjust the color palette


Mn_error <- ggplot(dt_error, aes(x = Total_Weight, y = Mn_error, color = Sample_ID, shape=Method)) +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0, 7.5), breaks = seq(0, 7.5, by = 0.5)) +
  labs(title = "Mn Error",
       x = "Total Weight",
       y = "Mn [ppm]",
       color = "Method & Optimization") +
  scale_color_brewer(palette = "Set1") # Adjust the color palette

Fe_error <- ggplot(dt_error, aes(x = Total_Weight, y = Fe_error, color = Sample_ID, shape=Method)) +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0, 7.5), breaks = seq(0, 7.5, by = 0.5)) +
  labs(title = "Fe Error",
       x = "Total Weight",
       y = "Fe [ppm]",
       color = "Method & Optimization") +
  scale_color_brewer(palette = "Set1") # Adjust the color palette
}
########

#### RMSE ERROR
{
dt_error_rmse <-read.delim("NIST_04.18.2024_PXRFandNIST.txt")


dt_error_rmse <- dt_error_rmse %>% 
  filter(Sample != "QA")

dt_error_rmse <- dt_error_rmse %>% 
  filter(Optimization == "T1.5")

dt_error_rmse <- dt_error_rmse %>% 
  filter(Method == "cup")


str(dt_error_rmse)

dt_error_rmse[, 9:45] <- sapply(dt_error_rmse[, 9:45], as.numeric)




dt_error_rmse <- dt_error_rmse %>%
  select(-matches("_unc"))
  

rmsd_data <- dt_error_rmse %>%
  rowwise() %>%
  mutate(across(P:Re, ~ (. - get(paste0(cur_column(), "_NIST")))^2, .names = "diff_{col}")) %>%
  group_by(Sample_ID, Total_Weight) %>%
  summarise(across(starts_with("diff_"), ~ sqrt(mean(., na.rm = TRUE)), .names = "rmsd_{col}")) %>%
  ungroup()



Cu_error_RMSE <- ggplot(rmsd_data, aes(x = Total_Weight, y = rmsd_diff_Cu, color = Sample_ID)) +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, by = 5)) +
  labs(title = "Cu RMSE",
       x = "Total Weight",
       y = "RMSE [ppm]",
       color = "Method & Optimization") +
  scale_color_brewer(palette = "Set1") # Adjust the color palette

Zn_error_RMSE <- ggplot(rmsd_data, aes(x = Total_Weight, y = rmsd_diff_Zn, color = Sample_ID)) +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, by = 5)) +
  labs(title = "Zn RMSE",
       x = "Total Weight",
       y = "RMSE [ppm]",
       color = "Method & Optimization") +
  scale_color_brewer(palette = "Set1") # Adjust the color palette

Mn_error_RMSE <- ggplot(rmsd_data, aes(x = Total_Weight, y = rmsd_diff_Mn, color = Sample_ID)) +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, by = 5)) +
  labs(title = "Mn RMSE",
       x = "Total Weight",
       y = "RMSE [ppm]",
       color = "Method & Optimization") +
  scale_color_brewer(palette = "Set1") # Adjust the color palette

Fe_error_RMSE <- ggplot(rmsd_data, aes(x = Total_Weight, y = rmsd_diff_Fe, color = Sample_ID)) +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, by = 5)) +
  labs(title = "Fe RMSE",
       x = "Total Weight",
       y = "RMSE [ppm]",
       color = "Method & Optimization") +
  scale_color_brewer(palette = "Set1") # Adjust the color palette

Se_error_RMSE <- ggplot(rmsd_data, aes(x = Total_Weight, y = rmsd_diff_Se, color = Sample_ID)) +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, by = 5)) +
  labs(title = "Se RMSE",
       x = "Total Weight",
       y = "RMSE [ppm]",
       color = "Method & Optimization") +
  scale_color_brewer(palette = "Set1") # Adjust the color palette


S_error_RMSE <- ggplot(rmsd_data, aes(x = Total_Weight, y = rmsd_diff_S, color = Sample_ID)) +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, by = 5)) +
  labs(title = "S RMSE",
       x = "Total Weight",
       y = "RMSE [ppm]",
       color = "Method & Optimization") +
  scale_color_brewer(palette = "Set1") # Adjust the color palette


P_error_RMSE <- ggplot(rmsd_data, aes(x = Total_Weight, y = rmsd_diff_P, color = Sample_ID)) +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, by = 5)) +
  labs(title = "P RMSE",
       x = "Total Weight",
       y = "RMSE [ppm]",
       color = "Method & Optimization") +
  scale_color_brewer(palette = "Set1") # Adjust the color palette

K_error_RMSE <- ggplot(rmsd_data, aes(x = Total_Weight, y = rmsd_diff_K, color = Sample_ID)) +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, by = 5)) +
  labs(title = "K RMSE",
       x = "Total Weight",
       y = "RMSE [ppm]",
       color = "Method & Optimization") +
  scale_color_brewer(palette = "Set1") # Adjust the color palette


cor.test(rmsd_data$Total_Weight, rmsd_data$rmsd_diff_Fe, method = "spearman")
cor.test(rmsd_data$Total_Weight, rmsd_data$rmsd_diff_Cu, method = "spearman")
cor.test(rmsd_data$Total_Weight, rmsd_data$rmsd_diff_Zn, method = "spearman")
cor.test(rmsd_data$Total_Weight, rmsd_data$rmsd_diff_Mn, method = "spearman")
cor.test(rmsd_data$Total_Weight, rmsd_data$rmsd_diff_Se, method = "spearman")


RMSE_NIST1515 <- subset(rmsd_data, Sample_ID=="NIST1515")

Zn_error_RMSE <- ggplot(RMSE_NIST1515, aes(x = Total_Weight, y = rmsd_diff_Zn, color = Sample_ID)) +
  geom_point() +
  theme_classic() +
  #scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, by = 5)) +
  labs(title = "Zn RMSE",
       x = "Total Weight",
       y = "RMSE [ppm]",
       color = "Method & Optimization") +
  scale_color_brewer(palette = "Set1") # Adjust the color palette





# Assuming your dataframe is called rmsd_data
# Filter out unnecessary columns and standardize the RMSE values
rmsd_data_long <- rmsd_data %>%
  pivot_longer(cols = starts_with("rmsd_diff_"), names_to = "Element", values_to = "RMSE") %>%
  group_by(Element) %>%
  mutate(
    Mean_RMSE = mean(RMSE, na.rm = TRUE),
    SD_RMSE = sd(RMSE, na.rm = TRUE),
    Std_RMSE = (RMSE - Mean_RMSE) / SD_RMSE
  ) %>%
  ungroup() %>%
  select(Sample_ID, Total_Weight, Element, Std_RMSE) %>%
  mutate(Element = sub("rmsd_diff_", "", Element))  # Clean element names

# Create the heatmap
ggplot(rmsd_data_long, aes(x = as.factor(Total_Weight), y = Element, fill = Std_RMSE)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       na.value = "grey50", limits = c(-3, 3)) +  # Adjust limits as needed
  labs(title = "Standardized RMSE Heatmap", x = "Total Weight", y = "Element") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

}


{
dt_error_rmse <-read.delim("NIST_04.18.2024_PXRFandNIST_TWSAME.txt")
dt_error_rmse <- dt_error_rmse %>% 
  filter(Sample != "QA")
dt_error_rmse <- dt_error_rmse %>% 
  filter(Optimization == "T1.5")
dt_error_rmse <- dt_error_rmse %>% 
  filter(Method == "cup")
dt_error_rmse[, 9:45] <- sapply(dt_error_rmse[, 9:45], as.numeric)
dt_error_rmse <- dt_error_rmse %>%
  select(-matches("_unc"))

rmsd_data <- dt_error_rmse %>%
  rowwise() %>%
  mutate(across(P:Re, ~ (. - get(paste0(cur_column(), "_NIST")))^2, .names = "diff_{col}")) %>%
  group_by(Sample_ID, Total_Weight) %>%
  summarise(across(starts_with("diff_"), ~ sqrt(mean(., na.rm = TRUE)), .names = "rmsd_{col}")) %>%
  ungroup()


# Updated normalization function
normalize <- function(x) {
  # Check if all values are NA or if max equals min
  if(all(is.na(x)) || max(x, na.rm = TRUE) == min(x, na.rm = TRUE)) {
    return(rep(NA, length(x))) # Return NA if not possible to normalize
  } else {
    return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
  }
}

# Apply normalization to each RMSE column in your dataframe
rmsd_data_normalized <- rmsd_data %>%
  mutate(across(starts_with("rmsd_diff_"), normalize, .names = "norm_{.col}"))

rmsd_data_long <- rmsd_data_normalized %>%
  pivot_longer(
    cols = starts_with("norm_rmsd_diff_"), 
    names_to = "Element", 
    values_to = "Normalized_RMSE",
    names_prefix = "norm_rmsd_diff_"
  ) %>%
  mutate(Element = gsub("_", " ", Element))  # Optional: replace underscores with spaces for better label readability

# View the long format data
print(rmsd_data_long)

ggplot(rmsd_data_long, aes(x = as.factor(Total_Weight), y = Element, fill = Normalized_RMSE)) +
  geom_tile() +  # Use geom_tile for heatmap
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       na.value = "grey50", limits = c(0, 1)) +  # Adjust limits and colors as needed
  labs(title = "Normalized RMSE Heatmap", x = "Total Weight", y = "Element") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




}
  
  











#### Adding mass from ICP
setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/ICP-Data")
dt <-read.delim("ICP-MASS.txt")



aggregated_data <- dt %>%
  group_by(Sample_ID) %>%
  summarize(Sample_mass = sum(Sample_mass, na.rm = TRUE))

#write.xlsx(aggregated_data, "Aggregated_Sample_Mass.xlsx")


}



#T -test - one more time - final2 - tutaj uwzgledniam wszystkie punkty!!!!!!!!!!! a nie meany, wyniki sa lepsze!!!

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
    
    dt1$Ca[dt1$Ca == 0] <- 10/2
    dt1$Ti[dt1$Ti == 0] <- 5/2
    dt1$Cr[dt1$Cr == 0] <- 2/2
    dt1$Mn[dt1$Mn == 0] <- 1/2
    dt1$Fe[dt1$Fe == 0] <- 5/2
    dt1$Co[dt1$Co == 0] <- 3/2
    dt1$Ni[dt1$Ni == 0] <- 0.2/2
    dt1$Cu[dt1$Cu == 0] <- 0.5/2
    dt1$Zn[dt1$Zn == 0] <- 0.6/2
    dt1$As[dt1$As == 0] <- 0.1/2
    dt1$Se[dt1$Se == 0] <- 0.1/2
    dt1$Cd[dt1$Cd == 0] <- 1/2
    dt1$Re[dt1$Re == 0] <- 0.5/2
  }
  
  
  
  
  
  
  dt2 <- dt1 %>% 
    filter(Optimization == "T1.5")
  dt3 <- dt2 %>% 
    filter(Method == "cup")
  
  
  
  #t test - FINAL
  
  

  # Ensure the relevant columns are numeric
  dt3[, 10:57] <- sapply(dt3[, 10:57], as.numeric)
  
  # List of elements to perform t-test on
  elements <- c("P", "S", "K", "Ca", "Mn", "Fe", "Ni", "Cu", "Zn", "As", "Se", "Re")
  
  # Function to perform one-sample t-test for a given element
  perform_t_test <- function(data, element, weight) {
    # Subset data for the specific Total_Weight
    dt_weight <- subset(data, Total_Weight == weight)
    
    # Calculate the difference (correct column names)
    difference <- dt_weight[[paste0(element, "_NIST")]] - dt_weight[[paste0(element)]]
    
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
    for (weight in unique(dt3$Total_Weight)) {
      p_value <- perform_t_test(dt3, element, weight)
      results <- rbind(results, data.frame(Element = element, Weight = weight, P_Value = p_value))
    }
  }
  
  # View results
  print(results)
  
  
  write.xlsx(results, "T_Test_Results_FINAL2.xlsx")
  
  
  
  
  
  #sprawdzam dodatowe punkty
  
  
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
    
    dt1$Ca[dt1$Ca == 0] <- 10/2
    dt1$Ti[dt1$Ti == 0] <- 5/2
    dt1$Cr[dt1$Cr == 0] <- 2/2
    dt1$Mn[dt1$Mn == 0] <- 1/2
    dt1$Fe[dt1$Fe == 0] <- 5/2
    dt1$Co[dt1$Co == 0] <- 3/2
    dt1$Ni[dt1$Ni == 0] <- 0.2/2
    dt1$Cu[dt1$Cu == 0] <- 0.5/2
    dt1$Zn[dt1$Zn == 0] <- 0.6/2
    dt1$As[dt1$As == 0] <- 0.1/2
    dt1$Se[dt1$Se == 0] <- 0.1/2
    dt1$Cd[dt1$Cd == 0] <- 1/2
    dt1$Re[dt1$Re == 0] <- 0.5/2
  }
  
  
  dt_0.05 <- subset(dt1, Total_Weight=="0.05")
  dt_0.05_point <- subset(dt_0.05, Method=="point")
  dt_0.05_point_T15 <- subset(dt_0.05_point, Optimization=="T1.5")
  difference <- dt_0.05_point_T15$Cu_NIST - dt_0.05_point_T15$Cu
  t_test <- t.test(difference, mu = 0) # p val 0.3929
  
  dt_0.05 <- subset(dt1, Total_Weight=="0.05")
  dt_0.05_point <- subset(dt_0.05, Method=="point")
  dt_0.05_point_T6 <- subset(dt_0.05_point, Optimization=="T6")
  difference <- dt_0.05_point_T6$Cu_NIST - dt_0.05_point_T6$Cu
  t_test <- t.test(difference, mu = 0) # p val 0.2213
  
  
  dt_2 <- subset(dt1, Total_Weight=="2")
  dt_2_cup <- subset(dt_2, Method=="cup")
  dt_2_cup_T6 <- subset(dt_2_cup, Optimization=="T6")
  difference <- dt_2_cup_T6$Cu_NIST - dt_2_cup_T6$Cu
  t_test <- t.test(difference, mu = 0) # p val 0.000768
  
  
  
  
  dt_0.05 <- subset(dt1, Total_Weight=="0.05")
  dt_0.05_point <- subset(dt_0.05, Method=="point")
  dt_0.05_point_T15 <- subset(dt_0.05_point, Optimization=="T1.5")
  difference <- dt_0.05_point_T15$Zn_NIST - dt_0.05_point_T15$Zn
  t_test <- t.test(difference, mu = 0) # p val 0.007816
  
  dt_0.05 <- subset(dt1, Total_Weight=="0.05")
  dt_0.05_point <- subset(dt_0.05, Method=="point")
  dt_0.05_point_T6 <- subset(dt_0.05_point, Optimization=="T6")
  difference <- dt_0.05_point_T6$Zn_NIST - dt_0.05_point_T6$Zn
  t_test <- t.test(difference, mu = 0) # p val 0.0004057
  
  
  dt_2 <- subset(dt1, Total_Weight=="2")
  dt_2_cup <- subset(dt_2, Method=="cup")
  dt_2_cup_T6 <- subset(dt_2_cup, Optimization=="T6")
  difference <- dt_2_cup_T6$Zn_NIST - dt_2_cup_T6$Zn
  t_test <- t.test(difference, mu = 0) # p val 0.0279
  
  
  
  
  
  dt_0.05 <- subset(dt1, Total_Weight=="0.05")
  dt_0.05_point <- subset(dt_0.05, Method=="point")
  dt_0.05_point_T15 <- subset(dt_0.05_point, Optimization=="T1.5")
  difference <- dt_0.05_point_T15$Mn_NIST - dt_0.05_point_T15$Mn
  t_test <- t.test(difference, mu = 0) # p val 0.0135
  
  dt_0.05 <- subset(dt1, Total_Weight=="0.05")
  dt_0.05_point <- subset(dt_0.05, Method=="point")
  dt_0.05_point_T6 <- subset(dt_0.05_point, Optimization=="T6")
  difference <- dt_0.05_point_T6$Mn_NIST - dt_0.05_point_T6$Mn
  t_test <- t.test(difference, mu = 0) # p val 0.00001564
  
  
  dt_2 <- subset(dt1, Total_Weight=="2")
  dt_2_cup <- subset(dt_2, Method=="cup")
  dt_2_cup_T6 <- subset(dt_2_cup, Optimization=="T6")
  difference <- dt_2_cup_T6$Mn_NIST - dt_2_cup_T6$Mn
  t_test <- t.test(difference, mu = 0) # p val 0.009459
  
  
  
  
  
  
  dt_0.05 <- subset(dt1, Total_Weight=="0.05")
  dt_0.05_point <- subset(dt_0.05, Method=="point")
  dt_0.05_point_T15 <- subset(dt_0.05_point, Optimization=="T1.5")
  difference <- dt_0.05_point_T15$Fe_NIST - dt_0.05_point_T15$Fe
  t_test <- t.test(difference, mu = 0) # p val 0.2575
  
  dt_0.05 <- subset(dt1, Total_Weight=="0.05")
  dt_0.05_point <- subset(dt_0.05, Method=="point")
  dt_0.05_point_T6 <- subset(dt_0.05_point, Optimization=="T6")
  difference <- dt_0.05_point_T6$Fe_NIST - dt_0.05_point_T6$Fe
  t_test <- t.test(difference, mu = 0) # p val 0.005261
  
  
  dt_2 <- subset(dt1, Total_Weight=="2")
  dt_2_cup <- subset(dt_2, Method=="cup")
  dt_2_cup_T6 <- subset(dt_2_cup, Optimization=="T6")
  difference <- dt_2_cup_T6$Fe_NIST - dt_2_cup_T6$Fe
  t_test <- t.test(difference, mu = 0) # p val 0.03243
  
  
  
}


#One sample wilcoxon test instead of one sample t-test - to check

{
  
  
  library(dplyr)
  library(openxlsx)
  
  # Ensure the relevant columns are numeric
  dt3[, 5:56] <- sapply(dt3[, 5:56], as.numeric)
  
  # List of elements to perform Wilcoxon test on
  elements <- c("P", "S", "K", "Ca", "Mn", "Fe", "Ni", "Cu", "Zn", "As", "Se", "Re")
  
  # Function to perform one-sample Wilcoxon test for a given element
  perform_wilcox_test <- function(data, element, weight) {
    # Subset data for the specific Total_Weight
    dt_weight <- subset(data, Total_Weight == weight)
    
    # Calculate the difference (correct column names)
    difference <- dt_weight[[paste0(element, "_NIST")]] - dt_weight[[element]]
    
    # Perform Wilcoxon test if there are enough non-NA differences
    if (sum(!is.na(difference)) >= 3) {
      wilcox_test_result <- wilcox.test(difference, mu = 0, na.action = na.omit)
      p_value <- wilcox_test_result$p.value
    } else {
      p_value <- NA  # Not enough data to perform Wilcoxon test
    }
    return(p_value)
  }
  
  # DataFrame to store results
  results <- data.frame(Element = character(),
                        Weight = numeric(),
                        P_Value = numeric())
  
  # Loop through each element and weight
  for (element in elements) {
    for (weight in unique(dt3$Total_Weight)) {
      p_value <- perform_wilcox_test(dt3, element, weight)
      results <- rbind(results, data.frame(Element = element, Weight = weight, P_Value = p_value))
    }
  }
  
  # View results
  print(results)
  
  # Save results to an Excel file
  write.xlsx(results, "Wilcoxon_Test_Results_FINAL.xlsx")
  
  # nie ma roznicy miedzy t-test dla wszystkich punktow a wilcoxon
  
  
}


