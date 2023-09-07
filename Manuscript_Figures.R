# Solitude Final worksheet - Manuscript figures
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
  library(ggstatsplot)
  library(gplots)
}


# Species Distribution by Plots - FIGURE 1

{
setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final")
dt <- read.delim("SLT_Final_3reps.09.06.23.txt")
dt <- subset(dt, Site != 'CONTROL')

dt_unique <- dt %>%
  distinct(Scientific_Name, Plot, Form) %>% # Get distinct rows based on these columns
  group_by(Form) %>% # Group by Form
  mutate(ordered_name = factor(Scientific_Name, levels = rev(unique(Scientific_Name)))) %>% # Reverse the factor levels
  ungroup()


ggplot(dt_unique, aes(x = Plot, y = ordered_name)) +
  geom_point(shape = 4, size = 1.7, color = "black", stroke = 0.5) +
  facet_grid(Form ~ ., scales = "free_y", space = "free_y") +
  labs(x = "Plot", color = "Form") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size=12, face="italic"),
        strip.text = element_text(size = 14, face = "bold"))
}

# Heatmap

{
setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final")
dt <- read.delim("SLT_heatmap_plants_2.txt")
dt <- subset(dt, Site != 'CONTROL')
dt <- dt[dt$Type_of_Sample != "stem", ]
dt <- dt[dt$Type_of_Sample != "root", ]



dt_removed_cols <- dt %>%
  select(-c(2, 3, 4, 5, 6,7,8,9, 14,15,16, 19))

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

}

# Barplots 

{
  
  setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final")
  dt <- read.delim("SLT_Final_3reps.09.06.23.txt")
  dt <- dt[dt$Type_of_Sample != "root", ]
  dt <- dt[dt$Site != "CONTROL", ]
  dt <- dt[dt$Type_of_Sample != "stem", ]
  
  
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
              Mean = mean(Predicted_Fe_ICP), 
              SD = sd(Predicted_Fe_ICP)/sqrt(n())) %>%
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
    scale_y_continuous(limits = c(0, 2100), breaks = seq(0, 2100, by = 450), expand = c(0, 35)) +
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
    scale_y_continuous(limits = c(0, 90), breaks = seq(0, 90, by = 20), expand = c(0, 1.4)) +
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
  
  
  
  
}

# Bars - TAILINGS vs CONTROL Supplement



