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
dt <- read.delim("SLT_heatmap_plants_3.txt")
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
element_order <- c("Cu", "Se", "Re", "Zn", "Mn", "Fe")

# Factor the variable column based on element_order
dt_melted$variable <- factor(dt_melted$variable, levels = element_order)

# Create the heatmap using the heatmap.2 function with dendrograms
heatmap.2(as.matrix(acast(dt_melted, Scientific_Name ~ variable, value.var = "value")),
          scale = "none", # Use "none" to keep the original values
          trace = "none", # Remove trace colors
          Rowv = TRUE, Colv = FALSE, # Add dendrograms
          col = colorRampPalette(c("white", "#AD0B0B"))(256),
          key = TRUE, keysize = 1, key.title = NA, # Add color scale
          symkey = FALSE, density.info = "none",
          lwid = c(0.5, 0.5))






##################NEW TESTS


setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final")
dt <- read.delim("SLT_heatmap_plants_3.txt")
dt <- subset(dt, Site != 'CONTROL')
dt <- dt[dt$Type_of_Sample != "stem", ]
dt <- dt[dt$Type_of_Sample != "root", ]



dt_removed_cols <- dt %>%
  select(-c(2, 3, 4, 5, 6,7,8,9, 11,12,13,14,15,16,17,18, 19))

dt_grouped <- dt_removed_cols %>%
  group_by(Scientific_Name) %>%
  summarize(across(Cu:Cu, median))

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
element_order <- c("Cu")

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


# Duplicate the column
mat <- as.matrix(acast(dt_melted, Scientific_Name ~ variable, value.var = "value"))
mat <- cbind(mat, mat)  # Add the same column again

# Create the heatmap using the heatmap.2 function with dendrograms
heatmap.2(mat,
          scale = "none",
          trace = "none",
          Rowv = TRUE, Colv = FALSE,
          col = colorRampPalette(c("#C5DFF8", "#4A55A2"))(256),
          key = TRUE, keysize = 1, key.title = NA,
          symkey = FALSE, density.info = "none",
          lwid = c(0.5, 0.5))


library(reshape2)

# 1. Calculate the distance matrix
dist_matrix <- dist(as.matrix(acast(dt_melted, Scientific_Name ~ variable, value.var = "value")))

# 2. Compute the hierarchical clustering
hc <- hclust(dist_matrix)

# 3. Plot the dendrogram
plot(hc, main="Dendrogram for Scientific_Name", xlab="Scientific Name", sub="", cex=0.9)


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
    geom_errorbar(aes(ymin = Mean, ymax = Mean + SD),
                  position = position_dodge(width = 0.85),
                  width = 0.25,
                  size = 0.2) +
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
  
  output_path <-  "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Figures_Manuscript/Figure 3 - Barplots/New/Cut_Final/Mn.pdf"
  ggsave(filename = output_path, plot = Mn, device = "pdf", 
         width = 4.35, height = 3.6, units = "in", dpi = 600)
  
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
    scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 30), expand = c(0, 2.5)) +
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
  
  output_path <-  "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Figures_Manuscript/Figure 3 - Barplots/New/Cut_Final/Zn.pdf"
  ggsave(filename = output_path, plot = Zn, device = "pdf", 
         width = 4.35, height = 3.6, units = "in", dpi = 600)
  
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
  
  
  
  output_path <-  "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Figures_Manuscript/Figure 3 - Barplots/New/Cut_Final/Fe.pdf"
  ggsave(filename = output_path, plot = Fe, device = "pdf", 
         width = 4.35, height = 3.6, units = "in", dpi = 600)
  
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
  
# New bar plots with cut bars
  
  {
  #######################NEW########
    
  
    
    # Cu
    
    dt_Cu <- dt %>%
      group_by(Scientific_Name, Site) %>%
      summarize(Median = median(Predicted_Cu_ICP), 
                Mean = mean(Predicted_Cu_ICP), 
                SD = sd(Predicted_Cu_ICP)/sqrt(n())) %>%
      arrange(Median) %>%
      ungroup()
    
    library(ggplot2)
    library(ggbreak)
    library(patchwork)
    
    # Create the primary plot without any breaks
    Cu_primary <- ggplot(dt_Cu, aes(x = reorder(Scientific_Name, Median), 
                                    y = Mean, fill = Site)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.01), size=0.22, color = "black") +
      geom_errorbar(aes(ymin = Mean, ymax = Mean + SD),
                    position = position_dodge(width = 0.85),
                    width = 0.25,
                    size = 0.2) +
      geom_hline(yintercept = 5, linetype = "dashed", color = "#AD0B0B", size = 0.3) +
      geom_hline(yintercept = 20, linetype = "twodash", color = "#003f5c", size = 0.3) +
      scale_fill_manual(values = c("lightgrey")) +
      coord_flip() +
      labs(x = "", y = "Cu (mg/kg)") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 7),
            axis.text.y = element_text(size = 7),
            legend.key.size = unit(1, "lines"),
            legend.text = element_text(size = 4),
            legend.title = element_text(size = 4, face = "bold"))
    
    # Create a secondary plot that has the y-axis break
    Cu_broken <- Cu_primary +
      scale_y_break(c(110, 140), scales="free") +
      theme(legend.position="none")  # Removing legend as it'll be redundant
    
    output_path <-  "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Figures_Manuscript/Figure 3 - Barplots/New/Cut_Final/Cu_cut2.pdf"
    ggsave(filename = output_path, plot = Cu_broken, device = "pdf", 
           width = 3.4, height = 3.8, units = "in", dpi = 600)

    # Se
    
    dt_Se <- dt %>%
      group_by(Scientific_Name, Site) %>%
      summarize(Median = median(Predicted_Cu_ICP), 
                Mean = mean(Predicted_Se_ICP), 
                SD = sd(Predicted_Se_ICP)/sqrt(n())) %>%
      arrange(Median) %>%
      ungroup()
    

    library(ggplot2)
    library(ggbreak)
    library(patchwork)
    
    # Create the primary plot without any breaks
    Se_primary <- ggplot(dt_Se, aes(x = reorder(Scientific_Name, Median), 
                                    y = Mean, fill = Site)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.01), size=0.18, color = "black") +
      geom_errorbar(aes(ymin = Mean, ymax = Mean + SD),
                    position = position_dodge(width = 0.85),
                    width = 0.25,
                    size = 0.2) +
      geom_hline(yintercept = 5, linetype = "twodash", color = "#003f5c", size = 0.3) +
            scale_fill_manual(values = c("lightgrey")) +
      #scale_y_continuous(expand= c(100, 10)) + # Adjust the expand argument as required
            coord_flip() +
      labs(x = "", y = "Se (mg/kg)") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 7),
            axis.text.y = element_text(size = 7),
            legend.key.size = unit(1, "lines"),
            legend.text = element_text(size = 4),
            legend.title = element_text(size = 4, face = "bold"))
    
    # Create a secondary plot that has the y-axis break
    Se_broken <- Se_primary +
      scale_y_break(c(8, 13), scales="free") +
      theme(legend.position="none")

    
    output_path <-  "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Figures_Manuscript/Figure 3 - Barplots/New/Cut_Final/Se_cut.pdf"
    ggsave(filename = output_path, plot = Se_broken, device = "pdf", 
           width = 3.8, height = 3.8, units = "in", dpi = 600)

    # Re
    
    dt_Re <- dt %>%
      group_by(Scientific_Name, Site) %>%
      summarize(Median = median(Predicted_Cu_ICP), 
                Mean = mean(Predicted_Re_ICP), 
                SD = sd(Predicted_Re_ICP)/sqrt(n())) %>%
      arrange(Median) %>%
      ungroup()
    
    
    library(ggplot2)
    library(ggbreak)
    library(patchwork)
    
    # Create the primary plot without any breaks
    Re_primary <- ggplot(dt_Re, aes(x = reorder(Scientific_Name, Median), 
                                    y = Mean, fill = Site)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.01), size=0.18, color = "black") +
      geom_errorbar(aes(ymin = Mean, ymax = Mean + SD),
                    position = position_dodge(width = 0.85),
                    width = 0.25,
                    size = 0.2) +
      geom_hline(yintercept = 5, linetype = "twodash", color = "#003f5c", size = 0.3) +
      scale_fill_manual(values = c("lightgrey")) +
      #scale_y_continuous(expand= c(100, 10)) + # Adjust the expand argument as required
      coord_flip() +
      labs(x = "", y = "Re (mg/kg)") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 7),
            axis.text.y = element_text(size = 7),
            legend.key.size = unit(1, "lines"),
            legend.text = element_text(size = 4),
            legend.title = element_text(size = 4, face = "bold"))
    
    # Create a secondary plot that has the y-axis break
    Re_broken <- Re_primary +
      scale_y_break(c(13, 21), scales="free") +
      theme(legend.position="none")
      

    output_path <-  "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Figures_Manuscript/Figure 3 - Barplots/New/Cut_Final/Re_cut.pdf"
    ggsave(filename = output_path, plot = Re_broken, device = "pdf", 
           width = 3.8, height = 3.8, units = "in", dpi = 600)
#    scale_y_continuous(limits = c(0, 405), breaks = seq(0, 405, by = 100), expand = c(0, 9)) +

    
    

#### Version 2 with shrinked right part 
    {
    library(ggplot2)
    library(patchwork)
    
    # Modify data for Cu_below_break
    dt_Cu_below_break <- dt_Cu
    dt_Cu_below_break$Mean[dt_Cu_below_break$Mean > 110] <- 110
    
    # Plot for values below break
    Cu_below_break <- ggplot(dt_Cu_below_break, aes(x = reorder(Scientific_Name, Median), 
                                                    y = Mean, fill = Site)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.01), size=0.22, color = "black") +
      geom_errorbar(aes(ymin = Mean, ymax = Mean + SD),
                    position = position_dodge(width = 0.85),
                    width = 0.25,
                    size = 0.2) +
      geom_hline(yintercept = 5, linetype = "dashed", color = "#AD0B0B", size = 0.3) +
      geom_hline(yintercept = 20, linetype = "twodash", color = "#003f5c", size = 0.3) +
      scale_fill_manual(values = c("lightgrey")) +
      coord_flip() +
      labs(x = "", y = "Cu (mg/kg)") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 7),
            axis.text.y = element_text(size = 7),
            legend.key.size = unit(1, "lines"),
            legend.text = element_text(size = 4),
            legend.title = element_text(size = 4, face = "bold"))
    
    # Modify data for Cu_above_break
    dt_Cu_above_break <- dt_Cu
    dt_Cu_above_break$Mean[dt_Cu_above_break$Mean <= 110] <- 0
    
    # Plot for values above break
    Cu_above_break <- ggplot(dt_Cu_above_break, aes(x = reorder(Scientific_Name, Median), 
                                                    y = Mean, fill = Site)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.01), size=0.22, color = "black") +
      geom_errorbar(aes(ymin = Mean, ymax = Mean + SD),
                    position = position_dodge(width = 0.85),
                    width = 0.25,
                    size = 0.2) +
      scale_fill_manual(values = c("lightgrey")) +
      coord_flip() +
      labs(x = "", y = NULL) +  # Remove the y-axis label for the above break section
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 7),
            axis.text.y = element_text(size = 7),
            legend.position = "none")  # No legend for this part
    
    # Combine both plots
    Cu_below_break <- Cu_below_break + theme(legend.position = "none")
    Cu_combined <- Cu_below_break + Cu_above_break + plot_layout(widths = c(3, 1))
    
    Cu_combined
    
    }
    
    
    
    
  
  
  }


# Bars - TAILINGS vs CONTROL Supplement
{
setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final")
dt <- read.delim("Solitude_Plants_Predicted_TLandCTRL.txt")
dt <- dt[dt$Type_of_Sample != "root", ]
dt <- dt[dt$Type_of_Sample != "stem", ]


dt_Cu <- dt %>%
  group_by(Scientific_Name, Site) %>%
  summarize(
    Median = median(Predicted_Cu_ICP),
    Mean = mean(Predicted_Cu_ICP),
    SD = sd(Predicted_Cu_ICP) / sqrt(n())
  ) %>%
  arrange(Median) %>%
  ungroup()
# Getting unique sites from dt_Cu
unique_sites <- unique(dt_Cu$Site)

# Check the length of unique sites, and based on that, assign colors
if (length(unique_sites) == 2) {
  site_colors <- c("darkgrey", "lightgrey")
} else {
  stop("Number of unique sites is not 2. Please specify colors for all sites.")
}

# Create a named vector for fill colors
fill_colors <- setNames(site_colors, unique_sites)

Cu <- ggplot(dt_Cu, aes(x = reorder(Scientific_Name, Median), 
                        y = Mean, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge", size = 0.22, 
           color = "black") +
  geom_errorbar(
    aes(ymin = Mean - SD, ymax = Mean + SD),
    position = position_dodge(width = 0.85),
    width = 0.25,
    size = 0.25
  ) +
  scale_fill_manual(values = fill_colors) +
  coord_flip() +
  geom_hline(yintercept = 5, linetype = "dashed", color = "#AD0B0B", size = 0.3) +
  geom_hline(yintercept = 20, linetype = "twodash", color = "#003f5c", size = 0.3) +
  labs(x = "", y = "Cu (mg/kg)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key.size = unit(1, "lines"),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.text = element_text(size = 4),
        legend.title = element_text(size = 4, face = "bold")
  )

Cu

# Fe
dt_Fe <- dt %>%
  group_by(Scientific_Name, Site) %>%
  summarize(
    Median = median(Predicted_Cu_ICP),
    Mean = mean(Predicted_Fe_ICP),
    SD = sd(Predicted_Fe_ICP) / sqrt(n())
  ) %>%
  arrange(Median) %>%
  ungroup()
# Getting unique sites from dt_Cu
unique_sites <- unique(dt_Fe$Site)

# Check the length of unique sites, and based on that, assign colors
if (length(unique_sites) == 2) {
  site_colors <- c("darkgrey", "lightgrey")
} else {
  stop("Number of unique sites is not 2. Please specify colors for all sites.")
}

# Create a named vector for fill colors
fill_colors <- setNames(site_colors, unique_sites)

Fe <- ggplot(dt_Fe, aes(x = reorder(Scientific_Name, Median), 
                        y = Mean, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge", size = 0.22, 
           color = "black") +
  geom_errorbar(
    aes(ymin = Mean - SD, ymax = Mean + SD),
    position = position_dodge(width = 0.85),
    width = 0.25,
    size = 0.25
  ) +
  scale_fill_manual(values = fill_colors) +
  scale_y_continuous(limits = c(0, 1600), breaks = seq(0, 1600, by = 400), expand = c(0, 60)) +
  coord_flip() +
  geom_hline(yintercept = 30, linetype = "dashed", color = "#AD0B0B", size = 0.3) +
  geom_hline(yintercept = 500, linetype = "twodash", color = "#003f5c", size = 0.3) +
  labs(x = "", y = "Fe (mg/kg)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key.size = unit(1, "lines"),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.text = element_text(size = 4),
        legend.title = element_text(size = 4, face = "bold")
  )

Fe



#Mn
dt_Mn <- dt %>%
  group_by(Scientific_Name, Site) %>%
  summarize(
    Median = median(Predicted_Cu_ICP),
    Mean = mean(Predicted_Mn_ICP),
    SD = sd(Predicted_Mn_ICP) / sqrt(n())
  ) %>%
  arrange(Median) %>%
  ungroup()
# Getting unique sites from dt_Cu
unique_sites <- unique(dt_Mn$Site)

# Check the length of unique sites, and based on that, assign colors
if (length(unique_sites) == 2) {
  site_colors <- c("darkgrey", "lightgrey")
} else {
  stop("Number of unique sites is not 2. Please specify colors for all sites.")
}

# Create a named vector for fill colors
fill_colors <- setNames(site_colors, unique_sites)

Mn <- ggplot(dt_Mn, aes(x = reorder(Scientific_Name, Median), 
                        y = Mean, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge", size = 0.22, 
           color = "black") +
  geom_errorbar(
    aes(ymin = Mean - SD, ymax = Mean + SD),
    position = position_dodge(width = 0.85),
    width = 0.25,
    size = 0.25
  ) +
  scale_fill_manual(values = fill_colors) +
  coord_flip() +
  geom_hline(yintercept = 20, linetype = "dashed", color = "#AD0B0B", size = 0.3) +

  labs(x = "", y = "Mn (mg/kg)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key.size = unit(1, "lines"),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.text = element_text(size = 4),
        legend.title = element_text(size = 4, face = "bold")
  )

Mn


#Zn
dt_Zn <- dt %>%
  group_by(Scientific_Name, Site) %>%
  summarize(
    Median = median(Predicted_Cu_ICP),
    Mean = mean(Predicted_Zn_ICP),
    SD = sd(Predicted_Zn_ICP) / sqrt(n())
  ) %>%
  arrange(Median) %>%
  ungroup()
# Getting unique sites from dt_Cu
unique_sites <- unique(dt_Zn$Site)

# Check the length of unique sites, and based on that, assign colors
if (length(unique_sites) == 2) {
  site_colors <- c("darkgrey", "lightgrey")
} else {
  stop("Number of unique sites is not 2. Please specify colors for all sites.")
}

# Create a named vector for fill colors
fill_colors <- setNames(site_colors, unique_sites)

Zn <- ggplot(dt_Zn, aes(x = reorder(Scientific_Name, Median), 
                        y = Mean, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge", size = 0.22, 
           color = "black") +
  geom_errorbar(
    aes(ymin = Mean - SD, ymax = Mean + SD),
    position = position_dodge(width = 0.85),
    width = 0.25,
    size = 0.25
  ) +
  scale_fill_manual(values = fill_colors) +
  coord_flip() +
  geom_hline(yintercept = 20, linetype = "dashed", color = "#AD0B0B", size = 0.3) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 15), expand = c(0, 2.5)) +
  labs(x = "", y = "Zn (mg/kg)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key.size = unit(1, "lines"),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.text = element_text(size = 4),
        legend.title = element_text(size = 4, face = "bold")
  )

Zn


#Se
dt_Se <- dt %>%
  group_by(Scientific_Name, Site) %>%
  summarize(
    Median = median(Predicted_Cu_ICP),
    Mean = mean(Predicted_Se_ICP),
    SD = sd(Predicted_Se_ICP) / sqrt(n())
  ) %>%
  arrange(Median) %>%
  ungroup()
# Getting unique sites from dt_Cu
unique_sites <- unique(dt_Se$Site)

# Check the length of unique sites, and based on that, assign colors
if (length(unique_sites) == 2) {
  site_colors <- c("darkgrey", "lightgrey")
} else {
  stop("Number of unique sites is not 2. Please specify colors for all sites.")
}

# Create a named vector for fill colors
fill_colors <- setNames(site_colors, unique_sites)

se<- ggplot(dt_Se, aes(x = reorder(Scientific_Name, Median), 
                        y = Mean, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge", size = 0.22, 
           color = "black") +
  geom_errorbar(
    aes(ymin = Mean - SD, ymax = Mean + SD),
    position = position_dodge(width = 0.85),
    width = 0.25,
    size = 0.25
  ) +
  scale_fill_manual(values = fill_colors) +
  coord_flip() +
  geom_hline(yintercept = 5, linetype = "twodash", color = "#003f5c", size = 0.3) +
  #scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 15), expand = c(0, 2.5)) +
  labs(x = "", y = "Se (mg/kg)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key.size = unit(1, "lines"),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.text = element_text(size = 4),
        legend.title = element_text(size = 4, face = "bold")
  )

se


#Re stack from two plots
dt_Re <- dt %>%
  group_by(Scientific_Name, Site) %>%
  summarize(
    Median = median(Predicted_Cu_ICP),
    Mean = mean(Predicted_Re_ICP),
    SD = sd(Predicted_Re_ICP) / sqrt(n())
  ) %>%
  arrange(Median) %>%
  ungroup()
# Getting unique sites from dt_Cu
unique_sites <- unique(dt_Re$Site)

# Check the length of unique sites, and based on that, assign colors
if (length(unique_sites) == 2) {
  site_colors <- c("darkgrey", "lightgrey")
} else {
  stop("Number of unique sites is not 2. Please specify colors for all sites.")
}

# Create a named vector for fill colors
fill_colors <- setNames(site_colors, unique_sites)

Re <- ggplot(dt_Re, aes(x = reorder(Scientific_Name, Median), 
                       y = Mean, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge", size = 0.22, 
           color = "black") +
  geom_errorbar(
    aes(ymin = Mean - SD, ymax = Mean + SD),
    position = position_dodge(width = 0.85),
    width = 0.25,
    size = 0.25
  ) +
  scale_fill_manual(values = fill_colors) +
  coord_flip() +
  geom_hline(yintercept = 5, linetype = "twodash", color = "#003f5c", size = 0.3) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 3), expand = c(0, 0.5)) +
  labs(x = "", y = "Re (mg/kg)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key.size = unit(1, "lines"),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.text = element_text(size = 4),
        legend.title = element_text(size = 4, face = "bold")
  )

Re





}

#Boxplots

{
setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Modified Final")
dt <- read.delim("SLT_Final_3reps.09.06.23.txt")
dt <- dt[dt$Type_of_Sample != "root", ]
dt <- dt[dt$Site != "CONTROL", ]
dt <- dt[dt$Type_of_Sample != "stem", ]



dt_selected <- dt[dt$Scientific_Name %in% c("Xanthisma gracile", "Pseudognaphalium canescens", "Boechera perennans",
                                            "Nultuma (Prosopis) velutina", "Tamarix chinensis", "Senegalia (Acacia) greggii","Isocoma acradenia"),]


Cu <- ggplot(dt_selected, aes(x = reorder(Scientific_Name, Predicted_Cu_ICP, FUN = median),
                              y = Predicted_Cu_ICP, Sceintific_Name = Scientific_Name)) +
  geom_boxplot(linewidth=0.3) +
  geom_point(aes(shape = Plot), size = 2.5) +  # Adjust the size parameter here
  scale_shape_manual(values = c(21, 21, 21, 4)) +
  geom_hline(yintercept = 70, linetype = "dashed", color = "#9a9a9a", size = 0.4) +
  geom_hline(yintercept = 300, linetype = "dotdash", color = "#454545", size = 0.4) +
  scale_y_continuous(limits = c(0, 900), breaks = seq(0, 900, by = 150)) +
  coord_flip() +
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=12),
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
  geom_boxplot(linewidth=0.3) +
  geom_point(aes(shape = Plot), size = 2.5) +  # Adjust the size parameter here
  scale_shape_manual(values = c(21, 21, 21, 4)) +
  scale_y_continuous(limits = c(0, 750), breaks = seq(0, 750, by = 150)) +
  coord_flip() +
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=12),
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
  geom_boxplot(linewidth=0.3) +
  geom_point(aes(shape = Plot), size = 2.5) +  # Adjust the size parameter here
  scale_shape_manual(values = c(21, 21, 21, 4)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  coord_flip() +
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=12),
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
  geom_boxplot(linewidth=0.3) +
  geom_point(aes(shape = Plot), size = 2.5) +  # Adjust the size parameter here
  scale_shape_manual(values = c(21, 21, 21, 4)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  coord_flip() +
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=12),
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
  geom_boxplot(linewidth=0.3) +
  geom_point(aes(shape = Plot), size = 2.5) +  # Adjust the size parameter here
  scale_shape_manual(values = c(21, 21, 21, 4)) +
  scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, by = 50)) +
  coord_flip() +
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=12),
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
  geom_boxplot(linewidth=0.3) +
  geom_point(aes(shape = Plot), size = 2.5) +  # Adjust the size parameter here
  scale_shape_manual(values = c(21, 21, 21, 4)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  coord_flip() +
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=12),
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


}

#Regression lines plot

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New")
dt <- read.delim("SLT_pXRF_ICP.txt")

#Cu
dt <- dt[dt$Cu_concentration != 0.25, ] # To remove LODs

dt$Predicted_Cu_ICP <- 28.88747 + (1.41673* dt$Cu_concentration) + (-316.95475* dt$Substrate_RT)
dt$Predicted_Cu_ICP3 <- 8.5563 + (1.4929* dt$Cu_concentration) #no RT no TW

lm_model1 <- lm(Cu_ICP ~ Cu_concentration, data = dt)

summary(lm_model1)
rsquared <- summary(lm_model1)$r.squared

lm_model2 <- lm(Cu_ICP ~ Predicted_Cu_ICP, data = dt)
summary(lm_model2)
rsquared2 <- summary(lm_model2)$r.squared


p <- ggplot(dt, aes(x = Cu_concentration, y = Cu_ICP)) +
  geom_point(color = "#003f5c", size=2.1, stroke=1, shape=1) +
  geom_point(aes(x = Predicted_Cu_ICP, y = Cu_ICP), color = "#AD0B0B", size=2.1, stroke=1, shape=3) + # New points
  geom_smooth(aes(x = Cu_concentration, y = Cu_ICP), method = "lm", se = FALSE, color = "#003f5c", linetype = "solid") +   # Regression line for the first model
  geom_smooth(aes(x = Predicted_Cu_ICP, y = Cu_ICP), method = "lm", se = FALSE, color = "#AD0B0B", linetype = "solid") +  # Regression line for the second model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=1) +
  labs(x = "pXRF Cu", y = "ICP concentration Cu") +
  scale_y_continuous(limits = c(0, 800), breaks = seq(0, 800, by = 200)) +
  annotate("text", x = 700, y = 800, 
           label = paste("R-squared =", round(rsquared, 3)), hjust = 1, vjust = 0) +
  annotate("text", x = 700, y=700, 
           label = paste("R-squared2 =", round(rsquared2, 3)), hjust = 1, vjust = 0)+
  theme_classic() +  # Using theme_classic as theme_classic2 is not part of base ggplot2
  theme(panel.grid.major = element_blank(), # Removing major grid lines
        panel.grid.minor = element_blank(), # Removing minor grid lines
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5), # Adding border around the plot using updated argument
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.title = element_text(size = 16, face = "bold"),  # Customize plot title
        axis.title = element_text(size = 20),  # Customize axis labels
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 16, face = "bold"),
        legend.position = "top")

print(p)


#Fe

dt <- read.delim("SLT_pXRF_ICP.txt")

dt$Predicted_Fe_ICP <- 28.88747 + (1.41673* dt$Fe_concentration) + (-316.95475* dt$Substrate_RT) #glm RT


lm_model1 <- lm(Fe_ICP ~ Fe_concentration, data = dt)

summary(lm_model1)
rsquared <- summary(lm_model1)$r.squared

lm_model2 <- lm(Fe_ICP ~ Predicted_Fe_ICP, data = dt)
summary(lm_model2)
rsquared2 <- summary(lm_model2)$r.squared


p <- ggplot(dt, aes(x = Fe_concentration, y = Fe_ICP)) +
  geom_point(color = "#003f5c", size=2.1, stroke=1, shape=1) +
  geom_point(aes(x = Predicted_Fe_ICP, y = Fe_ICP), color = "#AD0B0B", size=2.1, stroke=1, shape=3) + # New points
  geom_smooth(aes(x = Fe_concentration, y = Fe_ICP), method = "lm", se = FALSE, color = "#003f5c", linetype = "solid") +   # Regression line for the first model
  geom_smooth(aes(x = Predicted_Fe_ICP, y = Fe_ICP), method = "lm", se = FALSE, color = "#AD0B0B", linetype = "solid") +  # Regression line for the second model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=1) +
  labs(x = "pXRF Fe", y = "ICP concentration Fe") +
  scale_y_continuous(limits = c(0, 4000), breaks = seq(0, 4000, by = 800)) +
  scale_x_continuous(limits = c(0, 4000), breaks = seq(0, 4000, by = 800)) +
  annotate("text", x = 3900, y = 3900, 
           label = paste("R-squared =", round(rsquared, 3)), hjust = 1, vjust = 0) +
  annotate("text", x = 3900, y=3500, 
           label = paste("R-squared2 =", round(rsquared2, 3)), hjust = 1, vjust = 0)+
  theme_classic() +  # Using theme_classic as theme_classic2 is not part of base ggplot2
  theme(panel.grid.major = element_blank(), # Removing major grid lines
        panel.grid.minor = element_blank(), # Removing minor grid lines
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5), # Adding border around the plot using updated argument
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.title = element_text(size = 16, face = "bold"),  # Customize plot title
        axis.title = element_text(size = 20),  # Customize axis labels
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 16, face = "bold"),
        legend.position = "top")

print(p)


#Zn
dt <- read.delim("SLT_pXRF_ICP.txt")
dt <- dt[dt$Mn_concentration != 0.3, ] # To remove LODs
dt$Predicted_Zn_ICP <- 50.8422 + (0.9560* dt$Mn_concentration) + (-473.9784* dt$Substrate_RT)


lm_model1 <- lm(Zn_ICP ~ Mn_concentration, data = dt)

summary(lm_model1)
rsquared <- summary(lm_model1)$r.squared

lm_model2 <- lm(Zn_ICP ~ Predicted_Zn_ICP, data = dt)
summary(lm_model2)
rsquared2 <- summary(lm_model2)$r.squared


p <- ggplot(dt, aes(x = Mn_concentration, y = Zn_ICP)) +
  geom_point(color = "#003f5c", size=2.1, stroke=1, shape=1) +
  geom_point(aes(x = Predicted_Zn_ICP, y = Zn_ICP), color = "#AD0B0B", size=2.1, stroke=1, shape=3) + # New points
  geom_smooth(aes(x = Mn_concentration, y = Zn_ICP), method = "lm", se = FALSE, color = "#003f5c", linetype = "solid") +   # Regression line for the first model
  geom_smooth(aes(x = Predicted_Zn_ICP, y = Zn_ICP), method = "lm", se = FALSE, color = "#AD0B0B", linetype = "solid") +  # Regression line for the second model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=1) +
  labs(x = "pXRF Zn", y = "ICP concentration Zn") +
  scale_y_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 50)) +
  scale_x_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 50)) +
 annotate("text", x = 150, y = 150, 
           label = paste("R-squared =", round(rsquared, 3)), hjust = 1, vjust = 0) +
 annotate("text", x = 150, y=130, 
           label = paste("R-squared2 =", round(rsquared2, 3)), hjust = 1, vjust = 0)+
  theme_classic() +  # Using theme_classic as theme_classic2 is not part of base ggplot2
  theme(panel.grid.major = element_blank(), # Removing major grid lines
        panel.grid.minor = element_blank(), # Removing minor grid lines
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5), # Adding border around the plot using updated argument
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.title = element_text(size = 16, face = "bold"),  # Customize plot title
        axis.title = element_text(size = 20),  # Customize axis labels
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 16, face = "bold"),
        legend.position = "top")

print(p)



# Mn
dt <- read.delim("SLT_pXRF_ICP.txt")
dt <- dt[dt$Mn_concentration != 0.5, ] # To remove LODs
dt$Predicted_Mn_ICP <- 51.4943 + (1.0760* dt$Mn_concentration) + (-431.8509* dt$Substrate_RT)


lm_model1 <- lm(Mn_ICP ~ Mn_concentration, data = dt)

summary(lm_model1)
rsquared <- summary(lm_model1)$r.squared

lm_model2 <- lm(Mn_ICP ~ Predicted_Mn_ICP, data = dt)
summary(lm_model2)
rsquared2 <- summary(lm_model2)$r.squared


p <- ggplot(dt, aes(x = Mn_concentration, y = Mn_ICP)) +
  geom_point(color = "#003f5c", size=2.1, stroke=1, shape=1) +
  geom_point(aes(x = Predicted_Mn_ICP, y = Mn_ICP), color = "#AD0B0B", size=2.1, stroke=1, shape=3) + # New points
  geom_smooth(aes(x = Mn_concentration, y = Mn_ICP), method = "lm", se = FALSE, color = "#003f5c", linetype = "solid") +   # Regression line for the first model
  geom_smooth(aes(x = Predicted_Mn_ICP, y = Mn_ICP), method = "lm", se = FALSE, color = "#AD0B0B", linetype = "solid") +  # Regression line for the second model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=1) +
  labs(x = "pXRF Mn", y = "ICP concentration Mn") +
  scale_y_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 50)) +
  scale_x_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 50)) +
  annotate("text", x = 150, y = 150, 
           label = paste("R-squared =", round(rsquared, 3)), hjust = 1, vjust = 0) +
  annotate("text", x = 150, y=130, 
           label = paste("R-squared2 =", round(rsquared2, 3)), hjust = 1, vjust = 0)+
  theme_classic() +  # Using theme_classic as theme_classic2 is not part of base ggplot2
  theme(panel.grid.major = element_blank(), # Removing major grid lines
        panel.grid.minor = element_blank(), # Removing minor grid lines
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5), # Adding border around the plot using updated argument
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.title = element_text(size = 16, face = "bold"),  # Customize plot title
        axis.title = element_text(size = 20),  # Customize axis labels
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 16, face = "bold"),
        legend.position = "top")

print(p)


#Re
dt <-read.delim("Solitude_pXRF_ICP_correl_Re.txt")
dt$Predicted_Re_ICP <- 3.84146 + (0.91141* dt$Re_concentration) + (-33.18455* dt$Substrate_RT)


lm_model1 <- lm(Re_ICP ~ Re_concentration, data = dt)

summary(lm_model1)
rsquared <- summary(lm_model1)$r.squared

lm_model2 <- lm(Re_ICP ~ Predicted_Re_ICP, data = dt)
summary(lm_model2)
rsquared2 <- summary(lm_model2)$r.squared


p <- ggplot(dt, aes(x = Re_concentration, y = Re_ICP)) +
  geom_point(color = "#003f5c", size=2.1, stroke=1, shape=1) +
  geom_point(aes(x = Predicted_Re_ICP, y = Re_ICP), color = "#AD0B0B", size=2.1, stroke=1, shape=3) + # New points
  geom_smooth(aes(x = Re_concentration, y = Re_ICP), method = "lm", se = FALSE, color = "#003f5c", linetype = "solid") +   # Regression line for the first model
  geom_smooth(aes(x = Predicted_Re_ICP, y = Re_ICP), method = "lm", se = FALSE, color = "#AD0B0B", linetype = "solid") +  # Regression line for the second model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=1) +
  labs(x = "pXRF Re", y = "ICP concentration Re") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  annotate("text", x = 90, y = 90, 
           label = paste("R-squared =", round(rsquared, 3)), hjust = 1, vjust = 0) +
  annotate("text", x = 90, y=80, 
           label = paste("R-squared2 =", round(rsquared2, 3)), hjust = 1, vjust = 0)+
  theme_classic() +  # Using theme_classic as theme_classic2 is not part of base ggplot2
  theme(panel.grid.major = element_blank(), # Removing major grid lines
        panel.grid.minor = element_blank(), # Removing minor grid lines
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5), # Adding border around the plot using updated argument
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.title = element_text(size = 16, face = "bold"),  # Customize plot title
        axis.title = element_text(size = 20),  # Customize axis labels
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 16, face = "bold"),
        legend.position = "top")

print(p)


#Se
dt <- read.delim("SLT_pXRF_ICP.txt")
dt <- dt[dt$Se_concentration != 0.05, ] # To remove LODs

dt$Predicted_Se_ICP <- 0.4417 + (1.5683* dt$Se_concentration) + (-8.8017* dt$Substrate_RT)


lm_model1 <- lm(Se_ICP ~ Se_concentration, data = dt)

summary(lm_model1)
rsquared <- summary(lm_model1)$r.squared

lm_model2 <- lm(Se_ICP ~ Predicted_Se_ICP, data = dt)
summary(lm_model2)
rsquared2 <- summary(lm_model2)$r.squared


p <- ggplot(dt, aes(x = Se_concentration, y = Se_ICP)) +
  geom_point(color = "#003f5c", size=2.1, stroke=1, shape=1) +
  geom_point(aes(x = Predicted_Se_ICP, y = Se_ICP), color = "#AD0B0B", size=2.1, stroke=1, shape=3) + # New points
  geom_smooth(aes(x = Se_concentration, y = Se_ICP), method = "lm", se = FALSE, color = "#003f5c", linetype = "solid") +   # Regression line for the first model
  geom_smooth(aes(x = Predicted_Se_ICP, y = Se_ICP), method = "lm", se = FALSE, color = "#AD0B0B", linetype = "solid") +  # Regression line for the second model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=1) +
  labs(x = "pXRF Se", y = "ICP concentration Se") +
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 20)) +
  scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 20)) +
  annotate("text", x = 90, y = 90, 
         label = paste("R-squared =", round(rsquared, 3)), hjust = 1, vjust = 0) +
  annotate("text", x = 90, y=80, 
          label = paste("R-squared2 =", round(rsquared2, 3)), hjust = 1, vjust = 0)+
  theme_classic() +  # Using theme_classic as theme_classic2 is not part of base ggplot2
  theme(panel.grid.major = element_blank(), # Removing major grid lines
        panel.grid.minor = element_blank(), # Removing minor grid lines
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5), # Adding border around the plot using updated argument
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.title = element_text(size = 16, face = "bold"),  # Customize plot title
        axis.title = element_text(size = 20),  # Customize axis labels
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 16, face = "bold"),
        legend.position = "top")

print(p)
