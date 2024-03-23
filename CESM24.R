# Solitude Plant Cfigures
# Tomasz Wlodarczyk
# twlodarczyk@arizona.edu
# 2024 - 17 - 03


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
  library(psych)
  library(car)
  library(openxlsx)
}

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/")
dt <-read.delim("Solitude_CESM_Report24.txt")
dt <-read.delim("Solitude_Tailings_Natural_only.txt")

dt <- dt[dt$Type_of_Sample != "root", ] # tego nie dawaj
dt <- dt[dt$Site != "NATURAL", ]        # tegp nie dawaj
dt <- dt[dt$Type_of_Sample != "stem", ] # tego nie dawaj


#dt$Cl_concentration[dt$Cl_concentration == NA] <- 50/2
#dt$Ca_concentration[dt$Ca_concentration == 0] <- 10/2
#dt$Ti_concentration[dt$Ti_concentration == 0] <- 5/2
#dt$Cr_concentration[dt$Cr_concentration == 0] <- 2/2
#dt$Mn_concentration[dt$Mn_concentration == 0] <- 1/2
#dt$Fe_concentration[dt$Fe_concentration == 0] <- 5/2
#dt$Co_concentration[dt$Co_concentration == 0] <- 3/2
#dt$Ni_concentration[dt$Ni_concentration == 0] <- 0.2/2
#dt$Predicted_Cu_M3[dt$Predicted_Cu_M == NA] <- 0.5/2
#dt$Predicted_Zn_M3[dt$Predicted_Zn_M3 == NA] <- 0.6/2
#dt$As_concentration[dt$As_concentration == 0] <- 0.1/2
#dt$Predicted_Se_M3[dt$Predicted_Se_M3 == NA] <- 0.1/2
#dt$Cd_concentration[dt$Cd_concentration == 0] <- 1/2
#dt$Predicted_Re_M3[dt$Predicted_Re_M3 == NA] <- 0.5/2
#dt$Hg_concentration[dt$Hg_concentration == 0] <- 0.3/2
#dt$Tl_concentration[dt$Tl_concentration == 0] <- 1/2
#dt$Pb_concentration[dt$Pb_concentration == 0] <- 0.2/2

dt$Predicted_Zn_M3[is.na(dt$Predicted_Zn_M3)] <- 0.6/2
dt$Predicted_Mn_M3[is.na(dt$Predicted_Mn_M3)] <- 1/2
dt$Predicted_Cu_M3[is.na(dt$Predicted_Cu_M3)] <- 0.5 / 2
dt$Predicted_Se_M3[is.na(dt$Predicted_Se_M3)] <- 0.1/2
dt$Predicted_Re_M3[is.na(dt$Predicted_Re_M3)] <- 0.5 / 2
dt$Predicted_Fe_M3[is.na(dt$Predicted_Fe_M3)] <- 5/2

Cu <- ggplot(dt, aes(x = reorder(CESM_Name, Predicted_Cu_M3, FUN = median),
                             y = Predicted_Cu_M3, CESM_Name=CESM_Name)) +
  geom_boxplot() +
  geom_point(size = 1.4, stroke = 1, aes(color = Plot, shape = Form, fill = Plot)) +
  scale_shape_manual(values = c(21, 24, 23, 25)) +
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  geom_hline(yintercept = 40, linetype = "dashed", color = "#9a9a9a", size = 1.2) +
  geom_hline(yintercept = 300, linetype = "dotdash", color = "#454545", size = 1.2) +
  coord_flip() +
  scale_x_discrete(guide = guide_axis(angle = 0)) +
  scale_y_continuous(limits = c(0, 800), breaks = seq(0, 800, by = 100)) +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
        axis.title.x = element_text(size = 19),
        axis.text.y = element_text(size=14, face="italic"),
        axis.title.y = element_blank(),
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 13.5), 
        legend.title = element_text(size=15, face = "bold"))+
  guides(color = guide_legend(override.aes = list(size = 3.5)),
         shape = guide_legend(override.aes = list(size = 3.5))) +
  ylab("Cu (mg/kg)")

Cu


Se <- ggplot(dt, aes(x = reorder(CESM_Name, Predicted_Se_M3, FUN = median),
                     y = Predicted_Se_M3, CESM_Name=CESM_Name)) +
  geom_boxplot() +
  geom_point(size = 1.4, stroke = 1, aes(color = Plot, shape = Form, fill = Plot)) +
  scale_shape_manual(values = c(21, 24, 23, 25)) +
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  geom_hline(yintercept = 5, linetype = "dashed", color = "#9a9a9a", size = 1.2) +
  #geom_hline(yintercept = 100, linetype = "dotdash", color = "#454545", size = 1.2) +
  coord_flip() +
  scale_x_discrete(guide = guide_axis(angle = 0)) +
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, by = 10)) +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
        axis.title.x = element_text(size = 19),
        axis.text.y = element_text(size=14, face="italic"),
        axis.title.y = element_blank(),
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 13.5), 
        legend.title = element_text(size=15, face = "bold"))+
  guides(color = guide_legend(override.aes = list(size = 3.5)),
         shape = guide_legend(override.aes = list(size = 3.5))) +
  ylab("Se (mg/kg)")

Se


Re <- ggplot(dt, aes(x = reorder(CESM_Name, Predicted_Re_M3, FUN = median),
                     y = Predicted_Re_M3, CESM_Name=CESM_Name)) +
  geom_boxplot() +
  geom_point(size = 1.4, stroke = 1, aes(color = Plot, shape = Form, fill = Plot)) +
  scale_shape_manual(values = c(21, 24, 23, 25)) +
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  #geom_hline(yintercept = 500, linetype = "dashed", color = "#9a9a9a", size=1.2)+
  #geom_hline(yintercept = 300, linetype = "dotdash", color = "#454545", size = 1.2) +
  coord_flip() +
  scale_x_discrete(guide = guide_axis(angle = 0)) +
  #scale_y_continuous(limits = c(0, 2650), breaks = seq(0, 2650, by = 250)) +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
        axis.title.x = element_text(size = 19),
        axis.text.y = element_text(size=14, face="italic"),
        axis.title.y = element_blank(),
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 13.5), 
        legend.title = element_text(size=15, face = "bold"))+
  guides(color = guide_legend(override.aes = list(size = 3.5)),
         shape = guide_legend(override.aes = list(size = 3.5))) +
  ylab("Re (mg/kg)")

Re



Zn <- ggplot(dt, aes(x = reorder(CESM_Name, Predicted_Zn_M3, FUN = median),
                     y = Predicted_Zn_M3, CESM_Name=CESM_Name)) +
geom_boxplot() +
  geom_point(size = 1.4, stroke = 1, aes(color = Plot, shape = Form, fill = Plot)) +
  scale_shape_manual(values = c(21, 24, 23, 25)) +
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  #geom_hline(yintercept = 5, linetype = "dashed", color = "#9a9a9a", size = 1.2) +
  #geom_hline(yintercept = 100, linetype = "dotdash", color = "#454545", size = 1.2) +
  coord_flip() +
  scale_x_discrete(guide = guide_axis(angle = 0)) +
  scale_y_continuous(limits = c(0, 90), breaks = seq(0, 90, by = 15)) +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
        axis.title.x = element_text(size = 19),
        axis.text.y = element_text(size=14, face="italic"),
        axis.title.y = element_blank(),
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 13.5), 
        legend.title = element_text(size=15, face = "bold"))+
  guides(color = guide_legend(override.aes = list(size = 3.5)),
         shape = guide_legend(override.aes = list(size = 3.5))) +
  ylab("Zn (mg/kg)")

Zn




Mn <- ggplot(dt, aes(x = reorder(CESM_Name, Predicted_Mn_M3, FUN = median),
                     y = Predicted_Mn_M3, CESM_Name=CESM_Name)) +
  geom_boxplot() +
  geom_point(size = 1.4, stroke = 1, aes(color = Plot, shape = Form, fill = Plot)) +
  scale_shape_manual(values = c(21, 24, 23, 25)) +
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  #geom_hline(yintercept = 500, linetype = "dashed", color = "#9a9a9a", size=1.2)+
  #geom_hline(yintercept = 300, linetype = "dotdash", color = "#454545", size = 1.2) +
  coord_flip() +
  scale_x_discrete(guide = guide_axis(angle = 0)) +
  #scale_y_continuous(limits = c(0, 2650), breaks = seq(0, 2650, by = 250)) +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
        axis.title.x = element_text(size = 19),
        axis.text.y = element_text(size=14, face="italic"),
        axis.title.y = element_blank(),
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 13.5), 
        legend.title = element_text(size=15, face = "bold"))+
  guides(color = guide_legend(override.aes = list(size = 3.5)),
         shape = guide_legend(override.aes = list(size = 3.5))) +
  ylab("Mn (mg/kg)")

Mn


Fe <- ggplot(dt, aes(x = reorder(CESM_Name, Predicted_Fe_M3, FUN = median),
                     y = Predicted_Fe_M3, CESM_Name=CESM_Name)) +
  geom_boxplot() +
  geom_point(size = 1.4, stroke = 1, aes(color = Plot, shape = Form, fill = Plot)) +
  scale_shape_manual(values = c(21, 24, 23, 25)) +
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  geom_hline(yintercept = 500, linetype = "dashed", color = "#9a9a9a", size=1.2)+
  #geom_hline(yintercept = 300, linetype = "dotdash", color = "#454545", size = 1.2) +
  coord_flip() +
  scale_x_discrete(guide = guide_axis(angle = 0)) +
  scale_y_continuous(limits = c(0, 2650), breaks = seq(0, 2650, by = 250)) +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
        axis.title.x = element_text(size = 19),
        axis.text.y = element_text(size=14, face="italic"),
        axis.title.y = element_blank(),
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 13.5), 
        legend.title = element_text(size=15, face = "bold"))+
  guides(color = guide_legend(override.aes = list(size = 3.5)),
         shape = guide_legend(override.aes = list(size = 3.5))) +
  ylab("Fe (mg/kg)")

Fe


ggsave(filename = "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/CESM/Report24/Fe_Plot.png", 
       plot = Fe, 
       width = 10, 
       height = 8, 
       units = "in", 
       dpi = 300)

ggsave(filename = "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/CESM/Report24/Cu_Plot.png", 
       plot = Cu, 
       width = 10, 
       height = 8, 
       units = "in", 
       dpi = 300)

ggsave(filename = "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/CESM/Report24/Mn_Plot.png", 
       plot = Mn, 
       width = 10, 
       height = 8, 
       units = "in", 
       dpi = 300)

ggsave(filename = "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/CESM/Report24/Zn_Plot.png", 
       plot = Zn, 
       width = 10, 
       height = 8, 
       units = "in", 
       dpi = 300)

ggsave(filename = "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/CESM/Report24/Se_Plot.png", 
       plot = Se, 
       width = 10, 
       height = 8, 
       units = "in", 
       dpi = 300)

ggsave(filename = "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/CESM/Report24/Re_Plot.png", 
       plot = Re, 
       width = 10, 
       height = 8, 
       units = "in", 
       dpi = 300)






dt_Cu <- dt %>%
  group_by(Scientific_Name, Site) %>%
  summarize(
    Median = median(Predicted_Cu_M3),
    Mean = mean(Predicted_Cu_M3),
    SD = sd(Predicted_Cu_M3) / sqrt(n())
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





dt_Se <- dt %>%
  group_by(Scientific_Name, Site) %>%
  summarize(
    Median = median(Predicted_Cu_M3),
    Mean = mean(Predicted_Se_M3),
    SD = sd(Predicted_Se_M3) / sqrt(n())
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

Se <- ggplot(dt_Se, aes(x = reorder(Scientific_Name, Median), 
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
  labs(x = "", y = "Se (mg/kg)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key.size = unit(1, "lines"),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.text = element_text(size = 4),
        legend.title = element_text(size = 4, face = "bold")
  )

Se


dt_Re <- dt %>%
  group_by(Scientific_Name, Site) %>%
  summarize(
    Median = median(Predicted_Cu_M3),
    Mean = mean(Predicted_Re_M3),
    SD = sd(Predicted_Re_M3) / sqrt(n())
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
  geom_hline(yintercept = 5, linetype = "dashed", color = "#AD0B0B", size = 0.3) +
  geom_hline(yintercept = 20, linetype = "twodash", color = "#003f5c", size = 0.3) +
  labs(x = "", y = "Re (mg/kg)") +
  theme_bw() +
 scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key.size = unit(1, "lines"),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.text = element_text(size = 4),
        legend.title = element_text(size = 4, face = "bold")
  )

Re





dt_Zn <- dt %>%
  group_by(Scientific_Name, Site) %>%
  summarize(
    Median = median(Predicted_Cu_M3),
    Mean = mean(Predicted_Zn_M3),
    SD = sd(Predicted_Zn_M3) / sqrt(n())
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
  geom_hline(yintercept = 5, linetype = "dashed", color = "#AD0B0B", size = 0.3) +
  geom_hline(yintercept = 20, linetype = "twodash", color = "#003f5c", size = 0.3) +
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





dt_Mn <- dt %>%
  group_by(Scientific_Name, Site) %>%
  summarize(
    Median = median(Predicted_Cu_M3),
    Mean = mean(Predicted_Mn_M3),
    SD = sd(Predicted_Mn_M3) / sqrt(n())
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
  geom_hline(yintercept = 5, linetype = "dashed", color = "#AD0B0B", size = 0.3) +
  geom_hline(yintercept = 20, linetype = "twodash", color = "#003f5c", size = 0.3) +
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






dt_Fe <- dt %>%
  group_by(Scientific_Name, Site) %>%
  summarize(
    Median = median(Predicted_Cu_M3),
    Mean = mean(Predicted_Fe_M3),
    SD = sd(Predicted_Fe_M3) / sqrt(n())
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
  coord_flip() +
  geom_hline(yintercept = 5, linetype = "dashed", color = "#AD0B0B", size = 0.3) +
  geom_hline(yintercept = 20, linetype = "twodash", color = "#003f5c", size = 0.3) +
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
