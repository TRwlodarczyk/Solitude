# Solutude Figures - CESM
# Tomasz Wlodarczyk
# twlodarczyk@arizona.edu
# 2023-04-15
{
library(data.table)
library(reshape2)
library(reshape)
library("readxl")
library(ggpubr)
library(agricolae)
library(tidyverse)
library (readr) #to read URL
}

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/CESM/Boxplots")
dt_plants <-read_csv("dt_plants.csv")
#dt_plants2 <-read_csv("dt_plants2.csv") # this one includes the species number
dt_plants2 <-read_csv("Plants_new_LOD_nounc2.csv")
dt_soil_new <- read.delim("Soil_new_trim.txt") # this datase has added T and S column and P1.S P1.T etc


{

Cu <- ggplot(dt_plants2, aes(x = reorder(CESM_Name, Cu_concentration, FUN = median),
                            y = Cu_concentration, CESM_Name=CESM_Name)) +
  geom_boxplot() +
  geom_point(size = 1.4, stroke = 1, aes(color = Plot, shape = Form, fill = Plot)) +
  scale_shape_manual(values = c(21, 24, 23, 25)) +
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  geom_hline(yintercept = 40, linetype = "dashed", color = "#9a9a9a", size = 1.2) +
  geom_hline(yintercept = 300, linetype = "dotdash", color = "#454545", size = 1.2) +
  coord_flip() +
  scale_x_discrete(guide = guide_axis(angle = 0)) +
  scale_y_continuous(limits = c(0, 600), breaks = seq(0, 600, by = 50)) +
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

As <- ggplot(dt_plants2, aes(x = reorder(CESM_Name, As_concentration, FUN = median),
                                     y = As_concentration, CESM_Name=CESM_Name)) +
  geom_boxplot() +
  geom_point(size = 1.4, stroke = 1, aes(color = Plot, shape = Form, fill = Plot)) +
  scale_shape_manual(values = c(21, 24, 23, 25)) +
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  geom_hline(yintercept = 12, linetype = "dashed", color = "#9a9a9a", size = 1.2) +
  #geom_hline(yintercept = 300, linetype = "dotdash", color = "#454545", size = 1.2) +
  coord_flip() +
  scale_x_discrete(guide = guide_axis(angle = 0)) +
  scale_y_continuous(limits = c(0, 18), breaks = seq(0, 18, by = 2)) +
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
  ylab("As (mg/kg)")

As



Ca <- ggplot(dt_plants2, aes(x = reorder(CESM_Name, Ca_concentration, FUN = median),
                                     y = Ca_concentration, CESM_Name=CESM_Name)) +
  geom_boxplot() +
  geom_point(size = 1.4, stroke = 1, aes(color = Plot, shape = Form, fill = Plot)) +
  scale_shape_manual(values = c(21, 24, 23, 25)) +
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  geom_hline(yintercept = 15000, linetype = "dashed", color = "#9a9a9a", size = 1.2) +
  #geom_hline(yintercept = 40000, linetype = "dotdash", color = "#454545", size = 1.2) +
  coord_flip() +
  scale_x_discrete(guide = guide_axis(angle = 0)) +
  scale_y_continuous(limits = c(0, 55000), breaks = seq(0, 55000, by = 5000)) +
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
  ylab("Ca (mg/kg)")

Ca



Cr <- ggplot(dt_plants2, aes(x = reorder(CESM_Name, Cr_concentration, FUN = median),
                                     y = Cr_concentration, CESM_Name=CESM_Name)) +
  geom_boxplot() +
  geom_point(size = 1.4, stroke = 1, aes(color = Plot, shape = Form, fill = Plot)) +
  scale_shape_manual(values = c(21, 24, 23, 25)) +
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "#9a9a9a", size=1.2)+
  #geom_hline(yintercept = 300, linetype = "dotdash", color = "#454545", size = 1.2) +
  coord_flip() +
  scale_x_discrete(guide = guide_axis(angle = 0)) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 4)) +
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
  ylab("Cr (mg/kg)")

Cr



Fe <- ggplot(dt_plants2, aes(x = reorder(CESM_Name, Fe_concentration, FUN = median),
                                     y = Fe_concentration, CESM_Name=CESM_Name)) +
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



Mn <- ggplot(dt_plants2, aes(x = reorder(CESM_Name, Mn_concentration, FUN = median),
                            y = Mn_concentration, CESM_Name=CESM_Name)) +
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



Ni <- ggplot(dt_plants2, aes(x = reorder(CESM_Name, Ni_concentration, FUN = median),
                            y = Ni_concentration, CESM_Name=CESM_Name)) +
  geom_boxplot() +
  geom_point(size = 1.4, stroke = 1, aes(color = Plot, shape = Form, fill = Plot)) +
  scale_shape_manual(values = c(21, 24, 23, 25)) +
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  coord_flip() +
  scale_x_discrete(guide = guide_axis(angle = 0)) +
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 1)) +
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
  ylab("Ni (mg/kg)")

Ni



Re <- ggplot(dt_plants2, aes(x = reorder(CESM_Name, Re_concentration, FUN = median),
                            y = Re_concentration, CESM_Name=CESM_Name)) +
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

Se <- ggplot(dt_plants2, aes(x = reorder(CESM_Name, Se_concentration, FUN = median),
                            y = Se_concentration, CESM_Name=CESM_Name)) +
  geom_boxplot() +
  geom_point(size = 1.4, stroke = 1, aes(color = Plot, shape = Form, fill = Plot)) +
  scale_shape_manual(values = c(21, 24, 23, 25)) +
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  geom_hline(yintercept = 5, linetype = "dashed", color = "#9a9a9a", size = 1.2) +
  #geom_hline(yintercept = 100, linetype = "dotdash", color = "#454545", size = 1.2) +
  coord_flip() +
  scale_x_discrete(guide = guide_axis(angle = 0)) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 10)) +
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



Zn <- ggplot(dt_plants2, aes(x = reorder(CESM_Name, Zn_concentration, FUN = median),
                            y = Zn_concentration, CESM_Name=CESM_Name)) +
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

Cd <- ggplot(dt_plants2, aes(x = reorder(CESM_Name, Cd_concentration, FUN = median),
                             y = Cd_concentration, CESM_Name=CESM_Name)) +
  geom_boxplot() +
  geom_point(size = 1.4, stroke = 1, aes(color = Plot, shape = Form, fill = Plot)) +
  scale_shape_manual(values = c(21, 24, 23, 25)) +
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "#9a9a9a", size = 1.2) +
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
  ylab("Cd (mg/kg)")

Cd

}


# Counts across different pltos

ggplot(dt_plants2, aes(x = Plot, y = reorder(CESM_Name, table(CESM_Name)[CESM_Name]), group = Form)) +
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


















#########OTHER########
ggplot(dt_plants, aes(x = Scientific_Name, fill = Plot)) +
  geom_bar() +
  facet_wrap(~Form, nrow = 1, scales = "free_x") +
  coord_flip() +
  scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  theme_classic() +
  labs(x = "Scientific Name", y = "Counts", fill = "Plot") +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    strip.background = element_blank(),
    strip.text = element_text(size = 14, face = "bold")
  )


ggplot(dt_plants, aes(x = Plot, fill = Form)) +
  geom_bar(stat = "count", position = "stack") +
  labs(title = "Number of plant species in each plot by form", x = "Plot", y = "Proportion") +
  scale_fill_manual(values = c("#9dc183", "#cbd394", "#6f8817", "#3e4d34")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.key.size = unit(0.5, "cm"),
        strip.text = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 12, vjust = 0.5, angle = 90))


library(dplyr)

# Group the data by Scientific_Name and Plot, and count the number of rows in each group
species_per_plot <- dt_plants %>% 
  group_by(Scientific_Name, Plot) %>% 
  summarise(num_species = n())

# Group by plot only - i.e. how many plants collected from each plot
species_per_plot <- dt_plants %>% 
  group_by(Plot) %>% 
  summarise(num_species = n())





ggplot(dt_plants, aes(x = Plot, y = Scientific_Name, color = Form)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("blue", "green", "orange", "purple")) +
  labs(title = "Presence of plant species in different plots",
       x = "Plot",
       y = "Scientific Name",
       color = "Form") +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 12, face = "bold"))

#################################

#SOIL figures
#########################




#mean(dt_soil_new$Cu_Concentration[dt_soil_new$Plot2 == "P1"], na.rm = TRUE) this is to check if tidy grouping worked


#sd(dt_soil_new$Cu_Concentration[dt_soil_new$Plot2 == "P2"], na.rm = TRUE)


dt_soil_new$As_Concentration[dt_soil_new$As_Concentration == 0] <- 0.5/2
dt_soil_new$Ca_Concentration[dt_soil_new$Ca_Concentration == 0] <- 20/2
dt_soil_new$Cd_Concentration[dt_soil_new$Cd_Concentration == 0] <- 0.8/2
dt_soil_new$Cl_Concentration[dt_soil_new$Cl_Concentration == 0] <- 220/2
dt_soil_new$Cr_Concentration[dt_soil_new$Cr_Concentration == 0] <- 5/2
dt_soil_new$Cu_Concentration[dt_soil_new$Cu_Concentration == 0] <- 1.5/2
dt_soil_new$Fe_Concentration[dt_soil_new$Fe_Concentration == 0] <- 12/2
dt_soil_new$Mn_Concentration[dt_soil_new$Mn_Concentration == 0] <- 4/2
dt_soil_new$Ni_Concentration[dt_soil_new$Ni_Concentration == 0] <- 3/2
dt_soil_new$Pb_Concentration[dt_soil_new$Pb_Concentration == 0] <- 0.5/2
dt_soil_new$Re_Concentration[dt_soil_new$Re_Concentration == 0] <- 1/2
dt_soil_new$Se_Concentration[dt_soil_new$Se_Concentration == 0] <- 0.4/2
dt_soil_new$Ti_Concentration[dt_soil_new$Ti_Concentration == 0] <- 5/2
dt_soil_new$Zn_Concentration[dt_soil_new$Zn_Concentration == 0] <- 1/2
dt_soil_new$Hg_Concentration[dt_soil_new$Hg_Concentration == 0] <- 0.5/2
dt_soil_new$Tl_Concentration[dt_soil_new$Tl_Concentration == 0] <- 1/2
dt_soil_new$Co_Concentration[dt_soil_new$Co_Concentration == 0] <- 1/2



#write.table(dt_soil_new, file='C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Newest Data/Soil_new_trim_LOD.csv', sep=",", row.names = F)



library(tidyverse)

dt_soil_new_summary_Cu <- dt_soil_new %>%
  group_by(Plot2, Layer) %>%
  summarize(Mean = mean(Cu_Concentration), SD = sd(Cu_Concentration)/sqrt(n())) %>%
  ungroup()

Cu_soil <- ggplot(dt_soil_new_summary_Cu, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) + #forcats switch the position of S and T as they are displayed
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(Plot2~., scales = "free_y") + # facet_wrap would do 4 plots, facet_grid do all on one facet_wrap(~ Plot2, scales = "free_y")
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.9),
                width = 0.2) +
  coord_flip() +
 # scale_y_continuous(limits = c(0, 3000), breaks = seq(0, 3000, by = 500)) +
  labs(x = "", y = "Cu (mg/kg)") +
  scale_fill_manual(values = c("#0070C0", "#E69F00")) +
  theme_bw()

Cu_soil

dt_soil_new_summary_Mn <- dt_soil_new %>%
  group_by(Plot2, Layer) %>%
  summarize(Mean = mean(Mn_Concentration), SD = sd(Mn_Concentration)/sqrt(n())) %>%
  ungroup()

Mn_soil <- ggplot(dt_soil_new_summary_Mn, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) + #forcats switch the position of S and T as they are displayed
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(Plot2~., scales = "free_y") + # facet_wrap would do 4 plots, facet_grid do all on one facet_wrap(~ Plot2, scales = "free_y")
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.9),
                width = 0.2) +
  coord_flip() +
  #scale_y_continuous(limits = c(0, 3000), breaks = seq(0, 3000, by = 500)) +
  labs(x = "", y = "Mn (mg/kg)") +
  scale_fill_manual(values = c("#0070C0", "#E69F00")) +
  theme_bw()
Mn_soil


dt_soil_new_summary_Pb <- dt_soil_new %>%
  group_by(Plot2, Layer) %>%
  summarize(Mean = mean(Pb_Concentration), SD = sd(Pb_Concentration)/sqrt(n())) %>%
  ungroup()

Pb_soil <- ggplot(dt_soil_new_summary_Pb, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) + #forcats switch the position of S and T as they are displayed
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(Plot2~., scales = "free_y") + # facet_wrap would do 4 plots, facet_grid do all on one facet_wrap(~ Plot2, scales = "free_y")
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.9),
                width = 0.2) +
  coord_flip() +
  #scale_y_continuous(limits = c(0, 3000), breaks = seq(0, 3000, by = 500)) +
  labs(x = "", y = "Pb (mg/kg)") +
  scale_fill_manual(values = c("#0070C0", "#E69F00")) +
  theme_bw()
Pb_soil


dt_soil_new_summary_Ti <- dt_soil_new %>%
  group_by(Plot2, Layer) %>%
  summarize(Mean = mean(Ti_Concentration), SD = sd(Ti_Concentration)/sqrt(n())) %>%
  ungroup()

Ti_soil <- ggplot(dt_soil_new_summary_Ti, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) + #forcats switch the position of S and T as they are displayed
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(Plot2~., scales = "free_y") + # facet_wrap would do 4 plots, facet_grid do all on one facet_wrap(~ Plot2, scales = "free_y")
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.9),
                width = 0.2) +
  coord_flip() +
  #scale_y_continuous(limits = c(0, 3000), breaks = seq(0, 3000, by = 500)) +
  labs(x = "", y = "Ti (mg/kg)") +
  scale_fill_manual(values = c("#0070C0", "#E69F00")) +
  theme_bw()
Ti_soil

dt_soil_new_summary_Cr <- dt_soil_new %>%
  group_by(Plot2, Layer) %>%
  summarize(Mean = mean(Cr_Concentration), SD = sd(Cr_Concentration)/sqrt(n())) %>%
  ungroup()

Cr_soil <- ggplot(dt_soil_new_summary_Cr, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) + #forcats switch the position of S and T as they are displayed
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(Plot2~., scales = "free_y") + # facet_wrap would do 4 plots, facet_grid do all on one facet_wrap(~ Plot2, scales = "free_y")
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.9),
                width = 0.2) +
  coord_flip() +
  #scale_y_continuous(limits = c(0, 3000), breaks = seq(0, 3000, by = 500)) +
  labs(x = "", y = "Cr (mg/kg)") +
  scale_fill_manual(values = c("#0070C0", "#E69F00")) +
  theme_bw()
Cr_soil


dt_soil_new_summary_Fe <- dt_soil_new %>%
  group_by(Plot2, Layer) %>%
  summarize(Mean = mean(Fe_Concentration), SD = sd(Fe_Concentration)/sqrt(n())) %>%
  ungroup()

Fe_soil <- ggplot(dt_soil_new_summary_Fe, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) + #forcats switch the position of S and T as they are displayed
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(Plot2~., scales = "free_y") + # facet_wrap would do 4 plots, facet_grid do all on one facet_wrap(~ Plot2, scales = "free_y")
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.9),
                width = 0.2) +
  coord_flip() +
  #scale_y_continuous(limits = c(0, 3000), breaks = seq(0, 3000, by = 500)) +
  labs(x = "", y = "Fe (mg/kg)") +
  scale_fill_manual(values = c("#0070C0", "#E69F00")) +
  theme_bw()
Fe_soil

dt_soil_new_summary_Ca <- dt_soil_new %>%
  group_by(Plot2, Layer) %>%
  summarize(Mean = mean(Ca_Concentration), SD = sd(Ca_Concentration)/sqrt(n())) %>%
  ungroup()

Ca_soil <- ggplot(dt_soil_new_summary_Ca, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) + #forcats switch the position of S and T as they are displayed
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(Plot2~., scales = "free_y") + # facet_wrap would do 4 plots, facet_grid do all on one facet_wrap(~ Plot2, scales = "free_y")
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.9),
                width = 0.2) +
  coord_flip() +
  #scale_y_continuous(limits = c(0, 3000), breaks = seq(0, 3000, by = 500)) +
  labs(x = "", y = "Ca (mg/kg)") +
  scale_fill_manual(values = c("#0070C0", "#E69F00")) +
  theme_bw()
Ca_soil


dt_soil_new_summary_As <- dt_soil_new %>%
  group_by(Plot2, Layer) %>%
  summarize(Mean = mean(As_Concentration), SD = sd(As_Concentration)/sqrt(n())) %>%
  ungroup()

As_soil <- ggplot(dt_soil_new_summary_As, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) + #forcats switch the position of S and T as they are displayed
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(Plot2~., scales = "free_y") + # facet_wrap would do 4 plots, facet_grid do all on one facet_wrap(~ Plot2, scales = "free_y")
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.9),
                width = 0.2) +
  coord_flip() +
  #scale_y_continuous(limits = c(0, 3000), breaks = seq(0, 3000, by = 500)) +
  labs(x = "", y = "As (mg/kg)") +
  scale_fill_manual(values = c("#0070C0", "#E69F00")) +
  theme_bw()
As_soil

dt_soil_new_summary_Se <- dt_soil_new %>%
  group_by(Plot2, Layer) %>%
  summarize(Mean = mean(Se_Concentration), SD = sd(Se_Concentration)/sqrt(n())) %>%
  ungroup()

Se_soil <- ggplot(dt_soil_new_summary_Se, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) + #forcats switch the position of S and T as they are displayed
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(Plot2~., scales = "free_y") + # facet_wrap would do 4 plots, facet_grid do all on one facet_wrap(~ Plot2, scales = "free_y")
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.9),
                width = 0.2) +
  coord_flip() +
  #scale_y_continuous(limits = c(0, 3000), breaks = seq(0, 3000, by = 500)) +
  labs(x = "", y = "Se (mg/kg)") +
  scale_fill_manual(values = c("#0070C0", "#E69F00")) +
  theme_bw()
Se_soil



library(ggplot2)
library(ggpubr)

# Assuming you already have the six plots as ggplot objects named Cu_soil, Mn_soil, Pb_soil, Ti_soil, Cr_soil, Fe_soil

ggarrange(Cu_soil, Mn_soil, Pb_soil, Ti_soil, Cr_soil, Fe_soil, Ca_soil, As_soil, Se_soil,
          ncol = 3, nrow = 3, 
          common.legend = TRUE, legend = "bottom")




#stats

shapiro.test(dt_soil_new_P1$Fe_Concentration)

dt_soil_new_P6 <- subset(dt_soil_new, Plot2=="P6")

dt_soil_new_P1 <- subset(dt_soil_new, Plot2=="P1")
result <- wilcox.test(dt_soil_new_P1$Cu_Concentration ~ dt_soil_new_P1$Layer, paired = FALSE, exact = FALSE)
p_value <- result$p.value

dt_soil_new_P6 <- subset(dt_soil_new, Plot2=="P6")
result <- wilcox.test(dt_soil_new_P6$Cu_Concentration ~ dt_soil_new_P6$Layer, paired = FALSE, exact = FALSE)
p_value <- result$p.value



result <- t.test(dt_soil_new_P1$Fe_Concentration ~ dt_soil_new_P1$Layer)
p_value <- result$p.value

library(dplyr)
library(tidyr)

# Define the concentration variables
concentration_vars <- c("Cu_Concentration", "Mn_Concentration", "Pb_Concentration",
                        "Ti_Concentration", "Cr_Concentration", "Fe_Concentration",
                        "Ca_Concentration", "As_Concentration", "Se_Concentration")

# Define the plot names
plot_names <- c("P1", "P2", "P5", "P6")

# Perform Wilcoxon signed-rank test for each combination
results <- list()  # Store the results in a list

for (plot in plot_names) {
  dt_plot <- subset(dt_soil_new, Plot2 == plot)  # Subset the data for the current plot
  
  for (concentration_var in concentration_vars) {
    result <- wilcox.test(dt_plot[[concentration_var]] ~ dt_plot$Layer, paired = FALSE, exact = FALSE)
    p_value <- result$p.value
    
    # Store the results in the list
    results[[paste(plot, concentration_var, sep = "_")]] <- p_value
  }
}

# Create a data frame to store the results
result_table <- expand.grid(Plot = plot_names, Concentration = concentration_vars)
result_table$P_value <- NA

# Fill in the P_values in the table
for (i in 1:nrow(result_table)) {
  plot <- result_table$Plot[i]
  concentration_var <- result_table$Concentration[i]
  result_key <- paste(plot, concentration_var, sep = "_")
  result_table$P_value[i] <- results[[result_key]]
}

# Print the result table
print(result_table)


#and all samples at tailings surfave vs tailings


result <- wilcox.test(dt_soil_new$Cu_Concentration ~ dt_soil_new$Layer, paired = FALSE, exact = FALSE)
p_value <- result$p.value
length(dt_soil_new$Cu_Concentration) #24


