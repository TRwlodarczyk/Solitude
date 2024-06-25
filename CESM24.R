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





#Brakujace ploty z ICP only

dt <-read.delim("Solitude_CESM_Report24.txt")
dt <- dt[dt$ICP != "n", ] 




Cd <- ggplot(dt, aes(x = reorder(CESM_Name, Cd_ICP, FUN = median),
                     y = Cd_ICP, CESM_Name=CESM_Name)) +
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
  ylab("Cd (mg/kg) ICP-MS")

Cd


ggsave(filename = "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/CESM/1_CESM_Report_2024/Revision/Cd_ICP_Plot.png", 
       plot = Cd, 
       width = 10, 
       height = 8, 
       units = "in", 
       dpi = 300)



P <- ggplot(dt, aes(x = reorder(CESM_Name, P_ICP, FUN = median),
                     y = P_ICP, CESM_Name=CESM_Name)) +
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
  ylab("P (mg/kg) ICP-MS")

P


ggsave(filename = "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/CESM/1_CESM_Report_2024/Revision/P_ICP_Plot.png", 
       plot = P, 
       width = 10, 
       height = 8, 
       units = "in", 
       dpi = 300)





Ni <- ggplot(dt, aes(x = reorder(CESM_Name, Ni_ICP, FUN = median),
                    y = Ni_ICP, CESM_Name=CESM_Name)) +
  geom_boxplot() +
  geom_point(size = 1.4, stroke = 1, aes(color = Plot, shape = Form, fill = Plot)) +
  scale_shape_manual(values = c(21, 24, 23, 25)) +
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  #geom_hline(yintercept = 100, linetype = "dashed", color = "#9a9a9a", size=1.2)+
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
  ylab("Ni (mg/kg) ICP-MS")

Ni


ggsave(filename = "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/CESM/1_CESM_Report_2024/Revision/Ni_ICP_Plot.png", 
       plot = Ni, 
       width = 10, 
       height = 8, 
       units = "in", 
       dpi = 300)









Cr <- ggplot(dt, aes(x = reorder(CESM_Name, Cr_ICP, FUN = median),
                     y = Cr_ICP, CESM_Name=CESM_Name)) +
  geom_boxplot() +
  geom_point(size = 1.4, stroke = 1, aes(color = Plot, shape = Form, fill = Plot)) +
  scale_shape_manual(values = c(21, 24, 23, 25)) +
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  #geom_hline(yintercept = 100, linetype = "dashed", color = "#9a9a9a", size=1.2)+
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
  ylab("Cr (mg/kg) ICP-MS")

Cr


ggsave(filename = "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/CESM/1_CESM_Report_2024/Revision/Cr_ICP_Plot.png", 
       plot = Cr, 
       width = 10, 
       height = 8, 
       units = "in", 
       dpi = 300)




S <- ggplot(dt, aes(x = reorder(CESM_Name, S_ICP, FUN = median),
                     y = S_ICP, CESM_Name=CESM_Name)) +
  geom_boxplot() +
  geom_point(size = 1.4, stroke = 1, aes(color = Plot, shape = Form, fill = Plot)) +
  scale_shape_manual(values = c(21, 24, 23, 25)) +
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  #geom_hline(yintercept = 100, linetype = "dashed", color = "#9a9a9a", size=1.2)+
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
  ylab("S (mg/kg) ICP-MS")

S


ggsave(filename = "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/CESM/1_CESM_Report_2024/Revision/S_ICP_Plot.png", 
       plot = S, 
       width = 10, 
       height = 8, 
       units = "in", 
       dpi = 300)







As <- ggplot(dt, aes(x = reorder(CESM_Name, As_ICP, FUN = median),
                    y = As_ICP, CESM_Name=CESM_Name)) +
  geom_boxplot() +
  geom_point(size = 1.4, stroke = 1, aes(color = Plot, shape = Form, fill = Plot)) +
  scale_shape_manual(values = c(21, 24, 23, 25)) +
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  #geom_hline(yintercept = 100, linetype = "dashed", color = "#9a9a9a", size=1.2)+
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
  ylab("As (mg/kg) ICP-MS")

As


ggsave(filename = "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/CESM/1_CESM_Report_2024/Revision/As_ICP_Plot.png", 
       plot = As, 
       width = 10, 
       height = 8, 
       units = "in", 
       dpi = 300)


Ti <- ggplot(dt, aes(x = reorder(CESM_Name, Ti_ICP, FUN = median),
                     y = Ti_ICP, CESM_Name=CESM_Name)) +
  geom_boxplot() +
  geom_point(size = 1.4, stroke = 1, aes(color = Plot, shape = Form, fill = Plot)) +
  scale_shape_manual(values = c(21, 24, 23, 25)) +
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  #geom_hline(yintercept = 100, linetype = "dashed", color = "#9a9a9a", size=1.2)+
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
  ylab("Ti (mg/kg) ICP-MS")

Ti


ggsave(filename = "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/CESM/1_CESM_Report_2024/Revision/Ti_ICP_Plot.png", 
       plot = Ti, 
       width = 10, 
       height = 8, 
       units = "in", 
       dpi = 300)








Ca <- ggplot(dt, aes(x = reorder(CESM_Name, Ca_PXRF, FUN = median),
                     y = Ca_PXRF, CESM_Name=CESM_Name)) +
  geom_boxplot() +
  geom_point(size = 1.4, stroke = 1, aes(color = Plot, shape = Form, fill = Plot)) +
  scale_shape_manual(values = c(21, 24, 23, 25)) +
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  geom_hline(yintercept = 15000, linetype = "dashed", color = "#9a9a9a", size=1.2)+
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
  ylab("Ca (mg/kg)")

Ca


ggsave(filename = "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/CESM/1_CESM_Report_2024/Revision/Ca_PXRF_Plot.png", 
       plot = Ca, 
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






#########


#Tabellka ICP vs PXRF

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/")

dt <-read.delim("Solitude_CESM_Report24.txt")
dt <- dt[dt$ICP != "n", ] 


dt <- dt %>%
  mutate(
    Cu_diff = abs(Cu_ICP - Cu_PXRF),
    Se_diff = abs(Se_ICP - Se_PXRF),
    Re_diff = abs(Re_ICP - Re_PXRF),
    Zn_diff = abs(Zn_ICP - Zn_PXRF),
    Mn_diff = abs(Mn_ICP - Mn_PXRF),
    Fe_diff = abs(Fe_ICP - Fe_PXRF),
    As_diff = abs(As_ICP - As_PXRF),
    Cd_diff = abs(Cd_ICP - Cd_PXRF),
    Ni_diff = abs(Ni_ICP - Ni_PXRF),
    Ti_diff = abs(Ti_ICP - Ti_PXRF),
    S_diff = abs(S_ICP - S_PXRF),
    P_diff = abs(P_ICP - P_PXRF),
    Cr_diff = abs(Cr_ICP - Cr_PXRF)

  )





# Function to calculate summary statistics and reshape the data
calculate_summary <- function(data, element) {
  element_ICP <- paste0(element, "_ICP")
  element_PXRF <- paste0(element, "_PXRF")
  element_diff <- paste0(element, "_diff")
  
  summary_df <- data %>%
    summarize(
      min_ICP = min(get(element_ICP), na.rm = TRUE),
      max_ICP = max(get(element_ICP), na.rm = TRUE),
      mean_ICP = mean(get(element_ICP), na.rm = TRUE),
      median_ICP = median(get(element_ICP), na.rm = TRUE),
      sd_ICP = sd(as.numeric(get(element_ICP)), na.rm = TRUE),
      min_PXRF = min(get(element_PXRF), na.rm = TRUE),
      max_PXRF = max(get(element_PXRF), na.rm = TRUE),
      mean_PXRF = mean(get(element_PXRF), na.rm = TRUE),
      median_PXRF = median(get(element_PXRF), na.rm = TRUE),
      sd_PXRF = sd(as.numeric(get(element_PXRF)), na.rm = TRUE)
    ) %>%
    pivot_longer(cols = everything(), names_to = "Statistic", values_to = "Value") %>%
    separate(Statistic, into = c("Statistic", "Method"), sep = "_") %>%
    mutate(Element = element) %>%
    select(Element, Statistic, Method, Value)
  
  return(summary_df)
}

# Elements to process
elements <- c("Cu", "Se", "Re", "Zn", "Mn", "Fe", "As", "Cd", "Ni", "Ti", "S", "P", "Cr")

# Generate the summary statistics for each element
summary_list <- lapply(elements, function(element) calculate_summary(dt, element))

# Combine all summaries into one dataframe
summary_df <- bind_rows(summary_list)

# Reshape the data so that ICP and PXRF are in separate columns
final_df <- summary_df %>%
  pivot_wider(names_from = Method, values_from = Value)

# Function to conditionally round values, with special handling for certain elements
conditional_round <- function(x, element) {
  if (element %in% c("Se", "Re", "Cr", "As", "Cd", "Ni")) {
    if_else(abs(x) >= 10, round(x, 0), round(x, 2))
  } else {
    if_else(abs(x) >= 30, round(x, 0), round(x, 1))
  }
}

# Apply the conditional rounding to all numeric columns
final_df <- final_df %>%
  rowwise() %>%
  mutate(ICP = conditional_round(ICP, Element),
         PXRF = conditional_round(PXRF, Element)) %>%
  ungroup()

# Print the result
print(final_df)

# Save the result to an Excel file
write.xlsx(final_df, "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/CESM/1_CESM_Report_2024/Revision/CESM_ICP-PXRF2.xlsx")






#with diff

library(dplyr)
library(tidyr)
library(openxlsx)

# Read the data
dt <- read.delim("Solitude_CESM_Report24.txt")
dt <- dt[dt$ICP != "n", ] 

# Calculate the difference columns
dt <- dt %>%
  mutate(
    Cu_diff = abs(Cu_ICP - Cu_PXRF),
    Se_diff = abs(Se_ICP - Se_PXRF),
    Re_diff = abs(Re_ICP - Re_PXRF),
    Zn_diff = abs(Zn_ICP - Zn_PXRF),
    Mn_diff = abs(Mn_ICP - Mn_PXRF),
    Fe_diff = abs(Fe_ICP - Fe_PXRF),
    As_diff = abs(As_ICP - As_PXRF),
    Cd_diff = abs(Cd_ICP - Cd_PXRF),
    Ni_diff = abs(Ni_ICP - Ni_PXRF),
    Ti_diff = abs(Ti_ICP - Ti_PXRF),
    S_diff = abs(S_ICP - S_PXRF),
    P_diff = abs(P_ICP - P_PXRF),
    Cr_diff = abs(Cr_ICP - Cr_PXRF)
  )




# Function to calculate summary statistics and reshape the data
calculate_summary <- function(data, element) {
  element_ICP <- paste0(element, "_ICP")
  element_PXRF <- paste0(element, "_PXRF")
  element_diff <- paste0(element, "_diff")
  
  summary_df <- data %>%
    summarize(
      min_ICP = min(get(element_ICP), na.rm = TRUE),
      max_ICP = max(get(element_ICP), na.rm = TRUE),
      mean_ICP = mean(get(element_ICP), na.rm = TRUE),
      median_ICP = median(get(element_ICP), na.rm = TRUE),
      sd_ICP = sd(as.numeric(get(element_ICP)), na.rm = TRUE),
      min_PXRF = min(get(element_PXRF), na.rm = TRUE),
      max_PXRF = max(get(element_PXRF), na.rm = TRUE),
      mean_PXRF = mean(get(element_PXRF), na.rm = TRUE),
      median_PXRF = median(get(element_PXRF), na.rm = TRUE),
      sd_PXRF = sd(as.numeric(get(element_PXRF)), na.rm = TRUE),
      min_diff = min(get(element_diff), na.rm = TRUE),
      max_diff = max(get(element_diff), na.rm = TRUE),
      mean_diff = mean(get(element_diff), na.rm = TRUE),
      median_diff = median(get(element_diff), na.rm = TRUE),
      sd_diff = sd(as.numeric(get(element_diff)), na.rm = TRUE)
    ) %>%
    pivot_longer(cols = everything(), names_to = "Statistic", values_to = "Value") %>%
    separate(Statistic, into = c("Statistic", "Method"), sep = "_") %>%
    mutate(Element = element) %>%
    select(Element, Statistic, Method, Value)
  
  return(summary_df)
}

# Elements to process
elements <- c("Cu", "Se", "Re", "Zn", "Mn", "Fe", "As", "Cd", "Ni", "Ti", "S", "P", "Cr")

# Generate the summary statistics for each element
summary_list <- lapply(elements, function(element) calculate_summary(dt, element))

# Combine all summaries into one dataframe
summary_df <- bind_rows(summary_list)

# Reshape the data so that ICP and PXRF are in separate columns
final_df <- summary_df %>%
  pivot_wider(names_from = Method, values_from = Value)

# Function to conditionally round values, with special handling for certain elements
conditional_round <- function(x, element) {
  if (element %in% c("Se", "Re", "Cr", "As", "Cd", "Ni")) {
    if_else(abs(x) >= 10, round(x, 0), round(x, 2))
  } else {
    if_else(abs(x) >= 30, round(x, 0), round(x, 1))
  }
}

# Apply the conditional rounding to all numeric columns
final_df <- final_df %>%
  rowwise() %>%
  mutate(ICP = conditional_round(ICP, Element),
         PXRF = conditional_round(PXRF, Element)) %>%
  ungroup()

# Print the result
print(final_df)

# Save the result to an Excel file
#write.xlsx(final_df, "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/CESM/1_CESM_Report_2024/Revision/CESM_ICP-PXRF2.xlsx")



## Graphs correlation

dt <- read.delim("Solitude_CESM_Report24.txt")
dt <- dt[dt$ICP != "n", ] 

Cu <- ggplot(data=dt, aes(x = Cu_PXRF, y = Cu_ICP)) +
  geom_point(data=dt, color = "#92D050", size=2.5, stroke=0.7, shape=1) +
  geom_smooth(data=dt, aes(x = Cu_PXRF, y = Cu_ICP), method = "lm", se = FALSE, color = "#92D050", linetype = "solid", size=0.65) +   # Regression line for the first model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  labs(x = "Cu (pXRF)", y = "Cu (ICP-MS)") +
  scale_y_continuous(limits = c(0, 750), breaks = seq(0, 750, by = 150)) +
  scale_x_continuous(limits = c(0, 750), breaks = seq(0, 750, by = 150)) +
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

Cu



Se <- ggplot(data=dt, aes(x = Se_PXRF, y = Se_ICP)) +
  geom_point(data=dt, color = "#92D050", size=2.5, stroke=0.7, shape=1) +
  geom_smooth(data=dt, aes(x = Se_PXRF, y = Se_ICP), method = "lm", se = FALSE, color = "#92D050", linetype = "solid", size=0.65) +   # Regression line for the first model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  labs(x = "Se (pXRF)", y = "Se (ICP-MS)") +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 20)) +
  scale_x_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 20)) +
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

Se




Zn <- ggplot(data=dt, aes(x = Zn_PXRF, y = Zn_ICP)) +
  geom_point(data=dt, color = "#92D050", size=2.5, stroke=0.7, shape=1) +
  geom_smooth(data=dt, aes(x = Zn_PXRF, y = Zn_ICP), method = "lm", se = FALSE, color = "#92D050", linetype = "solid", size=0.65) +   # Regression line for the first model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  labs(x = "Zn (pXRF)", y = "Zn (ICP-MS)") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
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

Zn



Re <- ggplot(data=dt, aes(x = Re_PXRF, y = Re_ICP)) +
  geom_point(data=dt, color = "#92D050", size=2.5, stroke=0.7, shape=1) +
  geom_smooth(data=dt, aes(x = Re_PXRF, y = Re_ICP), method = "lm", se = FALSE, color = "#92D050", linetype = "solid", size=0.65) +   # Regression line for the first model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  labs(x = "Re (pXRF)", y = "Re (ICP-MS)") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25)) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25)) +
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

Re




Fe <- ggplot(data=dt, aes(x = Fe_PXRF, y = Fe_ICP)) +
  geom_point(data=dt, color = "#92D050", size=2.5, stroke=0.7, shape=1) +
  geom_smooth(data=dt, aes(x = Fe_PXRF, y = Fe_ICP), method = "lm", se = FALSE, color = "#92D050", linetype = "solid", size=0.65) +   # Regression line for the first model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  labs(x = "Fe (pXRF)", y = "Fe (ICP-MS)") +
  scale_y_continuous(limits = c(0, 4000), breaks = seq(0, 4000, by = 1000)) +
  scale_x_continuous(limits = c(0, 4000), breaks = seq(0, 4000, by = 1000)) +
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

Fe




Mn <- ggplot(data=dt, aes(x = Mn_PXRF, y = Mn_ICP)) +
  geom_point(data=dt, color = "#92D050", size=2.5, stroke=0.7, shape=1) +
  geom_smooth(data=dt, aes(x = Mn_PXRF, y = Mn_ICP), method = "lm", se = FALSE, color = "#92D050", linetype = "solid", size=0.65) +   # Regression line for the first model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  labs(x = "Mn (pXRF)", y = "Mn (ICP-MS)") +
  scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, by = 50)) +
  scale_x_continuous(limits = c(0, 200), breaks = seq(0, 200, by = 50)) +
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

Mn



P <- ggplot(data=dt, aes(x = P_PXRF, y = P_ICP)) +
  geom_point(data=dt, color = "#ED7D31", size=2.5, stroke=0.7, shape=1) +
  geom_smooth(data=dt, aes(x = P_PXRF, y = P_ICP), method = "lm", se = FALSE, color = "#ED7D31", linetype = "solid", size=0.65) +   # Regression line for the first model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  labs(x = "P (pXRF)", y = "P (ICP-MS)") +
  scale_y_continuous(limits = c(0, 17000), breaks = seq(0, 16000, by = 8000)) +
  scale_x_continuous(limits = c(0, 17000), breaks = seq(0, 16000, by = 8000)) +
  theme_classic() +
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

P



S <- ggplot(data=dt, aes(x = S_PXRF, y = S_ICP)) +
  geom_point(data=dt, color = "#ED7D31", size=2.5, stroke=0.7, shape=1) +
  geom_smooth(data=dt, aes(x = S_PXRF, y = S_ICP), method = "lm", se = FALSE, color = "#ED7D31", linetype = "solid", size=0.65) +   # Regression line for the first model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  labs(x = "S (pXRF)", y = "S (ICP-MS)") +
  scale_y_continuous(limits = c(0, 45000), breaks = seq(0, 45000, by = 20000)) +
  scale_x_continuous(limits = c(0, 45000), breaks = seq(0, 45000, by = 20000)) +
  theme_classic() + 
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

S



Cr <- ggplot(data=dt, aes(x = Cr_PXRF, y = Cr_ICP)) +
  geom_point(data=dt, color = "#ED7D31", size=2.5, stroke=0.7, shape=1) +
  geom_smooth(data=dt, aes(x = Cr_PXRF, y = Cr_ICP), method = "lm", se = FALSE, color = "#ED7D31", linetype = "solid", size=0.65) +   # Regression line for the first model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  labs(x = "Cr (pXRF)", y = "Cr (ICP-MS)") +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 10)) +
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 10)) +
  theme_classic() +
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

Cr



Ti <- ggplot(data=dt, aes(x = Ti_PXRF, y = Ti_ICP)) +
  geom_point(data=dt, color = "#ED7D31", size=2.5, stroke=0.7, shape=1) +
  geom_smooth(data=dt, aes(x = Ti_PXRF, y = Ti_ICP), method = "lm", se = FALSE, color = "#ED7D31", linetype = "solid", size=0.65) +   # Regression line for the first model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  labs(x = "Ti (pXRF)", y = "Ti (ICP-MS)") +
 scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, by = 50)) +
  scale_x_continuous(limits = c(0, 200), breaks = seq(0, 200, by = 50)) +
  theme_classic() + 
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
Ti


As <- ggplot(data=dt, aes(x = As_PXRF, y = As_ICP)) +
  geom_point(data=dt, color = "#ED7D31", size=2.5, stroke=0.7, shape=1) +
  geom_smooth(data=dt, aes(x = As_PXRF, y = As_ICP), method = "lm", se = FALSE, color = "#ED7D31", linetype = "solid", size=0.65) +   # Regression line for the first model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  labs(x = "As (pXRF)", y = "As (ICP-MS)") +
  scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 4)) +
  scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 4)) +
  theme_classic() +
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

As




Cd <- ggplot(data=dt, aes(x = Cd_PXRF, y = Cd_ICP)) +
  geom_point(data=dt, color = "#ED7D31", size=2.5, stroke=0.7, shape=1) +
  geom_smooth(data=dt, aes(x = Cd_PXRF, y = Cd_ICP), method = "lm", se = FALSE, color = "#ED7D31", linetype = "solid", size=0.65) +   # Regression line for the first model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  labs(x = "Cd (pXRF)", y = "Cd (ICP-MS)") +
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) +
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) +
  theme_classic() +
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

Cd


Ni <- ggplot(data=dt, aes(x = Ni_PXRF, y = Ni_ICP)) +
  geom_point(data=dt, color = "#ED7D31", size=2.5, stroke=0.7, shape=1) +
  geom_smooth(data=dt, aes(x = Ni_PXRF, y = Ni_ICP), method = "lm", se = FALSE, color = "#ED7D31", linetype = "solid", size=0.65) +   # Regression line for the first model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  labs(x = "Ni (pXRF)", y = "Ni (ICP-MS)") +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 2)) +
scale_x_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 2)) +
  theme_classic() + 
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

Ni




fig1 <- ggarrange(Cu,Se,Re,Zn,Mn,Fe,
          ncol = 3, nrow = 2, 
          common.legend = FALSE, legend = "bottom")


fig2 <- ggarrange(As, Cd, Ti, S, P, Cr,
          ncol = 3, nrow = 2, 
          common.legend = FALSE, legend = "bottom")

ggsave(filename = "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/CESM/1_CESM_Report_2024/Revision/Corr_good_v3.pdf", 
       plot = fig1, 
       width = 12, 
       height = 6.5, 
       units = "in", 
       dpi = 300)


ggsave(filename = "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/CESM/1_CESM_Report_2024/Revision/Corr_bad_v3.pdf", 
       plot = fig2, 
       width = 12, 
       height = 6.5, 
       units = "in", 
       dpi = 300)



corr.test(dt$Cu_PXRF, dt$Cu_ICP, method = "spearman") # 0.93
corr.test(dt$Se_PXRF, dt$Se_ICP, method = "spearman") # 0.9
corr.test(dt$Re_PXRF, dt$Re_ICP, method = "spearman") # 0.94
corr.test(dt$Zn_PXRF, dt$Zn_ICP, method = "spearman") # 0.64
corr.test(dt$Mn_PXRF, dt$Mn_ICP, method = "spearman") # 0.82
corr.test(dt$Fe_PXRF, dt$Fe_ICP, method = "spearman") # 0.98
corr.test(dt$As_PXRF, dt$As_ICP, method = "spearman") # 0.3
corr.test(dt$Cd_PXRF, dt$Cd_ICP, method = "spearman") # 0.25
corr.test(dt$Cr_PXRF, dt$Cr_ICP, method = "spearman") # 0.22
corr.test(dt$Ni_PXRF, dt$Ni_ICP, method = "spearman") # NA
corr.test(dt$P_PXRF, dt$P_ICP, method = "spearman") # 0.11
corr.test(dt$S_PXRF, dt$S_ICP, method = "spearman") # 0.97
corr.test(dt$Ti_PXRF, dt$Ti_ICP, method = "spearman") # 0.64





##### SOil table


setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/Tables")
dt <-read.delim("SLT_Soil_Brookside.txt")

dt$Zn_ext <- as.numeric(dt$Zn_ext)

library(dplyr)
library(openxlsx)

# Function to calculate summary statistics for specified parameters
calculate_soil_parameters_summary <- function(data) {
  summary_df <- data %>%
    group_by(Plot, Layer) %>%
    summarize(
      mean_TEC = mean(TEC, na.rm = TRUE),
      sd_TEC = sd(TEC, na.rm = TRUE),
      mean_pH = mean(pH, na.rm = TRUE),
      sd_pH = sd(pH, na.rm = TRUE),
      mean_OM = mean(OM, na.rm = TRUE),
      sd_OM = sd(OM, na.rm = TRUE),
      mean_ENR = mean(ENR, na.rm = TRUE),
      sd_ENR = sd(ENR, na.rm = TRUE),
      mean_S_ext = mean(S_ext, na.rm = TRUE),
      sd_S_ext = sd(S_ext, na.rm = TRUE),
      mean_P_ext = mean(P_ext, na.rm = TRUE),
      sd_P_ext = sd(P_ext, na.rm = TRUE),
      mean_Ca_ext = mean(Ca_ext, na.rm = TRUE),
      sd_Ca_ext = sd(Ca_ext, na.rm = TRUE),
      mean_K_ext = mean(K_ext, na.rm = TRUE),
      sd_K_ext = sd(K_ext, na.rm = TRUE),
      mean_Fe_ext = mean(Fe_ext, na.rm = TRUE),
      sd_Fe_ext = sd(Fe_ext, na.rm = TRUE),
      mean_Mn_ext = mean(Mn_ext, na.rm = TRUE),
      sd_Mn_ext = sd(Mn_ext, na.rm = TRUE),
      mean_Cu_ext = mean(Cu_ext, na.rm = TRUE),
      sd_Cu_ext = sd(Cu_ext, na.rm = TRUE),
      mean_Zn_ext = mean(Zn_ext, na.rm = TRUE),
      sd_Zn_ext = sd(Zn_ext, na.rm = TRUE),
      mean_Mg_ext = mean(Mg_ext, na.rm = TRUE),
      sd_Mg_ext = sd(Mg_ext, na.rm = TRUE),
      mean_Soluble_Salts = mean(Soluble_Salts, na.rm = TRUE),
      sd_Soluble_Salts = sd(Soluble_Salts, na.rm = TRUE),
      mean_NO3_N = mean(as.numeric(NO3.N), na.rm = TRUE),
      sd_NO3_N = sd(as.numeric(NO3.N), na.rm = TRUE),
      mean_NH4_N = mean(NH4.N, na.rm = TRUE),
      sd_NH4_N = sd(NH4.N, na.rm = TRUE),
      mean_Cu = mean(Cu, na.rm = TRUE),
      sd_Cu = sd(Cu, na.rm = TRUE),
      mean_Se = mean(Se, na.rm = TRUE),
      sd_Se = sd(Se, na.rm = TRUE),
      mean_Re = mean(Re, na.rm = TRUE),
      sd_Re = sd(Re, na.rm = TRUE),
      mean_Zn = mean(Zn, na.rm = TRUE),
      sd_Zn = sd(Zn, na.rm = TRUE),
      mean_Fe = mean(Fe, na.rm = TRUE),
      sd_Fe = sd(Fe, na.rm = TRUE),
      mean_Na_ext = mean(Na_ext, na.rm = TRUE),
      sd_Na_ext = sd(Na_ext, na.rm = TRUE),
      mean_Al = mean(Al_ext, na.rm = TRUE),
      sd_Al = sd(Al_ext, na.rm = TRUE),
      mean_Mn = mean(Mn, na.rm = TRUE),
      sd_Mn= sd(Mn, na.rm = TRUE)
    ) %>%
    ungroup() 

  
  
  return(summary_df)
}

# Generate the summary statistics for soil parameters
soil_parameters_summary_df <- calculate_soil_parameters_summary(dt)

soil_parameters_summary_df2 <- soil_parameters_summary_df %>%
  rowwise() %>%
  mutate(across(where(is.numeric), ~ if_else(abs(.) >= 10, 
                                             round(., 0), 
                                             if_else(cur_column() == "mean_Re" | cur_column() == "sd_Re", 
                                                     round(., 4), 
                                                     round(., 2))))) %>%
  ungroup()




# Save the result to an Excel file
write.xlsx(soil_parameters_summary_df2, "Table-Soil_with_Mn.xlsx")


