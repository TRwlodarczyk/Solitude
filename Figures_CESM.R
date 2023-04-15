# Solutude Figures - CESM
# Tomasz Wlodarczyk
# twlodarczyk@arizona.edu
# 2023-04-15


setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/CESM/Boxplots")
dt <-read_csv("dt_plants.csv")


Cu_AllPlots<- ggplot(dt_plants, aes(x = reorder(Group, Cu_concentration, FUN = median),
                                    y = Cu_concentration, group=Scientific_Name)) +
  geom_boxplot()+
  geom_point(aes(colour = Plot, Shape=)) +
  geom_hline(yintercept = 40, linetype = "dashed", color = "#9a9a9a", size=1.2)+
  #geom_hline(yintercept = 250, linetype = "longdash", color = "#707070", size=1.2)+
  geom_hline(yintercept = 300, linetype = "dotdash", color = "#454545", size=1.2)+
  coord_flip()+
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_x_discrete(guide = guide_axis(angle = 0))+
  scale_y_continuous(limits = c(0, 600), breaks = seq(0, 800, by = 50)) +
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
        axis.title.x = element_text(size = 19),
        axis.text.y = element_text(size=14, face="italic"),
        axis.title.y = element_blank(),
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 12.5)) +
  guides(colour = guide_legend(override.aes = list(size = 3.5)))+
  ylab("Copper Concentration (ppm)")
Cu_AllPlots


