#Tomasz Wlodarczyk

library(tidyverse)


setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research")
dt <-read.delim("mining3-17-23.txt")


plot(dt$mining00000.asd~dt$Wavelength)


ggplot(data=dt, aes(x=Wavelength, y=mining00000.asd))+
  geom_line()


ggplot(data=dt, aes(x=Wavelength)) +
  geom_line(aes(y=mining00001.asd), color="blue") +
  geom_line(aes(y=mining00002.asd), color="orange") +  
  geom_line(aes(y=mining00005.asd), color="green") +
  geom_line(aes(y=mining00004.asd), color="purple")+
  geom_line(aes(y=mining00007.asd), color="red") +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
        axis.title.x = element_text(size = 19),
        axis.title.y = element_text(size = 19),
        axis.text.y = element_text(size=14),
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 13.5), 
        legend.title = element_text(size=15, face = "bold"))+
  guides(color = guide_legend(override.aes = list(size = 3.5)),
         shape = guide_legend(override.aes = list(size = 3.5))) +
  ylab("Reflectance")+
  xlab("Wavelength")
