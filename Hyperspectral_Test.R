#Tomasz Wlodarczyk

library(tidyverse)


setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research")
dt <-read.delim("mining3-17-23.txt")


plot(dt$mining00000.asd~dt$Wavelength)


ggplot(data=dt, aes(x=Wavelength, y=mining00000.asd))+
  geom_point()
