# Solitude Brookside Soil analysis
# Tomasz Wlodarczyk
# twlodarczyk@arizona.edu
# 2024 - 19 - 01

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
}

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/")
dt <-read.delim("SLT_Soil_Brookside.txt")
dt_S <- subset(dt, Layer== "S")
dt_T <- subset(dt, Layer=="T")
dt_S_P2P6 <- subset(dt_S, Plot=="P2" | Plot=="P6")
summary(dt)

#If we don't need to show Control!
dt <- dt %>%
  filter(Layer != "TS")




dt_soil_layer_TEC <- dt %>%
  group_by(Plot, Layer) %>%
  summarize(Mean = mean(TEC, na.rm = TRUE), SD = sd(TEC, na.rm = TRUE)/sqrt(n()), .groups = 'drop') %>%
  ungroup()

  TEC <- ggplot(dt_soil_layer_TEC, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) + # Adjust the width here
    facet_grid(Plot~., scales = "free_y") +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                  position = position_dodge(width = 0.9), # Ensure this width matches the one in geom_bar
                  width = 0.2) +
    coord_flip() +
    labs(x = "", y = "Total Exchange Capacity (meq/100g)") +
    scale_fill_manual(values = c("#003f5c", "#AD0B0B", "#79AF97FF")) +
    theme_bw()
  
  
  
  dt_soil_layer_ph <- dt %>%
    group_by(Plot, Layer) %>%
    summarize(Mean = mean(pH, na.rm = TRUE), SD = sd(pH, na.rm = TRUE)/sqrt(n())) %>%
    ungroup()
  
  pH <- ggplot(dt_soil_layer_ph, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) + # Adjust the width here
    facet_grid(Plot~., scales = "free_y") +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                  position = position_dodge(width = 0.9), # Ensure this width matches the one in geom_bar
                  width = 0.2) +
    coord_flip() +
    labs(x = "", y = "pH") +
    scale_fill_manual(values = c("#003f5c", "#AD0B0B", "#79AF97FF")) +
    theme_bw()

  

  dt_soil_layer_OM <- dt %>%
    group_by(Plot, Layer) %>%
    summarize(Mean = mean(OM, na.rm = TRUE), SD = sd(OM, na.rm = TRUE)/sqrt(n()), .groups = 'drop') %>%
    ungroup()
  
 OM <- ggplot(dt_soil_layer_OM, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) + # Adjust the width here
    facet_grid(Plot~., scales = "free_y") +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                  position = position_dodge(width = 0.9), # Ensure this width matches the one in geom_bar
                  width = 0.2) +
    coord_flip() +
    labs(x = "", y = "Organic Matter (%)") +
    scale_fill_manual(values = c("#003f5c", "#AD0B0B", "#79AF97FF")) +
    theme_bw()


 
 dt_soil_layer_ENR <- dt %>%
   group_by(Plot, Layer) %>%
   summarize(Mean = mean(ENR, na.rm = TRUE), SD = sd(ENR, na.rm = TRUE)/sqrt(n()), .groups = 'drop') %>%
   ungroup()
 
ENR <- ggplot(dt_soil_layer_ENR, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) +
   geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) + # Adjust the width here
   facet_grid(Plot~., scales = "free_y") +
   geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                 position = position_dodge(width = 0.9), # Ensure this width matches the one in geom_bar
                 width = 0.2) +
   coord_flip() +
   labs(x = "", y = "Estimated Nitrogen Release (# N/acre)") +
   scale_fill_manual(values = c("#003f5c", "#AD0B0B", "#79AF97FF")) +
   theme_bw()




dt_soil_layer_S_ext <- dt %>%
  group_by(Plot, Layer) %>%
  summarize(Mean = mean(S_ext, na.rm = TRUE), SD = sd(S_ext, na.rm = TRUE)/sqrt(n()), .groups = 'drop') %>%
  ungroup()

S_ext <- ggplot(dt_soil_layer_S_ext, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) + # Adjust the width here
  facet_grid(Plot~., scales = "free_y") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.9), # Ensure this width matches the one in geom_bar
                width = 0.2) +
  coord_flip() +
  labs(x = "", y = "Extractable S (mg/kg)") +
  scale_fill_manual(values = c("#003f5c", "#AD0B0B", "#79AF97FF")) +
  theme_bw()



dt_soil_layer_P_ext <- dt %>%
  group_by(Plot, Layer) %>%
  summarize(Mean = mean(P_ext, na.rm = TRUE), SD = sd(P_ext, na.rm = TRUE)/sqrt(n()), .groups = 'drop') %>%
  ungroup()

P_ext <- ggplot(dt_soil_layer_P_ext, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) + # Adjust the width here
  facet_grid(Plot~., scales = "free_y") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.9), # Ensure this width matches the one in geom_bar
                width = 0.2) +
  coord_flip() +
  labs(x = "", y = "Extractable P (mg/kg)") +
  scale_fill_manual(values = c("#003f5c", "#AD0B0B", "#79AF97FF")) +
  theme_bw()





dt_soil_layer_Ca_ext <- dt %>%
  group_by(Plot, Layer) %>%
  summarize(Mean = mean(Ca_ext, na.rm = TRUE), SD = sd(Ca_ext, na.rm = TRUE)/sqrt(n()), .groups = 'drop') %>%
  ungroup()

Ca_ext <- ggplot(dt_soil_layer_Ca_ext, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) + # Adjust the width here
  facet_grid(Plot~., scales = "free_y") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.9), # Ensure this width matches the one in geom_bar
                width = 0.2) +
  coord_flip() +
  labs(x = "", y = "Extractable Ca (mg/kg)") +
  scale_fill_manual(values = c("#003f5c", "#AD0B0B", "#79AF97FF")) +
  theme_bw()













dt_soil_layer_K_ext <- dt %>%
  group_by(Plot, Layer) %>%
  summarize(Mean = mean(B_ext, na.rm = TRUE), SD = sd(B_ext, na.rm = TRUE)/sqrt(n()), .groups = 'drop') %>%
  ungroup()

K_ext <- ggplot(dt_soil_layer_K_ext, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) + # Adjust the width here
  facet_grid(Plot~., scales = "free_y") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.9), # Ensure this width matches the one in geom_bar
                width = 0.2) +
  coord_flip() +
  labs(x = "", y = "Extractable K") +
  scale_fill_manual(values = c("#003f5c", "#AD0B0B", "#79AF97FF")) +
  theme_bw()





dt_soil_layer_Fe_ext <- dt %>%
  group_by(Plot, Layer) %>%
  summarize(Mean = mean(Fe_ext, na.rm = TRUE), SD = sd(Fe_ext, na.rm = TRUE)/sqrt(n()), .groups = 'drop') %>%
  ungroup()

Fe_ext <- ggplot(dt_soil_layer_Fe_ext, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) + # Adjust the width here
  facet_grid(Plot~., scales = "free_y") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.9), # Ensure this width matches the one in geom_bar
                width = 0.2) +
  coord_flip() +
  labs(x = "", y = "Extractable Fe") +
  scale_fill_manual(values = c("#003f5c", "#AD0B0B", "#79AF97FF")) +
  theme_bw()







dt_soil_layer_Mn_ext <- dt %>%
  group_by(Plot, Layer) %>%
  summarize(Mean = mean(Mn_ext, na.rm = TRUE), SD = sd(Mn_ext, na.rm = TRUE)/sqrt(n()), .groups = 'drop') %>%
  ungroup()

Mn_ext <- ggplot(dt_soil_layer_Mn_ext, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) + # Adjust the width here
  facet_grid(Plot~., scales = "free_y") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.9), # Ensure this width matches the one in geom_bar
                width = 0.2) +
  coord_flip() +
  labs(x = "", y = "Extractable Mn") +
  scale_fill_manual(values = c("#003f5c", "#AD0B0B", "#79AF97FF")) +
  theme_bw()



dt_soil_layer_Cu_ext <- dt %>%
  group_by(Plot, Layer) %>%
  summarize(Mean = mean(Cu_ext, na.rm = TRUE), SD = sd(Cu_ext, na.rm = TRUE)/sqrt(n()), .groups = 'drop') %>%
  ungroup()

Cu_ext <- ggplot(dt_soil_layer_Cu_ext, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) + # Adjust the width here
  facet_grid(Plot~., scales = "free_y") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.9), # Ensure this width matches the one in geom_bar
                width = 0.2) +
  coord_flip() +
  labs(x = "", y = "Extractable Cu") +
  scale_fill_manual(values = c("#003f5c", "#AD0B0B", "#79AF97FF")) +
  theme_bw()


str(dt)

dt[,28] <- sapply(dt[,28],as.numeric)

dt_soil_layer_Zn_ext <- dt %>%
  group_by(Plot, Layer) %>%
  summarize(Mean = mean(Zn_ext, na.rm = TRUE), SD = sd(Zn_ext, na.rm = TRUE)/sqrt(n()), .groups = 'drop') %>%
  ungroup()

Zn_ext <- ggplot(dt_soil_layer_Zn_ext, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) + # Adjust the width here
  facet_grid(Plot~., scales = "free_y") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.9), # Ensure this width matches the one in geom_bar
                width = 0.2) +
  coord_flip() +
  labs(x = "", y = "Extractable Zn") +
  scale_fill_manual(values = c("#003f5c", "#AD0B0B", "#79AF97FF")) +
  theme_bw()



dt_soil_layer_Mg_ext <- dt %>%
  group_by(Plot, Layer) %>%
  summarize(Mean = mean(Mg_ext, na.rm = TRUE), SD = sd(Mg_ext, na.rm = TRUE)/sqrt(n()), .groups = 'drop') %>%
  ungroup()

Mg_ext <- ggplot(dt_soil_layer_Mg_ext, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) + # Adjust the width here
  facet_grid(Plot~., scales = "free_y") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.9), # Ensure this width matches the one in geom_bar
                width = 0.2) +
  coord_flip() +
  labs(x = "", y = "Extractable Mg") +
  scale_fill_manual(values = c("#003f5c", "#AD0B0B", "#79AF97FF")) +
  theme_bw()





dt_soil_layer_Soluble_Salts <- dt %>%
  group_by(Plot, Layer) %>%
  summarize(Mean = mean(Soluble_Salts, na.rm = TRUE), SD = sd(Soluble_Salts, na.rm = TRUE)/sqrt(n()), .groups = 'drop') %>%
  ungroup()

Soluble_Salts <- ggplot(dt_soil_layer_Soluble_Salts, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) + # Adjust the width here
  facet_grid(Plot~., scales = "free_y") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.9), # Ensure this width matches the one in geom_bar
                width = 0.2) +
  coord_flip() +
  labs(x = "", y = "Soluble Salts (mmhos/cm)") +
  scale_fill_manual(values = c("#003f5c", "#AD0B0B", "#79AF97FF")) +
  theme_bw()




dt[,31] <- sapply(dt[,31],as.numeric)


dt_soil_layer_NO3.N <- dt %>%
  group_by(Plot, Layer) %>%
  summarize(Mean = mean(NO3.N, na.rm = TRUE), SD = sd(NO3.N, na.rm = TRUE)/sqrt(n()), .groups = 'drop') %>%
  ungroup()

NO3.N <- ggplot(dt_soil_layer_NO3.N, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) + # Adjust the width here
  facet_grid(Plot~., scales = "free_y") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.9), # Ensure this width matches the one in geom_bar
                width = 0.2) +
  coord_flip() +
  labs(x = "", y = "NO3-N (mg/kg)") +
  scale_fill_manual(values = c("#003f5c", "#AD0B0B", "#79AF97FF")) +
  theme_bw()


dt_soil_layer_NH4.N <- dt %>%
  group_by(Plot, Layer) %>%
  summarize(Mean = mean(NH4.N, na.rm = TRUE), SD = sd(NH4.N, na.rm = TRUE)/sqrt(n()), .groups = 'drop') %>%
  ungroup()

NH4.N <- ggplot(dt_soil_layer_NH4.N, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) + # Adjust the width here
  facet_grid(Plot~., scales = "free_y") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.9), # Ensure this width matches the one in geom_bar
                width = 0.2) +
  coord_flip() +
  labs(x = "", y = "NH4-N (mg/kg)") +
  scale_fill_manual(values = c("#003f5c", "#AD0B0B", "#79AF97FF")) +
  theme_bw()


dt_soil_layer_Cu <- dt %>%
  group_by(Plot, Layer) %>%
  summarize(Mean = mean(Cu), SD = sd(Cu)/sqrt(n())) %>%
  ungroup()

Cu <- ggplot(dt_soil_layer_Cu, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) + # Adjust the width here
  facet_grid(Plot~., scales = "free_y") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.9), # Ensure this width matches the one in geom_bar
                width = 0.2) +
  coord_flip() +
  labs(x = "", y = "Cu (mg/kg)") +
  scale_fill_manual(values = c("#003f5c", "#AD0B0B", "#79AF97FF")) +
  theme_bw()

dt_soil_layer_Re <- dt %>%
  group_by(Plot, Layer) %>%
  summarize(Mean = mean(Re), SD = sd(Re)/sqrt(n())) %>%
  ungroup()

Re <- ggplot(dt_soil_layer_Re, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) + # Adjust the width here
  facet_grid(Plot~., scales = "free_y") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.9), # Ensure this width matches the one in geom_bar
                width = 0.2) +
  coord_flip() +
  labs(x = "", y = "Re (mg/kg)") +
  scale_fill_manual(values = c("#003f5c", "#AD0B0B", "#79AF97FF")) +
  theme_bw()


dt_soil_layer_Se <- dt %>%
  group_by(Plot, Layer) %>%
  summarize(Mean = mean(Se), SD = sd(Se)/sqrt(n())) %>%
  ungroup()

Se <- ggplot(dt_soil_layer_Se, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) + # Adjust the width here
  facet_grid(Plot~., scales = "free_y") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.9), # Ensure this width matches the one in geom_bar
                width = 0.2) +
  coord_flip() +
  labs(x = "", y = "Se (mg/kg)") +
  scale_fill_manual(values = c("#003f5c", "#AD0B0B", "#79AF97FF")) +
  theme_bw()

dt_soil_layer_Fe <- dt %>%
  group_by(Plot, Layer) %>%
  summarize(Mean = mean(Fe), SD = sd(Fe)/sqrt(n())) %>%
  ungroup()

Fe <- ggplot(dt_soil_layer_Fe, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) + # Adjust the width here
  facet_grid(Plot~., scales = "free_y") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.9), # Ensure this width matches the one in geom_bar
                width = 0.2) +
  coord_flip() +
  labs(x = "", y = "Fe (mg/kg)") +
  scale_fill_manual(values = c("#003f5c", "#AD0B0B", "#79AF97FF")) +
  theme_bw()


dt_soil_layer_Zn <- dt %>%
  group_by(Plot, Layer) %>%
  summarize(Mean = mean(Zn), SD = sd(Zn)/sqrt(n())) %>%
  ungroup()

Zn <- ggplot(dt_soil_layer_Zn, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) + # Adjust the width here
  facet_grid(Plot~., scales = "free_y") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.9), # Ensure this width matches the one in geom_bar
                width = 0.2) +
  coord_flip() +
  labs(x = "", y = "Zn (mg/kg)") +
  scale_fill_manual(values = c("#003f5c", "#AD0B0B", "#79AF97FF")) +
  theme_bw()


dt_soil_layer_Mn <- dt %>%
  group_by(Plot, Layer) %>%
  summarize(Mean = mean(Mn), SD = sd(Mn)/sqrt(n())) %>%
  ungroup()

Mn <- ggplot(dt_soil_layer_Zn, aes(x = forcats::fct_rev(Layer), y = Mean, fill = Layer)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) + # Adjust the width here
  facet_grid(Plot~., scales = "free_y") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.9), # Ensure this width matches the one in geom_bar
                width = 0.2) +
  coord_flip() +
  labs(x = "", y = "Mn (mg/kg)") +
  scale_fill_manual(values = c("#003f5c", "#AD0B0B", "#79AF97FF")) +
  theme_bw()



Zn <- ggplot(dt, aes(x = forcats::fct_rev(Layer), y = Zn, fill = Layer)) +
  geom_boxplot() + # Adjust the width here
  facet_grid(Plot~., scales = "free_y") +
  coord_flip() +
  labs(x = "", y = "Zn (mg/kg)") +
  scale_fill_manual(values = c("#003f5c", "#AD0B0B", "#79AF97FF")) +
  theme_bw()


# Assuming you already have the six plots as ggplot objects named Cu_soil, Mn_soil, Pb_soil, Ti_soil, Cr_soil, Fe_soil

ggarrange(TEC, pH, OM, ENR, S_ext, P_ext, Ca_ext, K_ext, Fe_ext, Mn_ext, Cu_ext, Zn_ext, Mg_ext, Soluble_Salts, NO3.N, NH4.N, Cu, Se, Re, Zn, Fe,
          ncol = 7, nrow = 3, 
          common.legend = TRUE, legend = "bottom")





# Pryklad na boxplot zamiast barplotu

Zn <- ggplot(dt, aes(x = forcats::fct_rev(Layer), y = Zn, fill = Layer)) +
  geom_boxplot() + # Adjust the width here
  facet_grid(Plot~., scales = "free_y") +
  coord_flip() +
  labs(x = "", y = "Zn (mg/kg)") +
  scale_fill_manual(values = c("#003f5c", "#AD0B0B", "#79AF97FF")) +
  theme_bw()
###



library(tidyverse)

# Step 1: Reshape the data
dt_long <- dt %>%
  pivot_longer(cols = c(TEC, pH, OM, ENR, P_ext, Ca_ext, K_ext, Fe_ext, Mn_ext, Cu_ext, Zn_ext, Mg_ext, Soluble_Salts, `NO3.N`, `NH4.N`, Cu, Se, Re, Zn, Fe, Mn),
               names_to = "Variable", values_to = "Value")

# Step 2: Define the plotting function
plot_variable <- function(data, variable_name) {
  variable_label <- str_replace_all(variable_name, "_", " ") # Replace underscores with spaces for better readability in labels
  
  data %>%
    filter(Variable == variable_name) %>%
    ggplot(aes(x = forcats::fct_rev(Layer), y = Value, fill = Layer)) +
    geom_boxplot() +
    facet_grid(Plot~., scales = "free_y") +
    coord_flip() +
    labs(x = "", y = paste(variable_label, "(unit)")) + # Replace "(unit)" with the actual unit if known
    scale_fill_manual(values = c("#D9D9B1", "#ddd1ea")) +
    theme_bw()
}

# Step 3: Iterate over variables and create plots (Example shown for a subset)
#variable_names <- c(
#  "TEC", "pH", "OM", "ENR", "S_ext", "P_ext", "Ca_ext", "K_ext", 
#  "Fe_ext", "Mn_ext", "Cu_ext", "Zn_ext", "Mg_ext", "Soluble_Salts", 
#  "NO3.N", "NH4.N", "Cu", "Se", "Re", "Zn", "Fe"
#) # Extend this list to include all your variables

variable_names <- c(
  "TEC", "pH", "Ca_ext", "Cu_ext",  
   "Cu", "OM" 
) # Extend this list to include all your variables

plots <- lapply(variable_names, function(var) plot_variable(dt_long, var))

# Optional: Print or save plots
# print(plots[[1]]) # Example to print the first plot
# ggsave("plot1.pdf", plot = plots[[1]]) # Example to save the first plot


 library(ggpubr)
 
 # Assuming 'plots' is your list of plot objects
 # You might need to adjust the number of rows and columns depending on the total number of plots
 n_cols = 3
 n_rows = 3
 total_plots = length(plots)
 
 # Use do.call with ggarrange
 do.call(ggarrange, c(plots, list(ncol = n_cols, nrow = n_rows, common.legend = TRUE, legend = "bottom")))
 
########
 #Stat for soil for TEC, pH, Ca ext, Cu, ext, Cu total
 
 library(agricolae)
print(kruskal(dt_S$TEC, dt_S$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt_S$pH, dt_S$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt_S$Ca_ext, dt_S$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt_S$Cu_ext, dt_S$Plot, group=TRUE,p.adj="bonferroni"))
print(kruskal(dt_S$Cu, dt_S$Plot, group=TRUE,p.adj="bonferroni"))


wilcox_result <- pairwise.wilcox.test(dt_S_P2P6$pH, dt_S_P2P6$Plot, p.adjust.method = "BH")
print(wilcox_result) # nie sa rozne


#

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 

############################
## Some old stuff below?
dt_soil <- dt %>%
  filter(Plot %in% c("P1", "P2", "P5", "P6", "C1", "C2")) %>%
  pivot_longer(cols = 46:75, names_to = "Concentration", values_to = "Value") %>%
  group_by(Concentration, Plot) %>%
  summarise(min = min(Value), 
            max = max(Value), 
            mean = mean(Value), 
            median = median(Value), 
            se = sd(Value) / sqrt(n()),  # Calculate standard error
            .groups = "drop") %>%
  pivot_wider(names_from = Plot, values_from = c(min, max, mean, median, se), names_glue = "{Plot}_{.value}") %>%
  select(Concentration, contains("_")) %>%
  pivot_longer(cols = -Concentration, names_to = "Plot_Stat", values_to = "Value") %>%
  separate(Plot_Stat, into = c("Plot", "Stat"), sep = "_") %>%
  pivot_wider(names_from = Plot, values_from = Value) %>%
  arrange(Concentration)


dt_soil_summary3 <- dt_soil_summary_Re %>%
  mutate(across(where(is.numeric), 
                ~ ifelse(. > 10, round(., 0), round(., 1))))

#write.csv(dt_soil_summary3, "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Soil_Table_Re.csv", row.names=FALSE)

dt_soil_summary1 <- dt1 %>%
  filter(Site %in% c("TAILINGS", "CONTROL")) %>%
  pivot_longer(cols = 42:69, names_to = "Concentration", values_to = "Value") %>%
  group_by(Concentration, Site) %>%
  summarise(min = min(Value), 
            max = max(Value), 
            mean = mean(Value), 
            median = median(Value), 
            se = sd(Value) / sqrt(n()),  # Calculate standard error
            .groups = "drop") %>%
  pivot_wider(names_from = Site, values_from = c(min, max, mean, median, se), names_glue = "{Site}_{.value}") %>%
  select(Concentration, contains("_")) %>%
  pivot_longer(cols = -Concentration, names_to = "Site_Stat", values_to = "Value") %>%
  separate(Site_Stat, into = c("Site", "Stat"), sep = "_") %>%
  pivot_wider(names_from = Site, values_from = Value) %>%
  arrange(Concentration)


dt_soil_summary4 <- dt_soil_summary1 %>%
  mutate(across(where(is.numeric), 
                ~ ifelse(. > 10, round(., 0), round(., 1))))

#write.csv(dt_soil_summary1, "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Soil_Table_Average.csv", row.names=FALSE)

