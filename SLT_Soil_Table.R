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

summary(dt)





dt_soil_layer_TEC <- dt %>%
  group_by(Plot, Layer) %>%
  summarize(Mean = mean(TEC), SD = sd(TEC)/sqrt(n())) %>%
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
    summarize(Mean = mean(pH), SD = sd(pH)/sqrt(n())) %>%
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
    summarize(Mean = mean(OM), SD = sd(OM)/sqrt(n())) %>%
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
   summarize(Mean = mean(ENR), SD = sd(ENR)/sqrt(n())) %>%
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
  summarize(Mean = mean(S_ext), SD = sd(S_ext)/sqrt(n())) %>%
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
  summarize(Mean = mean(P_ext), SD = sd(P_ext)/sqrt(n())) %>%
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
  summarize(Mean = mean(Ca_ext), SD = sd(Ca_ext)/sqrt(n())) %>%
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
  summarize(Mean = mean(B_ext), SD = sd(B_ext)/sqrt(n())) %>%
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
  summarize(Mean = mean(Fe_ext), SD = sd(Fe_ext)/sqrt(n())) %>%
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
  summarize(Mean = mean(Mn_ext), SD = sd(Mn_ext)/sqrt(n())) %>%
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
  summarize(Mean = mean(Cu_ext), SD = sd(Cu_ext)/sqrt(n())) %>%
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
  summarize(Mean = mean(Zn_ext), SD = sd(Zn_ext)/sqrt(n())) %>%
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
  summarize(Mean = mean(Mg_ext), SD = sd(Mg_ext)/sqrt(n())) %>%
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
  summarize(Mean = mean(Soluble_Salts), SD = sd(Soluble_Salts)/sqrt(n())) %>%
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
  summarize(Mean = mean(NO3.N), SD = sd(NO3.N)/sqrt(n())) %>%
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
  summarize(Mean = mean(NH4.N), SD = sd(NH4.N)/sqrt(n())) %>%
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




# Assuming you already have the six plots as ggplot objects named Cu_soil, Mn_soil, Pb_soil, Ti_soil, Cr_soil, Fe_soil

ggarrange(TEC, pH, OM, ENR, S_ext, P_ext, Ca_ext, K_ext, Fe_ext, Mn_ext, Cu_ext, Zn_ext, Mg_ext, Soluble_Salts, NO3.N, NH4.N,
          ncol = 4, nrow = 4, 
          common.legend = TRUE, legend = "bottom")





############################

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

write.csv(dt_soil_summary3, "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Soil_Table_Re.csv", row.names=FALSE)

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

write.csv(dt_soil_summary1, "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Soil_Table_Average.csv", row.names=FALSE)

