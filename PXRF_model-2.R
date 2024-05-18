#PXRF MODELS



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
  library(stringr) # For str_replace_all
  library(psych)
  library(car)
  library(openxlsx)
  library(writexl)
  
}

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/Models_pxrf")
dt <-read.delim("PXRF_models.txt")




# Cu filtrs

{
dt_Cu_best <- dt %>%
  filter(
    # Include criteria indicated with '-'
    (
      TubeTW == "oneTW.SMALL" |
        Qpercent_mass.TW == "80to100TW.SMALL" |
        Tube_No == "one" |
        TW_Q == "TW.SMALL" |
        TubeTW == "oneTW.MEDIUM" |
        Scientific_Name == "Xanthisma gracile" |
        Form == "Shrub" |
        ScieNamePlot == "P5Xanthisma gracile" |
        ScieNamePlot == "P5Gutierrezia sarothrae" |
        TubeTW == "oneTW.LARGE" |
        ScieNamePlot == "P2Xanthisma gracile" |
        Scientific_Name == "Ceanothus greggii" |
        ScieNamePlot == "P6Xanthisma gracile" |
        ScieNamePlot == "P1Euphorbia melanadenia" |
        Scientific_Name == "Mimosa biuncifera (=aculeaticarpa)" |
        Plot == "P5" |
        Scientific_Name == "Pectis papposa" |
        ScieNamePlot == "P5Pectis papposa" |
        Qpercent_mass.TW == "40to60TW.SMALL" |
        Scientific_Name == "Gutierrezia sarothrae" |
        Scientific_Name == "Bouteloua aristidoides" |
        ScieNamePlot == "P5Bouteloua aristidoides" |
        Type_of_Sample == "leaf-stem" |
        ScieNamePlot == "P5Ceanothus greggii" |
        ScieNamePlot == "P2Baccharis sarothroides" |
        Scientific_Name == "Euphorbia melanadenia" |
        Scientific_Name == "Boechera perennans" |
        ScieNamePlot == "P6Boechera perennans" |
        ScieNamePlot == "P1Mimosa biuncifera (=aculeaticarpa)" 
      
      
    ) &
      # Exclude criteria indicated with '+'
      !(
        TW_Q == "TW.VSMALL" |
          Qpercent_mass.TW == "80to100TW.VSMALL" |
          TubeTW == "oneTW.VSMALL" |
          Scientific_Name == "Allionia incarnata" |
          # Tube_No == "two" |
          Scientific_Name == "Solanum elaeagnifolium" |
          Scientific_Name == "Populus fremontii" |
          ScieNamePlot == "P2Populus fremontii" |
          Scientific_Name == "Fraxinus velutina" |
          ScieNamePlot == "P2Fraxinus velutina" |
          ScieNamePlot == "P5Solanum elaeagnifolium" |  
          TubeTW == "twoTW.MEDIUM" |
          Scientific_Name == "Phyla nodiflora" |
          ScieNamePlot == "P2Phyla nodiflora" |
          ScieNamePlot == "P1Allionia incarnata" |  
          ScieNamePlot == "P5Allionia incarnata" |  
          ScieNamePlot == "P6Amaranthus palmeri" |  
          ScieNamePlot == "P1Eragrostis lehmanniana" |  
          TubeTW == "twoTW.SMALL" |
          Scientific_Name == "Eragrostis lehmanniana" |
          ScieNamePlot == "P1Xanthisma gracile" |   
          #Type_of_Sample == "leaf" |
          # Form == "Forb" |
          ScieNamePlot == "P2Portulaca suffrutescens" |   
          TubeTW == "twoTW.VSMALL" |
          #Plot == "P2" |
          Scientific_Name == "Mentzelia longiloba" |
          ScieNamePlot == "P6Mentzelia longiloba" 
      )
  )





# Find the remaining data points not in `dt_Cu_best`!!!!!!!!!!!!!!!!!!!!!!
dt_Cu_worst <- anti_join(dt, dt_Cu_best, by = "SampleID")

#Remove NAs
dt_Cu_worst <- dt_Cu_worst %>% 
  filter(!is.na(Cu_PXRF))

dt_Cu_best <- dt_Cu_best %>% 
  filter(!is.na(Cu_PXRF))

#Remove errorous plants from dt_Cu_worst - MAYBE SKIP THIS

dt_Cu_worst <- dt_Cu_worst %>%  # SKIP THIS
  filter(
    SampleID!="57" &
      SampleID!="86" &
      SampleID!="27" &
      SampleID!="18" &
      SampleID!="87" &
      SampleID!="53" &
      SampleID!="21" &
      SampleID!="17" &
      SampleID!="116" &
      SampleID!="20" &
      SampleID!="110" &
      SampleID!="56" &
      SampleID!="115" 
  )


ggplot(data=dt_Cu_best, aes(x = Cu_PXRF, y = Cu_ICP)) +
  geom_point(data=dt_Cu_best, color = "#003f5c", size=2.5, stroke=0.6, shape=1) +
  geom_smooth(data=dt_Cu_best, aes(x = Cu_PXRF, y =Cu_ICP), method = "lm", se = FALSE, color = "#003f5c", linetype = "solid", size=0.65) +   # Regression line for the first model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  geom_point(data=dt_Cu_worst, aes(x = Cu_PXRF, y = Cu_ICP), color = "green", size=2.5, stroke=0.6, shape=4) + # New points
  geom_smooth(data=dt_Cu_worst, aes(x = Cu_PXRF, y = Cu_ICP), method = "lm", se = FALSE, color = "green", linetype = "solid", size=0.65) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25)) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25)) +
  labs(x = "pXRF Cu", y = "ICP Cu") +
  theme_classic()  

rmse_best_RAW <- sqrt(mean((dt_Cu_best$Cu_ICP - dt_Cu_best$Cu_PXRF)^2)) # 15 bez Mn, 15.69 z MN
rmse_worst_RAW <- sqrt(mean((dt_Cu_worst$Cu_ICP - dt_Cu_worst$Cu_PXRF)^2)) # 70 bez Mn, 68 z Mn

lm1 <- lm(dt_Cu_best$Cu_ICP~dt_Cu_best$Cu_PXRF) #R2 = 0.9931 bez Mn, 0.9894 z Mn
summary(lm1)

lm1 <- lm(dt_Cu_worst$Cu_ICP~dt_Cu_worst$Cu_PXRF) #R2 = 0.933, 0.934 z Mn
summary(lm1)




#Training model 
{
  
  
  # Initialize lists to store RMSE results and model data
  results <- list(M1 = numeric(), M2 = numeric(), M3 = numeric())
  iterations <- 100  # Adjust the number of iterations as desired
  top_models <- list()  # Store top models with high ICP values
  top_n <- 5  # Number of top models to retain
  rmse_threshold <- Inf  # Initialize with infinity
  
  # Loop over multiple repetitions
  for (i in 1:iterations) {
    set.seed(123 + i)  # Ensure reproducibility
    
    # Randomly split the data into training and testing sets
    train_indices <- sample(1:nrow(dt_Cu_worst), 0.75 * nrow(dt_Cu_worst))
    train_data <- dt_Cu_worst[train_indices, ]
    test_data <- dt_Cu_worst[-train_indices, ]
    
    # Train the models
    M1Cu_train <- glm(Cu_ICP ~ Cu_PXRF, data = train_data, family = Gamma(link = "identity"))
    M2Cu_train <- glm(Cu_ICP ~ Cu_PXRF + Total_Weight, data = train_data, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
    M3Cu_train <- glm(Cu_ICP ~ Cu_PXRF + Substrate_RT, data = train_data, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
    M4Cu_train <- glm(Cu_ICP ~ Cu_PXRF + Substrate_RT + Mn_ICP, data = train_data, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
    
    # Make predictions on the test set
    test_data$Predicted_Cu_M1 <- predict(M1Cu_train, newdata = test_data, type = "response")
    test_data$Predicted_Cu_M2 <- predict(M2Cu_train, newdata = test_data, type = "response")
    test_data$Predicted_Cu_M3 <- predict(M3Cu_train, newdata = test_data, type = "response")
    test_data$Predicted_Cu_M4 <- predict(M4Cu_train, newdata = test_data, type = "response")
    
    # Calculate RMSE for each model
    rmse_M1 <- sqrt(mean((test_data$Cu_ICP - test_data$Predicted_Cu_M1)^2))
    rmse_M2 <- sqrt(mean((test_data$Cu_ICP - test_data$Predicted_Cu_M2)^2))
    rmse_M3 <- sqrt(mean((test_data$Cu_ICP - test_data$Predicted_Cu_M3)^2))
    rmse_M4 <- sqrt(mean((test_data$Cu_ICP - test_data$Predicted_Cu_M4)^2))
    # Store RMSE scores for each iteration
    results$M1 <- c(results$M1, rmse_M1)
    results$M2 <- c(results$M2, rmse_M2)
    results$M3 <- c(results$M3, rmse_M3)
    results$M4 <- c(results$M4, rmse_M4)
    # Check if the model includes points above the ICP threshold (e.g., 200) and update the top models
    if (rmse_M3 < rmse_threshold && any(test_data$Cu_ICP > 200)) {
      top_models[[length(top_models) + 1]] <- list(train = train_data, test = test_data, rmse = rmse_M3)
      # Keep only the top N models sorted by RMSE
      top_models <- top_models[order(sapply(top_models, function(x) x$rmse))]
      if (length(top_models) > top_n) {
        top_models <- top_models[1:top_n]
      }
      # Update the RMSE threshold to the current highest RMSE among top models
      rmse_threshold <- max(sapply(top_models, function(x) x$rmse))
    }
  }
  
  # Save RMSE results to an Excel file
  rmse_df <- data.frame(
    Iteration = 1:iterations,
    RMSE_M1 = results$M1,
    RMSE_M2 = results$M2,
    RMSE_M3 = results$M3,
    RMSE_M4 = results$M4
  )
  
  #write_xlsx(rmse_df, "RMSE_Results.xlsx")
  
  # Extract the best set of models for plotting or further analysis
  best_models <- top_models  # List of top models
  
  # You can now select any model in `best_models` for plotting purposes
  # For example, to use the first model in the list:
  best_train <- best_models[[1]]$train
  best_test <- best_models[[1]]$test
  
 # write_xlsx(best_train, "Cu_best_train.xlsx") 
#  write_xlsx(best_test, "Cu_best_test.xlsx")
#  write_xlsx(dt_Cu_best, "Cu_best_dt.xlsx")
 # write_xlsx(dt_Cu_worst, "Cu_worst_dt.xlsx")
  

  # write.table(best_train, "Cu_best_train.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
  # write.table(best_test, "Cu_best_test.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
  # write.table(dt_Cu_best, "Cu_best_dt.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
  # write.table(dt_Cu_worst, "Cu_worst_dt.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
  
  }


cor.test(dt_Cu_best$Cu_ICP, dt_Cu_best$Cu_PXRF, method="spearman") # 0.97158, p-val < 2.2e-16
cor.test(best_test$Cu_ICP, best_test$Predicted_Cu_M1, method="spearman") # 0.9607843, p-value = 4.605e-07
cor.test(best_test$Cu_ICP, best_test$Predicted_Cu_M2, method="spearman") # 0.9534314  , p-value < 2.2e-16
cor.test(best_test$Cu_ICP, best_test$Predicted_Cu_M3, method="spearman") # 0.9583333 , p-value = < 2.2e-16
cor.test(dt_Cu_worst$Cu_ICP, dt_Cu_worst$Cu_PXRF, method="spearman") # 0.9404  ,  p-value < 2.2e-16

Cu <- ggplot(data=dt_Cu_best, aes(x = Cu_PXRF, y = Cu_ICP)) +
  geom_point(data=dt_Cu_best, color = "#4793AF", size=2.5, stroke=0.6, shape=1) +
  geom_point(data=best_test, aes(x = Predicted_Cu_M3, y = Cu_ICP), color = "#FEB941", size=2.5, stroke=0.6, shape=3) + # New points
  geom_smooth(data=dt_Cu_best, aes(x = Cu_PXRF, y = Cu_ICP), method = "lm", se = FALSE, color = "#4793AF", linetype = "solid", size=0.65) +   # Regression line for the first model
  geom_smooth(data=best_test, aes(x = Predicted_Cu_M3, y = Cu_ICP), method = "lm", se = FALSE, color = "#FEB941", linetype = "solid", size=0.65) +  # Regression line for the second model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  geom_point(data=dt_Cu_worst, aes(x = Cu_PXRF, y = Cu_ICP), color = "#8B322C", size=2.5, stroke=0.6, shape=4) + # New points
  geom_smooth(data=dt_Cu_worst, aes(x = Cu_PXRF, y = Cu_ICP), method = "lm", se = FALSE, color = "#8B322C", linetype = "solid", size=0.65) +  
  labs(x = "pXRF Cu", y = "ICP concentration Cu") +
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

print(Cu)

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/Models_pxrf/Figures and datasets MODELS/Cu datasets")
dt_Cu_best <-read.delim("Cu_best_dt.txt")
dt_Cu_worst <-read.delim("Cu_worst_dt.txt")
best_test <-read.delim("Cu_best_test.txt")

pzoom <- ggplot(data=dt_Cu_best, aes(x = Cu_PXRF, y = Cu_ICP)) +
  geom_point(data=dt_Cu_best, color = "#4793AF", size=2.5, stroke=0.45, shape=1) +
  geom_point(data=best_test, aes(x = Predicted_Cu_M3, y = Cu_ICP), color = "#FEB941", size=2.5, stroke=0.45, shape=3) + # New points
  geom_smooth(data=dt_Cu_best, aes(x = Cu_PXRF, y = Cu_ICP), method = "lm", se = FALSE, color = "#4793AF", linetype = "solid", size=0.45) +   # Regression line for the first model
  geom_smooth(data=best_test, aes(x = Predicted_Cu_M3, y = Cu_ICP), method = "lm", se = FALSE, color = "#FEB941", linetype = "solid", size=0.45) +  # Regression line for the second model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.45) +
  geom_point(data=dt_Cu_worst, aes(x = Cu_PXRF, y = Cu_ICP), color = "#8B322C", size=2.5, stroke=0.45, shape=4) + # New points
  geom_smooth(data=dt_Cu_worst, aes(x = Cu_PXRF, y = Cu_ICP), method = "lm", se = FALSE, color = "#8B322C", linetype = "solid", size=0.45) +  
  labs(x = "pXRF Cu", y = "ICP concentration Cu") +
  coord_cartesian(xlim = c(0, 125), ylim = c(0, 125)) +
  scale_x_continuous(breaks = seq(0, 150, by = 25)) +  # Set x-axis breaks
  scale_y_continuous(breaks = seq(0, 150, by = 25)) +
  theme_classic() +  # Using theme_classic as theme_classic2 is not part of base ggplot2
  theme(panel.grid.major = element_blank(), # Removing major grid lines
        panel.grid.minor = element_blank(), # Removing minor grid lines
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.3), # Adding border around the plot using updated argument
        axis.line = element_line(linewidth = 0.3, colour = "black"),
        plot.title = element_text(size = 16, face = "bold"),  # Customize plot title
        axis.title = element_text(size = 20),  # Customize axis labels
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 16, face = "bold"),
        legend.position = "top")

print(pzoom)

sqrt(mean((dt_Cu_best$Cu_ICP - dt_Cu_best$Cu_PXRF)^2)) # 15.697
sqrt(mean((best_test$Cu_ICP - best_test$Predicted_Cu_M3)^2)) #model 2: RMSE = 15.41 Model 1: RMSE = 13.29
lm1 <- lm(best_test$Cu_ICP~best_test$Predicted_Cu_M3) # model 2: R2 = 0.994, model 1: 0.9706
summary(lm1)




}


##### Se modele
{
  
  cor.test(dt$Se_ICP, dt$Se_PXRF, method="spearman") # 0.8972659 ,  p-value < 2.2e-16

  
  Se <- ggplot(data=dt, aes(x = Se_PXRF, y = Se_ICP)) +
    geom_point(data=dt, color = "#4793AF", size=2.5, stroke=0.6, shape=1) +
    geom_smooth(data=dt, aes(x = Se_PXRF, y = Se_ICP), method = "lm", se = FALSE, color = "#4793AF", linetype = "solid", size=0.65) +   # Regression line for the first model
    geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
    labs(x = "pXRF Se", y = "ICP concentration Se") +
    scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10)) +
    scale_x_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10)) +
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
  
  print(Se)
  
  

  
  Sezoom <- ggplot(data=dt, aes(x = Se_PXRF, y = Se_ICP)) +
    geom_point(data=dt, color = "#4793AF", size=2.5, stroke=0.45, shape=1) +
    geom_smooth(data=dt, aes(x = Se_PXRF, y = Se_ICP), method = "lm", se = FALSE, color = "#4793AF", linetype = "solid", size=0.45) +   # Regression line for the first model
    geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.45) +
    labs(x = "pXRF Se", y = "ICP concentration Se") +
    coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
    scale_x_continuous(breaks = seq(0, 10, by = 2)) +  # Set x-axis breaks
    scale_y_continuous(breaks = seq(0, 10, by = 2)) +
    theme_classic() +  # Using theme_classic as theme_classic2 is not part of base ggplot2
    theme(panel.grid.major = element_blank(), # Removing major grid lines
          panel.grid.minor = element_blank(), # Removing minor grid lines
          panel.border = element_rect(colour = "black", fill=NA, linewidth=0.3), # Adding border around the plot using updated argument
          axis.line = element_line(linewidth = 0.3, colour = "black"),
          plot.title = element_text(size = 16, face = "bold"),  # Customize plot title
          axis.title = element_text(size = 20),  # Customize axis labels
          axis.text.x = element_text(size = 16),
          axis.title.x = element_text(size = 20),
          axis.text.y = element_text(size = 16),
          axis.title.y = element_text(size = 20),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 16, face = "bold"),
          legend.position = "top")
  
  print(Sezoom)
  
  
  dt_Se <- dt %>% 
    filter(!is.na(Se_PXRF))
  
}


##### Re modele
{
  dt <-read.delim("PXRF_models.txt")
  
  dt_Re_best <- dt %>%
    filter(
      # Include criteria indicated with '-'
      (

          Tube_No =="two"  |
          TW_Q == "TW.LARGE" |
          Scientific_Name == "Isocoma acradenia" | 
          ScieNamePlot == "P2Isocoma acradenia" |
          Zn_ICP_Q == "Zn.SMALL" 
      ) &
        # Exclude criteria indicated with '+'
        !(
          Scientific_Name == "Xanthisma gracile" |
            ScieNamePlot == "P2Xanthisma gracile" |
            Scientific_Name == "Mentzelia longiloba" | 
            ScieNamePlot == "P1Mentzelia longiloba" |
            Zn_ICP_Q == "Zn.LARGE" |
            Tube_No =="one" |
            Form == "Forb" |
            Scientific_Name == "Tamarix chinensis" | 
            ScieNamePlot == "P2Tamarix chinensis" |
            TW_Q == "TW.SMALL" |
            TW_Q == "TW.MEDIUM" 
            
        )
    )
  
  
  
  dt_Re_worst <- anti_join(dt, dt_Re_best, by = "SampleID")
  
  dt_Re_worst <- dt_Re_worst %>% 
    filter(!is.na(Re_PXRF))
  
  dt_Re_best <- dt_Re_best %>% 
    filter(!is.na(Re_PXRF))
  
  dt_Re <- dt %>% 
    filter(!is.na(Re_PXRF))
  
  # Model Performance - RMSE
  #rmse_best_RAW <- sqrt(mean((dt_Re_best$Re_ICP - dt_Re_best$Re_PXRF)^2)) # not much difference between those two, 2 values of RMSE
  #rmse_worst_RAW <- sqrt(mean((dt_Re_worst$Re_ICP - dt_Re_worst$Re_PXRF)^2))
  
  
  
  
  Re <- ggplot(data=dt, aes(x = Re_PXRF, y = Re_ICP)) +
    geom_point(data=dt, color = "#4793AF", size=2.5, stroke=0.6, shape=1) +
    # geom_point(data=best_test, aes(x = Predicted_Cu_M3, y = Cu_ICP), color = "#AD0B0B", size=2.5, stroke=0.6, shape=3) + # New points
    geom_smooth(data=dt, aes(x = Re_PXRF, y = Re_ICP), method = "lm", se = FALSE, color = "#4793AF", linetype = "solid", size=0.65) +   # Regression line for the first model
    # geom_smooth(data=best_test, aes(x = Predicted_Cu_M3, y = Cu_ICP), method = "lm", se = FALSE, color = "#AD0B0B", linetype = "solid", size=0.65) +  # Regression line for the second model
    geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
    #geom_point(data=dt_, aes(x = Re_PXRF, y = Re_ICP), color = "#8B322C", size=2.5, stroke=0.6, shape=4) + # New points
    #geom_smooth(data=dt_, aes(x = Re_PXRF, y = Re_ICP), method = "lm", se = FALSE, color = "#8B322C", linetype = "solid", size=0.65) +  
    labs(x = "pXRF Re", y = "ICP concentration Re") +
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
  
  print(Re)
  

  Rezoom <- ggplot(data=dt, aes(x = Re_PXRF, y = Re_ICP)) +
    geom_point(data=dt, color = "#4793AF", size=2.5, stroke=0.45, shape=1) +
    geom_smooth(data=dt, aes(x = Re_PXRF, y = Re_ICP), method = "lm", se = FALSE, color = "#4793AF", linetype = "solid", size=0.45) +   # Regression line for the first model
    geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.45) +
    labs(x = "pXRF Re", y = "ICP Re") +
    coord_cartesian(xlim = c(0, 25), ylim = c(0, 25)) +
    scale_x_continuous(breaks = seq(0, 100, by = 5)) +  # Set x-axis breaks
    scale_y_continuous(breaks = seq(0, 100, by = 5)) +
    theme_classic() +  # Using theme_classic as theme_classic2 is not part of base ggplot2
    theme(panel.grid.major = element_blank(), # Removing major grid lines
          panel.grid.minor = element_blank(), # Removing minor grid lines
          panel.border = element_rect(colour = "black", fill=NA, linewidth=0.3), # Adding border around the plot using updated argument
          axis.line = element_line(linewidth = 0.3, colour = "black"),
          plot.title = element_text(size = 16, face = "bold"),  # Customize plot title
          axis.title = element_text(size = 20),  # Customize axis labels
          axis.text.x = element_text(size = 16),
          axis.title.x = element_text(size = 20),
          axis.text.y = element_text(size = 16),
          axis.title.y = element_text(size = 20),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 16, face = "bold"),
          legend.position = "top")
  
  print(Rezoom)
  
  cor.test(dt$Re_ICP, dt$Re_PXRF, method="spearman") # 0.9382305 ,  p-value = 1.971e-14
  
}



############ ZN
{
dt <-read.delim("PXRF_models.txt")


dt_Zn_best <- dt %>%
  filter(
    # Include criteria indicated with '-'
    (
      TubeTW == "oneTW.MEDIUM" |

        Form == "Shrub" | 
        TubeTW == "oneTW.LARGE" |
        Form == "Tree" |  
        Type_of_Sample == "leaf" | 
        Scientific_Name == "Xanthisma gracile" |
        Scientific_Name == "Ceanothus greggii" |
        TW_Q == "TW.LARGE" |
        Scientific_Name == "Mimosa biuncifera (=aculeaticarpa)" |
        Scientific_Name == "Gutierrezia sarothrae" |
        ScieNamePlot == "P1Ceanothus greggii" |
        Scientific_Name == "Baccharis sarothroides" |
        Scientific_Name == "Senegalia (Acacia) greggii" |
        ScieNamePlot == "P2Senegalia (Acacia) greggii" |
        ScieNamePlot == "P5Gutierrezia sarothrae" |
        ScieNamePlot == "P6Xanthisma gracile" |
        TubeTW == "oneTW.SMALL" |
        Tube_No == "one" |
        Scientific_Name == "Datura wrightii" |
        ScieNamePlot == "P1Datura wrightii" |
        ScieNamePlot == "P2Xanthisma gracile" |
        ScieNamePlot == "P2Baccharis sarothroides" |
        ScieNamePlot == "P5Mimosa biuncifera (=aculeaticarpa)" |
        Scientific_Name == "Isocoma acradenia" |
        ScieNamePlot == "P2Isocoma acradenia" |
        ScieNamePlot == "P1Mimosa biuncifera (=aculeaticarpa)" 
        
    ) &
      # Exclude criteria indicated with '+'
      !(
        #Form == "Grass" | #This is likely because grasses have low Zn concentration and low concentraion has high error
        TW_Q == "TW.VSMALL" |
          TubeTW == "oneTW.VSMALL" |
          Scientific_Name == "Mentzelia longiloba" |
          Scientific_Name == "Bothriochloa barbinodis" |
          #Type_of_Sample == "leaf-stem" |
          ScieNamePlot == "P6Mentzelia longiloba" |
          ScieNamePlot == "P2Portulaca suffrutescens" |
          TubeTW == "twoTW.MEDIUM" |
          ScieNamePlot == "P1Mentzelia longiloba" |
          Scientific_Name == "Portulaca oleracea" |
          ScieNamePlot == "P5Portulaca oleracea" |
          Scientific_Name == "Portulaca suffrutescens" |
          Scientific_Name == "Phyla nodiflora" |
          ScieNamePlot == "P2Phyla nodiflora" |
          ScieNamePlot == "P1Bothriochloa barbinodis" |
          Scientific_Name == "Cynodon dactylon" |
          Scientific_Name == "Amaranthus palmeri" |
          Scientific_Name == "Eragrostis lehmanniana" |

          ScieNamePlot == "P5Bothriochloa barbinodis" |
          Scientific_Name == "Allionia incarnata" |
          ScieNamePlot == "P1Allionia incarnata" |
          Scientific_Name == "Solanum elaeagnifolium" |
          ScieNamePlot == "P2Solanum elaeagnifolium" |
          ScieNamePlot == "P5Amaranthus palmeri" |
          #Tube_No == "two" |
          Scientific_Name == "Pseudognaphalium canescens" |
          ScieNamePlot == "P6Pseudognaphalium canescens" |
          ScieNamePlot == "P5Dasyochloa pulchella" |
          #Form == "Forb" |
          # Plot == "P6" |
          TubeTW == "twoTW.SMALL" |
          ScieNamePlot == "P5Cynodon dactylon" |
          Scientific_Name == "Eragrostis curvula" |
          ScieNamePlot == "P6Eragrostis curvula" |
          ScieNamePlot == "P1Pectis papposa" |
          ScieNamePlot == "P6Eragrostis lehmanniana"
          
      )
  )



dt_Zn_worst <- anti_join(dt, dt_Zn_best, by = "SampleID")

dt_Zn_worst <- dt_Zn_worst %>% 
  filter(!is.na(Zn_PXRF))

dt_Zn_best <- dt_Zn_best %>% 
  filter(!is.na(Zn_PXRF))

ggplot(data=dt_Zn_best, aes(x = Zn_PXRF, y = Zn_ICP)) +
  geom_point(data=dt_Zn_best, color = "#003f5c", size=2.5, stroke=0.6, shape=1) +
  geom_smooth(data=dt_Zn_best, aes(x = Zn_PXRF, y =Zn_ICP), method = "lm", se = FALSE, color = "#003f5c", linetype = "solid", size=0.65) +   # Regression line for the first model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  geom_point(data=dt_Zn_worst, aes(x = Zn_PXRF, y = Zn_ICP), color = "green", size=2.5, stroke=0.6, shape=4) + # New points
  geom_smooth(data=dt_Zn_worst, aes(x = Zn_PXRF, y = Zn_ICP), method = "lm", se = FALSE, color = "green", linetype = "solid", size=0.65) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25)) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25)) +
  labs(x = "pXRF ", y = "ICP") +
  theme_classic()  


rmse_best_RAW <- sqrt(mean((dt_Zn_best$Zn_ICP - dt_Zn_best$Zn_PXRF)^2)) # 9.5 jak zostawiam Fe.MEDIUM i Fe.VSMALL, 9.02 jak filtruje tylko VSMALL a zostawiam MEDIUM, 9.04 jak calkowicie nie ma Fe_ICP
rmse_worst_RAW <- sqrt(mean((dt_Zn_worst$Zn_ICP - dt_Zn_worst$Zn_PXRF)^2)) # 26.5 jak zostawiam Fe.MEDIUM i Fe.VSMALL, 28.57 jak filtruje ylko VSMALL a zostawiam MEDIUM, 31 jak calkowicie nie ma Fe_ICP
rmse_worst_RAW <- sqrt(mean((best_test$Zn_ICP - best_test$Predicted_Zn_M3)^2))

lm1 <- lm(dt_Zn_best$Zn_ICP~dt_Zn_best$Zn_PXRF) #R2 = 0.917 jak filtruje Fe.MEDIUM i Fe.VSMALL, 0.927 jak filtruje ylko VSMALL a zostawiam MEDIUM, 0.9136 jak calkowicie nie ma Fe_ICP
summary(lm1)

lm1 <- lm(dt_Zn_worst$Zn_ICP~dt_Zn_worst$Zn_PXRF) #R2 = 0.322 jak filtruje Fe.MEDIUM i Fe.VSMALL, 0.2839 jak filtruje ylko VSMALL a zostawiam MEDIUM, 0.2617 ak calkowicie nie ma Fe_ICP
summary(lm1)




# Initialize lists to store RMSE results and model data
results <- list(M1 = numeric(), M2 = numeric(), M3 = numeric())
iterations <- 100  # Adjust the number of iterations as desired
top_models <- list()  # Store top models with high ICP values
top_n <- 5  # Number of top models to retain
rmse_threshold <- Inf  # Initialize with infinity

# Loop over multiple repetitions
for (i in 1:iterations) {
  set.seed(123 + i)  # Ensure reproducibility
  
  # Randomly split the data into training and testing sets
  train_indices <- sample(1:nrow(dt_Zn_worst), 0.75 * nrow(dt_Zn_worst))
  train_data <- dt_Zn_worst[train_indices, ]
  test_data <- dt_Zn_worst[-train_indices, ]
  
  # Train the models
  M1Zn_train <- glm(Zn_ICP ~ Zn_PXRF, data = train_data, family = Gamma(link = "identity"))
  M2Zn_train <- glm(Zn_ICP ~ Zn_PXRF + Total_Weight, data = train_data, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Zn_train <- glm(Zn_ICP ~ Zn_PXRF + Substrate_RT, data = train_data, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  
  # Make predictions on the test set
  test_data$Predicted_Zn_M1 <- predict(M1Zn_train, newdata = test_data, type = "response")
  test_data$Predicted_Zn_M2 <- predict(M2Zn_train, newdata = test_data, type = "response")
  test_data$Predicted_Zn_M3 <- predict(M3Zn_train, newdata = test_data, type = "response")
  
  # Calculate RMSE for each model
  rmse_M1 <- sqrt(mean((test_data$Zn_ICP - test_data$Predicted_Zn_M1)^2))
  rmse_M2 <- sqrt(mean((test_data$Zn_ICP - test_data$Predicted_Zn_M2)^2))
  rmse_M3 <- sqrt(mean((test_data$Zn_ICP - test_data$Predicted_Zn_M3)^2))
  
  # Store RMSE scores for each iteration
  results$M1 <- c(results$M1, rmse_M1)
  results$M2 <- c(results$M2, rmse_M2)
  results$M3 <- c(results$M3, rmse_M3)
  
  # Check if the model includes points above the ICP threshold (e.g., 200) and update the top models
  if (rmse_M3 < rmse_threshold) {
    top_models[[length(top_models) + 1]] <- list(train = train_data, test = test_data, rmse = rmse_M3)
    # Keep only the top N models sorted by RMSE
    top_models <- top_models[order(sapply(top_models, function(x) x$rmse))]
    if (length(top_models) > top_n) {
      top_models <- top_models[1:top_n]
    }
    # Update the RMSE threshold to the current highest RMSE among top models
    rmse_threshold <- max(sapply(top_models, function(x) x$rmse))
  }
}

# Save RMSE results to an Excel file
rmse_df <- data.frame(
  Iteration = 1:iterations,
  RMSE_M1 = results$M1,
  RMSE_M2 = results$M2,
  RMSE_M3 = results$M3
)

#write_xlsx(rmse_df, "RMSE_Results.xlsx")

# Extract the best set of models for plotting or further analysis
best_models <- top_models  # List of top models

# You can now select any model in `best_models` for plotting purposes
# For example, to use the first model in the list:
best_train <- best_models[[1]]$train
best_test <- best_models[[1]]$test


#write_xlsx(best_train, "Zn_best_train.xlsx") 
#write_xlsx(best_test, "Zn_best_test.xlsx")
#write_xlsx(dt_Zn_best, "Zn_best_dt.xlsx")
#write_xlsx(dt_Zn_worst, "Zn_worst_dt.xlsx")


#write.table(best_train, "Zn_best_train.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
#write.table(best_test, "Zn_best_test.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
#write.table(dt_Zn_best, "Zn_best_dt.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
#write.table(dt_Zn_worst, "Zn_worst_dt.txt", sep = "\t", row.names = FALSE, col.names = TRUE)



Zn <- ggplot(data=dt_Zn_best, aes(x = Zn_PXRF, y = Zn_ICP)) +
  geom_point(data=dt_Zn_best, color = "#4793AF", size=2.5, stroke=0.6, shape=1) +
  geom_point(data=best_test, aes(x = Predicted_Zn_M3, y = Zn_ICP), color = "#FEB941", size=2.5, stroke=0.6, shape=3) + # New points
  geom_smooth(data=dt_Zn_best, aes(x = Zn_PXRF, y = Zn_ICP), method = "lm", se = FALSE, color = "#4793AF", linetype = "solid", size=0.65) +   # Regression line for the first model
  geom_smooth(data=best_test, aes(x = Predicted_Zn_M3, y = Zn_ICP), method = "lm", se = FALSE, color = "#FEB941", linetype = "solid", size=0.65) +  # Regression line for the second model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  geom_point(data=dt_Zn_worst, aes(x = Zn_PXRF, y = Zn_ICP), color = "#8B322C", size=2.5, stroke=0.6, shape=4) + # New points
  geom_smooth(data=dt_Zn_worst, aes(x = Zn_PXRF, y = Zn_ICP), method = "lm", se = FALSE, color = "#8B322C", linetype = "solid", size=0.65) +  
  labs(x = "pXRF Zn", y = "ICP concentration Zn") +
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

print(Zn)



setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/Models_pxrf/Figures and datasets MODELS/Zn datasets")
dt_Zn_best <-read.delim("Zn_best_dt.txt")
dt_Zn_worst <-read.delim("Zn_worst_dt.txt")
best_test <-read.delim("Zn_best_test.txt")


Znzoom <- ggplot(data=dt_Zn_best, aes(x = Zn_PXRF, y = Zn_ICP)) +
  geom_point(data=dt_Zn_best, color = "#4793AF", size=2.5, stroke=0.45, shape=1) +
  geom_point(data=best_test, aes(x = Predicted_Zn_M3, y = Zn_ICP), color = "#FEB941", size=2.5, stroke=0.45, shape=3) + # New points
  geom_smooth(data=dt_Zn_best, aes(x = Zn_PXRF, y = Zn_ICP), method = "lm", se = FALSE, color = "#4793AF", linetype = "solid", size=0.45) +   # Regression line for the first model
  geom_smooth(data=best_test, aes(x = Predicted_Zn_M3, y = Zn_ICP), method = "lm", se = FALSE, color = "#FEB941", linetype = "solid", size=0.45) +  # Regression line for the second model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.45) +
  geom_point(data=dt_Zn_worst, aes(x = Zn_PXRF, y = Zn_ICP), color = "#8B322C", size=2.5, stroke=0.45, shape=4) + # New points
  geom_smooth(data=dt_Zn_worst, aes(x = Zn_PXRF, y = Zn_ICP), method = "lm", se = FALSE, color = "#8B322C", linetype = "solid", size=0.45) +  
  labs(x = "pXRF Zn", y = "ICP concentration Zn") +
  coord_cartesian(xlim = c(0, 40), ylim = c(0, 40)) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +  # Set x-axis breaks
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  theme_classic() +  # Using theme_classic as theme_classic2 is not part of base ggplot2
  theme(panel.grid.major = element_blank(), # Removing major grid lines
        panel.grid.minor = element_blank(), # Removing minor grid lines
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.3), # Adding border around the plot using updated argument
        axis.line = element_line(linewidth = 0.3, colour = "black"),
        plot.title = element_text(size = 16, face = "bold"),  # Customize plot title
        axis.title = element_text(size = 20),  # Customize axis labels
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 16, face = "bold"),
        legend.position = "top")

print(Znzoom)



M1Zn_train <- glm(Zn_ICP ~ Zn_PXRF, data = train_data, family = Gamma(link = "identity"))
M2Zn_train <- glm(Zn_ICP ~ Zn_PXRF + Total_Weight, data = train_data, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
M3Zn_train <- glm(Zn_ICP ~ Zn_PXRF + Substrate_RT, data = train_data, family = Gamma(link = "identity"), control = glm.control(maxit = 50))

summary(M1Zn_train)
summary(M2Zn_train)
summary(M3Zn_train)



cor.test(dt_Zn_best$Zn_ICP, dt_Zn_best$Zn_PXRF, method="spearman") # 0.902973, p-val < 2.2e-16
cor.test(best_test$Zn_ICP, best_test$Predicted_Zn_M1, method="spearman") # 0.80419, p-value = 0.002746
cor.test(best_test$Zn_ICP, best_test$Predicted_Zn_M2, method="spearman") # 0.8461538  , pval = 0.0009695
cor.test(best_test$Zn_ICP, best_test$Predicted_Zn_M3, method="spearman") # 0.9230769  , p-value = < 2.2e-16
cor.test(dt_Cu_worst$Zn_ICP, dt_Cu_worst$Zn_PXRF, method="spearman") #0.6052184 p-value = 3.185e-05


}


######### Mn
{
dt <-read.delim("PXRF_models.txt")


dt_Mn_best <- dt %>%
  filter(
    # Include criteria indicated with '-'
    (
      
      Tube_No == "one" |
      TubeTW == "oneTW.SMALL" |
      TubeTW == "oneTW.MEDIUM" |
        Scientific_Name == "Ceanothus greggii" |
        TubeTW == "oneTW.LARGE" |
        Scientific_Name == "Senegalia (Acacia) greggii" |
        ScieNamePlot == "P2Senegalia (Acacia) greggii" |
        Scientific_Name == "Gutierrezia sarothrae" |
        ScieNamePlot == "P5Gutierrezia sarothrae" |
        ScieNamePlot == "P5Ceanothus greggii" |
        ScieNamePlot == "P1Euphorbia melanadenia" |
        Scientific_Name == "Dasyochloa pulchella" |
        ScieNamePlot == "P1Dasyochloa pulchella" |
        Scientific_Name == "Pectis papposa" |
        ScieNamePlot == "P5Pectis papposa" |
        TW_Q == "TW.SMALL" |
        TW_Q == "TW.LARGE" |
        Scientific_Name == "Xanthisma gracile" |
        Scientific_Name == "Juniperus arizonica" |
        ScieNamePlot == "P5Juniperus arizonica" |
        ScieNamePlot == "P1Ceanothus greggii" |
        Form == "Shrub"  
        
      ) &
      # Exclude criteria indicated with '+'
      !(
        #Form == "Grass" | #This is likely because grasses have low Zn concentration and low concentraion has high error
        #Tube_No == "two" |
        TubeTW == "twoTW.MEDIUM" |
          Scientific_Name == "Allionia incarnata" |
          TW_Q == "TW.VSMALL" |
          ScieNamePlot == "P2Solanum elaeagnifolium" |
          Scientific_Name == "Solanum elaeagnifolium" |
          #Form == "Forb" |
          Scientific_Name == "Populus fremontii" |
          ScieNamePlot == "P2Populus fremontii" |
          ScieNamePlot == "P2Allionia incarnata" |
          TubeTW == "twoTW.VSMALL" |
          TubeTW == "oneTW.VSMALL" |
          Scientific_Name == "Pseudognaphalium canescens" |
          ScieNamePlot == "P6Pseudognaphalium canescens" |
          ScieNamePlot == "P1Allionia incarnata" |
          Scientific_Name == "Ambrosia confertiflora" |
          ScieNamePlot == "P2Ambrosia confertiflora" |
          Scientific_Name == "Tamarix chinensis" |
          ScieNamePlot == "P2Tamarix chinensis" |
          Scientific_Name == "Phyla nodiflora" |
          ScieNamePlot == "P2Phyla nodiflora" |
          Scientific_Name == "Sphaeralcea parvifolia" |
          ScieNamePlot == "P5Sphaeralcea parvifolia" |
          #TubeTW == "twoTW.SMALL" |
          Scientific_Name == "Portulaca suffrutescens" |
          ScieNamePlot == "P2Portulaca suffrutescens" |
          #TW_Q == "TW.MEDIUM" |
          ScieNamePlot == "P5Euphorbia melanadenia" |
          ScieNamePlot == "P6Mentzelia longiloba" |
          Scientific_Name == "Eragrostis curvula" |
          ScieNamePlot == "P6Eragrostis curvula" 
        
      )
  )



dt_Mn_worst <- anti_join(dt, dt_Mn_best, by = "SampleID")

dt_Mn_worst <- dt_Mn_worst %>% 
  filter(!is.na(Mn_PXRF))

dt_Mn_best <- dt_Mn_best %>% 
  filter(!is.na(Mn_PXRF))

ggplot(data=dt_Mn_best, aes(x = Mn_PXRF, y = Mn_ICP)) +
  geom_point(data=dt_Mn_best, color = "#003f5c", size=2.5, stroke=0.6, shape=1) +
  geom_smooth(data=dt_Mn_best, aes(x = Mn_PXRF, y =Mn_ICP), method = "lm", se = FALSE, color = "#003f5c", linetype = "solid", size=0.65) +   # Regression line for the first model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  geom_point(data=dt_Mn_worst, aes(x = Mn_PXRF, y = Mn_ICP), color = "green", size=2.5, stroke=0.6, shape=4) + # New points
  geom_smooth(data=dt_Mn_worst, aes(x = Mn_PXRF, y = Mn_ICP), method = "lm", se = FALSE, color = "green", linetype = "solid", size=0.65) + 
  scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, by = 25)) +
  scale_x_continuous(limits = c(0, 200), breaks = seq(0, 200, by = 25)) +
  labs(x = "pXRF ", y = "ICP") +
  theme_classic()  


rmse_best_RAW <- sqrt(mean((dt_Mn_best$Mn_ICP - dt_Mn_best$Mn_PXRF)^2)) # 9.5 jak zostawiam Fe.MEDIUM i Fe.VSMALL, 9.02 jak filtruje tylko VSMALL a zostawiam MEDIUM, 9.04 jak calkowicie nie ma Fe_ICP
rmse_worst_RAW <- sqrt(mean((dt_Mn_worst$Mn_ICP - dt_Mn_worst$Mn_PXRF)^2)) # 26.5 jak zostawiam Fe.MEDIUM i Fe.VSMALL, 28.57 jak filtruje ylko VSMALL a zostawiam MEDIUM, 31 jak calkowicie nie ma Fe_ICP
rmse_M1 <- sqrt(mean((best_test$Mn_ICP - best_test$Predicted_Mn_M1)^2))
rmse_M2 <- sqrt(mean((best_test$Mn_ICP - best_test$Predicted_Mn_M2)^2))
rmse_M3 <- sqrt(mean((best_test$Mn_ICP - best_test$Predicted_Mn_M3)^2))

lm1 <- lm(dt_Mn_best$Mn_ICP~dt_Mn_best$Mn_PXRF) #R2 = 0.917 jak filtruje Fe.MEDIUM i Fe.VSMALL, 0.927 jak filtruje ylko VSMALL a zostawiam MEDIUM, 0.9136 jak calkowicie nie ma Fe_ICP
summary(lm1)

lm1 <- lm(dt_Mn_worst$Mn_ICP~dt_Mn_worst$Mn_PXRF) #R2 = 0.322 jak filtruje Fe.MEDIUM i Fe.VSMALL, 0.2839 jak filtruje ylko VSMALL a zostawiam MEDIUM, 0.2617 ak calkowicie nie ma Fe_ICP
summary(lm1)





# Initialize lists to store RMSE results and model data
results <- list(M1 = numeric(), M2 = numeric(), M3 = numeric())
iterations <- 100  # Adjust the number of iterations as desired
top_models <- list()  # Store top models with high ICP values
top_n <- 5  # Number of top models to retain
rmse_threshold <- Inf  # Initialize with infinity

# Loop over multiple repetitions
for (i in 1:iterations) {
  set.seed(123 + i)  # Ensure reproducibility
  
  # Randomly split the data into training and testing sets
  train_indices <- sample(1:nrow(dt_Mn_worst), 0.75 * nrow(dt_Mn_worst))
  train_data <- dt_Mn_worst[train_indices, ]
  test_data <- dt_Mn_worst[-train_indices, ]
  
  # Train the models
  M1Mn_train <- glm(Mn_ICP ~ Mn_PXRF, data = train_data, family = Gamma(link = "identity"))
  M2Mn_train <- glm(Mn_ICP ~ Mn_PXRF + Total_Weight, data = train_data, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  M3Mn_train <- glm(Mn_ICP ~ Mn_PXRF + Substrate_RT, data = train_data, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  
  # Make predictions on the test set
  test_data$Predicted_Mn_M1 <- predict(M1Mn_train, newdata = test_data, type = "response")
  test_data$Predicted_Mn_M2 <- predict(M2Mn_train, newdata = test_data, type = "response")
  test_data$Predicted_Mn_M3 <- predict(M3Mn_train, newdata = test_data, type = "response")
  
  # Calculate RMSE for each model
  rmse_M1 <- sqrt(mean((test_data$Mn_ICP - test_data$Predicted_Mn_M1)^2))
  rmse_M2 <- sqrt(mean((test_data$Mn_ICP - test_data$Predicted_Mn_M2)^2))
  rmse_M3 <- sqrt(mean((test_data$Mn_ICP - test_data$Predicted_Mn_M3)^2))
  
  # Store RMSE scores for each iteration
  results$M1 <- c(results$M1, rmse_M1)
  results$M2 <- c(results$M2, rmse_M2)
  results$M3 <- c(results$M3, rmse_M3)
  
  # Check if the model includes points above the ICP threshold (e.g., 200) and update the top models
  if (rmse_M3 < rmse_threshold) {
    top_models[[length(top_models) + 1]] <- list(train = train_data, test = test_data, rmse = rmse_M3)
    # Keep only the top N models sorted by RMSE
    top_models <- top_models[order(sapply(top_models, function(x) x$rmse))]
    if (length(top_models) > top_n) {
      top_models <- top_models[1:top_n]
    }
    # Update the RMSE threshold to the current highest RMSE among top models
    rmse_threshold <- max(sapply(top_models, function(x) x$rmse))
  }
}

# Save RMSE results to an Excel file
rmse_df <- data.frame(
  Iteration = 1:iterations,
  RMSE_M1 = results$M1,
  RMSE_M2 = results$M2,
  RMSE_M3 = results$M3
)

#write_xlsx(rmse_df, "RMSE_Results.xlsx")

# Extract the best set of models for plotting or further analysis
best_models <- top_models  # List of top models

# You can now select any model in `best_models` for plotting purposes
# For example, to use the first model in the list:
best_train <- best_models[[1]]$train
best_test <- best_models[[1]]$test


#write_xlsx(best_train, "Mn_best_train.xlsx") 
#write_xlsx(best_test, "Mn_best_test.xlsx")
#write_xlsx(dt_Mn_best, "Mn_best_dt.xlsx")
#write_xlsx(dt_Mn_worst, "Mn_worst_dt.xlsx")


#write.table(best_train, "Mn_best_train.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
#write.table(best_test, "Mn_best_test.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
#write.table(dt_Mn_best, "Mn_best_dt.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
#write.table(dt_Mn_worst, "Mn_worst_dt.txt", sep = "\t", row.names = FALSE, col.names = TRUE)



Mn<- ggplot(data=dt_Mn_best, aes(x = Mn_PXRF, y = Mn_ICP)) +
  geom_point(data=dt_Mn_best, color = "#4793AF", size=2.5, stroke=0.6, shape=1) +
  geom_point(data=best_test, aes(x = Predicted_Mn_M3, y = Mn_ICP), color = "#FEB941", size=2.5, stroke=0.6, shape=3) + # New points
  geom_smooth(data=dt_Mn_best, aes(x = Mn_PXRF, y = Mn_ICP), method = "lm", se = FALSE, color = "#4793AF", linetype = "solid", size=0.65) +   # Regression line for the first model
  geom_smooth(data=best_test, aes(x = Predicted_Mn_M3, y = Mn_ICP), method = "lm", se = FALSE, color = "#FEB941", linetype = "solid", size=0.65) +  # Regression line for the second model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  geom_point(data=dt_Mn_worst, aes(x = Mn_PXRF, y = Mn_ICP), color = "#8B322C", size=2.5, stroke=0.6, shape=4) + # New points
  geom_smooth(data=dt_Mn_worst, aes(x = Mn_PXRF, y = Mn_ICP), method = "lm", se = FALSE, color = "#8B322C", linetype = "solid", size=0.65) +  
  labs(x = "pXRF Mn", y = "ICP concentration Mn") +
  scale_y_continuous(limits = c(0, 180), breaks = seq(0, 180, by = 35)) +
  scale_x_continuous(limits = c(0, 180), breaks = seq(0, 180, by = 35)) +
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

print(Mn)


MnZoom<- ggplot(data=dt_Mn_best, aes(x = Mn_PXRF, y = Mn_ICP)) +
  geom_point(data=dt_Mn_best, color = "#4793AF", size=2.5, stroke=0.45, shape=1) +
  geom_point(data=best_test, aes(x = Predicted_Mn_M3, y = Mn_ICP), color = "#FEB941", size=2.5, stroke=0.45, shape=3) + # New points
  geom_smooth(data=dt_Mn_best, aes(x = Mn_PXRF, y = Mn_ICP), method = "lm", se = FALSE, color = "#4793AF", linetype = "solid", size=0.45) +   # Regression line for the first model
  geom_smooth(data=best_test, aes(x = Predicted_Mn_M3, y = Mn_ICP), method = "lm", se = FALSE, color = "#FEB941", linetype = "solid", size=0.45) +  # Regression line for the second model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.45) +
  geom_point(data=dt_Mn_worst, aes(x = Mn_PXRF, y = Mn_ICP), color = "#8B322C", size=2.5, stroke=0.45, shape=4) + # New points
  geom_smooth(data=dt_Mn_worst, aes(x = Mn_PXRF, y = Mn_ICP), method = "lm", se = FALSE, color = "#8B322C", linetype = "solid", size=0.45) +  
  labs(x = "pXRF Mn", y = "ICP concentration Mn") +
  coord_cartesian(xlim = c(0, 70), ylim = c(0, 70)) +
  scale_x_continuous(breaks = seq(0, 180, by = 10)) +  # Set x-axis breaks
  scale_y_continuous(breaks = seq(0, 180, by = 10)) +
  theme_classic() +  # Using theme_classic as theme_classic2 is not part of base ggplot2
  theme(panel.grid.major = element_blank(), # Removing major grid lines
        panel.grid.minor = element_blank(), # Removing minor grid lines
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.3), # Adding border around the plot using updated argument
        axis.line = element_line(linewidth = 0.3, colour = "black"),
        plot.title = element_text(size = 16, face = "bold"),  # Customize plot title
        axis.title = element_text(size = 20),  # Customize axis labels
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 16, face = "bold"),
        legend.position = "top")

print(MnZoom)





M1Zn_train <- glm(Zn_ICP ~ Zn_PXRF, data = train_data, family = Gamma(link = "identity"))
M2Zn_train <- glm(Zn_ICP ~ Zn_PXRF + Total_Weight, data = train_data, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
M3Zn_train <- glm(Zn_ICP ~ Zn_PXRF + Substrate_RT, data = train_data, family = Gamma(link = "identity"), control = glm.control(maxit = 50))

summary(M1Zn_train)
summary(M2Zn_train)
summary(M3Zn_train)


setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/Models_pxrf/Figures and datasets MODELS/Mn datasets")
dt_Mn_best <-read.delim("Mn_best_dt.txt")
dt_Mn_worst <-read.delim("Mn_worst_dt.txt")
best_test <-read.delim("Mn_best_test.txt")

cor.test(dt_Mn_best$Mn_ICP, dt_Mn_best$Mn_PXRF, method="spearman") # 0.9644, p-val < 2.2e-16
cor.test(best_test$Mn_ICP, best_test$Predicted_Mn_M1, method="spearman") # 0.8928, p-value = 0.0123
cor.test(best_test$Mn_ICP, best_test$Predicted_Mn_M2, method="spearman") # 0.8928, p-value = 0.0123
cor.test(best_test$Mn_ICP, best_test$Predicted_Mn_M3, method="spearman") # 0.8571429, p-value = 0.02381
cor.test(dt_Mn_worst$Mn_ICP, dt_Mn_worst$Mn_PXRF, method="spearman") #804597 p-value = 1.777e-06



}
#########


dt <-read.delim("PXRF_models.txt")


cor.test(dt$Fe_ICP, dt$Fe_PXRF, method="spearman") # 0.97758 ,  p-value < 2.2e-16


Fe <- ggplot(data=dt, aes(x = Fe_PXRF, y = Fe_ICP)) +
  geom_point(data=dt, color = "#4793AF", size=2.5, stroke=0.6, shape=1) +
  geom_smooth(data=dt, aes(x = Fe_PXRF, y = Fe_ICP), method = "lm", se = FALSE, color = "#4793AF", linetype = "solid", size=0.65) +   # Regression line for the first model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  labs(x = "pXRF Fe", y = "ICP concentration Fe") +
  scale_y_continuous(limits = c(0, 4000), breaks = seq(0, 4000, by = 800)) +
  scale_x_continuous(limits = c(0, 4000), breaks = seq(0, 4000, by = 800)) +
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

print(Fe)


FeZoom <- ggplot(data=dt, aes(x = Fe_PXRF, y = Fe_ICP)) +
  geom_point(data=dt, color = "#4793AF", size=2.5, stroke=0.45, shape=1) +
  geom_smooth(data=dt, aes(x = Fe_PXRF, y = Fe_ICP), method = "lm", se = FALSE, color = "#4793AF", linetype = "solid", size=0.45) +   # Regression line for the first model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.45) +
  labs(x = "pXRF Fe", y = "ICP concentration Fe") +
  coord_cartesian(xlim = c(0, 800), ylim = c(0, 800)) +
  scale_x_continuous(breaks = seq(0, 4000, by = 200 )) +  # Set x-axis breaks
  scale_y_continuous(breaks = seq(0, 4000, by = 200)) +
  theme_classic() +  # Using theme_classic as theme_classic2 is not part of base ggplot2
  theme(panel.grid.major = element_blank(), # Removing major grid lines
        panel.grid.minor = element_blank(), # Removing minor grid lines
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.3), # Adding border around the plot using updated argument
        axis.line = element_line(linewidth = 0.3, colour = "black"),
        plot.title = element_text(size = 16, face = "bold"),  # Customize plot title
        axis.title = element_text(size = 20),  # Customize axis labels
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 16, face = "bold"),
        legend.position = "top")

print(FeZoom)



######## Titanium

{
  dt <-read.delim("PXRF_models.txt")
  
  
  dt_Ti_best <- dt %>%
    filter(
      # Include criteria indicated with '-'
      (
        
        Type_of_Sample == "leaf" |
          Form == "Forb" |
          Scientific_Name == "Isocoma acradenia" |
          ScieNamePlot == "P2Isocoma acradenia" |
          Scientific_Name == "Allionia incarnata" |
          Tube_No == "one" |
          ScieNamePlot == "P2Allionia incarnata" |
          TubeTW == "oneTW.SMALL" 
        
      ) &
        # Exclude criteria indicated with '+'
        !(
          Scientific_Name == "Eragrostis lehmanniana" |
          ScieNamePlot == "P6Eragrostis lehmanniana" |
          Type_of_Sample == "leaf-stem" |
          Form == "Grass" |
            ScieNamePlot == "P6Portulaca suffrutescens" | 
            ScieNamePlot == "P6Nultuma (Prosopis) velutina" | 
            Scientific_Name == "Portulaca suffrutescens" |
            Tube_No == "two" |
            ScieNamePlot == "P5Eragrostis lehmanniana" |
            TubeTW == "twoTW.LARGE" |
            TW_Q == "TW.LARGE"  
          )
    )
  
  
  
  dt_Ti_worst <- anti_join(dt, dt_Ti_best, by = "SampleID")
  
  dt_Ti_worst <- dt_Ti_worst %>% 
    filter(!is.na(Ti_PXRF))
  
  dt_Ti_worst <- dt_Ti_worst %>% 
    filter(!is.na(Ti_ICP))
  
  dt_Ti_best <- dt_Ti_best %>% 
    filter(!is.na(Ti_PXRF))
  
  dt_Ti_best <- dt_Ti_best %>% 
    filter(!is.na(Ti_ICP))
  
  #tylko 8 best i 25 worst. za malo na model training. Odrzucam Ti z papera. 

  
  Ti <- ggplot(data=dt_Ti_best, aes(x = Ti_PXRF, y = Ti_ICP)) +
    geom_point(data=dt_Ti_best, color = "#4793AF", size=2.5, stroke=0.6, shape=1) +
    geom_smooth(data=dt, aes(x = Ti_PXRF, y = Ti_ICP), method = "lm", se = FALSE, color = "#4793AF", linetype = "solid", size=0.65) +   # Regression line for the first model
    geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
    geom_point(data=dt_Ti_worst, aes(x = Ti_PXRF, y = Ti_ICP), color = "#8B322C", size=2.5, stroke=0.6, shape=4) + # New points
    geom_smooth(data=dt_Ti_worst, aes(x = Ti_PXRF, y = Ti_ICP), method = "lm", se = FALSE, color = "#8B322C", linetype = "solid", size=0.65) + 
    labs(x = "pXRF Ti", y = "ICP concentration Ti") +
    scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, by = 50)) +
    scale_x_continuous(limits = c(0, 200), breaks = seq(0, 200, by = 50)) +
    theme_classic() 
  print(Ti)
  
  rmse_best_RAW <- sqrt(mean((dt_Ti_best$Zn_ICP - dt_Ti_best$Ti_PXRF)^2)) # 9.5 jak zostawiam Fe.MEDIUM i Fe.VSMALL, 9.02 jak filtruje tylko VSMALL a zostawiam MEDIUM, 9.04 jak calkowicie nie ma Fe_ICP
  rmse_worst_RAW <- sqrt(mean((dt_Ti_worst$Ti_ICP - dt_Ti_worst$Ti_PXRF)^2)) # 26.5 jak zostawiam Fe.MEDIUM i Fe.VSMALL, 28.57 jak filtruje ylko VSMALL a zostawiam MEDIUM, 31 jak calkowicie nie ma Fe_ICP
  
  lm1 <- lm(dt_Ti_best$Ti_ICP~dt_Ti_best$Ti_PXRF) #R2 = 0.917 jak filtruje Fe.MEDIUM i Fe.VSMALL, 0.927 jak filtruje ylko VSMALL a zostawiam MEDIUM, 0.9136 jak calkowicie nie ma Fe_ICP
  summary(lm1)
  
  lm1 <- lm(dt_Ti_worst$Ti_ICP~dt_Ti_worst$Ti_PXRF) #R2 = 0.322 jak filtruje Fe.MEDIUM i Fe.VSMALL, 0.2839 jak filtruje ylko VSMALL a zostawiam MEDIUM, 0.2617 ak calkowicie nie ma Fe_ICP
  summary(lm1)
  
  

  
}



### Model Performances
{
setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/Models_pxrf")
dt <-read.delim("PXRF_models.txt")
setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/Models_pxrf/Figures and datasets MODELS/Cu datasets")
dt_Cu_best <-read.delim("Cu_best_dt.txt")
dt_Cu_worst <-read.delim("Cu_worst_dt.txt")
Cu_best_test <-read.delim("Cu_best_test.txt")
setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/Models_pxrf/Figures and datasets MODELS/Zn datasets")
dt_Zn_best <-read.delim("Zn_best_dt.txt")
dt_Zn_worst <-read.delim("Zn_worst_dt.txt")
Zn_best_test <-read.delim("Zn_best_test.txt")
setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/Models_pxrf/Figures and datasets MODELS/Mn datasets")
dt_Mn_best <-read.delim("Mn_best_dt.txt")
dt_Mn_worst <-read.delim("Mn_worst_dt.txt")
Mn_best_test <-read.delim("Mn_best_test.txt")




# Load necessary libraries
library(Metrics)
library(psych)    # For ICC calculation
library(openxlsx) # For Excel operations
library(dplyr)    # For data manipulation
library(tidyr)    # For pivoting data


# Define columns
icp_col <- "Cu_ICP"
pxrf_col <- "Cu_PXRF"
predicted_cols <- c("Predicted_Cu_M1", "Predicted_Cu_M2", "Predicted_Cu_M3")

calculate_metrics_df <- function(data, icp_col, pxrf_col, predicted_cols) {
  # Initialize a list for results
  results_list <- list()
  
  # Basic calculations
  rmse <- sqrt(mean((data[[icp_col]] - data[[pxrf_col]])^2, na.rm = TRUE))
  mae <- mean(abs(data[[icp_col]] - data[[pxrf_col]]), na.rm = TRUE)
  mean_icp <- mean(data[[icp_col]], na.rm = TRUE)
  nrmse <- rmse / mean_icp
  sd_icp <- sd(data[[icp_col]], na.rm = TRUE)
  rpd <- sd_icp / rmse
  lm_model <- lm(data[[icp_col]] ~ data[[pxrf_col]], data = data, na.action = na.exclude)
  r_squared <- summary(lm_model)$r.squared
  
  # ICC calculation for raw data
  icc_data <- na.omit(data.frame(data[[icp_col]], data[[pxrf_col]]))
  if (nrow(icc_data) > 1) {
    icc_result <- ICC(icc_data)
    icc_value <- icc_result$results["Single_random_raters", "ICC"]
  } else {
    icc_value <- NA  # Default to NA if not enough data
  }
  
  # Store base metrics
  results_list$RMSE <- rmse
  results_list$MAE <- mae
  results_list$NRMSE <- nrmse
  results_list$RPD <- rpd
  results_list$R_squared <- r_squared
  results_list$ICC <- icc_value
  
  # Metrics for predicted models
  for (pred_col in predicted_cols) {
    if (pred_col %in% names(data)) {
      pred_data <- na.omit(data.frame(data[[icp_col]], data[[pred_col]]))
      if (nrow(pred_data) > 1) {
        pred_rmse <- sqrt(mean((data[[icp_col]] - data[[pred_col]])^2, na.rm = TRUE))
        pred_mae <- mean(abs(data[[icp_col]] - data[[pred_col]]), na.rm = TRUE)
        pred_nrmse <- pred_rmse / mean_icp
        pred_rpd <- sd_icp / pred_rmse
        lm_model_pred <- lm(data[[icp_col]] ~ data[[pred_col]], data = data, na.action = na.exclude)
        pred_r_squared <- summary(lm_model_pred)$r.squared
        icc_pred_result <- ICC(pred_data)
        pred_icc_value <- icc_pred_result$results["Single_random_raters", "ICC"]
        
        # Store predicted model metrics
        results_list[[paste("RMSE", pred_col)]] <- pred_rmse
        results_list[[paste("MAE", pred_col)]] <- pred_mae
        results_list[[paste("NRMSE", pred_col)]] <- pred_nrmse
        results_list[[paste("RPD", pred_col)]] <- pred_rpd
        results_list[[paste("R_squared", pred_col)]] <- pred_r_squared
        results_list[[paste("ICC", pred_col)]] <- pred_icc_value
      } else {
        # Handle case where not enough data is available for prediction column
        results_list[[paste("RMSE", pred_col)]] <- NA
        results_list[[paste("MAE", pred_col)]] <- NA
        results_list[[paste("NRMSE", pred_col)]] <- NA
        results_list[[paste("RPD", pred_col)]] <- NA
        results_list[[paste("R_squared", pred_col)]] <- NA
        results_list[[paste("ICC", pred_col)]] <- NA
      }
    }
  }
  
  # Convert list to data frame for easier handling in Excel
  results_df <- as.data.frame(t(unlist(results_list)))
  colnames(results_df) <- names(results_list)
  
  return(results_df)
}

# Apply to datasets
results_best <- calculate_metrics_df(dt_Cu_best, "Cu_ICP", "Cu_PXRF", c("Predicted_Cu_M1", "Predicted_Cu_M2", "Predicted_Cu_M3"))
results_worst <- calculate_metrics_df(dt_Cu_worst, "Cu_ICP", "Cu_PXRF", c("Predicted_Cu_M1", "Predicted_Cu_M2", "Predicted_Cu_M3"))
results_test <- calculate_metrics_df(Cu_best_test, "Cu_ICP", "Cu_PXRF", c("Predicted_Cu_M1", "Predicted_Cu_M2", "Predicted_Cu_M3"))


# Create Excel workbook and add sheets with results
wb <- createWorkbook()
addWorksheet(wb, "Cu Metrics Best")
writeData(wb, "Cu Metrics Best", results_best)
addWorksheet(wb, "Cu Metrics Worst")
writeData(wb, "Cu Metrics Worst", results_worst)
addWorksheet(wb, "Cu Metrics Test")
writeData(wb, "Cu Metrics Test", results_test)

# Save the workbook with the full path
full_path <- "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/Models_pxrf/Cu_Metrics_Analysis.xlsx"
saveWorkbook(wb, full_path, overwrite = TRUE)



###### Zn




# Define columns
icp_col <- "Zn_ICP"
pxrf_col <- "Zn_PXRF"
predicted_cols <- c("Predicted_Zn_M1", "Predicted_Zn_M2", "Predicted_Zn_M3")

calculate_metrics_df <- function(data, icp_col, pxrf_col, predicted_cols) {
  # Initialize a list for results
  results_list <- list()
  
  # Basic calculations
  rmse <- sqrt(mean((data[[icp_col]] - data[[pxrf_col]])^2, na.rm = TRUE))
  mae <- mean(abs(data[[icp_col]] - data[[pxrf_col]]), na.rm = TRUE)
  mean_icp <- mean(data[[icp_col]], na.rm = TRUE)
  nrmse <- rmse / mean_icp
  sd_icp <- sd(data[[icp_col]], na.rm = TRUE)
  rpd <- sd_icp / rmse
  lm_model <- lm(data[[icp_col]] ~ data[[pxrf_col]], data = data, na.action = na.exclude)
  r_squared <- summary(lm_model)$r.squared
  
  # ICC calculation for raw data
  icc_data <- na.omit(data.frame(data[[icp_col]], data[[pxrf_col]]))
  if (nrow(icc_data) > 1) {
    icc_result <- ICC(icc_data)
    icc_value <- icc_result$results["Single_random_raters", "ICC"]
  } else {
    icc_value <- NA  # Default to NA if not enough data
  }
  
  # Store base metrics
  results_list$RMSE <- rmse
  results_list$MAE <- mae
  results_list$NRMSE <- nrmse
  results_list$RPD <- rpd
  results_list$R_squared <- r_squared
  results_list$ICC <- icc_value
  
  # Metrics for predicted models
  for (pred_col in predicted_cols) {
    if (pred_col %in% names(data)) {
      pred_data <- na.omit(data.frame(data[[icp_col]], data[[pred_col]]))
      if (nrow(pred_data) > 1) {
        pred_rmse <- sqrt(mean((data[[icp_col]] - data[[pred_col]])^2, na.rm = TRUE))
        pred_mae <- mean(abs(data[[icp_col]] - data[[pred_col]]), na.rm = TRUE)
        pred_nrmse <- pred_rmse / mean_icp
        pred_rpd <- sd_icp / pred_rmse
        lm_model_pred <- lm(data[[icp_col]] ~ data[[pred_col]], data = data, na.action = na.exclude)
        pred_r_squared <- summary(lm_model_pred)$r.squared
        icc_pred_result <- ICC(pred_data)
        pred_icc_value <- icc_pred_result$results["Single_random_raters", "ICC"]
        
        # Store predicted model metrics
        results_list[[paste("RMSE", pred_col)]] <- pred_rmse
        results_list[[paste("MAE", pred_col)]] <- pred_mae
        results_list[[paste("NRMSE", pred_col)]] <- pred_nrmse
        results_list[[paste("RPD", pred_col)]] <- pred_rpd
        results_list[[paste("R_squared", pred_col)]] <- pred_r_squared
        results_list[[paste("ICC", pred_col)]] <- pred_icc_value
      } else {
        # Handle case where not enough data is available for prediction column
        results_list[[paste("RMSE", pred_col)]] <- NA
        results_list[[paste("MAE", pred_col)]] <- NA
        results_list[[paste("NRMSE", pred_col)]] <- NA
        results_list[[paste("RPD", pred_col)]] <- NA
        results_list[[paste("R_squared", pred_col)]] <- NA
        results_list[[paste("ICC", pred_col)]] <- NA
      }
    }
  }
  
  # Convert list to data frame for easier handling in Excel
  results_df <- as.data.frame(t(unlist(results_list)))
  colnames(results_df) <- names(results_list)
  
  return(results_df)
}

# Apply to datasets
results_best <- calculate_metrics_df(dt_Zn_best, "Zn_ICP", "Zn_PXRF", c("Predicted_Zn_M1", "Predicted_Zn_M2", "Predicted_Zn_M3"))
results_worst <- calculate_metrics_df(dt_Zn_worst, "Zn_ICP", "Zn_PXRF", c("Predicted_Zn_M1", "Predicted_Zn_M2", "Predicted_Zn_M3"))
results_test <- calculate_metrics_df(Zn_best_test, "Zn_ICP", "Zn_PXRF", c("Predicted_Zn_M1", "Predicted_Zn_M2", "Predicted_Zn_M3"))


# Create Excel workbook and add sheets with results
wb <- createWorkbook()
addWorksheet(wb, "Zn Metrics Best")
writeData(wb, "Zn Metrics Best", results_best)
addWorksheet(wb, "Zn Metrics Worst")
writeData(wb, "Zn Metrics Worst", results_worst)
addWorksheet(wb, "Zn Metrics Test")
writeData(wb, "Zn Metrics Test", results_test)

# Save the workbook with the full path
full_path <- "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/Models_pxrf/Zn_Metrics_Analysis.xlsx"
saveWorkbook(wb, full_path, overwrite = TRUE)







###### Mn




# Define columns
icp_col <- "Mn_ICP"
pxrf_col <- "Mn_PXRF"
predicted_cols <- c("Predicted_Mn_M1", "Predicted_Mn_M2", "Predicted_Mn_M3")

calculate_metrics_df <- function(data, icp_col, pxrf_col, predicted_cols) {
  # Initialize a list for results
  results_list <- list()
  
  # Basic calculations
  rmse <- sqrt(mean((data[[icp_col]] - data[[pxrf_col]])^2, na.rm = TRUE))
  mae <- mean(abs(data[[icp_col]] - data[[pxrf_col]]), na.rm = TRUE)
  mean_icp <- mean(data[[icp_col]], na.rm = TRUE)
  nrmse <- rmse / mean_icp
  sd_icp <- sd(data[[icp_col]], na.rm = TRUE)
  rpd <- sd_icp / rmse
  lm_model <- lm(data[[icp_col]] ~ data[[pxrf_col]], data = data, na.action = na.exclude)
  r_squared <- summary(lm_model)$r.squared
  
  # ICC calculation for raw data
  icc_data <- na.omit(data.frame(data[[icp_col]], data[[pxrf_col]]))
  if (nrow(icc_data) > 1) {
    icc_result <- ICC(icc_data)
    icc_value <- icc_result$results["Single_random_raters", "ICC"]
  } else {
    icc_value <- NA  # Default to NA if not enough data
  }
  
  # Store base metrics
  results_list$RMSE <- rmse
  results_list$MAE <- mae
  results_list$NRMSE <- nrmse
  results_list$RPD <- rpd
  results_list$R_squared <- r_squared
  results_list$ICC <- icc_value
  
  # Metrics for predicted models
  for (pred_col in predicted_cols) {
    if (pred_col %in% names(data)) {
      pred_data <- na.omit(data.frame(data[[icp_col]], data[[pred_col]]))
      if (nrow(pred_data) > 1) {
        pred_rmse <- sqrt(mean((data[[icp_col]] - data[[pred_col]])^2, na.rm = TRUE))
        pred_mae <- mean(abs(data[[icp_col]] - data[[pred_col]]), na.rm = TRUE)
        pred_nrmse <- pred_rmse / mean_icp
        pred_rpd <- sd_icp / pred_rmse
        lm_model_pred <- lm(data[[icp_col]] ~ data[[pred_col]], data = data, na.action = na.exclude)
        pred_r_squared <- summary(lm_model_pred)$r.squared
        icc_pred_result <- ICC(pred_data)
        pred_icc_value <- icc_pred_result$results["Single_random_raters", "ICC"]
        
        # Store predicted model metrics
        results_list[[paste("RMSE", pred_col)]] <- pred_rmse
        results_list[[paste("MAE", pred_col)]] <- pred_mae
        results_list[[paste("NRMSE", pred_col)]] <- pred_nrmse
        results_list[[paste("RPD", pred_col)]] <- pred_rpd
        results_list[[paste("R_squared", pred_col)]] <- pred_r_squared
        results_list[[paste("ICC", pred_col)]] <- pred_icc_value
      } else {
        # Handle case where not enough data is available for prediction column
        results_list[[paste("RMSE", pred_col)]] <- NA
        results_list[[paste("MAE", pred_col)]] <- NA
        results_list[[paste("NRMSE", pred_col)]] <- NA
        results_list[[paste("RPD", pred_col)]] <- NA
        results_list[[paste("R_squared", pred_col)]] <- NA
        results_list[[paste("ICC", pred_col)]] <- NA
      }
    }
  }
  
  # Convert list to data frame for easier handling in Excel
  results_df <- as.data.frame(t(unlist(results_list)))
  colnames(results_df) <- names(results_list)
  
  return(results_df)
}

# Apply to datasets
results_best <- calculate_metrics_df(dt_Mn_best, "Mn_ICP", "Mn_PXRF", c("Predicted_Mn_M1", "Predicted_Mn_M2", "Predicted_Mn_M3"))
results_worst <- calculate_metrics_df(dt_Mn_worst, "Mn_ICP", "Mn_PXRF", c("Predicted_Mn_M1", "Predicted_Mn_M2", "Predicted_Mn_M3"))
results_test <- calculate_metrics_df(Mn_best_test, "Mn_ICP", "Mn_PXRF", c("Predicted_Mn_M1", "Predicted_Mn_M2", "Predicted_Mn_M3"))


# Create Excel workbook and add sheets with results
wb <- createWorkbook()
addWorksheet(wb, "Mn Metrics Best")
writeData(wb, "Mn Metrics Best", results_best)
addWorksheet(wb, "Mn Metrics Worst")
writeData(wb, "Mn Metrics Worst", results_worst)
addWorksheet(wb, "Mn Metrics Test")
writeData(wb, "Mn Metrics Test", results_test)

# Save the workbook with the full path
full_path <- "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/Models_pxrf/Mn_Metrics_Analysis.xlsx"
saveWorkbook(wb, full_path, overwrite = TRUE)






#### Fe


# Define columns
icp_col <- "Fe_ICP"
pxrf_col <- "Fe_PXRF"


calculate_metrics_df <- function(data, icp_col, pxrf_col) {
  # Initialize a list for results
  results_list <- list()
  
  # Basic calculations
  rmse <- sqrt(mean((data[[icp_col]] - data[[pxrf_col]])^2, na.rm = TRUE))
  mae <- mean(abs(data[[icp_col]] - data[[pxrf_col]]), na.rm = TRUE)
  mean_icp <- mean(data[[icp_col]], na.rm = TRUE)
  nrmse <- rmse / mean_icp
  sd_icp <- sd(data[[icp_col]], na.rm = TRUE)
  rpd <- sd_icp / rmse
  lm_model <- lm(data[[icp_col]] ~ data[[pxrf_col]], data = data, na.action = na.exclude)
  r_squared <- summary(lm_model)$r.squared
  
  # ICC calculation for raw data
  icc_data <- na.omit(data.frame(data[[icp_col]], data[[pxrf_col]]))
  if (nrow(icc_data) > 1) {
    icc_result <- ICC(icc_data)
    icc_value <- icc_result$results["Single_random_raters", "ICC"]
  } else {
    icc_value <- NA  # Default to NA if not enough data
  }
  
  # Store base metrics
  results_list$RMSE <- rmse
  results_list$MAE <- mae
  results_list$NRMSE <- nrmse
  results_list$RPD <- rpd
  results_list$R_squared <- r_squared
  results_list$ICC <- icc_value
  

  # Convert list to data frame for easier handling in Excel
  results_df <- as.data.frame(t(unlist(results_list)))
  colnames(results_df) <- names(results_list)
  
  return(results_df)
}

# Apply to datasets
results_best <- calculate_metrics_df(dt, "Fe_ICP", "Fe_PXRF")




# Create Excel workbook and add sheets with results
wb <- createWorkbook()
addWorksheet(wb, "Fe Metrics Best")
writeData(wb, "Fe Metrics Best", results_best)


# Save the workbook with the full path
full_path <- "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/Models_pxrf/Fe_Metrics_Analysis.xlsx"
saveWorkbook(wb, full_path, overwrite = TRUE)




#### Se


# Define columns
icp_col <- "Se_ICP"
pxrf_col <- "Se_PXRF"


calculate_metrics_df <- function(data, icp_col, pxrf_col) {
  # Initialize a list for results
  results_list <- list()
  
  # Basic calculations
  rmse <- sqrt(mean((data[[icp_col]] - data[[pxrf_col]])^2, na.rm = TRUE))
  mae <- mean(abs(data[[icp_col]] - data[[pxrf_col]]), na.rm = TRUE)
  mean_icp <- mean(data[[icp_col]], na.rm = TRUE)
  nrmse <- rmse / mean_icp
  sd_icp <- sd(data[[icp_col]], na.rm = TRUE)
  rpd <- sd_icp / rmse
  lm_model <- lm(data[[icp_col]] ~ data[[pxrf_col]], data = data, na.action = na.exclude)
  r_squared <- summary(lm_model)$r.squared
  
  # ICC calculation for raw data
  icc_data <- na.omit(data.frame(data[[icp_col]], data[[pxrf_col]]))
  if (nrow(icc_data) > 1) {
    icc_result <- ICC(icc_data)
    icc_value <- icc_result$results["Single_random_raters", "ICC"]
  } else {
    icc_value <- NA  # Default to NA if not enough data
  }
  
  # Store base metrics
  results_list$RMSE <- rmse
  results_list$MAE <- mae
  results_list$NRMSE <- nrmse
  results_list$RPD <- rpd
  results_list$R_squared <- r_squared
  results_list$ICC <- icc_value
  
  
  # Convert list to data frame for easier handling in Excel
  results_df <- as.data.frame(t(unlist(results_list)))
  colnames(results_df) <- names(results_list)
  
  return(results_df)
}

# Apply to datasets
results_best <- calculate_metrics_df(dt, "Se_ICP", "Se_PXRF")




# Create Excel workbook and add sheets with results
wb <- createWorkbook()
addWorksheet(wb, "Se Metrics Best")
writeData(wb, "Se Metrics Best", results_best)


# Save the workbook with the full path
full_path <- "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/Models_pxrf/Se_Metrics_Analysis.xlsx"
saveWorkbook(wb, full_path, overwrite = TRUE)






#### Re


# Define columns
icp_col <- "Re_ICP"
pxrf_col <- "Re_PXRF"


calculate_metrics_df <- function(data, icp_col, pxrf_col) {
  # Initialize a list for results
  results_list <- list()
  
  # Basic calculations
  rmse <- sqrt(mean((data[[icp_col]] - data[[pxrf_col]])^2, na.rm = TRUE))
  mae <- mean(abs(data[[icp_col]] - data[[pxrf_col]]), na.rm = TRUE)
  mean_icp <- mean(data[[icp_col]], na.rm = TRUE)
  nrmse <- rmse / mean_icp
  sd_icp <- sd(data[[icp_col]], na.rm = TRUE)
  rpd <- sd_icp / rmse
  lm_model <- lm(data[[icp_col]] ~ data[[pxrf_col]], data = data, na.action = na.exclude)
  r_squared <- summary(lm_model)$r.squared
  
  # ICC calculation for raw data
  icc_data <- na.omit(data.frame(data[[icp_col]], data[[pxrf_col]]))
  if (nrow(icc_data) > 1) {
    icc_result <- ICC(icc_data)
    icc_value <- icc_result$results["Single_random_raters", "ICC"]
  } else {
    icc_value <- NA  # Default to NA if not enough data
  }
  
  # Store base metrics
  results_list$RMSE <- rmse
  results_list$MAE <- mae
  results_list$NRMSE <- nrmse
  results_list$RPD <- rpd
  results_list$R_squared <- r_squared
  results_list$ICC <- icc_value
  
  
  # Convert list to data frame for easier handling in Excel
  results_df <- as.data.frame(t(unlist(results_list)))
  colnames(results_df) <- names(results_list)
  
  return(results_df)
}

# Apply to datasets
results_best <- calculate_metrics_df(dt, "Re_ICP", "Re_PXRF")




# Create Excel workbook and add sheets with results
wb <- createWorkbook()
addWorksheet(wb, "Re Metrics Best")
writeData(wb, "Re Metrics Best", results_best)


# Save the workbook with the full path
full_path <- "C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/Models_pxrf/Re_Metrics_Analysis.xlsx"
saveWorkbook(wb, full_path, overwrite = TRUE)



#testing significance

dt_ICC <- dt[, c("Se_ICP", "Se_PXRF")]
ICC(dt_ICC, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE)
dt_ICC <- dt[, c("Re_ICP", "Re_PXRF")]
ICC(dt_ICC, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE)
dt_ICC <- dt[, c("Fe_ICP", "Fe_PXRF")]
ICC(dt_ICC, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE)

dt_ICC <- dt_Cu_best[, c("Cu_ICP", "Cu_PXRF")]
ICC(dt_ICC, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE)
dt_ICC <- dt_Cu_worst[, c("Cu_ICP", "Cu_PXRF")]
ICC(dt_ICC, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE)
dt_ICC <- Cu_best_test[, c("Cu_ICP", "Predicted_Cu_M3")]
ICC(dt_ICC, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE)
dt_ICC <- Zn_best_test[, c("Zn_ICP", "Predicted_Zn_M3")]
ICC(dt_ICC, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE)
dt_ICC <- dt_Zn_best[, c("Zn_ICP", "Zn_PXRF")]
ICC(dt_ICC, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE)
dt_ICC <- dt_Zn_worst[, c("Zn_ICP", "Zn_PXRF")]
ICC(dt_ICC, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE)


dt_ICC <- Mn_best_test[, c("Mn_ICP", "Predicted_Mn_M3")]
ICC(dt_ICC, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE)
dt_ICC <- dt_Mn_best[, c("Mn_ICP", "Mn_PXRF")]
ICC(dt_ICC, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE)


lm1 <- lm(dt$Fe_ICP~dt$Fe_PXRF) #R2 = 0.933, 0.934 z Mn
summary(lm1)

lm1 <- lm(dt_Cu_best$Cu_ICP~dt_Cu_best$Cu_PXRF) #R2 = 0.933, 0.934 z Mn
summary(lm1)

lm1 <- lm(dt_Zn_worst$Zn_ICP~dt_Zn_worst$Zn_PXRF) #R2 = 0.933, 0.934 z Mn
summary(lm1)

lm1 <- lm(dt_Mn_best$Mn_ICP~dt_Mn_best$Mn_PXRF) #R2 = 0.933, 0.934 z Mn
summary(lm1)

lm1 <- lm(Zn_best_test$Zn_ICP~Zn_best_test$Predicted_Zn_M3) #R2 = 0.933, 0.934 z Mn
summary(lm1)

lm1 <- lm(Mn_best_test$Mn_ICP~Mn_best_test$Predicted_Mn_M3) #R2 = 0.933, 0.934 z Mn
summary(lm1)

}



## Round up performance Table
{
setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/1_Manuscript_Analysis/Models_pxrf")
dt <-read.delim("Performance_Table_roundup.txt")

# Assuming 'dt' is your data frame
dt[, 3:8] <- round(dt[, 3:8], digits = 2)  # Replace '2' with the number of decimal places you want

#write_xlsx(dt, "performance-round-3digits.xlsx")
#write_xlsx(dt, "performance-round-2digits.xlsx")
}


