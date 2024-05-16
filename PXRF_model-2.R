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

#Remove errorous plants from dt_Cu_worst - YES

dt_Cu_worst <- dt_Cu_worst %>% 
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
  
  #write_xlsx(best_train, "Cu_best_train.xlsx") 
  #write_xlsx(best_test, "Cu_best_test.xlsx")
  #write_xlsx(dt_Cu_best, "Cu_best_dt.xlsx")
  #write_xlsx(dt_Cu_worst, "Cu_worst_dt.xlsx")
  

  #write.table(best_train, "Cu_best_train.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
  #write.table(best_test, "Cu_best_test.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
  #write.table(dt_Cu_best, "Cu_best_dt.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
  #write.table(dt_Cu_worst, "Cu_worst_dt.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
  
  }


cor.test(dt_Cu_best$Cu_ICP, dt_Cu_best$Cu_PXRF, method="spearman") # 0.97158, p-val < 2.2e-16
cor.test(best_test$Cu_ICP, best_test$Predicted_Cu_M1, method="spearman") # 0.7942799, p-value = 0.0006912
cor.test(best_test$Cu_ICP, best_test$Predicted_Cu_M2, method="spearman") # 0.7934066 , p-value = 0.001151
cor.test(best_test$Cu_ICP, best_test$Predicted_Cu_M3, method="spearman") # 0.8769231 , p-value = < 2.2e-16
cor.test(best_test$Cu_ICP, best_test$Predicted_Cu_M4, method="spearman") # 0.8725275  ,  p-value < 2.2e-16

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


pzoom <- ggplot(data=dt_Cu_best, aes(x = Cu_PXRF, y = Cu_ICP)) +
  geom_point(data=dt_Cu_best, color = "#4793AF", size=2.5, stroke=0.6, shape=1) +
  geom_point(data=best_test, aes(x = Predicted_Cu_M3, y = Cu_ICP), color = "#FEB941", size=2.5, stroke=0.6, shape=3) + # New points
  geom_smooth(data=dt_Cu_best, aes(x = Cu_PXRF, y = Cu_ICP), method = "lm", se = FALSE, color = "#4793AF", linetype = "solid", size=0.65) +   # Regression line for the first model
  geom_smooth(data=best_test, aes(x = Predicted_Cu_M3, y = Cu_ICP), method = "lm", se = FALSE, color = "#FEB941", linetype = "solid", size=0.65) +  # Regression line for the second model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  geom_point(data=dt_Cu_worst, aes(x = Cu_PXRF, y = Cu_ICP), color = "#8B322C", size=2.5, stroke=0.6, shape=4) + # New points
  geom_smooth(data=dt_Cu_worst, aes(x = Cu_PXRF, y = Cu_ICP), method = "lm", se = FALSE, color = "#8B322C", linetype = "solid", size=0.65) +  
  labs(x = "pXRF Cu", y = "ICP concentration Cu") +
  coord_cartesian(xlim = c(0, 150), ylim = c(0, 150)) +
  scale_x_continuous(breaks = seq(0, 150, by = 25)) +  # Set x-axis breaks
  scale_y_continuous(breaks = seq(0, 150, by = 25)) +
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

print(pzoom)

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
    geom_point(data=dt, color = "#4793AF", size=2.5, stroke=0.6, shape=1) +
    geom_smooth(data=dt, aes(x = Se_PXRF, y = Se_ICP), method = "lm", se = FALSE, color = "#4793AF", linetype = "solid", size=0.65) +   # Regression line for the first model
    geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
    labs(x = "pXRF Se", y = "ICP concentration Se") +
    coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
    scale_x_continuous(breaks = seq(0, 10, by = 2)) +  # Set x-axis breaks
    scale_y_continuous(breaks = seq(0, 10, by = 2)) +
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
  
  print(Sezoom)
  
  
  
  
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
  
  
  # Model Performance - RMSE
  #rmse_best_RAW <- sqrt(mean((dt_Re_best$Re_ICP - dt_Re_best$Re_PXRF)^2)) # not much difference between those two, 2 values of RMSE
  #rmse_worst_RAW <- sqrt(mean((dt_Re_worst$Re_ICP - dt_Re_worst$Re_PXRF)^2))
  
  
  
  
  Re <- ggplot(data=dt_Re_best, aes(x = Re_PXRF, y = Re_ICP)) +
    geom_point(data=dt_Re_best, color = "#4793AF", size=2.5, stroke=0.6, shape=1) +
    # geom_point(data=best_test, aes(x = Predicted_Cu_M3, y = Cu_ICP), color = "#AD0B0B", size=2.5, stroke=0.6, shape=3) + # New points
    geom_smooth(data=dt_Re_best, aes(x = Re_PXRF, y = Re_ICP), method = "lm", se = FALSE, color = "#4793AF", linetype = "solid", size=0.65) +   # Regression line for the first model
    # geom_smooth(data=best_test, aes(x = Predicted_Cu_M3, y = Cu_ICP), method = "lm", se = FALSE, color = "#AD0B0B", linetype = "solid", size=0.65) +  # Regression line for the second model
    geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
    geom_point(data=dt_Re_worst, aes(x = Re_PXRF, y = Re_ICP), color = "#8B322C", size=2.5, stroke=0.6, shape=4) + # New points
    geom_smooth(data=dt_Re_worst, aes(x = Re_PXRF, y = Re_ICP), method = "lm", se = FALSE, color = "#8B322C", linetype = "solid", size=0.65) +  
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
  
  
  Rezoom <- ggplot(data=dt_Re_best, aes(x = Re_PXRF, y = Re_ICP)) +
    geom_point(data=dt_Re_best, color = "#4793AF", size=2.5, stroke=0.6, shape=1) +
    # geom_point(data=best_test, aes(x = Predicted_Cu_M3, y = Cu_ICP), color = "#AD0B0B", size=2.5, stroke=0.6, shape=3) + # New points
    geom_smooth(data=dt_Re_best, aes(x = Re_PXRF, y = Re_ICP), method = "lm", se = FALSE, color = "#4793AF", linetype = "solid", size=0.65) +   # Regression line for the first model
    # geom_smooth(data=best_test, aes(x = Predicted_Cu_M3, y = Cu_ICP), method = "lm", se = FALSE, color = "#AD0B0B", linetype = "solid", size=0.65) +  # Regression line for the second model
    geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
    geom_point(data=dt_Re_worst, aes(x = Re_PXRF, y = Re_ICP), color = "#8B322C", size=2.5, stroke=0.6, shape=4) + # New points
    geom_smooth(data=dt_Re_worst, aes(x = Re_PXRF, y = Re_ICP), method = "lm", se = FALSE, color = "#8B322C", linetype = "solid", size=0.65) +  
    labs(x = "pXRF Re", y = "ICP concentration Re") +
    coord_cartesian(xlim = c(0, 25), ylim = c(0, 25)) +
    scale_x_continuous(breaks = seq(0, 25, by = 5)) +  # Set x-axis breaks
    scale_y_continuous(breaks = seq(0, 25, by = 5)) +
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
  
  print(Rezoom)
  
}


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
  train_indices <- sample(1:nrow(dt_Zn_worst), 0.8 * nrow(dt_Zn_worst))
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


write_xlsx(best_train, "Zn_best_train.xlsx") 
write_xlsx(best_test, "Zn_best_test.xlsx")
write_xlsx(dt_Zn_best, "Zn_best_dt.xlsx")
write_xlsx(dt_Zn_worst, "Zn_worst_dt.xlsx")


write.table(best_train, "Zn_best_train.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
write.table(best_test, "Zn_best_test.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
write.table(dt_Zn_best, "Zn_best_dt.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
write.table(dt_Zn_worst, "Zn_worst_dt.txt", sep = "\t", row.names = FALSE, col.names = TRUE)



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



Znzoom <- ggplot(data=dt_Zn_best, aes(x = Zn_PXRF, y = Zn_ICP)) +
  geom_point(data=dt_Zn_best, color = "#4793AF", size=2.5, stroke=0.6, shape=1) +
  geom_point(data=best_test, aes(x = Predicted_Zn_M3, y = Zn_ICP), color = "#FEB941", size=2.5, stroke=0.6, shape=3) + # New points
  geom_smooth(data=dt_Zn_best, aes(x = Zn_PXRF, y = Zn_ICP), method = "lm", se = FALSE, color = "#4793AF", linetype = "solid", size=0.65) +   # Regression line for the first model
  geom_smooth(data=best_test, aes(x = Predicted_Zn_M3, y = Zn_ICP), method = "lm", se = FALSE, color = "#FEB941", linetype = "solid", size=0.65) +  # Regression line for the second model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  geom_point(data=dt_Zn_worst, aes(x = Zn_PXRF, y = Zn_ICP), color = "#8B322C", size=2.5, stroke=0.6, shape=4) + # New points
  geom_smooth(data=dt_Zn_worst, aes(x = Zn_PXRF, y = Zn_ICP), method = "lm", se = FALSE, color = "#8B322C", linetype = "solid", size=0.65) +  
  labs(x = "pXRF Zn", y = "ICP concentration Zn") +
  coord_cartesian(xlim = c(0, 40), ylim = c(0, 40)) +
  scale_x_continuous(breaks = seq(0, 100, by = 5)) +  # Set x-axis breaks
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
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

print(Znzoom)



M1Zn_train <- glm(Zn_ICP ~ Zn_PXRF, data = train_data, family = Gamma(link = "identity"))
M2Zn_train <- glm(Zn_ICP ~ Zn_PXRF + Total_Weight, data = train_data, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
M3Zn_train <- glm(Zn_ICP ~ Zn_PXRF + Substrate_RT, data = train_data, family = Gamma(link = "identity"), control = glm.control(maxit = 50))

summary(M1Zn_train)
summary(M2Zn_train)
summary(M3Zn_train)


######### Mn
{
dt <-read.delim("PXRF_models.txt")


dt_Mn_best <- dt %>%
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



dt_Mn_worst <- anti_join(dt, dt_Mn_best, by = "SampleID")

dt_Mn_worst <- dt_Mn_worst %>% 
  filter(!is.na(Zn_PXRF))

dt_Mn_best <- dt_Mn_best %>% 
  filter(!is.na(Mn_PXRF))

ggplot(data=dt_Mn_best, aes(x = Mn_PXRF, y = Mn_ICP)) +
  geom_point(data=dt_Mn_best, color = "#003f5c", size=2.5, stroke=0.6, shape=1) +
  geom_smooth(data=dt_Mn_best, aes(x = Mn_PXRF, y =Mn_ICP), method = "lm", se = FALSE, color = "#003f5c", linetype = "solid", size=0.65) +   # Regression line for the first model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  geom_point(data=dt_Mn_worst, aes(x = Mn_PXRF, y = Mn_ICP), color = "green", size=2.5, stroke=0.6, shape=4) + # New points
  geom_smooth(data=dt_Mn_worst, aes(x = Mn_PXRF, y = Mn_ICP), method = "lm", se = FALSE, color = "green", linetype = "solid", size=0.65) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25)) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25)) +
  labs(x = "pXRF ", y = "ICP") +
  theme_classic()  


rmse_best_RAW <- sqrt(mean((dt_Mn_best$Mn_ICP - dt_Mn_best$Zn_PXRF)^2)) # 9.5 jak zostawiam Fe.MEDIUM i Fe.VSMALL, 9.02 jak filtruje tylko VSMALL a zostawiam MEDIUM, 9.04 jak calkowicie nie ma Fe_ICP
rmse_worst_RAW <- sqrt(mean((dt_Zn_worst$Mn_ICP - dt_Mn_worst$Mn_PXRF)^2)) # 26.5 jak zostawiam Fe.MEDIUM i Fe.VSMALL, 28.57 jak filtruje ylko VSMALL a zostawiam MEDIUM, 31 jak calkowicie nie ma Fe_ICP

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
  train_indices <- sample(1:nrow(dt_Zn_worst), 0.8 * nrow(dt_Zn_worst))
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


write_xlsx(best_train, "Zn_best_train.xlsx") 
write_xlsx(best_test, "Zn_best_test.xlsx")
write_xlsx(dt_Zn_best, "Zn_best_dt.xlsx")
write_xlsx(dt_Zn_worst, "Zn_worst_dt.xlsx")


write.table(best_train, "Zn_best_train.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
write.table(best_test, "Zn_best_test.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
write.table(dt_Zn_best, "Zn_best_dt.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
write.table(dt_Zn_worst, "Zn_worst_dt.txt", sep = "\t", row.names = FALSE, col.names = TRUE)



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



Znzoom <- ggplot(data=dt_Zn_best, aes(x = Zn_PXRF, y = Zn_ICP)) +
  geom_point(data=dt_Zn_best, color = "#4793AF", size=2.5, stroke=0.6, shape=1) +
  geom_point(data=best_test, aes(x = Predicted_Zn_M3, y = Zn_ICP), color = "#FEB941", size=2.5, stroke=0.6, shape=3) + # New points
  geom_smooth(data=dt_Zn_best, aes(x = Zn_PXRF, y = Zn_ICP), method = "lm", se = FALSE, color = "#4793AF", linetype = "solid", size=0.65) +   # Regression line for the first model
  geom_smooth(data=best_test, aes(x = Predicted_Zn_M3, y = Zn_ICP), method = "lm", se = FALSE, color = "#FEB941", linetype = "solid", size=0.65) +  # Regression line for the second model
  geom_abline(intercept = 0, slope = 1, color = "darkgrey",linetype = "dashed", linewidth=0.65) +
  geom_point(data=dt_Zn_worst, aes(x = Zn_PXRF, y = Zn_ICP), color = "#8B322C", size=2.5, stroke=0.6, shape=4) + # New points
  geom_smooth(data=dt_Zn_worst, aes(x = Zn_PXRF, y = Zn_ICP), method = "lm", se = FALSE, color = "#8B322C", linetype = "solid", size=0.65) +  
  labs(x = "pXRF Zn", y = "ICP concentration Zn") +
  coord_cartesian(xlim = c(0, 40), ylim = c(0, 40)) +
  scale_x_continuous(breaks = seq(0, 100, by = 5)) +  # Set x-axis breaks
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
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

print(Znzoom)



M1Zn_train <- glm(Zn_ICP ~ Zn_PXRF, data = train_data, family = Gamma(link = "identity"))
M2Zn_train <- glm(Zn_ICP ~ Zn_PXRF + Total_Weight, data = train_data, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
M3Zn_train <- glm(Zn_ICP ~ Zn_PXRF + Substrate_RT, data = train_data, family = Gamma(link = "identity"), control = glm.control(maxit = 50))

summary(M1Zn_train)
summary(M2Zn_train)
summary(M3Zn_train)


}
#########



