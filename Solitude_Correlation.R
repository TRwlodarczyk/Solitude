# Soolitude New Summaries
# Tomasz Wlodarczyk
# twlodarczyk@arizona.edu
# 2023-04-23

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
}

setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New")
dt <-read.delim("Solitude_pXRF_ICP_correl.txt")


{
  tr <- matrix(data = NA, ncol = ncol(dt[,c(1:67)]), nrow=nrow(dt)) # select all columns 1:46
  colnames(tr) <- colnames(dt[,c(1:67)])
  for (i in 13:46) # select when the concentrations start
  {
    tr[,c(i)] <- gsub(".*ND.*", 0, dt[,i])
  }
  
  for(i in c(1:12, 47:67)) # select columns that need to stay the same 1:11 include character and double (weight)
  {
    tr[,c(i)] <- dt[,c(i)]
  }
  tr   
  
  #transform to dataframe
  tr <- as.data.frame.matrix(tr) #A correct command to change the dataset to dataframe after transformations
  tr[,13:67] <- sapply(tr[,13:67],as.numeric) # Change a character to numeric (double)
  typeof(tr$Cu_concentration) # confirm the value is no longer a character
}

{
tr$Cl_concentration[tr$Cl_concentration == 0] <- 50/2
tr$Ca_concentration[tr$Ca_concentration == 0] <- 10/2
tr$Ti_concentration[tr$Ti_concentration == 0] <- 5/2
tr$Cr_concentration[tr$Cr_concentration == 0] <- 2/2
tr$Mn_concentration[tr$Mn_concentration == 0] <- 1/2
tr$Fe_concentration[tr$Fe_concentration == 0] <- 5/2
tr$Co_concentration[tr$Co_concentration == 0] <- 3/2
tr$Ni_concentration[tr$Ni_concentration == 0] <- 0.2/2
tr$Cu_concentration[tr$Cu_concentration == 0] <- 0.5/2
tr$Zn_concentration[tr$Zn_concentration == 0] <- 0.6/2
tr$As_concentration[tr$As_concentration == 0] <- 0.1/2
tr$Se_concentration[tr$Se_concentration == 0] <- 0.1/2
tr$Cd_concentration[tr$Cd_concentration == 0] <- 1/2
tr$Re_concentration[tr$Re_concentration == 0] <- 0.5/2
tr$Hg_concentration[tr$Hg_concentration == 0] <- 0.3/2
tr$Tl_concentration[tr$Tl_concentration == 0] <- 1/2
tr$Pb_concentration[tr$Pb_concentration == 0] <- 0.2/2
}

write.table(tr, file='C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/SLT_pXRF_ICP.csv', sep=",", row.names = F)


# remove LODs rows

tr_Cu <- tr[tr$Cu_concentration != 0.25, ]

write.table(tr_Cu, file='C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/SLT_tr_Cu.csv', sep=",", row.names = F)


#Normality
shapiro.test(tr_Cu$Cu_concentration)
shapiro.test(tr_Cu$Cu_ICP)

cor.test(tr_Cu$Cu_concentration, tr_Cu$Cu_ICP, method="spearman")

# Plot the correlation
plot(tr_Cu$Cu_concentration ~ tr_Cu$Cu_ICP, xlab = "Cu_ICP", ylab = "Cu_concentration")

# Add labels for each point using Sample.ID
text(tr_Cu$Cu_ICP, tr_Cu$Cu_concentration, labels = tr_Cu$Sample.ID, pos = 3)

#################################################################
# Scatter plot
plot(tr_Cu$Cu_ICP, tr_Cu$Cu_concentration, xlab = "Cu_ICP", ylab = "Cu_concentration", main = "Scatter Plot")

# Residuals
residuals <- tr_Cu$Cu_concentration - tr_Cu$Cu_ICP
points(tr_Cu$Cu_ICP, residuals, col = "red", pch = 16)



# Residuals
residuals <- tr_Cu$Cu_concentration - tr_Cu$Cu_ICP

# Histogram
hist(residuals, breaks = "FD", main = "Histogram of Residuals", xlab = "Residuals")

# Density plot
plot(density(residuals), main = "Density Plot of Residuals", xlab = "Residuals")




# Calculate the errors
errors <- tr_Cu$Cu_concentration - tr_Cu$Cu_ICP

# Mean Absolute Error (MAE)
mae <- mean(abs(errors))
cat("Mean Absolute Error (MAE):", mae, "\n")

# Mean Squared Error (MSE)
mse <- mean(errors^2)
cat("Mean Squared Error (MSE):", mse, "\n")



# Calculate the average of measurements
average <- (tr_Cu$Cu_concentration + tr_Cu$Cu_ICP) / 2

# Calculate the difference (error)
difference <- tr_Cu$Cu_concentration - tr_Cu$Cu_ICP

# Bland-Altman plot
plot(average, difference, xlab = "Average", ylab = "Difference", main = "Bland-Altman Plot")
abline(h = mean(difference), col = "red", lty = 2)




# QQ plot
qqnorm(tr_Cu$Cu_concentration - tr_Cu$Cu_ICP)
qqline(tr_Cu$Cu_concentration - tr_Cu$Cu_ICP, col = "red")






subset_1_tr_Cu <- tr_Cu[tr_Cu$Total_Weight > 1, ]

cor.test(subset_tr_Cu$Cu_concentration, subset_tr_Cu$Cu_ICP, method="spearman")




