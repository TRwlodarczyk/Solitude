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

{
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

}




dt <- read.delim("SLT_pXRF_ICP.txt")

#Cu boxplots
{
Cu <- ggplot(dt, aes(x = reorder(Scientific_Name, Cu_ICP, FUN = median),
                             y = Cu_ICP, Sceintific_Name=Scientific_Name)) +
  geom_boxplot() +
  geom_point(size = 1.4, stroke = 1, aes(color = Plot, shape = Form, fill = Plot)) +
  scale_shape_manual(values = c(21, 24, 23, 25)) +
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  #geom_hline(yintercept = 40, linetype = "dashed", color = "#9a9a9a", size = 1.2) +
  #geom_hline(yintercept = 300, linetype = "dotdash", color = "#454545", size = 1.2) +
  coord_flip() +
  scale_x_discrete(guide = guide_axis(angle = 0)) +
  #scale_y_continuous(limits = c(0, 600), breaks = seq(0, 600, by = 50)) +
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


#dt_selected <- dt[dt$Scientific_Name %in% c("Xanthisma gracile", "Pseudognaphalium canescens", "Boechera perennans", "Nultuma (Prosopis) velutina")] 
dt_10ppm <- dt[dt$Cu_concentration > 10,]

# Convert Sample_Name to an ordered factor based on Cu_concentration
dt_10ppm$Sample_Name <- reorder(dt_10ppm$Sample_Name, dt_10ppm$Cu_concentration)

corel <- ggplot(data = dt_10ppm, aes(x = Sample_Name)) +
  geom_point(aes(y = Cu_concentration), color = "blue") +
  geom_text(aes(y = Cu_concentration, label = Sample.ID), 
            color = "black", vjust = -0.5, hjust = 0.5, size = 3) +
  geom_point(aes(y = Cu_ICP), color = "orange") +
  theme_classic()

corel




}

#Excel Error

dt <- dt[dt$Cu_concentration != 0.25, ] # To remove LODs

dt$Cu_Error <- abs(((dt$Cu_ICP - dt$Cu_concentration) / dt$Cu_ICP) * 100)

Cu_corr <- ggplot(dt, aes(x = Total_Weight, y = Cu_Error)) +
  geom_point(size = 1.7, stroke = 1, aes(color = Plot, shape = Form, fill = Plot)) +
  scale_shape_manual(values = c(21, 24, 23, 25)) +
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  theme_classic() +
  ylab("Percent Error")
Cu_corr


Cu_corr2 <- ggplot(dt, aes(x = Substrate_RT, y = Cu_Error)) +
  geom_point(size = 1.7, stroke = 1, aes(color = Plot, shape = Form, fill = Plot)) +
  scale_shape_manual(values = c(21, 24, 23, 25)) +
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  theme_classic() +
  ylab("Percent Error")
Cu_corr2



RT_Weight <- ggplot(dt, aes(x = Substrate_RT, y = Total_Weight)) +
  geom_point(size = 1.7, stroke = 1, aes(color = Plot, shape = Form, fill = Plot)) +
  scale_shape_manual(values = c(21, 24, 23, 25)) +
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  theme_classic() +
  ylab("Total Weight")
RT_Weight


cor.test(dt$Cu_Error, dt$Total_Weight, method="spearman")
cor.test(dt$Cu_Error, dt$Substrate_RT, method="spearman")
cor.test(dt$Total_Weight, dt$Substrate_RT, method="spearman")



P6 <- subset(dt, Plot=="P6")

CorrCu <- ggplot(P6, aes(x = Cu_ICP, y = Cu_concentration)) +
  geom_point(size = 1.7, stroke = 1, aes(color = Scientific_Name)) +
  scale_shape_manual(values = c(21, 24, 23, 25)) +
  #scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  #scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  theme_classic() +
  ylab("Total Weight")
CorrCu

#Bland Altman
{
# Calculate the average of measurements
average <- (dt$Cu_concentration + dt$Cu_ICP) / 2

# Calculate the difference (error)
difference <- dt$Cu_concentration - dt$Cu_ICP

# Bland-Altman plot
# Assuming you have already calculated the average and difference variables

# Plot Bland-Altman plot with centered mean difference line
plot(average, difference, xlab = "Average", ylab = "Difference", main = "Bland-Altman Plot", ylim = c(mean(difference) - 2.5 * sd(difference), mean(difference) + 2.5 * sd(difference)))
abline(h = mean(difference), col = "red", lty = 2)
text(average, difference, labels = dt$Sample.ID, pos = 3)
sd_value <- sd(difference)
abline(h = mean(difference) + 1.96 * sd_value, col = "blue", lty = 3)
abline(h = mean(difference) - 1.96 * sd_value, col = "blue", lty = 3)



library(psych)
library(BlandAltmanLeh)

dt_ICC <- dt[, c("Cu_concentration", "Cu_ICP")]

ICC(dt_ICC, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients
#we need to use ICC2, it's two way random effect model https://www.youtube.com/watch?v=lFF3wp3dk2Y
#Report in paper: Interclass correlation value ICC 0.86. Test-retest reliability was found to be good, ICC=0.86, p<.001, 95%CI .62 to .93
#CI - confidence interval
#Koo and Li (2016) :
#below 0.5: poor
#between 0.5 - 0.75 - moderate
#0.75 - 0.9 good
# above 0.9 excellent











}



