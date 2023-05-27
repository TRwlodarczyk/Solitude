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

#remove ND
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
#apply LODs
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
#correlation tests tr dataset
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

shapiro.test(dt$Total_Weight) # non normally
shapiro.test(dt$Substrate_RT) # normally!!

}




dt <- read.delim("SLT_pXRF_ICP.txt")
dt <- dt[dt$Cu_concentration != 0.25, ] # To remove LODs
#Cu boxplots
{
Cu <- ggplot(dt, aes(x = reorder(Scientific_Name, Cu_ICP, FUN = median),
                             y = Cu_ICP, Sceintific_Name=Scientific_Name)) +
  geom_boxplot() +
  geom_point(size = 1.7, stroke = 1, aes(color = Plot, shape = Form, fill = Plot)) +
  scale_shape_manual(values = c(21, 24, 22, 3)) +
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  geom_hline(yintercept = 40, linetype = "dashed", color = "#9a9a9a", size = 1.2) +
  geom_hline(yintercept = 300, linetype = "dotdash", color = "#454545", size = 1.2) +
  coord_flip() +
  scale_x_discrete(guide = guide_axis(angle = 0)) +
  scale_y_continuous(limits = c(0, 800), breaks = seq(0, 800, by = 50)) +
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

dt_selected <- dt[dt$Scientific_Name %in% c("Xanthisma gracile", "Pseudognaphalium canescens", "Boechera perennans", "Nultuma (Prosopis) velutina", "Tamarix chinesis", "Allionia incarnata"),] 

dt_Cu <- dt_selected[, c(1:11, 30, 58)]
dt_Fe <- dt_selected[, c(1:11, 24, 55)]

library(reshape2)

melted_dt_Cu <- melt(dt_Cu, measure.vars = c("Cu_concentration", "Cu_ICP"),
                     variable.name = "Method", value.name = "Cu_value")

melted_dt_Fe <- melt(dt_Fe, measure.vars = c("Fe_concentration", "Fe_ICP"),
                     variable.name = "Method", value.name = "Fe_value")


Cu <- ggplot(melted_dt_Cu, aes(x = reorder(Scientific_Name, value, FUN = median),
                               y = value, fill = variable)) +
  geom_boxplot(position = position_dodge(width = 0.9)) +
  geom_point(size = 1.9, stroke = 1, aes(color = variable, fill = variable),
             position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("#068DA9", "#643A6B")) +
  scale_color_manual(values = c("#34495E", "#B0A4A4")) +
  labs(x = "Scientific Name", y = "Cu Value", fill = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(size=14),
        axis.title.x = element_text(size = 19),
        axis.text.y = element_text(size=14, face="italic"),
        axis.title.y = element_blank(),
        legend.key.size = unit(2, "lines"),
        legend.text = element_text(size = 13.5), 
        legend.title = element_text(size=20, face = "bold"))+
  coord_flip() +
  theme(legend.position = "bottom")

Cu

library(agricolae)
PC <- subset(melted_dt_Cu, Group=="G39")
BP <- subset(melted_dt_Cu, Group=="G37")
TC <- subset(melted_dt_Cu, Group=="G11")
AI <- subset(melted_dt_Cu, Group=="G3")
XG <- subset(melted_dt_Cu, Group=="G36")
NV <- subset(melted_dt_Cu, Group=="G8")

k <- kruskal(PC$value, PC$variable, p.adj = "bonferroni")
k
k <- kruskal(TC$value, TC$variable, p.adj = "bonferroni")
k
k <- kruskal(XG$value, XG$variable, p.adj = "bonferroni")
k

result <- wilcox.test(PC$value ~ PC$variable, paired = TRUE)
print(result)
result <- wilcox.test(BP$value ~ BP$variable, paired = TRUE)
print(result)
result <- wilcox.test(TC$value ~ TC$variable, paired = TRUE)
print(result)
result <- wilcox.test(XG$value ~ XG$variable, paired = TRUE)
print(result)
result <- wilcox.test(AI$value ~ AI$variable, paired = TRUE)
print(result)
result <- wilcox.test(NV$value ~ NV$variable, paired = TRUE)
print(result)



?kruskal
library(ggplot2)



Fe <- ggplot(melted_dt_Fe, aes(x = reorder(Scientific_Name, value, FUN = median),
                               y = value, fill = variable)) +
  geom_boxplot(position = position_dodge(width = 0.9)) +
  geom_point(size = 1.9, stroke = 1, aes(color = variable, fill = variable),
             position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("#068DA9", "#643A6B")) +
  scale_color_manual(values = c("#34495E", "#B0A4A4")) +
  labs(x = "Scientific Name", y = "Fe Value", fill = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(size=14),
        axis.title.x = element_text(size = 19),
        axis.text.y = element_text(size=14, face="italic"),
        axis.title.y = element_blank(),
        legend.key.size = unit(2, "lines"),
        legend.text = element_text(size = 13.5), 
        legend.title = element_text(size=20, face = "bold"))+
  coord_flip() +
  theme(legend.position = "bottom")

Fe



PC <- subset(melted_dt_Fe, Group=="G39")
BP <- subset(melted_dt_Fe, Group=="G37")
TC <- subset(melted_dt_Fe, Group=="G11")
AI <- subset(melted_dt_Fe, Group=="G3")
XG <- subset(melted_dt_Fe, Group=="G36")

result <- wilcox.test(PC$value ~ PC$variable, paired = TRUE)
print(result)
result <- wilcox.test(BP$value ~ BP$variable, paired = TRUE)
print(result)
result <- wilcox.test(XG$value ~ XG$variable, paired = TRUE)
print(result)
result <- wilcox.test(AI$value ~ AI$variable, paired = TRUE)
print(result)



# Define the plot
ggplot(dt, aes(x = Total_Weight, y = Substrate_RT, shape = Form, color = Cu_concentration)) +
  geom_point(size = 4, stroke=1) +  # Add points with specified size
  labs(x = "Total_Weight", y = "Substrate_RT", title = "Correlation RT~Weight (rho = 0.89)") +  # Set axis labels and title
  scale_color_gradient(low = "#FFEAE9", high = "#660000", name = "Cu_concentration") 
cor.test(dt$Total_Weight, dt$Substrate_RT, method="spearman")



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




cor.test(dt$Cu_concentration, dt$Cu_ICP, method="spearman") #0.8713 with LODS, 0.913 without LODs


}

#Excel Error

dt <- dt[dt$Cu_concentration != 0.25, ] # To remove LODs
#write.table(dt, file='C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/SLT_pXRF_ICP_NoCuLODs.csv', sep=",", row.names = F)



#Correlations and Error Plot
{
dt <- read.delim("SLT_pXRF_ICP.txt")
dt <- dt[dt$Cu_concentration != 0.25, ]
dt <- subset(dt, Form=="Forb")
dt <- subset(dt, Form=="Grass")
dt <- subset(dt, Form=="Tree")
dt <- subset(dt, Form=="Shrub")

dt$Cu_Error <- abs(((dt$Cu_ICP - dt$Cu_concentration) / dt$Cu_ICP) * 100)

  ggplot(dt, aes(x = Total_Weight, y = Cu_Error, shape = Form, color = Substrate_RT)) +
    geom_point(size = 4, stroke=1) +  # Add points with specified size
    labs(x = "Total Sample Weight [g]", y = "Percent Error [%]") +  # Set axis labels and title
    scale_color_gradient(low = "#FFEAE9", high = "#660000", name = "Relative thickness") +  # Gradient of red color based on Total_Weight column
    #scale_y_continuous(limits = c(-200, 100), breaks = seq(-200, 100, by = 50), expand = c(0, 0)) + # Set y-axis limits
    theme_minimal() +  # Use a minimal theme
    theme(
      plot.title = element_text(size = 16, face = "bold"),  # Customize plot title
      axis.title = element_text(size = 20),  # Customize axis labels
      axis.text.x = element_text(size = 16),
      axis.title.x = element_text(size = 20),
      axis.text.y = element_text(size = 16),
      axis.title.y = element_text(size = 20),
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 16, face = "bold"),
      legend.position = "top"  # Position the legend at the top
    ) +
    guides(
      shape = guide_legend(title = "Form", override.aes = list(size = 5))
    )  # Add legend for shape aesthetic with specified size
  
  
  cor.test(dt$Cu_Error, dt$Total_Weight, method="spearman")
  cor.test(dt$Cu_Error, dt$Substrate_RT, method="spearman")  
  
  
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
#cor.test(dt$Cu_concentration, dt$Cu_ICP, method="spearman")
#cor.test(dt$Fe_concentration, dt$Fe_ICP, method="spearman")



P6 <- subset(dt, Plot=="P6")

CorrCu <- ggplot(P6, aes(x = Cu_ICP, y = Cu_concentration)) +
  geom_point(size = 1.7, stroke = 1, aes(color = Scientific_Name)) +
  scale_shape_manual(values = c(21, 24, 23, 25)) +
  #scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  #scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  theme_classic() +
  ylab("Total Weight")
CorrCu
}

#Bland Altman Cu
{
  
#dt <- dt[!dt$Scientific_Name %in% c("Pseudognaphalium canescens", "Boechera perennans", "Xanthisma gracile"), ]# To remove PC and BP as a source of bias
#dt <- dt[dt$Cu_concentration < 100 & dt$Cu_concentration > 10, ]

  
  # Calculate the average of measurements
  average <- (dt$Cu_concentration + dt$Cu_ICP) / 2
  
  # Calculate the difference (error)
  difference <- dt$Cu_concentration - dt$Cu_ICP
  
  # Create a data frame with the variables
  df <- data.frame(average = average, difference = difference, Sample.ID = dt$Sample.ID, Form = dt$Form, Plot = dt$Plot, Total_Weight = dt$Total_Weight)
  
  # Calculate the mean difference and standard deviation
  mean_diff <- mean(difference)
  sd_value <- sd(difference)
  
  # Define the plot
  ggplot(df, aes(x = average, y = difference, shape = Form, color = Total_Weight)) +
    geom_point(size = 4, stroke=1) +  # Add points with specified size
    geom_hline(aes(yintercept = mean_diff), color="#7D7CAF", linetype = "dashed", size = 1.5) +  # Add dashed line at the mean difference
    geom_hline(aes(yintercept = mean_diff + 1.96 * sd_value), color="#AFB07D", linetype = "dashed", size = 1.5) +  # Add upper limit line
    geom_hline(aes(yintercept = mean_diff - 1.96 * sd_value), color="#AFB07D",linetype = "dashed", size = 1.5) +  # Add lower limit line
    labs(x = "Average", y = "Difference", title = "Bland-Altman Plot Cu") +  # Set axis labels and title
    scale_color_gradient(low = "#FFEAE9", high = "#660000", name = "Total Weight") +  # Gradient of red color based on Total_Weight column
    scale_y_continuous(limits = c(-200, 100), breaks = seq(-200, 100, by = 50), expand = c(0, 0)) + # Set y-axis limits
    theme_minimal() +  # Use a minimal theme
    theme(
      plot.title = element_text(size = 16, face = "bold"),  # Customize plot title
      axis.title = element_text(size = 20),  # Customize axis labels
      axis.text.x = element_text(size = 16),
      axis.title.x = element_text(size = 20),
      axis.text.y = element_text(size = 16),
      axis.title.y = element_text(size = 20),
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 16, face = "bold"),
      legend.position = "top"  # Position the legend at the top
    ) +
    guides(
      shape = guide_legend(title = "Form", override.aes = list(size = 5))
    )  # Add legend for shape aesthetic with specified size
  
# Perform a one-sample t-test
t_test <- t.test(difference, mu = 0)

# Print the results
cat("Mean Difference:", mean_diff, "\n")
cat("One-sample t-test:\n")
cat("  t-value:", t_test$statistic, "\n")
cat("  p-value:", t_test$p.value, "\n")

# Calculate mean difference and standard deviation of the differences
mean_diff <- mean(difference)
sd_diff <- sd(difference)

# Calculate confidence interval for the mean difference
n <- length(difference)
se_diff <- sd_diff / sqrt(n)
t_value <- qt(0.975, df = n - 1)  # 95% confidence interval
ci_diff <- mean_diff + c(-1, 1) * t_value * se_diff

# Calculate effect size (Cohen's d)
cohens_d <- mean_diff / sd_diff

# Print the results
cat("Mean Difference:", mean_diff, "\n")
cat("95% Confidence Interval:", ci_diff[1], "-", ci_diff[2], "\n")
cat("Effect Size (Cohen's d):", cohens_d, "\n")


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

#Fe Bland Altman
{

#Normality
shapiro.test(dt$Fe_concentration)
shapiro.test(dt$Fe_ICP)

cor.test(dt$Fe_concentration, dt$Fe_ICP, method="spearman") # Rho = 0.974

# Plot the correlation
plot(dt$Cu_concentration ~ dt$Cu_ICP, xlab = "Fe_ICP", ylab = "Fe_concentration")

# Add labels for each point using Sample.ID
text(dt$Fe_ICP, dt$Fe_concentration, labels = dt$Sample.ID, pos = 3)


library(psych)

dt_ICC_Fe <- dt[, c("Fe_concentration", "Fe_ICP")]

ICC(dt_ICC_Fe, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients ICC2 = 0.87


#Bland Altman Fe
# Calculate the average of measurements
average <- (dt$Fe_concentration + dt$Fe_ICP) / 2
# Calculate the difference (error)
difference <- dt$Fe_concentration - dt$Fe_ICP

# Create a data frame with the variables
df <- data.frame(average = average, difference = difference, Sample.ID = dt$Sample.ID)

# Calculate the mean difference and standard deviation
mean_diff <- mean(difference)
sd_value <- sd(difference)

# Define the plot
ggplot(df, aes(x = average, y = difference, label = Sample.ID)) +
  geom_point() +  # Add points
  geom_hline(aes(yintercept = mean_diff, color = "Mean Difference"), linetype = "dashed", size = 1.1) +  # Add dashed line at the mean difference
  geom_hline(aes(yintercept = mean_diff + 1.96 * sd_value, color = "Limits of Agreement"), linetype = "dashed", size = 1.1) +  # Add upper limit line
  geom_hline(aes(yintercept = mean_diff - 1.96 * sd_value, color = "Limits of Agreement"), linetype = "dashed", size = 1.1) +  # Add lower limit line
  labs(x = "Average", y = "Difference", title = "Bland-Altman Plot") +  # Set axis labels and title
  ylim(mean_diff - 2.5 * sd_value, mean_diff + 2.5 * sd_value) +  # Set y-axis limits
  theme_minimal() +  # Use a minimal theme
  theme(plot.title = element_text(size = 14, face = "bold"),  # Customize plot title
        axis.title = element_text(size = 14),  # Customize axis labels
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=13),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size=13),
        axis.title.y = element_text(size=14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size=12, face = "bold"),
        legend.position = "top") +  # Position the legend at the top
  geom_text(vjust = -0.8) +  # Add sample ID labels above the points
  guides(color = guide_legend(title = "Limits:", override.aes = list(size = 1.1)))  # Add a legend for the dashed lines with thicker lines


# Perform a one-sample t-test
t_test <- t.test(difference, mu = 0)

# Print the results
cat("Mean Difference:", mean_diff, "\n")
cat("One-sample t-test:\n")
cat("  t-value:", t_test$statistic, "\n")
cat("  p-value:", t_test$p.value, "\n")

}




typeof(dt$Total_Weight)

