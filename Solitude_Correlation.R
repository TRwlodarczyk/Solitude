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
  library(car)
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

#Linear regression model assumptions check
{
#Linearity
plot(Cu_ICP ~ Cu_concentration, data = dt)
abline(lm(Cu_ICP ~ Cu_concentration, data = dt), col = "red")
#Normality of Residuals
model <- lm(Cu_ICP ~ Cu_concentration, data = dt)
residuals <- resid(model)

# Histogram of residuals
hist(residuals, breaks = 10, col = "lightblue")
plot(density(residuals))
shapiro.test(residuals)

# Q-Q plot of residuals
qqnorm(residuals)
qqline(residuals)
#Homoscedasticity
# Residuals vs. fitted values plot
plot(fitted(model), residuals, xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red")

# Residuals vs. predictor variable plot
plot(dt$Cu_concentration, residuals, xlab = "Cu_concentration", ylab = "Residuals")
abline(h = 0, col = "red")

#Independence of Residuals:
# Check for autocorrelation in residuals (for time series data)
acf(residuals)

# Check for residuals vs. other variables (for potential dependencies)
plot(residuals ~ Cu_ICP, data = dt)

}

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
  geom_hline(yintercept = 100, linetype = "dotdash", color = "#454545", size = 1.2) +
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
dt$Cu_Error <- abs(((dt$Cu_ICP - dt$Cu_concentration) / dt$Cu_ICP) * 100)
#dt$Cu_Error <- abs(((dt$Cu_ICP - dt$Predicted_Cu_ICP) / dt$Cu_ICP) * 100)
dtF <- subset(dt, Form=="Forb")
dtG <- subset(dt, Form=="Grass")
dtT <- subset(dt, Form=="Tree")
dtS <- subset(dt, Form=="Shrub")

shapiro.test(dt$Cu_Error) # Normal
shapiro.test(dt$Cu_Error2) # Non normal
cor.test(dt$Cu_Error, dt$Substrate_RT, method="pearson") #  R2 = -0.414
cor.test(dt$Cu_Error, dt$Total_Weight, method="pearson") #  R2 = -0.279
cor.test(dt$Cu_Error, dt$Substrate_RT, method="spearman") #  R2 = -0.4028
cor.test(dt$Cu_Error, dt$Total_Weight, method="spearman") #  R2 = -0.2684
cor.test(dt$Cu_Error2, dt$Substrate_RT, method="spearman") #Rho = -0.217
cor.test(dt$Cu_Error2, dt$Total_Weight, method="spearman") #Rho -0.208

summary(lm(dt$Cu_Error ~ dt$Substrate_RT))


length(dtF$Cu_ICP) #56
length(dtG$Cu_ICP) #9
length(dtT$Cu_ICP) #20
length(dtS$Cu_ICP) #13

mean(dtF$Cu_Error) #43.157
mean(dtG$Cu_Error) #38.54
mean(dtT$Cu_Error) #35.7
mean(dtS$Cu_Error) #31.22




a <-   ggplot(dt, aes(x = Total_Weight, y = Cu_Error, shape = Form, color = Substrate_RT)) +
    geom_point(size = 4, stroke=1) +  # Add points with specified size
    labs(x = "Total Sample Weight [g]", y = "Percent Error [%]") +  # Set axis labels and title
    scale_color_gradient(low = "#068DA9", high = "#660000", name = "Relative thickness") +  # Gradient of red color based on Total_Weight column
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25), expand = c(0, 0)) + # Set y-axis limits
    theme_minimal() +  # Use a minimal theme
    theme(
      plot.title = element_text(size = 16, face = "bold"),  # Customize plot title
      axis.title = element_text(size = 20),  # Customize axis labels
      axis.text.x = element_text(size = 16),
      axis.title.x = element_text(size = 20),
      axis.text.y = element_text(size = 16),
      axis.title.y = element_text(size = 20),
      legend.text = element_text(size = 6),
      legend.title = element_text(size = 16, face = "bold"),
      legend.position = "top"  # Position the legend at the top
    ) +
    guides(
      shape = guide_legend(title = "Form", override.aes = list(size = 5))
    )  # Add legend for shape aesthetic with specified size
  a
  
  
  aS <-   ggplot(dt, aes(x = Substrate_RT, y = Cu_Error, shape = Form, color = Total_Weight)) +
    geom_point(size = 4, stroke=1) +  # Add points with specified size
    labs(x = "Total Sample Weight [g]", y = "Percent Error [%]") +  # Set axis labels and title
    scale_color_gradient(low = "#FFEAE9", high = "#660000", name = "Relative thickness") +  # Gradient of red color based on Total_Weight column
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25), expand = c(0, 0)) + # Set y-axis limits
    theme_minimal() +  # Use a minimal theme
    theme(
      plot.title = element_text(size = 16, face = "bold"),  # Customize plot title
      axis.title = element_text(size = 20),  # Customize axis labels
      axis.text.x = element_text(size = 16),
      axis.title.x = element_text(size = 20),
      axis.text.y = element_text(size = 16),
      axis.title.y = element_text(size = 20),
      legend.text = element_text(size = 6),
      legend.title = element_text(size = 16, face = "bold"),
      legend.position = "top"  # Position the legend at the top
    ) +
    guides(
      shape = guide_legend(title = "Form", override.aes = list(size = 5))
    )  # Add legend for shape aesthetic with specified size
  aS
  
  sum(dt$Cu_Error > 50, na.rm = TRUE) # 34/98 = 34.6% 
  sum(dt$Cu_Error < 25, na.rm = TRUE) # 26/98 = 26.5%
  sum(dt$Cu_Error > 70, na.rm = TRUE) # 8/98 = 8.16%
  
  sum(dt$Cu_Error2 > 50, na.rm = TRUE) # 13/98 = 13.26%
  sum(dt$Cu_Error2 < 25, na.rm = TRUE) # 34/98 = 34.69%
  sum(dt$Cu_Error2 > 70, na.rm = TRUE) # 2/98 = 2%
  
  cor.test(dt$Cu_Error, dt$Total_Weight, method="spearman")
  cor.test(dt$Cu_Error, dt$Substrate_RT, method="spearman")  
  
  
  dt$Predicted_Cu_ICP <- 28.88747 + (1.41673* dt$Cu_concentration) + (-316.95475* dt$Substrate_RT)
  dt$Cu_Error2 <- abs(((dt$Cu_ICP - dt$Predicted_Cu_ICP) / dt$Cu_ICP) * 100)
  
b <-   ggplot(dt, aes(x = Total_Weight, y = Cu_Error2, shape = Form, color = Substrate_RT)) +
    geom_point(size = 4, stroke=1) +  # Add points with specified size
    labs(x = "Total Sample Weight [g]", y = "Percent Error [%]") +  # Set axis labels and title
    scale_color_gradient(low = "#FFEAE9", high = "#660000", name = "Relative thickness") +  # Gradient of red color based on Total_Weight column
   # scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25), expand = c(0, 0)) + # Set y-axis limits
  geom_text(aes(label = Cup_No), vjust = -0.5, size = 3.5) +  # Add text labels
  
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
b  
  
cor.test(dt$Cu_Error2, dt$Substrate_RT, method="spearman")  
  
  library(ggpubr)
  
  ggarrange(a, b,
            ncol = 2, nrow = 1, 
            common.legend = TRUE, legend = "bottom")
  

# two on one
  dt <- read.delim("SLT_pXRF_ICP.txt")
  dt <- dt[dt$Cu_concentration != 0.25, ]
  dt$Cu_Error <- abs(((dt$Cu_ICP - dt$Cu_concentration) / dt$Cu_ICP) * 100)
  dt$Predicted_Cu_ICP <- 28.88747 + (1.41673* dt$Cu_concentration) + (-316.95475* dt$Substrate_RT)
  dt$Cu_Error2 <- abs(((dt$Cu_ICP - dt$Predicted_Cu_ICP) / dt$Cu_ICP) * 100)
  
  cor.test(dt$Cu_Error2, dt$Substrate_RT, method="spearman")
  
 ggplot(dt, aes(x = Total_Weight)) +
    geom_point(aes(y = Cu_Error, color = "Cu_Error"), size = 3) +
    geom_smooth(aes(y = Cu_Error), method = "lm", se = TRUE, color = "#068DA9") +
    geom_point(aes(y = Cu_Error2, color = "Cu_Error2"), size = 3) +
    geom_smooth(aes(y = Cu_Error2), method = "lm", se = TRUE, color = "#660000") +
    labs(x = "Total Weight", y = "Error Value", color = "Legend") +
    scale_color_manual(values = c(Cu_Error = "#068DA9", Cu_Error2 = "#660000")) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25), expand = c(0, 0)) + # Set y-axis limits
    theme_minimal()


 
 ggplot() + #it's screwed because there are two points very high for Error2
   geom_density(data = dt, aes(x = Cu_Error, fill = "Cu_Error"), alpha = 0.5) +
   geom_density(data = dt, aes(x = Cu_Error2, fill = "Cu_Error2"), alpha = 0.5) +
   labs(x = "Error Value", y = "Density", fill = "Legend") +
   scale_fill_manual(values = c(Cu_Error = "blue", Cu_Error2 = "red")) +
   theme_minimal()
 
 
sd(dt$Cu_Error)
  
dt <- read.delim("SLT_pXRF_ICP.txt")
dt$Fe_Error <- abs(((dt$Fe_ICP - dt$Fe_concentration) / dt$Fe_ICP) * 100)

c <-   ggplot(dt, aes(x = Total_Weight, y = Fe_Error, shape = Form, color = Substrate_RT)) +
  geom_point(size = 4, stroke=1) +  # Add points with specified size
  labs(x = "Total Sample Weight [g]", y = "Percent Error [%]") +  # Set axis labels and title
  scale_color_gradient(low = "#FFEAE9", high = "#660000", name = "Relative thickness") +  # Gradient of red color based on Total_Weight column
  #scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25), expand = c(0, 0)) + # Set y-axis limits
  theme_minimal() +  # Use a minimal theme
  theme(
    plot.title = element_text(size = 16, face = "bold"),  # Customize plot title
    axis.title = element_text(size = 20),  # Customize axis labels
    axis.text.x = element_text(size = 16),
    axis.title.x = element_text(size = 20),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 20),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 16, face = "bold"),
    legend.position = "top"  # Position the legend at the top
  ) +
  guides(
    shape = guide_legend(title = "Form", override.aes = list(size = 5))
  )  # Add legend for shape aesthetic with specified size
c
cor.test(dt$Fe_Error, dt$Substrate_RT, method="spearman")  # rho 0.0236, pval. 0.7853


dt <- read.delim("SLT_pXRF_ICP.txt")
dt <- dt[dt$Zn_concentration != 0.3, ] # To remove LODs
dt$Zn_Error <- abs(((dt$Zn_ICP - dt$Zn_concentration) / dt$Zn_ICP) * 100)
shapiro.test(dt$Zn_Error) # Normal
cor.test(dt$Zn_Error, dt$Substrate_RT, method="spearman")  # rho -0.392, pval. 0.000104


c <-   ggplot(dt, aes(x = Total_Weight, y = Zn_Error, shape = Form, color = Substrate_RT)) +
  geom_point(size = 4, stroke=1) +  # Add points with specified size
  labs(x = "Total Sample Weight [g]", y = "Percent Error [%]") +  # Set axis labels and title
  scale_color_gradient(low = "#FFEAE9", high = "#660000", name = "Relative thickness") +  # Gradient of red color based on Total_Weight column
  #scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25), expand = c(0, 0)) + # Set y-axis limits
  theme_minimal() +  # Use a minimal theme
  theme(
    plot.title = element_text(size = 16, face = "bold"),  # Customize plot title
    axis.title = element_text(size = 20),  # Customize axis labels
    axis.text.x = element_text(size = 16),
    axis.title.x = element_text(size = 20),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 20),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 16, face = "bold"),
    legend.position = "top"  # Position the legend at the top
  ) +
  guides(
    shape = guide_legend(title = "Form", override.aes = list(size = 5))
  )  # Add legend for shape aesthetic with specified size
c

}

#Bland Altman Cu + ICC
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

#GLMs Cu #model changes if you have LODs or not, AIC is lower when LODs are removed. 
{
  
dt <- read.delim("SLT_pXRF_ICP.txt")
dt <- dt[dt$Cu_concentration != 0.25, ] # To remove LODs

shapiro.test(dt$Cu_concentration)
shapiro.test(dt$Cu_ICP)
leveneTest(dt$Cu_concentration, dt$Cu_ICP)
library(lmtest)
lm_model <- lm(Cu_ICP~Cu_concentration, data=dt)
breusch_pagan_test <- bptest(lm_model) # jest hetero


model1 <- glm(Cu_ICP ~ Cu_concentration, data = dt)
summary(model1)
model2 <- glm(Cu_ICP ~ Cu_concentration + Total_Weight, data = dt)
summary(model2)
model3 <- glm(Cu_ICP ~ Cu_concentration + Substrate_RT, data = dt)
summary(model3)

# Best AIC value for model4!!
# Provide starting values for the gamma glm model
start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
model4 <- glm(Cu_ICP ~ Cu_concentration, data = dt, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
summary(model4)
model5 <- glm(Cu_ICP ~ Cu_concentration + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
summary(model5)
model6 <- glm(Cu_ICP ~ Cu_concentration, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
summary(model6)
model7 <- glm(Cu_ICP ~ Cu_concentration + Substrate_RT, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
summary(model7)


## Predicted ICP values for Cu from model 7 - Substrate_RT as explanatory
Predicted_Cu_ICP = 28.88747 + (1.41673* dt$Cu_concentration) + (-316.95475* dt$Substrate_RT) 
dt$Predicted_Cu_ICP <- 28.88747 + (1.41673* dt$Cu_concentration) + (-316.95475* dt$Substrate_RT)

## Predicted ICP values for Cu from model 5 - Total_Weight as explanatory
dt$Predicted_Cu_ICP2 <- 17.03270 + (1.45362* dt$Cu_concentration) + (-11.13508* dt$Total_Weight)

#write.table(dt, file='C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/SLT_ICP_predicted_Cu.csv', sep=",", row.names = F)


cor.test(dt$Cu_ICP, dt$Cu_concentration, method="spearman") # rho = 0.9131926, p.val < 2.2e-16
cor.test(dt$Cu_ICP, dt$Predicted_Cu_ICP, method="spearman") # rho = 0.9433337, p.val < 2.2e-16
cor.test(dt$Cu_ICP, dt$Predicted_Cu_ICP2, method="spearman") # rho = 0.9300852, p.val < 2.2e-16
cor.test(dt$Cu_concentration, dt$Predicted_Cu_ICP, method="spearman") # rho = 0.9767824, p.val < 2.2e-16 



library(psych)
dt_ICC <- dt[, c("Cu_ICP", "Cu_concentration")]

ICC(dt_ICC, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients

dt_ICC1 <- dt[, c("Cu_ICP", "Predicted_Cu_ICP")]

ICC(dt_ICC1, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients


dt_ICC2 <- dt[, c("Cu_ICP", "Predicted_Cu_ICP2")]
ICC(dt_ICC1, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients


dt_ICC3 <- dt[, c("Cu_concentration", "Predicted_Cu_ICP")]
ICC(dt_ICC3, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients


#Check pvalue for bland altman of new predicted variables vs ICP

difference <- dt$Cu_ICP - dt$Cu_concentration
t_test <- t.test(difference, mu = 0)

#wilcox.test(difference, mu = 0, paired = TRUE) # for no normal we should use Wilcoxon signed-rank test

# Calculate the average of measurements
average1 <- (dt$Predicted_Cu_ICP + dt$Cu_ICP) / 2

# Calculate the difference (error)
difference1 <- dt$Predicted_Cu_ICP - dt$Cu_ICP
t_test <- t.test(difference1, mu = 0)
difference2 <- dt$Predicted_Cu_ICP2 - dt$Cu_ICP
t_test <- t.test(difference2, mu = 0)
# Create a data frame with the variables
df <- data.frame(average = average1, difference = difference1, Sample.ID = dt$Sample.ID, Form = dt$Form, Plot = dt$Plot, Total_Weight = dt$Total_Weight)

# Calculate the mean difference and standard deviation
mean_diff <- mean(difference1)
sd_value <- sd(difference1)

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
t_test <- t.test(difference1, mu = 0)

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
ci_diff <- mean_diff + c(-1, 1) * t_value * se_di



dt_selected <- dt[dt$Scientific_Name %in% c("Xanthisma gracile", "Pseudognaphalium canescens", "Boechera perennans", "Nultuma (Prosopis) velutina", "Tamarix chinesis", "Allionia incarnata"),] 

dt_Cu <- dt_selected[, c(1:11, 58, 69)] #69 is the Predicted_Cu_ICP
melted_dt_Cu <- melt(dt_Cu, measure.vars = c("Cu_ICP", "Predicted_Cu_ICP"),
                     variable.name = "Method", value.name = "Cu_value")

XG <- subset(melted_dt_Cu, Group=="G36")
result <- wilcox.test(XG$value ~ XG$variable, paired = TRUE)
print(result) # using Predicted_Cu_ICP, there's now no significant differences

AI <- subset(melted_dt_Cu, Group=="G3")
result <- wilcox.test(XG$value ~ XG$variable, paired = TRUE)
print(result) # using Predicted_Cu_ICP there's now no significant difference



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


#Boxplots with predicted 


Cu <- ggplot(dt, aes(x = reorder(Scientific_Name, Predicted_Cu_ICP, FUN = median),
                     y = Predicted_Cu_ICP, Sceintific_Name=Scientific_Name)) +
  geom_boxplot() +
  geom_point(size = 1.7, stroke = 1, aes(color = Plot, shape = Form, fill = Plot)) +
  scale_shape_manual(values = c(21, 24, 22, 3)) +
  scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  geom_hline(yintercept = 40, linetype = "dashed", color = "#9a9a9a", size = 1.2) +
  geom_hline(yintercept = 300, linetype = "dotdash", color = "#454545", size = 1.2) +
  geom_hline(yintercept = 100, linetype = "dotdash", color = "#454545", size = 1.2) +
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
#anova(model, test='Chisq') tis is a test to check if there's a difference between models I guess. If not, there should be another test to check differences between models. 



#ICC and one sample when mass is <0.4 (n=27)

dt04 <-  dt[dt$Total_Weight < 0.4, ]
cor.test(dt06$Cu_ICP, dt06$Cu_concentration, method="spearman") #0.878
library(psych)
dt_ICC <- dt04[, c("Cu_ICP", "Cu_concentration")]
ICC(dt_ICC, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #0.78
difference <- dt04$Cu_ICP - dt04$Cu_concentration
t.test(difference, mu = 0) #p < 0.05 signif different from 0. t = 5.26

#ICC and one sample when mass 04to06 (n=15)

dt04to06 <- dt[dt$Total_Weight > 0.4 & dt$Total_Weight < 0.6, ]
cor.test(dt04to06$Cu_ICP, dt04to06$Cu_concentration, method="spearman")  #rho 0.878
library(psych)
dt_ICC <- dt04to06[, c("Cu_ICP", "Cu_concentration")]
ICC(dt_ICC, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #0.89
difference <- dt04to06$Cu_ICP - dt04to06$Cu_concentration #pval 0.02992, t = 2.416
t.test(difference, mu = 0) #p =0.029 , t=2.416

#ICC and one sample when mass is 06 to 08 (n=29)
dt06to08 <- dt[dt$Total_Weight > 0.6 & dt$Total_Weight < 0.8, ]
cor.test(dt06to08$Cu_ICP, dt06to08$Cu_concentration, method="spearman") #0.86594
library(psych)
dt_ICC <- dt06to08[, c("Cu_ICP", "Cu_concentration")]
ICC(dt_ICC, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) # 0.87
difference <- dt06to08$Cu_ICP - dt06to08$Cu_concentration
t.test(difference, mu = 0) #p < 0.05, t = 5.779

#ICC and one sample when mass is >0.8 (n=26)
dt08 <- dt[dt$Total_Weight > 0.8,]
cor.test(dt08$Cu_ICP, dt08$Cu_concentration, method="spearman") #0.966
library(psych)
dt_ICC <- dt08[, c("Cu_ICP", "Cu_concentration")]
ICC(dt_ICC, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients 0.86
difference <- dt08$Cu_ICP - dt08$Cu_concentration
t.test(difference, mu = 0) #p < 0.05 t=4.69




# Removing outliers from the model2 before applying model!~~~~~!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


dt <- read.delim("SLT_pXRF_ICP.txt")
dt <- dt[dt$Cu_concentration != 0.25, ] # To remove LODs
dt <- dt[dt$Cup_No != 196,]
dt <- dt[dt$Cup_No != 4,]




model1 <- glm(Cu_ICP ~ Cu_concentration, data = dt)
summary(model1) #1005
model2 <- glm(Cu_ICP ~ Cu_concentration + Total_Weight, data = dt)
summary(model2) #998
model3 <- glm(Cu_ICP ~ Cu_concentration + Substrate_RT, data = dt)
summary(model3) #989

# Best AIC value for model4!!
# Provide starting values for the gamma glm model
start_vals <- c(coeff_cu_concentration = 0, coeff_intercept = 18.4)
model4 <- glm(Cu_ICP ~ Cu_concentration, data = dt, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
summary(model4) #879
model5 <- glm(Cu_ICP ~ Cu_concentration + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
summary(model5) #855
model6 <- glm(Cu_ICP ~ Cu_concentration, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
summary(model6) #879
model7 <- glm(Cu_ICP ~ Cu_concentration + Substrate_RT, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
summary(model7) #827


## Predicted ICP values for Cu from model 7 - Substrate_RT as explanatory
dt$Predicted_Cu_ICP = 32.58433 + (1.39091* dt$Cu_concentration) + (-356.22532* dt$Substrate_RT) 


## Predicted ICP values for Cu from model 5 - Total_Weight as explanatory
dt$Predicted_Cu_ICP2 <-  18.53305 + (1.43619* dt$Cu_concentration) + (-11.92409* dt$Total_Weight)

#write.table(dt, file='C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/SLT_ICP_predicted_Cu.csv', sep=",", row.names = F)


cor.test(dt$Cu_ICP, dt$Cu_concentration, method="spearman") # rho = 0.91132 p.val < 2.2e-16
cor.test(dt$Cu_ICP, dt$Predicted_Cu_ICP, method="spearman") # rho = 0.948, p.val < 2.2e-16
cor.test(dt$Cu_ICP, dt$Predicted_Cu_ICP2, method="spearman") # rho = 0.932, p.val < 2.2e-16


library(psych)
dt_ICC <- dt[, c("Cu_ICP", "Cu_concentration")]

ICC(dt_ICC, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients

dt_ICC1 <- dt[, c("Cu_ICP", "Predicted_Cu_ICP")]

ICC(dt_ICC1, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients


dt_ICC2 <- dt[, c("Cu_ICP", "Predicted_Cu_ICP2")]
ICC(dt_ICC1, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients


dt_ICC3 <- dt[, c("Cu_concentration", "Predicted_Cu_ICP")]
ICC(dt_ICC3, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients


#Check pvalue for bland altman of new predicted variables vs ICP

difference1 <- dt$Predicted_Cu_ICP - dt$Cu_ICP
t_test <- t.test(difference1, mu = 0)
difference2 <- dt$Predicted_Cu_ICP2 - dt$Cu_ICP
t_test <- t.test(difference2, mu = 0)

dt$Cu_Error2 <- abs(((dt$Cu_ICP - dt$Predicted_Cu_ICP) / dt$Cu_ICP) * 100)

b <-   ggplot(dt, aes(x = Total_Weight, y = Cu_Error2, shape = Form, color = Substrate_RT)) +
  geom_point(size = 4, stroke=1) +  # Add points with specified size
  labs(x = "Total Sample Weight [g]", y = "Percent Error [%]") +  # Set axis labels and title
  scale_color_gradient(low = "#FFEAE9", high = "#660000", name = "Relative thickness") +  # Gradient of red color based on Total_Weight column
  # scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25), expand = c(0, 0)) + # Set y-axis limits
  geom_text(aes(label = Cup_No), vjust = -0.5, size = 3.5) +  # Add text labels
  
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
b  

cor.test(dt$Cu_Error2, dt$Substrate_RT, method="spearman") 


#compare points from model where 196 and 4 are removed to the model where those were not removed

dt$Predicted_Cu_ICP_NEW <-  32.58433 + (1.39091* dt$Cu_concentration) + (-356.22532* dt$Substrate_RT) 
dt$Predicted_Cu_ICP <- 28.88747 + (1.41673* dt$Cu_concentration) + (-316.95475* dt$Substrate_RT)

#write.table(dt, file='C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Final/Error/CompareModelError.csv', sep=",", row.names = F)


}

#GLMs Fe# not aplicable

{
  dt <- read.delim("SLT_pXRF_ICP.txt")

  model1 <- glm(Fe_ICP ~ Fe_concentration, data = dt)
  summary(model1)
  model2 <- glm(Fe_ICP ~ Fe_concentration + Total_Weight, data = dt)
  summary(model2)
  model3 <- glm(Fe_ICP ~ Fe_concentration + Substrate_RT, data = dt)
  summary(model3)
  
  # Best AIC value for model4!!
  # Provide starting values for the gamma glm model
  start_vals <- c(coeff_Fe_concentration = 0, coeff_intercept = 18.4)
  model4 <- glm(Fe_ICP ~ Fe_concentration, data = dt, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  summary(model4)
  model5 <- glm(Fe_ICP ~ Fe_concentration + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(model5)
  model6 <- glm(Fe_ICP ~ Fe_concentration, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(model6)
  model7 <- glm(Fe_ICP ~ Fe_concentration + Substrate_RT, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(model7)
  

  # Predicted ICP values for Cu from model 7 - Substrate_RT as explanatory
  Predicted_Fe_ICP = 28.88747 + (1.41673* dt$Fe_concentration) + (-316.95475* dt$Substrate_RT) 
  dt$Predicted_Fe_ICP <- 28.88747 + (1.41673* dt$Fe_concentration) + (-316.95475* dt$Substrate_RT)
  
  #Predicted ICP values for Cu from model 5 - Total_Weight as explanatory
  dt$Predicted_Fe_ICP2 <- 17.03270 + (1.45362* dt$Fe_concentration) + (-11.13508* dt$Total_Weight)
  
  cor.test(dt$Fe_ICP, dt$Fe_concentration, method="spearman") # rho = 0.9131926, p.val < 2.2e-16
  cor.test(dt$Fe_ICP, dt$Predicted_Fe_ICP, method="spearman") # rho = 0.9433337, p.val < 2.2e-16
  cor.test(dt$Fe_ICP, dt$Predicted_Fe_ICP2, method="spearman") # rho = 0.9300852, p.val < 2.2e-16

  
  library(psych)
  
  dt_ICC <- dt[, c("Fe_ICP", "Fe_concentration")]
  ICC(dt_ICC, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients
  
  dt_ICC1 <- dt[, c("Fe_ICP", "Predicted_Fe_ICP")]
  ICC(dt_ICC1, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients
  
  dt_ICC2 <- dt[, c("Fe_ICP", "Predicted_Fe_ICP2")]
  ICC(dt_ICC1, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients
  
  
  
  
  
  #Check pvalue for bland altman of new predicted variables vs ICP
  
  # Calculate the average of measurements
  average <- (dt$Fe_ICP + dt$Fe_concentration) / 2
  
  # Calculate the difference (error)
  difference <- dt$Fe_ICP - dt$Fe_concentration
  t_test <- t.test(difference, mu = 0)
  difference1 <- dt$Predicted_Fe_ICP - dt$Fe_ICP
  t_test <- t.test(difference1, mu = 0)
  difference2 <- dt$Predicted_Fe_ICP2 - dt$Fe_ICP
  t_test <- t.test(difference2, mu = 0)
  
  # Create a data frame with the variables
  df <- data.frame(average = average1, difference = difference1, Sample.ID = dt$Sample.ID, Form = dt$Form, Plot = dt$Plot, Total_Weight = dt$Total_Weight)
  
  # Calculate the mean difference and standard deviation
  mean_diff <- mean(difference1)
  sd_value <- sd(difference1)
  
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
  t_test <- t.test(difference1, mu = 0)
  
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
  ci_diff <- mean_diff + c(-1, 1) * t_value * se_di
  
  
  
  dt_selected <- dt[dt$Scientific_Name %in% c("Xanthisma gracile", "Pseudognaphalium canescens", "Boechera perennans", "Nultuma (Prosopis) velutina", "Tamarix chinesis", "Allionia incarnata"),] 
  
  dt_Cu <- dt_selected[, c(1:11, 58, 69)] #69 is the Predicted_Fe_ICP
  melted_dt_Cu <- melt(dt_Cu, measure.vars = c("Fe_ICP", "Predicted_Fe_ICP"),
                       variable.name = "Method", value.name = "Cu_value")
  
  XG <- subset(melted_dt_Cu, Group=="G36")
  result <- wilcox.test(XG$value ~ XG$variable, paired = TRUE)
  print(result) # using Predicted_Fe_ICP, there's now no significant differences
  
  AI <- subset(melted_dt_Cu, Group=="G3")
  result <- wilcox.test(XG$value ~ XG$variable, paired = TRUE)
  print(result) # using Predicted_Fe_ICP there's now no significant difference
  
  
  
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
  
}

#GLMs Zn 

{
  dt <- read.delim("SLT_pXRF_ICP.txt")
  dt <- dt[dt$Zn_concentration != 0.3, ] # To remove LODs
  
  shapiro.test(dt$Zn_concentration)
  shapiro.test(dt$Zn_ICP)
  library(lmtest)
  lm_model <- lm(Zn_ICP~Zn_concentration, data=dt)
  breusch_pagan_test <- bptest(lm_model) #pval 0.7 - nie ma heteroscedasticity
  
  plot(dt$Zn_concentration~dt$Zn_ICP)
  cor.test(dt$Zn_ICP, dt$Zn_concentration, method="spearman") # rho = 0.5615571, p.val = 3.905e-09

  summary(lm_model) 
  
  model1 <- glm(Zn_ICP ~ Zn_concentration, data = dt)
  summary(model1)
  model2 <- glm(Zn_ICP ~ Zn_concentration + Total_Weight, data = dt)
  summary(model2)
  model3 <- glm(Zn_ICP ~ Zn_concentration + Substrate_RT, data = dt)
  summary(model3)
  

  # Provide starting values for the gamma glm model
  start_vals <- c(coeff_Zn_concentration = 0, coeff_intercept = 37)
  model4 <- glm(Zn_ICP ~ Zn_concentration, data = dt, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  summary(model4)
  model5 <- glm(Zn_ICP ~ Zn_concentration + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(model5)
  model6 <- glm(Zn_ICP ~ Zn_concentration, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(model6)
  model7 <- glm(Zn_ICP ~ Zn_concentration + Substrate_RT, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(model7)
  
  
  # Predicted ICP values for Cu from model 7 - Substrate_RT as explanatory
  dt$Predicted_Zn_ICP <- 50.8422 + (0.9560* dt$Zn_concentration) + (-473.9784* dt$Substrate_RT)
  
  #Predicted ICP values for Cu from model 5 - Total_Weight as explanatory
  dt$Predicted_Zn_ICP2 <- 33.6939 + (0.9314* dt$Zn_concentration) + (-16.8131* dt$Total_Weight)
  
  cor.test(dt$Zn_ICP, dt$Zn_concentration, method="spearman") # rho = 0.5615571, p.val = 3.905e-09
  cor.test(dt$Zn_ICP, dt$Predicted_Zn_ICP, method="spearman") # rho = 0.787104, p.val < 2.2e-16
  cor.test(dt$Zn_ICP, dt$Predicted_Zn_ICP2, method="spearman") # rho = 0.7381209, p.val < 2.2e-16 
  
  
  library(psych)
  
  dt_ICC <- dt[, c("Zn_ICP", "Zn_concentration")]
  ICC(dt_ICC, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients
  
  dt_ICC1 <- dt[, c("Zn_ICP", "Predicted_Zn_ICP")]
  ICC(dt_ICC1, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients
  
  dt_ICC2 <- dt[, c("Zn_ICP", "Predicted_Zn_ICP2")]
  ICC(dt_ICC1, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients
  
  
  
  
  #One sample t-test for pxrf ICP
  average <- (dt$Zn_ICP + dt$Zn_concentration) / 2
  difference <- dt$Zn_ICP - dt$Zn_concentration
  t_test <- t.test(difference, mu = 0)
  t_test <- t.test(difference1, mu = 0)

  #Check pvalue for bland altman of new predicted variables vs ICP
  average1 <- (dt$Predicted_Zn_ICP + dt$Zn_ICP) / 2
  difference1 <- dt$Predicted_Zn_ICP - dt$Zn_ICP
  t_test <- t.test(difference1, mu = 0)
  difference2 <- dt$Predicted_Zn_ICP2 - dt$Zn_ICP
  t_test <- t.test(difference2, mu = 0)
  
  
  df <- data.frame(average = average1, difference = difference1, Sample.ID = dt$Sample.ID, Form = dt$Form, Plot = dt$Plot, Total_Weight = dt$Total_Weight)
  mean_diff <- mean(difference1)
  sd_value <- sd(difference1)
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
  t.test(difference1, mu = 0)
  
# testing significance between species when no model is applied. Only X.Gracile and Nultuma and B. perennans have enough datapoints for the analysis.
  dt_selected <- dt[dt$Scientific_Name %in% c("Xanthisma gracile", "Pseudognaphalium canescens", "Boechera perennans", "Nultuma (Prosopis) velutina", "Tamarix chinesis", "Allionia incarnata"),] 
  
  dt_Zn <- dt_selected[, c(1:11, 32, 59)] #59 is the Zn_ICP
  dt_Zn <- dt_selected[, c(1:11, 59, 69)] #59 is the Zn_ICP, 69 is Predicted
  melted_dt_Zn <- melt(dt_Zn, measure.vars = c("Zn_ICP", "Zn_concentration"),
                       variable.name = "Method", value.name = "Zn_value")  
  
  melted_dt_Zn <- melt(dt_Zn, measure.vars = c("Zn_ICP", "Predicted_Zn_ICP"),
                       variable.name = "Method", value.name = "Cu_value")
  
  XG <- subset(melted_dt_Zn, Group=="G36")
  result <- wilcox.test(XG$value ~ XG$variable, paired = TRUE) # significant difference without model
  PV <- subset(melted_dt_Zn, Group=="G8")
  result <- wilcox.test(PV$value ~ PV$variable, paired = TRUE) # not significant difference without model
  BP <- subset(melted_dt_Zn, Group=="G37")
  result <- wilcox.test(BP$value ~ BP$variable, paired = TRUE) # not significant difference without model ERROR
  PC <- subset(melted_dt_Zn, Group=="G39")
  result <- wilcox.test(PC$value ~ PC$variable, paired = TRUE) # not significant difference without model ERROR
  
  
  
  melted_dt_Zn <- melt(dt_Zn, measure.vars = c("Zn_ICP", "Predicted_Zn_ICP"),
                       variable.name = "Method", value.name = "Cu_value")
  
  XG <- subset(melted_dt_Cu, Group=="G36")
  result <- wilcox.test(XG$value ~ XG$variable, paired = TRUE)
  print(result) # using Predicted_Zn_ICP, there's now no significant differences
  
  AI <- subset(melted_dt_Cu, Group=="G3")
  result <- wilcox.test(XG$value ~ XG$variable, paired = TRUE)
  print(result) # using Predicted_Zn_ICP there's now no significant difference
  
  
  
  Zn <- ggplot(melted_dt_Zn, aes(x = reorder(Scientific_Name, value, FUN = median),
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
  
  Zn
  
}

#GLMs Se 

{
  dt <- read.delim("SLT_pXRF_ICP.txt")
  dt <- dt[dt$Se_concentration != 0.05, ] # To remove LODs
  
  shapiro.test(dt$Se_concentration)
  shapiro.test(dt$Se_ICP)
  library(lmtest)
  lm_model <- lm(Se_ICP~Se_concentration, data=dt)
  breusch_pagan_test <- bptest(lm_model) # jest heterosca
  
  plot(dt$Se_concentration~dt$Se_ICP)
  plot(density(dt$Se_ICP))
  cor.test(dt$Se_ICP, dt$Se_concentration, method="spearman") # rho = 0.9521751, p.val < 2.2e-16
  
  
  model1 <- glm(Se_ICP ~ Se_concentration, data = dt)
  summary(model1)
  model2 <- glm(Se_ICP ~ Se_concentration + Total_Weight, data = dt)
  summary(model2)
  model3 <- glm(Se_ICP ~ Se_concentration + Substrate_RT, data = dt)
  summary(model3)
  
  # Best AIC value for model4!!
  # Provide starting values for the gamma glm model
  start_vals <- c(coeff_Se_concentration = 0, coeff_intercept = 37)
  model4 <- glm(Se_ICP ~ Se_concentration, data = dt, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  summary(model4)
  model5 <- glm(Se_ICP ~ Se_concentration + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(model5)
  model6 <- glm(Se_ICP ~ Se_concentration, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(model6)
  model7 <- glm(Se_ICP ~ Se_concentration + Substrate_RT, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(model7)
  
  
  # Predicted ICP values for Cu from model 7 - Substrate_RT as explanatory
  dt$Predicted_Se_ICP <- 0.4417 + (1.5683* dt$Se_concentration) + (-8.8017* dt$Substrate_RT)
  
  #Predicted ICP values for Cu from model 5 - Total_Weight as explanatory
  dt$Predicted_Se_ICP2 <- 0.07610 + (1.58532* dt$Se_concentration) + (-0.32381* dt$Total_Weight) # but intercept is not significant, I wouldnt use this
  
  cor.test(dt$Se_ICP, dt$Se_concentration, method="spearman") # rho = 0.9521751, p.val < 2.2e-16
  cor.test(dt$Se_ICP, dt$Predicted_Se_ICP, method="spearman") # rho = 0.9548323, p.val < 2.2e-16
  cor.test(dt$Se_ICP, dt$Predicted_Se_ICP2, method="spearman") # rho = 0.9546346, p.val < 2.2e-16 
  
  
  library(psych)
  
  dt_ICC <- dt[, c("Se_ICP", "Se_concentration")]
  ICC(dt_ICC, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients 0.83
  
  dt_ICC1 <- dt[, c("Se_ICP", "Predicted_Se_ICP")]
  ICC(dt_ICC1, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients 0.99
  
  dt_ICC2 <- dt[, c("Se_ICP", "Predicted_Se_ICP2")]
  ICC(dt_ICC1, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients 0.99
  
  
  
  
  #One sample t-test for pxrf ICP
  average <- (dt$Se_ICP + dt$Se_concentration) / 2
  difference <- dt$Se_ICP - dt$Se_concentration
  t_test <- t.test(difference, mu = 0) #p < 0.05 signif different from 0
  
  
  #Check pvalue for bland altman of new predicted variables vs ICP
  average1 <- (dt$Predicted_Se_ICP + dt$Se_ICP) / 2
  difference1 <- dt$Predicted_Se_ICP - dt$Se_ICP
  t_test <- t.test(difference1, mu = 0) # p > 0.05 not sig different from 0
  difference2 <- dt$Predicted_Se_ICP2 - dt$Se_ICP
  t_test <- t.test(difference2, mu = 0) # p > 0.05 not sig different from 0
  
  
  df <- data.frame(average = average1, difference = difference1, Sample.ID = dt$Sample.ID, Form = dt$Form, Plot = dt$Plot, Total_Weight = dt$Total_Weight)
  mean_diff <- mean(difference1)
  sd_value <- sd(difference1)
  # Define the plot
  ggplot(df, aes(x = average, y = difference, shape = Form, color = Total_Weight)) +
    geom_point(size = 4, stroke=1) +  # Add points with specified size
    geom_hline(aes(yintercept = mean_diff), color="#7D7CAF", linetype = "dashed", size = 1.5) +  # Add dashed line at the mean difference
    geom_hline(aes(yintercept = mean_diff + 1.96 * sd_value), color="#AFB07D", linetype = "dashed", size = 1.5) +  # Add upper limit line
    geom_hline(aes(yintercept = mean_diff - 1.96 * sd_value), color="#AFB07D",linetype = "dashed", size = 1.5) +  # Add lower limit line
    labs(x = "Average", y = "Difference", title = "Bland-Altman Plot Cu") +  # Set axis labels and title
    scale_color_gradient(low = "#FFEAE9", high = "#660000", name = "Total Weight") +  # Gradient of red color based on Total_Weight column
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
  
  # Perform a one-sample t-test
  t.test(difference1, mu = 0)
  
  # testing significance between species when no model is applied. Only X.Gracile and Nultuma and B. perennans have enough datapoints for the analysis.
  dt_selected <- dt[dt$Scientific_Name %in% c("Xanthisma gracile", "Pseudognaphalium canescens", 
  "Boechera perennans", "Nultuma (Prosopis) velutina", "Tamarix chinesis", "Allionia incarnata", "Isocoma cf. tenuisecta", "Senegalia (Acacia) greggii", "Populus fremontii"),] 
  
  dt_Se <- dt_selected[, c(1:11, 36, 61)] #59 is the Se_ICP
  dt_Se <- dt_selected[, c(1:11, 61, 69)] #59 is the Se_ICP, 69 is Predicted 
  melted_dt_Se <- melt(dt_Se, measure.vars = c("Se_ICP", "Se_concentration"),
                       variable.name = "Method", value.name = "Se_value")  
  
  melted_dt_Se <- melt(dt_Se, measure.vars = c("Se_ICP", "Predicted_Se_ICP"),
                       variable.name = "Method", value.name = "Se_value")
  
  XG <- subset(melted_dt_Se, Group=="G36")
  result <- wilcox.test(XG$value ~ XG$variable, paired = TRUE) # significant difference without model, not significant with model
  print(result)
  PV <- subset(melted_dt_Se, Group=="G8")
  result <- wilcox.test(PV$value ~ PV$variable, paired = TRUE) # not significant difference without model
  print(result)
  BP <- subset(melted_dt_Se, Group=="G37")
  result <- wilcox.test(BP$value ~ BP$variable, paired = TRUE) # not significant difference without model ERROR
  print(result)
  PC <- subset(melted_dt_Se, Group=="G39")
  result <- wilcox.test(PC$value ~ PC$variable, paired = TRUE) # not significant difference without model ERROR
  print(result)
  AI <- subset(melted_dt_Se, Group=="G3")
  result <- wilcox.test(AI$value ~ AI$variable, paired = TRUE) # not significant difference without model ERROR
  print(result)
  IT <- subset(melted_dt_Se, Group=="G18")
  result <- wilcox.test(IT$value ~ IT$variable, paired = TRUE) # not significant difference without model ERROR
  print(result)
  SG <- subset(melted_dt_Se, Group=="G19")
  result <- wilcox.test(SG$value ~ SG$variable, paired = TRUE) # not significant difference without model ERROR
  print(result)
  
  
  
  
  melted_dt_Se <- melt(dt_Se, measure.vars = c("Se_ICP", "Predicted_Se_ICP"),
                       variable.name = "Method", value.name = "Se_value")
  
  XG <- subset(melted_dt_Se, Group=="G36")
  result <- wilcox.test(XG$value ~ XG$variable, paired = TRUE)
  print(result) # using Predicted_Se_ICP, there's now no significant differences
  
  AI <- subset(melted_dt_Se, Group=="G3")
  result <- wilcox.test(XG$value ~ XG$variable, paired = TRUE)
  print(result) # using Predicted_Se_ICP there's now no significant difference
  
  
  
  Se <- ggplot(melted_dt_Se, aes(x = reorder(Scientific_Name, value, FUN = median),
                                 y = value, fill = variable)) +
    geom_boxplot(position = position_dodge(width = 0.9)) +
    geom_point(size = 1.9, stroke = 1, aes(color = variable, fill = variable),
               position = position_dodge(width = 0.8)) +
    scale_fill_manual(values = c("#068DA9", "#643A6B")) +
    scale_color_manual(values = c("#34495E", "#B0A4A4")) +
    labs(x = "Scientific Name", y = "Se Value", fill = "Variable") +
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
  
  Se
  
}

#GLMs Mn

{
  dt <- read.delim("SLT_pXRF_ICP.txt")
  dt <- dt[dt$Mn_concentration != 0.5, ] # To remove LODs
  
  shapiro.test(dt$Mn_concentration)
  shapiro.test(dt$Mn_ICP)
  library(lmtest)
  lm_model <- lm(Mn_ICP~Mn_concentration, data=dt)
  breusch_pagan_test <- bptest(lm_model) # nie ma heterosca
  
  plot(dt$Mn_concentration~dt$Mn_ICP)
  plot(density(dt$Mn_ICP))
  cor.test(dt$Mn_ICP, dt$Mn_concentration, method="spearman") # rho = 0.7655086 , p-value = 2.737e-13
  
  
  model1 <- glm(Mn_ICP ~ Mn_concentration, data = dt)
  summary(model1)
  model2 <- glm(Mn_ICP ~ Mn_concentration + Total_Weight, data = dt)
  summary(model2)
  model3 <- glm(Mn_ICP ~ Mn_concentration + Substrate_RT, data = dt)
  summary(model3)
  
  # Best AIC value for model4!!
  # Provide starting values for the gamma glm model
  start_vals <- c(coeff_Mn_concentration = 0, coeff_intercept = 37)
  model4 <- glm(Mn_ICP ~ Mn_concentration, data = dt, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  summary(model4)
  model5 <- glm(Mn_ICP ~ Mn_concentration + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(model5)
  model6 <- glm(Mn_ICP ~ Mn_concentration, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(model6)
  model7 <- glm(Mn_ICP ~ Mn_concentration + Substrate_RT, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(model7)
  
  
  # Predicted ICP values for Cu from model 7 - Substrate_RT as explanatory
  dt$Predicted_Mn_ICP <- 51.4943 + (1.0760* dt$Mn_concentration) + (-431.8509* dt$Substrate_RT)
  
  #Predicted ICP values for Cu from model 5 - Total_Weight as explanatory
  dt$Predicted_Mn_ICP2 <- 40.6027 + (1.0494* dt$Mn_concentration) + (-20.5045* dt$Total_Weight) # but intercept is not significant, I wouldnt use this
  
  cor.test(dt$Mn_ICP, dt$Mn_concentration, method="spearman") # rho = 0.7655086 , p-value = 2.737e-13
  cor.test(dt$Mn_ICP, dt$Predicted_Mn_ICP, method="spearman") # rho = 0.8058756 , p.val < 2.2e-16
  cor.test(dt$Mn_ICP, dt$Predicted_Mn_ICP2, method="spearman") # rho = 0.8096198 , p.val < 2.2e-16 
  
  
  library(psych)
  
  dt_ICC <- dt[, c("Mn_ICP", "Mn_concentration")]
  ICC(dt_ICC, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients 0.83
  
  dt_ICC1 <- dt[, c("Mn_ICP", "Predicted_Mn_ICP")]
  ICC(dt_ICC1, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients 0.99
  
  dt_ICC2 <- dt[, c("Mn_ICP", "Predicted_Mn_ICP2")]
  ICC(dt_ICC1, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients 0.99
  
  
  
  
  #One sample t-test for pxrf ICP
  average <- (dt$Mn_ICP + dt$Mn_concentration) / 2
  difference <- dt$Mn_ICP - dt$Mn_concentration
  t_test <- t.test(difference, mu = 0) #p < 0.05 signif different from 0
  
  #Check pvalue for bland altman of new predicted variables vs ICP
  average1 <- (dt$Predicted_Mn_ICP + dt$Mn_ICP) / 2
  difference1 <- dt$Predicted_Mn_ICP - dt$Mn_ICP
  t_test <- t.test(difference1, mu = 0) # p > 0.05 not sig different from 0
  difference2 <- dt$Predicted_Mn_ICP2 - dt$Mn_ICP
  t_test <- t.test(difference2, mu = 0)
  
  df <- data.frame(average = average1, difference = difference1, Sample.ID = dt$Sample.ID, Form = dt$Form, Plot = dt$Plot, Total_Weight = dt$Total_Weight)
  mean_diff <- mean(difference1)
  sd_value <- sd(difference1)
  # Define the plot
  ggplot(df, aes(x = average, y = difference, shape = Form, color = Total_Weight)) +
    geom_point(size = 4, stroke=1) +  # Add points with specified size
    geom_hline(aes(yintercept = mean_diff), color="#7D7CAF", linetype = "dashed", size = 1.5) +  # Add dashed line at the mean difference
    geom_hline(aes(yintercept = mean_diff + 1.96 * sd_value), color="#AFB07D", linetype = "dashed", size = 1.5) +  # Add upper limit line
    geom_hline(aes(yintercept = mean_diff - 1.96 * sd_value), color="#AFB07D",linetype = "dashed", size = 1.5) +  # Add lower limit line
    labs(x = "Average", y = "Difference", title = "Bland-Altman Plot Cu") +  # Set axis labels and title
    scale_color_gradient(low = "#FFEAE9", high = "#660000", name = "Total Weight") +  # Gradient of red color based on Total_Weight column
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
  
  # Perform a one-sample t-test
  t.test(difference1, mu = 0)
  
  # testing significance between species when no model is applied. Only X.Gracile and Nultuma and B. perennans have enough datapoints for the analysis.
  dt_selected <- dt[dt$Scientific_Name %in% c("Xanthisma gracile", "Pseudognaphalium canescens", 
                                              "Boechera perennans", "Nultuma (Prosopis) velutina", "Tamarix chinesis", "Allionia incarnata", "Isocoma cf. tenuisecta",
                                              "Senegalia (Acacia) greggii", "Populus fremontii", "Ambrosia confertiflora", "Dasyochloa pulchella", "Portulaca suffrutescens", "Sphaeralcea cf. ambigua", "Datura wrightii"),] 
  
  dt_Mn <- dt_selected[, c(1:11, 22, 54)] #59 is the Mn_ICP
  dt_Mn <- dt_selected[, c(1:11, 54, 69)] #59 is the Mn_ICP, 69 is Predicted 
  melted_dt_Mn <- melt(dt_Mn, measure.vars = c("Mn_ICP", "Mn_concentration"),
                       variable.name = "Method", value.name = "Mn_value")  
  
  melted_dt_Mn <- melt(dt_Mn, measure.vars = c("Mn_ICP", "Predicted_Mn_ICP"),
                       variable.name = "Method", value.name = "Mn_value")
  
  XG <- subset(melted_dt_Mn, Group=="G36")
  result <- wilcox.test(XG$value ~ XG$variable, paired = TRUE) # significant difference without model, not significant with model
  print(result)
  PV <- subset(melted_dt_Mn, Group=="G8")
  result <- wilcox.test(PV$value ~ PV$variable, paired = TRUE) # not significant difference without model
  print(result)
  BP <- subset(melted_dt_Mn, Group=="G37")
  result <- wilcox.test(BP$value ~ BP$variable, paired = TRUE) # not significant difference without model ERROR
  print(result)
  PC <- subset(melted_dt_Mn, Group=="G39")
  result <- wilcox.test(PC$value ~ PC$variable, paired = TRUE) # not significant difference without model ERROR
  print(result)
  AI <- subset(melted_dt_Mn, Group=="G3")
  result <- wilcox.test(AI$value ~ AI$variable, paired = TRUE) # not significant difference without model ERROR
  print(result)
  IT <- subset(melted_dt_Mn, Group=="G18")
  result <- wilcox.test(IT$value ~ IT$variable, paired = TRUE) # not significant difference without model ERROR
  print(result)
  SG <- subset(melted_dt_Mn, Group=="G19")
  result <- wilcox.test(SG$value ~ SG$variable, paired = TRUE) # not significant difference without model ERROR
  print(result)
  
  
  
  
  melted_dt_Mn <- melt(dt_Mn, measure.vars = c("Mn_ICP", "Predicted_Mn_ICP"),
                       variable.name = "Method", value.name = "Mn_value")
  
  XG <- subset(melted_dt_Se, Group=="G36")
  result <- wilcox.test(XG$value ~ XG$variable, paired = TRUE)
  print(result) # using Predicted_Mn_ICP, there's now no significant differences
  
  AI <- subset(melted_dt_Se, Group=="G3")
  result <- wilcox.test(XG$value ~ XG$variable, paired = TRUE)
  print(result) # using Predicted_Mn_ICP there's now no significant difference
  
  
  
  Mn <- ggplot(melted_dt_Mn, aes(x = reorder(Scientific_Name, value, FUN = median),
                                 y = value, fill = variable)) +
    geom_boxplot(position = position_dodge(width = 0.9)) +
    geom_point(size = 1.9, stroke = 1, aes(color = variable, fill = variable),
               position = position_dodge(width = 0.8)) +
    scale_fill_manual(values = c("#068DA9", "#643A6B")) +
    scale_color_manual(values = c("#34495E", "#B0A4A4")) +
    labs(x = "Scientific Name", y = "Mn Value", fill = "Variable") +
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
  
  Mn
  
}

#GLMs As
{
  dt <- read.delim("SLT_pXRF_ICP.txt")
  dt <- dt[dt$As_concentration != 0.05, ] # To remove LODs
  
  shapiro.test(dt$As_concentration)
  shapiro.test(dt$As_ICP)
  library(lmtest)
  lm_model <- lm(As_ICP~As_concentration, data=dt)
  breusch_pagan_test <- bptest(lm_model) # nie ma hetero
  
  plot(dt$As_concentration~dt$As_ICP)
  plot(density(dt$As_ICP))
  cor.test(dt$As_ICP, dt$As_concentration, method="spearman") # rho = 0.7655086 , p-value = 2.737e-13
  
  
  model1 <- glm(As_ICP ~ As_concentration, data = dt)
  summary(model1)
  model2 <- glm(As_ICP ~ As_concentration + Total_Weight, data = dt)
  summary(model2)
  model3 <- glm(As_ICP ~ As_concentration + Substrate_RT, data = dt)
  summary(model3)
  
  # Best AIC value for model4!!
  # Provide starting values for the gamma glm model
  start_vals <- c(coeff_As_concentration = 0, coeff_intercept = 37)
  model4 <- glm(As_ICP ~ As_concentration, data = dt, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  summary(model4)
  model5 <- glm(As_ICP ~ As_concentration + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(model5)
  model6 <- glm(As_ICP ~ As_concentration, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(model6)
  model7 <- glm(As_ICP ~ As_concentration + Substrate_RT, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(model7)
  
  
  # Predicted ICP values for Cu from model 7 - Substrate_RT as explanatory
  dt$Predicted_As_ICP <- 0.5213 + (0.0643* dt$As_concentration) + (-3.8479* dt$Substrate_RT)
  
  #Predicted ICP values for Cu from model 5 - Total_Weight as explanatory
  #dt$Predicted_As_ICP2 <- 40.6027 + (1.0494* dt$As_concentration) + (-20.5045* dt$Total_Weight) # but intercept is not significant, I wouldnt use this
  
  cor.test(dt$As_ICP, dt$As_concentration, method="spearman") # rho = 0.7655086 , p-value = 2.737e-13
  cor.test(dt$As_ICP, dt$Predicted_As_ICP, method="spearman") # rho = 0.8058756 , p.val < 2.2e-16
  cor.test(dt$As_ICP, dt$Predicted_As_ICP2, method="spearman") # rho = 0.8096198 , p.val < 2.2e-16 
  
  
  library(psych)
  
  dt_ICC <- dt[, c("As_ICP", "As_concentration")]
  ICC(dt_ICC, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients 0.83
  
  dt_ICC1 <- dt[, c("As_ICP", "Predicted_As_ICP")]
  ICC(dt_ICC1, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients 0.99
  
  dt_ICC2 <- dt[, c("As_ICP", "Predicted_As_ICP2")]
  ICC(dt_ICC1, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients 0.99
  
  
  
  
  #One sample t-test for pxrf ICP
  average <- (dt$As_ICP + dt$As_concentration) / 2
  difference <- dt$As_ICP - dt$As_concentration
  t_test <- t.test(difference, mu = 0) #p < 0.05 signif different from 0
  
  #Check pvalue for bland altman of new predicted variables vs ICP
  average1 <- (dt$Predicted_As_ICP + dt$As_ICP) / 2
  difference1 <- dt$Predicted_As_ICP - dt$As_ICP
  t_test <- t.test(difference1, mu = 0) # p > 0.05 not sig different from 0
  
  df <- data.frame(average = average1, difference = difference1, Sample.ID = dt$Sample.ID, Form = dt$Form, Plot = dt$Plot, Total_Weight = dt$Total_Weight)
  mean_diff <- mean(difference1)
  sd_value <- sd(difference1)
  # Define the plot
  ggplot(df, aes(x = average, y = difference, shape = Form, color = Total_Weight)) +
    geom_point(size = 4, stroke=1) +  # Add points with specified size
    geom_hline(aes(yintercept = mean_diff), color="#7D7CAF", linetype = "dashed", size = 1.5) +  # Add dashed line at the mean difference
    geom_hline(aes(yintercept = mean_diff + 1.96 * sd_value), color="#AFB07D", linetype = "dashed", size = 1.5) +  # Add upper limit line
    geom_hline(aes(yintercept = mean_diff - 1.96 * sd_value), color="#AFB07D",linetype = "dashed", size = 1.5) +  # Add lower limit line
    labs(x = "Average", y = "Difference", title = "Bland-Altman Plot Cu") +  # Set axis labels and title
    scale_color_gradient(low = "#FFEAE9", high = "#660000", name = "Total Weight") +  # Gradient of red color based on Total_Weight column
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
  
  # Perform a one-sample t-test
  t.test(difference1, mu = 0)
  
  # testing significance between species when no model is applied. Only X.Gracile and Nultuma and B. perennans have enough datapoints for the analysis.
  dt_selected <- dt[dt$Scientific_Name %in% c("Xanthisma gracile", "Pseudognaphalium canescens", 
                                              "Boechera perennans", "Nultuma (Prosopis) velutina", "Tamarix chinesis", "Allionia incarnata", "Isocoma cf. tenuisecta",
                                              "Senegalia (Acacia) greggii", "Populus fremontii", "Ambrosia confertiflora", "Dasyochloa pulchella", "Portulaca suffrutescens", "Sphaeralcea cf. ambigua", "Datura wrightii"),] 
  
  dt_As <- dt_selected[, c(1:11, 34, 60)] #60 is the As_ICP
  dt_As <- dt_selected[, c(1:11, 60, 69)] #59 is the As_ICP, 69 is Predicted 
  melted_dt_As <- melt(dt_As, measure.vars = c("As_ICP", "As_concentration"),
                       variable.name = "Method", value.name = "Mn_value")  
  
  melted_dt_As <- melt(dt_As, measure.vars = c("As_ICP", "Predicted_As_ICP"),
                       variable.name = "Method", value.name = "Mn_value")
  
  XG <- subset(melted_dt_As, Group=="G36")
  result <- wilcox.test(XG$value ~ XG$variable, paired = TRUE) # significant difference without model, not significant with model
  print(result)
  PV <- subset(melted_dt_As, Group=="G8")
  result <- wilcox.test(PV$value ~ PV$variable, paired = TRUE) # not significant difference without model
  print(result)
  BP <- subset(melted_dt_As, Group=="G37")
  result <- wilcox.test(BP$value ~ BP$variable, paired = TRUE) # not significant difference without model ERROR
  print(result)
  PC <- subset(melted_dt_As, Group=="G39")
  result <- wilcox.test(PC$value ~ PC$variable, paired = TRUE) # not significant difference without model ERROR
  print(result)
  AI <- subset(melted_dt_As, Group=="G3")
  result <- wilcox.test(AI$value ~ AI$variable, paired = TRUE) # not significant difference without model ERROR
  print(result)
  IT <- subset(melted_dt_As, Group=="G18")
  result <- wilcox.test(IT$value ~ IT$variable, paired = TRUE) # not significant difference without model ERROR
  print(result)
  SG <- subset(melted_dt_As, Group=="G19")
  result <- wilcox.test(SG$value ~ SG$variable, paired = TRUE) # not significant difference without model ERROR
  print(result)
  
  
  
  
  melted_dt_As <- melt(dt_As, measure.vars = c("As_ICP", "Predicted_As_ICP"),
                       variable.name = "Method", value.name = "Mn_value")
  
  XG <- subset(melted_dt_As, Group=="G36")
  result <- wilcox.test(XG$value ~ XG$variable, paired = TRUE)
  print(result) # using Predicted_As_ICP, there's now no significant differences
  
  AI <- subset(melted_dt_As, Group=="G3")
  result <- wilcox.test(XG$value ~ XG$variable, paired = TRUE)
  print(result) # using Predicted_As_ICP there's now no significant difference
  
  
  
  As <- ggplot(melted_dt_As, aes(x = reorder(Scientific_Name, value, FUN = median),
                                 y = value, fill = variable)) +
    geom_boxplot(position = position_dodge(width = 0.9)) +
    geom_point(size = 1.9, stroke = 1, aes(color = variable, fill = variable),
               position = position_dodge(width = 0.8)) +
    scale_fill_manual(values = c("#068DA9", "#643A6B")) +
    scale_color_manual(values = c("#34495E", "#B0A4A4")) +
    labs(x = "Scientific Name", y = "Mn Value", fill = "Variable") +
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
  
  As
  
}

#GLMs Cr # not significant coeficients
{
  dt <- read.delim("SLT_pXRF_ICP.txt")
  dt <- dt[dt$Cr_concentration != 1, ] # To remove LODs
  dt <- dt[dt$Cr_ICP != 0, ] # To remove LODs
  
  shapiro.test(dt$Cr_concentration)
  shapiro.test(dt$Cr_ICP)
  library(lmtest)
  lm_model <- lm(Cr_ICP~Cr_concentration, data=dt)
  breusch_pagan_test <- bptest(lm_model) # jest hetero
  
  plot(dt$Cr_concentration~dt$Cr_ICP)
  plot(density(dt$Cr_ICP))
  cor.test(dt$Cr_ICP, dt$Cr_concentration, method="spearman") # rho = 0.7655086 , p-value = 2.737e-13
  
  
  model1 <- glm(Cr_ICP ~ Cr_concentration, data = dt)
  summary(model1)
  model2 <- glm(Cr_ICP ~ Cr_concentration + Total_Weight, data = dt)
  summary(model2)
  model3 <- glm(Cr_ICP ~ Cr_concentration + Substrate_RT, data = dt)
  summary(model3)
  
  # Best AIC value for model4!!
  # Provide starting values for the gamma glm model
  start_vals <- c(coeff_Cr_concentration = 0, coeff_intercept = 37)
  model4 <- glm(Cr_ICP ~ Cr_concentration, data = dt, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  summary(model4)
  model5 <- glm(Cr_ICP ~ Cr_concentration + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(model5)
  model6 <- glm(Cr_ICP ~ Cr_concentration, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(model6)
  model7 <- glm(Cr_ICP ~ Cr_concentration + Substrate_RT, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(model7)
  
  
  # Predicted ICP values for Cu from model 7 - Substrate_RT as explanatory
  dt$Predicted_Cr_ICP <- 0.60775 + (0.01228* dt$Cr_concentration) + (-5.78566* dt$Substrate_RT)
  dt$Predicted_Cr_ICP2 <- 0.39856 + (0.02* dt$Cr_concentration) + (-0.26371* dt$Total_Weight)
  
  #Predicted ICP values for Cu from model 5 - Total_Weight as explanatory
  #dt$Predicted_Cr_ICP2 <- 40.6027 + (1.0494* dt$Cr_concentration) + (-20.5045* dt$Total_Weight) # but intercept is not significant, I wouldnt use this
  
  cor.test(dt$Cr_ICP, dt$Cr_concentration, method="spearman") # rho = 
  cor.test(dt$Cr_ICP, dt$Predicted_Cr_ICP, method="spearman") # rho = 
  cor.test(dt$Cr_ICP, dt$Predicted_Cr_ICP2, method="spearman") # rho = 
  
  
  library(psych)
  
  dt_ICC <- dt[, c("Cr_ICP", "Cr_concentration")]
  ICC(dt_ICC, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients 0.83
  
  dt_ICC1 <- dt[, c("Cr_ICP", "Predicted_Cr_ICP")]
  ICC(dt_ICC1, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients 0.99
  
  dt_ICC2 <- dt[, c("Cr_ICP", "Predicted_Cr_ICP2")]
  ICC(dt_ICC1, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients 0.99
  
  
  
  
  #One sample t-test for pxrf ICP
  average <- (dt$Cr_ICP + dt$Cr_concentration) / 2
  difference <- dt$Cr_ICP - dt$Cr_concentration
  t_test <- t.test(difference, mu = 0) #p < 0.05 signif different from 0
  
  #Check pvalue for bland altman of new predicted variables vs ICP
  average1 <- (dt$Predicted_Cr_ICP + dt$Cr_ICP) / 2
  difference1 <- dt$Predicted_Cr_ICP - dt$Cr_ICP
  t_test <- t.test(difference1, mu = 0) # p > 0.05 not sig different from 0
  
  average2 <- (dt$Predicted_Cr_ICP2 + dt$Cr_ICP) / 2
  difference2 <- dt$Predicted_Cr_ICP2 - dt$Cr_ICP
  t_test <- t.test(difference2, mu = 0) # p > 0.05 not sig different from 0
  
  df <- data.frame(average = average1, difference = difference1, Sample.ID = dt$Sample.ID, Form = dt$Form, Plot = dt$Plot, Total_Weight = dt$Total_Weight)
  mean_diff <- mean(difference1)
  sd_value <- sd(difference1)
  # Define the plot
  ggplot(df, aes(x = average, y = difference, shape = Form, color = Total_Weight)) +
    geom_point(size = 4, stroke=1) +  # Add points with specified size
    geom_hline(aes(yintercept = mean_diff), color="#7D7CAF", linetype = "dashed", size = 1.5) +  # Add dashed line at the mean difference
    geom_hline(aes(yintercept = mean_diff + 1.96 * sd_value), color="#AFB07D", linetype = "dashed", size = 1.5) +  # Add upper limit line
    geom_hline(aes(yintercept = mean_diff - 1.96 * sd_value), color="#AFB07D",linetype = "dashed", size = 1.5) +  # Add lower limit line
    labs(x = "Average", y = "Difference", title = "Bland-Altman Plot Cu") +  # Set axis labels and title
    scale_color_gradient(low = "#FFEAE9", high = "#660000", name = "Total Weight") +  # Gradient of red color based on Total_Weight column
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
  
  # Perform a one-sample t-test
  t.test(difference1, mu = 0)
  
  # testing significance between species when no model is applied. Only X.Gracile and Nultuma and B. perennans have enough datapoints for the analysis.
  dt_selected <- dt[dt$Scientific_Name %in% c("Xanthisma gracile", "Pseudognaphalium canescens", 
                                              "Boechera perennans", "Nultuma (Prosopis) velutina", "Tamarix chinesis", "Allionia incarnata", "Isocoma cf. tenuisecta",
                                              "Senegalia (Acacia) greggii", "Populus fremontii", "Ambrosia confertiflora", "Dasyochloa pulchella", "Portulaca suffrutescens", "Sphaeralcea cf. ambigua", "Datura wrightii"),] 
  
  dt_Cr <- dt_selected[, c(1:11, 34, 60)] #60 is the Cr_ICP
  dt_Cr <- dt_selected[, c(1:11, 60, 69)] #59 is the Cr_ICP, 69 is Predicted 
  melted_dt_Cr <- melt(dt_Cr, measure.vars = c("Cr_ICP", "Cr_concentration"),
                       variable.name = "Method", value.name = "Mn_value")  
  
  melted_dt_Cr <- melt(dt_Cr, measure.vars = c("Cr_ICP", "Predicted_Cr_ICP"),
                       variable.name = "Method", value.name = "Mn_value")
  
  XG <- subset(melted_dt_Cr, Group=="G36")
  result <- wilcox.test(XG$value ~ XG$variable, paired = TRUE) # significant difference without model, not significant with model
  print(result)
  PV <- subset(melted_dt_Cr, Group=="G8")
  result <- wilcox.test(PV$value ~ PV$variable, paired = TRUE) # not significant difference without model
  print(result)
  BP <- subset(melted_dt_Cr, Group=="G37")
  result <- wilcox.test(BP$value ~ BP$variable, paired = TRUE) # not significant difference without model ERROR
  print(result)
  PC <- subset(melted_dt_Cr, Group=="G39")
  result <- wilcox.test(PC$value ~ PC$variable, paired = TRUE) # not significant difference without model ERROR
  print(result)
  AI <- subset(melted_dt_Cr, Group=="G3")
  result <- wilcox.test(AI$value ~ AI$variable, paired = TRUE) # not significant difference without model ERROR
  print(result)
  IT <- subset(melted_dt_Cr, Group=="G18")
  result <- wilcox.test(IT$value ~ IT$variable, paired = TRUE) # not significant difference without model ERROR
  print(result)
  SG <- subset(melted_dt_Cr, Group=="G19")
  result <- wilcox.test(SG$value ~ SG$variable, paired = TRUE) # not significant difference without model ERROR
  print(result)
  
  
  
  
  melted_dt_Cr <- melt(dt_Cr, measure.vars = c("Cr_ICP", "Predicted_Cr_ICP"),
                       variable.name = "Method", value.name = "Mn_value")
  
  XG <- subset(melted_dt_Cr, Group=="G36")
  result <- wilcox.test(XG$value ~ XG$variable, paired = TRUE)
  print(result) # using Predicted_Cr_ICP, there's now no significant differences
  
  AI <- subset(melted_dt_Cr, Group=="G3")
  result <- wilcox.test(XG$value ~ XG$variable, paired = TRUE)
  print(result) # using Predicted_Cr_ICP there's now no significant difference
  
  
  
  As <- ggplot(melted_dt_Cr, aes(x = reorder(Scientific_Name, value, FUN = median),
                                 y = value, fill = variable)) +
    geom_boxplot(position = position_dodge(width = 0.9)) +
    geom_point(size = 1.9, stroke = 1, aes(color = variable, fill = variable),
               position = position_dodge(width = 0.8)) +
    scale_fill_manual(values = c("#068DA9", "#643A6B")) +
    scale_color_manual(values = c("#34495E", "#B0A4A4")) +
    labs(x = "Scientific Name", y = "Mn Value", fill = "Variable") +
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
  
  As
  
}

#GLMs Ti # not significant coeficients
{
  dt <-read.delim("Solitude_pXRF_ICP_correl_Re.txt")
  
  {
    tr <- matrix(data = NA, ncol = ncol(dt[,c(1:59)]), nrow=nrow(dt)) # select all columns 1:46
    colnames(tr) <- colnames(dt[,c(1:59)])
    for (i in 14:48) # select when the concentrations start
    {
      tr[,c(i)] <- gsub(".*ND.*", 0, dt[,i])
    }
    
    for(i in c(1:13, 49:59)) # select columns that need to stay the same 1:11 include character and double (weight)
    {
      tr[,c(i)] <- dt[,c(i)]
    }
 
  }
    #transform to dataframe
    tr <- as.data.frame.matrix(tr) #A correct command to change the dataset to dataframe after transformations
    tr[,14:59] <- sapply(tr[,14:59],as.numeric)# Change a character to numeric (double)
    tr[,9] <- sapply(tr[,9],as.numeric)
    typeof(tr$Cu_concentration) # confirm the value is no longer a character

  dt <- tr
  
  shapiro.test(dt$Ti_concentration)
  shapiro.test(dt$Ti_ICP)

  plot(dt$Ti_concentration~dt$Ti_ICP)
  plot(density(dt$Ti_ICP))
  cor.test(dt$Ti_ICP, dt$Ti_concentration, method="spearman") # rho = 0.7655086 , p-value = 2.737e-13
  
  
  model1 <- glm(Ti_ICP ~ Ti_concentration, data = dt)
  summary(model1)
  model2 <- glm(Ti_ICP ~ Ti_concentration + Total_Weight, data = dt)
  summary(model2)
  model3 <- glm(Ti_ICP ~ Ti_concentration + Substrate_RT, data = dt)
  summary(model3)
  
  # Best AIC value for model4!!
  # Provide starting values for the gamma glm model
  start_vals <- c(coeff_Ti_concentration = 0, coeff_intercept = 37)
  model4 <- glm(Ti_ICP ~ Ti_concentration, data = dt, family = Gamma(link = "identity"), start = start_vals) # gamma family is for modeling continuous, positive response variables with right-skewed distributions, The link function is typically "log" or "inverse.
  summary(model4)
  model5 <- glm(Ti_ICP ~ Ti_concentration + Total_Weight, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(model5)
  model6 <- glm(Ti_ICP ~ Ti_concentration, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(model6)
  model7 <- glm(Ti_ICP ~ Ti_concentration + Substrate_RT, data = dt, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(model7)
  
  
  # Predicted ICP values for Cu from model 7 - Substrate_RT as explanatory
  dt$Predicted_Ti_ICP <- -8.80946 + (0.46543* dt$Ti_concentration) + (162.79067* dt$Substrate_RT)
  dt$Predicted_Ti_ICP2 <- 4.0911 + (0.3378* dt$Ti_concentration) + (-0.2289* dt$Total_Weight)
  
  #Predicted ICP values for Cu from model 5 - Total_Weight as explanatory
  #dt$Predicted_Cr_ICP2 <- 40.6027 + (1.0494* dt$Cr_concentration) + (-20.5045* dt$Total_Weight) # but intercept is not significant, I wouldnt use this
  
  cor.test(dt$Ti_ICP, dt$Ti_concentration, method="spearman") # rho = 
  cor.test(dt$Ti_ICP, dt$Predicted_Ti_ICP, method="spearman") # rho = 0.62
  cor.test(dt$Ti_ICP, dt$Predicted_Ti_ICP2, method="spearman") # rho = 0.775
  
  
  library(psych)
  
  dt_ICC <- dt[, c("Ti_ICP", "Ti_concentration")]
  ICC(dt_ICC, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients 0.83
  
  dt_ICC1 <- dt[, c("Ti_ICP", "Predicted_Ti_ICP")]
  ICC(dt_ICC1, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients 0.99
  
  dt_ICC2 <- dt[, c("Ti_ICP", "Predicted_Ti_ICP2")]
  ICC(dt_ICC1, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients 0.99
  
  
  
  
  #One sample t-test for pxrf ICP
  average <- (dt$Ti_ICP + dt$Ti_concentration) / 2
  difference <- dt$Ti_ICP - dt$Ti_concentration
  t_test <- t.test(difference, mu = 0) #p < 0.05 signif different from 0
  
  #Check pvalue for bland altman of new predicted variables vs ICP
  average1 <- (dt$Predicted_Ti_ICP + dt$Ti_ICP) / 2
  difference1 <- dt$Predicted_Ti_ICP - dt$Ti_ICP
  t_test <- t.test(difference1, mu = 0) # p > 0.05 not sig different from 0
  
  average2 <- (dt$Predicted_Ti_ICP2 + dt$Ti_ICP) / 2
  difference2 <- dt$Predicted_Ti_ICP2 - dt$Ti_ICP
  t_test <- t.test(difference2, mu = 0) # p > 0.05 not sig different from 0
  
  
}

#Re correl
{
  dt <-read.delim("Solitude_pXRF_ICP_correl_Re.txt")
  
  shapiro.test(dt$Re_concentration)
  shapiro.test(dt$Re_ICP)
  library(lmtest)
  lm_model <- lm(Re_ICP~Re_concentration, data=dt)
  breusch_pagan_test <- bptest(lm_model) # jest heteros
  
  
  {
    tr <- matrix(data = NA, ncol = ncol(dt[,c(1:59)]), nrow=nrow(dt)) # select all columns 1:46
    colnames(tr) <- colnames(dt[,c(1:59)])
    for (i in 14:48) # select when the concentrations start
    {
      tr[,c(i)] <- gsub(".*ND.*", 0, dt[,i])
    }
    
    for(i in c(1:13, 49:59)) # select columns that need to stay the same 1:11 include character and double (weight)
    {
      tr[,c(i)] <- dt[,c(i)]
    }
    tr   
    
    #transform to dataframe
    tr <- as.data.frame.matrix(tr) #A correct command to change the dataset to dataframe after transformations
    tr[,14:59] <- sapply(tr[,14:59],as.numeric)# Change a character to numeric (double)
    tr[,9] <- sapply(tr[,9],as.numeric)
    typeof(tr$Cu_concentration) # confirm the value is no longer a character
  }
  
  shapiro.test(dt$Re_ICP)
  
  model7 <- glm(Re_ICP ~ Re_concentration + Substrate_RT, data = tr, family = Gamma(link = "identity"), control = glm.control(maxit = 50)) # RT not significant
  summary(model7)
  model5 <- glm(Re_ICP ~ Re_concentration + Total_Weight, data = tr, family = Gamma(link = "identity"), control = glm.control(maxit = 50))
  summary(model5)
  
  
  tr$Predicted_Re_ICP <- 3.84146 + (0.91141* tr$Re_concentration) + (-33.18455* tr$Substrate_RT)
  #Predicted ICP values for Cu from model 5 - Total_Weight as explanatory
  tr$Predicted_Re_ICP2 <- 4.29996 + (0.93425* tr$Re_concentration) + (-3.64517* tr$Total_Weight)
  
  
  cor.test(tr$Re_ICP, tr$Re_concentration, method="spearman")
  cor.test(tr$Re_ICP, tr$Predicted_Re_ICP, method="spearman")
  cor.test(tr$Re_ICP, tr$Predicted_Re_ICP2, method="spearman")
  
  library(psych)
  
  tr_ICC <- tr[, c("Re_ICP", "Re_concentration")]
  ICC(tr_ICC, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients 0.99
  tr_ICC2 <- tr[, c("Re_ICP", "Predicted_Re_ICP")]
  ICC(tr_ICC2, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients 0.99
  tr_ICC3 <- tr[, c("Re_ICP", "Predicted_Re_ICP2")]
  ICC(tr_ICC3, missing=TRUE, alpha=.05, lmer=TRUE,check.keys=FALSE) #interclass corelation coefficients 0.99
  
  difference <- tr$Re_ICP - tr$Re_concentration
  t_test <- t.test(difference, mu = 0) #p < 0.05 signif different from 0
  
  difference1 <- tr$Re_ICP - tr$Predicted_Re_ICP
  t_test <- t.test(difference1, mu = 0) #p < 0.05 signif different from 0
  
  difference2 <- tr$Re_ICP - tr$Predicted_Re_ICP2
  t_test <- t.test(difference2, mu = 0) #p < 0.05 signif different from 0
  
  
  wilcox.test(tr$Re_ICP,tr$Re_concentration, mu = 0, paired = TRUE) # for no normal we should use Wilcoxon signed-rank test
  
  
}

### Aplying new model to pXRF data ### Update to the latest one
{
setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New")
dt <- read.delim("Solitude_Complete_List_5.17.23.txt")
#dt <- subset(dt, Scientific_Name != 'QA_Sample')


#removing LODs
{
  tr <- matrix(data = NA, ncol = ncol(dt[,c(1:48)]), nrow=nrow(dt)) # select all columns 1:46
  colnames(tr) <- colnames(dt[,c(1:48)])
  for (i in 14:48) # select when the concentrations start
  {
    tr[,c(i)] <- gsub(".*ND.*", 0, dt[,i])
  }
  
  for(i in 1:13) # select columns that need to stay the same 1:11 include character and double (weight)
  {
    tr[,c(i)] <- dt[,c(i)]
  }
  tr   
  
  #transform to dataframe
  tr <- as.data.frame.matrix(tr) #A correct command to change the dataset to dataframe after transformations
  tr[,14:48] <- sapply(tr[,14:48],as.numeric) # Change a character to numeric (double)
  typeof(tr$Cu_concentration) # confirm the value is no longer a character
}
#apply new LODs
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
#write.table(tr, file='C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/SLT_pXRF_LODs_NoQA.csv', sep=",", row.names = F)
#dt <- dt[dt$Cu_concentration != 0.25, ] # To remove LODs

  
setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Predicted datasets")
dt <- read.delim("SLT_pXRF_LODs_NoQA.txt")

dt$Predicted_Cu_ICP <- 28.88747 + (1.41673* dt$Cu_concentration) + (-316.95475* dt$Substrate_RT)

## Predicted ICP values for Cu from model 5 - Total_Weight as explanatory
dt$Predicted_Cu_ICP2 <- 17.03270 + (1.45362* dt$Cu_concentration) + (-11.13508* dt$Total_Weight)
#write.table(dt, file='C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/SLT_Predicted_Cu_NoQA.csv', sep=",", row.names = F)


#Had to trim some data to avoid repeated datapoints/measurements
dt <- read.delim("SLT_Predicted_Cu_NoQA-trimmed.txt")

dt <- subset(dt, Site=="TAILINGS")

Cu <- ggplot(dt, aes(x = reorder(Scientific_Name, Cu_concentration, FUN = median),
                             y = Cu_concentration, Scientific_Name=Scientific_Name)) +
  geom_boxplot() +
  geom_point( stroke = 1, aes(color = Plot, shape = Form, fill = Plot)) +
  scale_shape_manual(values = c(21, 24, 23, 25, 1)) +
  #scale_color_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  #scale_fill_manual(values = c("#0070C0", "#92D050", "#EDAD08", "#ED7D31")) +
  geom_hline(yintercept = 40, linetype = "dashed", color = "#9a9a9a", size = 1.2) +
  geom_hline(yintercept = 300, linetype = "dotdash", color = "#454545", size = 1.2) +
  coord_flip() +
  scale_x_discrete(guide = guide_axis(angle = 0)) +
  scale_y_continuous(limits = c(0, 600), breaks = seq(0, 600, by = 50)) +
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

}

# Factor analysis # it is in the Guarino 2019, "Identification of native-metal toleran plant species in situ"
{
library(psych)
metal_vars <- c("Cu_concentration", "Fe_concentration", "Zn_concentration", "Mn_concentration")

dt_subset <- dt[, metal_vars]
# Perform factor analysis with varimax rotation
factor_result <- fa(dt_subset, nfactors = 3, rotate = "varimax")

# Print the factor analysis results
print(factor_result)
}



#Model test
{dft <- read.delim("Model_Test_3_replicates.txt")

dft$Predicted_Cu_ICP <- 28.88747 + (1.41673* dft$Cu_concentration) + (-316.95475* dft$Substrate_RT)

## Predicted ICP values for Cu from model 5 - Total_Weight as explanatory
dft$Predicted_Cu_ICP2 <- 17.03270 + (1.45362* dft$Cu_concentration) + (-11.13508* dft$Total_Weight)

write.table(dft, file='C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/Model_test_3reps.csv', sep=",", row.names = F)
}


#RMSE error between diff elements

{
  
  dt <- read.delim("SLT_pXRF_ICP.txt")
  dt <- dt[dt$Cu_concentration != 0.25, ] # To remove LODs
  
  library(caret)
  
rmse <- RMSE(dt$Cu_concentration, dt$Cu_ICP)
rmse

rmse2 <- RMSE(dt$Predicted_Cu_ICP2, dt$Cu_ICP)
rmse2
  
rmse <- RMSE(dt$Predicted_Fe_ICP, dt$Fe_ICP)
rmse
  
}

#NEW DATASET REMOVING NDs (hisstogram)
{
setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New")
dt <- read.delim("Solitude_Complete_List_6.2.23_Final.txt")
dt <- subset(dt, Scientific_Name != 'QA_Sample')


#removing LODs
{
  tr <- matrix(data = NA, ncol = ncol(dt[,c(1:48)]), nrow=nrow(dt)) # select all columns 1:46
  colnames(tr) <- colnames(dt[,c(1:48)])
  for (i in 14:48) # select when the concentrations start
  {
    tr[,c(i)] <- gsub(".*ND.*", 0, dt[,i])
  }
  
  for(i in 1:13) # select columns that need to stay the same 1:11 include character and double (weight)
  {
    tr[,c(i)] <- dt[,c(i)]
  }
  tr   
  
  #transform to dataframe
  tr <- as.data.frame.matrix(tr) #A correct command to change the dataset to dataframe after transformations
  tr[,14:48] <- sapply(tr[,14:48],as.numeric) # Change a character to numeric (double)
  typeof(tr$Cu_concentration) # confirm the value is no longer a character
}
#apply new LODs
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

#write.table(tr, file='C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New/SLT_pXRF_Final_06.06.23.csv', sep=",", row.names = F)
dt <- read.delim("SLT_pXRF_Final_06.06.23.txt")


dt$Predicted_Cu_ICP <- 28.88747 + (1.41673* dt$Cu_concentration) + (-316.95475* dt$Substrate_RT)



dt <- subset(dt, Site=="TAILINGS")
dt <- dt[dt$Cu_concentration != 0.25, ] # To remove LODs
hist(dt$Cu_concentration, breaks=50)


colors <- c(P1 = "#0070C0", P2 = "#92D050", P5 = "#EDAD08", P6 = "#ED7D31")

# Create the violin plot
Cu <- ggplot(dt, aes(x = Plot, y = Cu_concentration, fill=Plot)) +
  geom_violin() +
  geom_point(position = position_jitter(width = 0.1), size = 2, alpha = 0.5) +
  labs(x = "Plot", y = "Cu Concentration", fill = "Plot") +
  geom_hline(yintercept = 300, linetype = "dotdash", color = "#454545", size = 1.2) +
  coord_flip() +
  scale_fill_manual(values = colors) +
  scale_y_continuous(limits = c(0, 600), breaks = seq(0, 600, by = 100)) +
  theme_minimal()+
  theme(
        axis.text.x = element_text(  size=14),
        axis.title.x = element_text(size = 19),
        axis.text.y = element_text(size=14, face="italic"),
        axis.title.y = element_blank(),
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 13.5), 
        legend.title = element_text(size=15, face = "bold"))+
  guides(color = guide_legend(override.aes = list(size = 3.5)),
         shape = guide_legend(override.aes = list(size = 3.5))) 
Cu

Se <- ggplot(dt, aes(x = Plot, y = Se_concentration, fill=Plot)) +
  geom_violin() +
  geom_point(position = position_jitter(width = 0.1), size = 2, alpha = 0.5) +
  labs(x = "Plot", y = "Se Concentration", fill = "Plot") +
  #geom_hline(yintercept = 300, linetype = "dotdash", color = "#454545", size = 1.2) +
  coord_flip() +
  scale_fill_manual(values = colors) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 20)) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(size=14),
    axis.title.x = element_text(size = 19),
    axis.text.y = element_text(size=14, face="italic"),
    axis.title.y = element_blank(),
    legend.key.size = unit(1, "lines"),
    legend.text = element_text(size = 13.5), 
    legend.title = element_text(size=15, face = "bold"))+
  guides(color = guide_legend(override.aes = list(size = 3.5)),
         shape = guide_legend(override.aes = list(size = 3.5))) 
Se

Re <- ggplot(dt, aes(x = Plot, y = Re_concentration, fill=Plot)) +
  geom_violin() +
  geom_point(position = position_jitter(width = 0.1), size = 2, alpha = 0.5) +
  labs(x = "Plot", y = "Re Concentration", fill = "Plot") +
  #geom_hline(yintercept = 300, linetype = "dotdash", color = "#454545", size = 1.2) +
  coord_flip() +
  scale_fill_manual(values = colors) +
  #scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 20)) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(size=14),
    axis.title.x = element_text(size = 19),
    axis.text.y = element_text(size=14, face="italic"),
    axis.title.y = element_blank(),
    legend.key.size = unit(1, "lines"),
    legend.text = element_text(size = 13.5), 
    legend.title = element_text(size=15, face = "bold"))+
  guides(color = guide_legend(override.aes = list(size = 3.5)),
         shape = guide_legend(override.aes = list(size = 3.5))) 

Re

library(ggpubr)

ggarrange(Cu, Se, Re, ncol = 1, nrow = 3, 
          common.legend = TRUE, legend = "bottom")

}


#Correlation heatmap for ICP only
{
library(corrplot)

# Select the columns you want to include in the correlation heatmap
selected_columns <- dt[, 49:67]

# Calculate the correlation matrix
cor_matrix <- cor(selected_columns)

# Create a correlation heatmap using corrplot
corrplot(cor_matrix, method = "color")
}


#Outliers check with DIXON
{
#Outliers check:

# Your dataset 'dt'
# Assuming 'Predicted_Cu_ICP' is the column of interest
data_to_test <- tr$Cd_concentration

# Sort the data in ascending order
sorted_data <- sort(data_to_test)

# Calculate the range
range_data <- max(sorted_data) - min(sorted_data)

# Calculate ratios of gaps to the range
ratios <- diff(sorted_data) / range_data

# Dixon's Q-test critical values for various sample sizes and significance levels
# For a 95% confidence level, use Q_crit = 0.704 for n = 5 and Q_crit = 0.425 for n = 10
Q_crit <- c(0.704, 0.425)

# Check if any ratios exceed the critical value
outliers <- ratios > Q_crit[length(sorted_data)]

# Print the results
cat("Ratios:", ratios, "\n")
cat("Outliers Detected:", sorted_data[outliers], "\n")


#Cu, Fe, Zn, Mn, Se, Cr, As, Re no outliers

}

# Wilcoxon between plant before and after the model

{
  setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Solitude New")
  dt <- read.delim("SLT_pXRF_ICP.txt")
  dt <- dt[dt$Cu_concentration != 0.25, ] # To remove LODs
  dt$Predicted_Cu_ICP <- 28.88747 + (1.41673* dt$Cu_concentration) + (-316.95475* dt$Substrate_RT)
  
  
  dt_selected <- dt[dt$Scientific_Name %in% c("Xanthisma gracile", "Pseudognaphalium canescens", "Boechera perennans",
                                              "Nultuma (Prosopis) velutina", 'Allionia incarnata', "Isocoma cf. tenuisecta","Mimosa biuncifera (=aculeaticarpa)", "Portulaca suffrutescens"),]
  #Copper
  dt_long <- dt_selected %>%
    pivot_longer(cols = c(Cu_ICP, Predicted_Cu_ICP, Cu_concentration),
                 names_to = "Variable",
                 values_to = "Value")
  
  ggplot(dt_long, aes(x = Scientific_Name, y = Value, fill = Variable)) +
    geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.7, size=0.2, outlier.size = 0.6) +
    labs(x = "Scientific Name", y = "Values") +
    scale_fill_manual(values = c("Cu_ICP" = "#6699CC", "Predicted_Cu_ICP" = "#36454F", "Cu_concentration" = "#D3D3D3")) +
    coord_flip()+
    theme_minimal() +
    theme(
      axis.text.x = element_text()
    )
  
  
  
  
  library(agricolae)
  dt_long_XG <- subset(dt_long, Scientific_Name=='Xanthisma gracile')
  dt_long_PC <- subset(dt_long, Scientific_Name=='Pseudognaphalium canescens')
  dt_long_PS <- subset(dt_long, Scientific_Name=='Portulaca suffrutescens')
  dt_long_NV <- subset(dt_long, Scientific_Name=='Nultuma (Prosopis) velutina')
  dt_long_MB <- subset(dt_long, Scientific_Name=='Mimosa biuncifera (=aculeaticarpa)')
  dt_long_IT <- subset(dt_long, Scientific_Name=='Isocoma cf. tenuisecta')
  dt_long_AI <- subset(dt_long, Scientific_Name=='Allionia incarnata')
  dt_long_BP <- subset(dt_long, Scientific_Name=='Boechera perennans')
  
  print(kruskal(dt_long_XG$Value, dt_long_XG$Variable, p.adj = "bonferroni")) # Predicted a, ICP a, pxRF a,
  print(kruskal(dt_long_PC$Value, dt_long_PC$Variable, p.adj = "bonferroni")) # a a a
  print(kruskal(dt_long_PS$Value, dt_long_PS$Variable, p.adj = "bonferroni")) # a a a
  print(kruskal(dt_long_NV$Value, dt_long_NV$Variable, p.adj = "bonferroni")) # a a a
  print(kruskal(dt_long_MB$Value, dt_long_MB$Variable, p.adj = "bonferroni")) # abc
  print(kruskal(dt_long_IT$Value, dt_long_IT$Variable, p.adj = "bonferroni")) # aab
  print(kruskal(dt_long_AI$Value, dt_long_AI$Variable, p.adj = "bonferroni")) # a,ab,b
  print(kruskal(dt_long_BP$Value, dt_long_BP$Variable, p.adj = "bonferroni")) # aaa
  
  
  #Fe
  dt <- read.delim("SLT_pXRF_ICP.txt")
  dt$Predicted_Fe_ICP <- 28.88747 + (1.41673* dt$Fe_concentration) + (-316.95475* dt$Substrate_RT)
  
  
  dt_selected <- dt[dt$Scientific_Name %in% c("Xanthisma gracile", "Pseudognaphalium canescens", "Boechera perennans",
                                              "Nultuma (Prosopis) velutina", 'Allionia incarnata', "Isocoma cf. tenuisecta","Mimosa biuncifera (=aculeaticarpa)", "Portulaca suffrutescens"),]
 
  dt_long <- dt_selected %>%
    pivot_longer(cols = c(Fe_ICP, Predicted_Fe_ICP, Fe_concentration),
                 names_to = "Variable",
                 values_to = "Value")
  
  ggplot(dt_long, aes(x = Scientific_Name, y = Value, fill = Variable)) +
    geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.7, size=0.2, outlier.size = 0.6) +
    labs(x = "Scientific Name", y = "Values") +
    scale_fill_manual(values = c("Fe_ICP" = "#6699CC", "Predicted_Fe_ICP" = "#36454F", "Fe_concentration" = "#D3D3D3")) +
    coord_flip()+
    theme_minimal() +
    theme(
      axis.text.x = element_text()
    )
  
  library(agricolae)
  dt_long_XG <- subset(dt_long, Scientific_Name=='Xanthisma gracile')
  dt_long_PC <- subset(dt_long, Scientific_Name=='Pseudognaphalium canescens')
  dt_long_PS <- subset(dt_long, Scientific_Name=='Portulaca suffrutescens')
  dt_long_NV <- subset(dt_long, Scientific_Name=='Nultuma (Prosopis) velutina')
  dt_long_MB <- subset(dt_long, Scientific_Name=='Mimosa biuncifera (=aculeaticarpa)')
  dt_long_IT <- subset(dt_long, Scientific_Name=='Isocoma cf. tenuisecta')
  dt_long_AI <- subset(dt_long, Scientific_Name=='Allionia incarnata')
  dt_long_BP <- subset(dt_long, Scientific_Name=='Boechera perennans')
  
  print(kruskal(dt_long_XG$Value, dt_long_XG$Variable, p.adj = "bonferroni")) # Predicted a, ICP a, pxRF a,
  print(kruskal(dt_long_PC$Value, dt_long_PC$Variable, p.adj = "bonferroni")) # a a a
  print(kruskal(dt_long_PS$Value, dt_long_PS$Variable, p.adj = "bonferroni")) # a a a
  print(kruskal(dt_long_NV$Value, dt_long_NV$Variable, p.adj = "bonferroni")) # a a a
  print(kruskal(dt_long_MB$Value, dt_long_MB$Variable, p.adj = "bonferroni")) # a a a
  print(kruskal(dt_long_IT$Value, dt_long_IT$Variable, p.adj = "bonferroni")) # a a a
  print(kruskal(dt_long_AI$Value, dt_long_AI$Variable, p.adj = "bonferroni")) # a a a
  print(kruskal(dt_long_BP$Value, dt_long_BP$Variable, p.adj = "bonferroni")) # a ab b
  
  
  
  #Zn
  dt <- read.delim("SLT_pXRF_ICP.txt")
  dt <- dt[dt$Zn_concentration != 0.3, ] # To remove LODs
  dt$Predicted_Zn_ICP <- 50.8422 + (0.9560* dt$Zn_concentration) + (-473.9784* dt$Substrate_RT)
  
  
  dt_selected <- dt[dt$Scientific_Name %in% c("Xanthisma gracile", 
                                              "Nultuma (Prosopis) velutina", "Isocoma cf. tenuisecta","Mimosa biuncifera (=aculeaticarpa)", "Portulaca suffrutescens"),]
  
  dt_long <- dt_selected %>%
    pivot_longer(cols = c(Zn_ICP, Predicted_Zn_ICP, Zn_concentration),
                 names_to = "Variable",
                 values_to = "Value")
  
  ggplot(dt_long, aes(x = Scientific_Name, y = Value, fill = Variable)) +
    geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.7, size=0.3) +
    labs(x = "Scientific Name", y = "Values") +
    scale_fill_manual(values = c("Zn_ICP" = "#6699CC", "Predicted_Zn_ICP" = "#36454F", "Zn_concentration" = "#D3D3D3")) +
    coord_flip()+
    theme_minimal() +
    theme(
      axis.text.x = element_text()
    )
  
  library(agricolae)
  dt_long_XG <- subset(dt_long, Scientific_Name=='Xanthisma gracile')
  dt_long_PC <- subset(dt_long, Scientific_Name=='Pseudognaphalium canescens')
  dt_long_PS <- subset(dt_long, Scientific_Name=='Portulaca suffrutescens')
  dt_long_NV <- subset(dt_long, Scientific_Name=='Nultuma (Prosopis) velutina')
  dt_long_MB <- subset(dt_long, Scientific_Name=='Mimosa biuncifera (=aculeaticarpa)')
  dt_long_IT <- subset(dt_long, Scientific_Name=='Isocoma cf. tenuisecta')
  dt_long_AI <- subset(dt_long, Scientific_Name=='Allionia incarnata')
  dt_long_BP <- subset(dt_long, Scientific_Name=='Boechera perennans')
  
  print(kruskal(dt_long_XG$Value, dt_long_XG$Variable, p.adj = "bonferroni")) # Predicted a, ICP a, pxRF a,

  print(kruskal(dt_long_PS$Value, dt_long_PS$Variable, p.adj = "bonferroni")) # a ab a
  print(kruskal(dt_long_NV$Value, dt_long_NV$Variable, p.adj = "bonferroni")) # a a a
  print(kruskal(dt_long_MB$Value, dt_long_MB$Variable, p.adj = "bonferroni")) # a ab a
  print(kruskal(dt_long_IT$Value, dt_long_IT$Variable, p.adj = "bonferroni")) # a a a

  
  
  #Mn
  dt <- read.delim("SLT_pXRF_ICP.txt")
  dt <- dt[dt$Mn_concentration != 0.5, ] # To remove LODs
  dt$Predicted_Mn_ICP <- 51.4943 + (1.0760* dt$Mn_concentration) + (-431.8509* dt$Substrate_RT)

  
  
  dt_selected <- dt[dt$Scientific_Name %in% c("Xanthisma gracile","Nultuma (Prosopis) velutina", "Isocoma cf. tenuisecta",
                                              "Mimosa biuncifera (=aculeaticarpa)"),]
  
  dt_long <- dt_selected %>%
    pivot_longer(cols = c(Mn_ICP, Predicted_Mn_ICP, Mn_concentration),
                 names_to = "Variable",
                 values_to = "Value")
  
  ggplot(dt_long, aes(x = Scientific_Name, y = Value, fill = Variable)) +
    geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.7, size=0.2, outlier.size = 0.6) +
    labs(x = "Scientific Name", y = "Values") +
    scale_fill_manual(values = c("Mn_ICP" = "#6699CC", "Predicted_Mn_ICP" = "#36454F", "Mn_concentration" = "#D3D3D3")) +
    coord_flip()+
    theme_minimal() +
    theme(
      axis.text.x = element_text()
    )
  
  library(agricolae)
  dt_long_XG <- subset(dt_long, Scientific_Name=='Xanthisma gracile')
  dt_long_PC <- subset(dt_long, Scientific_Name=='Pseudognaphalium canescens')
  dt_long_PS <- subset(dt_long, Scientific_Name=='Portulaca suffrutescens')
  dt_long_NV <- subset(dt_long, Scientific_Name=='Nultuma (Prosopis) velutina')
  dt_long_MB <- subset(dt_long, Scientific_Name=='Mimosa biuncifera (=aculeaticarpa)')
  dt_long_IT <- subset(dt_long, Scientific_Name=='Isocoma cf. tenuisecta')
  dt_long_AI <- subset(dt_long, Scientific_Name=='Allionia incarnata')
  dt_long_BP <- subset(dt_long, Scientific_Name=='Boechera perennans')
  
  print(kruskal(dt_long_XG$Value, dt_long_XG$Variable, p.adj = "bonferroni")) # Predicted a, ICP a, pxRF a,
  print(kruskal(dt_long_PC$Value, dt_long_PC$Variable, p.adj = "bonferroni")) # a a a
  print(kruskal(dt_long_PS$Value, dt_long_PS$Variable, p.adj = "bonferroni")) # a a a
  print(kruskal(dt_long_NV$Value, dt_long_NV$Variable, p.adj = "bonferroni")) # a a a
  print(kruskal(dt_long_MB$Value, dt_long_MB$Variable, p.adj = "bonferroni")) # a a a
  print(kruskal(dt_long_IT$Value, dt_long_IT$Variable, p.adj = "bonferroni")) # a a a
  print(kruskal(dt_long_AI$Value, dt_long_AI$Variable, p.adj = "bonferroni")) # a a a
  print(kruskal(dt_long_BP$Value, dt_long_BP$Variable, p.adj = "bonferroni")) # a ab b
  
  
  #Se
  dt <- read.delim("SLT_pXRF_ICP.txt")
  dt <- dt[dt$Se_concentration != 0.05, ] # To remove LODs

  
  dt_selected <- dt[dt$Scientific_Name %in% c("Xanthisma gracile", "Boechera perennans",
                                              "Nultuma (Prosopis) velutina", 'Allionia incarnata', "Isocoma cf. tenuisecta","Mimosa biuncifera (=aculeaticarpa)"),]
  
  
  
}


