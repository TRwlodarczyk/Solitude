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
  scale_x_continuous(limits = c(350, 2500), breaks = c(seq(350, 2500, by = 175),2500)) +
  scale_y_continuous(limits = c(0, 0.95), breaks = seq(0, 0.95, by = 0.05)) +
  geom_vline(xintercept = 700, linetype = "dashed", color = "#9a9a9a", size = 1.2) +
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



setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research")
dt <-read.delim("Correlation_table.txt")


# Assuming your dataset is named "dt"

# Create an empty vector to store the correlation coefficients
correlations <- vector("double", length = 2151)  # 2151 = 2155 - 5 + 1

# Loop through each column from 5 to 2155
for (i in 5:2155) {
  # Calculate the Spearman correlation between column i and "Zn_concentration"
  correlation <- cor(dt[, i], dt$Zn_concentration, method = "spearman")
  
  # Store the correlation coefficient in the vector
  correlations[i - 4] <- correlation
}

# Print the correlation coefficients
print(correlations)

# Assuming your dataset is named "dt"
library(openxlsx)

# Create an empty dataframe to store the correlation coefficients
cor_df <- data.frame(variable = character(2151), correlation = numeric(2151), stringsAsFactors = FALSE)

# Loop through each column from 5 to 2155
for (i in 5:2155) {
  # Calculate the Spearman correlation between column i and "Zn_concentration"
  correlation <- cor(dt[, i], dt$Zn_concentration, method = "spearman")
  
  # Store the variable name and correlation coefficient in the dataframe
  cor_df[i - 4, "variable"] <- names(dt)[i]
  cor_df[i - 4, "correlation"] <- correlation
}

# Save the dataframe as an Excel file
write.xlsx(cor_df, "correlations.xlsx", row.names = FALSE)

write.table(cor_df, file="C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research/corr.csv", sep=",", row.names = F)


cor.test(dt$Zn_concentration, dt$X1120, method="spearman")
cor.test(dt$Zn_concentration, dt$X686, method="spearman")
cor.test(dt$Zn_concentration, dt$X687, method="spearman")
cor.test(dt$Zn_concentration, dt$X1121, method="spearman")
cor.test(dt$Zn_concentration, dt$X1122, method="spearman")
cor.test(dt$Zn_concentration, dt$X684, method="spearman")
cor.test(dt$Zn_concentration, dt$X685, method="spearman")







corr <-read.delim("corr2.txt")

ggplot(data=corr, aes(x=variable, y=correlation)) +
  geom_line()





############# 1st derivative maxima


install.packages("pracma")  # For numerical differentiation

library(readxl)
library(pracma)
library(ggplot2)


setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Hyperspectral Research")
dt <-read.delim("Correlation_table.txt")



# Assuming reflectance data starts from column 5 and ends at column 2155
reflectance_data <- dt[, 5:2155]

# Calculate the first derivative of the reflectance data for each row
first_derivative <- apply(reflectance_data, 1, function(row) diff(row))

# Find the maxima in the first derivative (Red Edge position) for each row
maxima_indices <- sapply(seq_len(nrow(first_derivative)), function(i) {
  peaks <- pracma::findpeaks(first_derivative[i,])
  if (length(peaks) > 0) {
    peaks[which.max(peaks)]
  } else {
    NA
  }
})

# Get the corresponding wavelengths for the maxima
wavelengths <- seq(350, 2500, length.out = 2150)  # Keep the same length as maxima_indices

# Calculate the REP wavelengths for each sample based on the maxima indices
REP_wavelength <- wavelengths[maxima_indices]

# Create a data frame for plotting
plot_data <- data.frame(wavelength = wavelengths, first_derivative = first_derivative)

# Load the reshape2 package
library(reshape2)

# Convert first_derivative to a long format
plot_data_long <- melt(plot_data, id.vars = "wavelength", variable.name = "Sample", value.name = "FirstDerivative")

# Plot the first derivative
# Define a custom color palette with 12 distinct colors for each sample
custom_color_palette <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#a6cee3",
                                   "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6", "#ffff99", "#b15928")
                                   
# Plot the first derivative with different colors for each sample
ggplot(plot_data_long, aes(x = wavelength, y = FirstDerivative, color = Sample)) +
  geom_line() +
  geom_vline(xintercept = REP_wavelength, linetype = "dashed", color = "red") +
  scale_color_manual(values = custom_color_palette) +  # Apply the custom color palette
  labs(x = "Wavelength (nm)", y = "First Derivative", title = "First Derivative of Reflectance Spectrum")


