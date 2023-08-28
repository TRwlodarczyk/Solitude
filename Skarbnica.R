# Tomasz Wlodarczyk
#Skarbnica kodow


setwd("C:/Users/twl")
dt <-read.delim("So.txt")
str(dt)

{
  tr <- matrix(data = NA, ncol = ncol(dt[,c(1:75)]), nrow=nrow(dt)) # select all columns 1:46
  colnames(tr) <- colnames(dt[,c(1:75)])
  for (i in 12:75) # select when the concentrations start
  {
    tr[,c(i)] <- gsub(".*ND.*", 0, dt[,i])
  }
  
  for(i in c(1:11)) # select columns that need to stay the same 1:11 include character and double (weight)
  {
    tr[,c(i)] <- dt[,c(i)]
  }
  
  
  #transform to dataframe
  tr <- as.data.frame.matrix(tr) #A correct command to change the dataset to dataframe after transformations
  tr[,12:75] <- sapply(tr[,12:75],as.numeric) # Change a character to numeric (double)
  typeof(tr$Cu_pXRF) # confirm the value is no longer a character
  dt <- tr
}
dt_T <- subset(dt, Site == "TAILINGS")



# Tests

shapiro.test(dt$Cu_ICP)

t.test(C2$Cu_ICP, P1$Cu_ICP) # two-sample t-test
t.test(C2$Cu_ICP, P1$Cu_ICP, paired = TRUE) # dependent t-test

anova_result <- aov(Cu_ICP ~ Plot, data = dt)
tukey_result <- TukeyHSD(anova_result) # posthoc for anova

library(agricolae)
print(kruskal(dt$Be_ICP, dt$Plot, group=TRUE,p.adj="bonferroni"))

wilcox.test(C2$Cu_ICP, P1$Cu_ICP, mu = 0, paired = TRUE, alternative = "two.sided") # Wilcoxon Signed Rank Test for one-sample data
wilcox.test(C2$Cu_ICP, P1$Cu_ICP, alternative = "two.sided") # Mann-Whitney U Test for two-sample data - Wilcoxon Sum Rank
