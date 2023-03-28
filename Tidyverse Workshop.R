# Iris and tidyverse
# Tomasz Wlodarczyk
# twlodarczyk@arizona.edu
# 2023-03-28

library(tidyverse)

setosa_mean <- mean(iris$Sepal.Length[iris$Species=="setosa"]) # mean for sepal length for setosa species

# Group the iris data by values in the Species column using tidyverse

iris_grouped <- group_by(iris, Species) # <dbl> is double, <fct> is factor so character
iris_grouped

# Calculate sepal length mean 

iris_means <- summarize(iris_grouped, SL_mean = mean(Sepal.Length))
iris_means

iris_means2 <- summarize(iris, SL_mean = mean(Sepal.Length)) # if you don't group by species you will get a mean for all species at once


# pipe is %. % takses whatever is on the left and send it to the right

iris %>% group_by(Species) # we take it iris and stick it right before Species
group_by(iris, Species) # the same without pipe

# the same way we did before but all together
iris_means <- iris %>% # and then
  group_by(Species) %>% # and then
  summarize(SL_mean = mean(Sepal.Length)) # creat a new column that does mean of sepal length
iris_means


#%>% # ctrl + shift + m : create a shortcut 
# <- # shortcut is ALT + -

# calculate a standard error sigma/square root of n

iris_means <- iris %>% # and then
  group_by(Species) %>% # and then
  summarize(SL_mean = mean(Sepal.Length),
            SL_se = sd(Sepal.Length)/sqrt(n())) # create new column for the standard error
iris_means


# Plot sepal length data

ggplot(data = iris_means, mapping = aes(x=Species, y=SL_mean))+
  geom_point()+
  geom_errorbar(mapping=aes(ymin = SL_mean - SL_se, 
                            ymax = SL_mean + SL_se), 
                width=0.3)
