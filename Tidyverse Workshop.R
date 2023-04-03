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

#Make long-format data for calculations. Melt the table into Species, trait, value

iris_long <- pivot_longer(data=iris, 
                          cols = -Species, # tell to ignore Species column so it doesn't melt it
                          names_to = "trait",
                          values_to = "measurement") 
iris_long


iris_means <- iris %>% 
  pivot_longer(cols= -Species,
               names_to="trait",
               values_to="measurement") %>%
  group_by(Species, trait) %>%
  summarize(trait_mean=mean(measurement),
            trait_se=sd(measurement)/sqrt(n()))
iris_means # you can always ungroup by ungroup()


# ggplot 

ggplot(data = iris_means, mapping = aes(x=Species, y=trait_mean))+
  geom_point()+
  geom_errorbar(mapping=aes(ymin = trait_mean - trait_se, 
                            ymax = trait_mean + trait_se), 
                width=0.3)+
  facet_wrap(~ trait) # create separate for each column instead one graph

iris_means

# remove dot # do SLT!!!!
iris_means$trait <- gsub(pattern=".",
                         replacement=" ",
                         x = iris_means$trait,
                         fixed = TRUE)
iris_means
