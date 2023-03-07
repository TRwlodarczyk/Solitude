library(readr)
library(dplyr)
library(tidyr)

baked_goods <- read_csv("https://gist.githubusercontent.com/Aariq/5f990c9e3067ddcdbae6f63a2726dfed/raw/6c383454c0a4018c5dce90dce903081537bbca3e/baked_goods.csv")
head(baked_goods)

ingredients <- baked_goods |> select(-type, -recipe_id) # with dplyr package, PCA doesnt care whether it's a cupcake and muffin so we remove it, also we can do %>% instead of |>
head(ingredients)


baked_pca <- prcomp(ingredients, scale. = TRUE)


ingredients # flour will be bigger because we use more flour than baking soda


scale(ingredients) # scaled by standard deviation (flour >>> normally than baking soda). prcomp do it for you



summary(baked_pca)
#for every PC through 29 it shows us the proportion of SD, proportion of variance explained by that component
#those in the end don't explain additional component of the data!!!!! so the following PC explain more and more


plot(baked_pca) # each bar is the component and what it explains

#Visualize results


biplot(baked_pca) # overplotted plotting scores and loading on the top of each other

library(palmerpenguins)
head(penguins)

#MY VERSION
trimmed <- penguins |> select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) |> drop_na() #library(tidyr) to drop na
trimmed # I think it only shows those two because it remove the whole row

trimmedPCA <- prcomp(trimmed, scale. = TRUE)
summary(trimmedPCA) #68% explained

#WORKSHOP VERSION BY ERIC SCOTT

X<-penguins %>%
  select(-species, -island, -sex, -year) %>%
  drop_na()

penguin_pca <- prcomp(X, scale.=TRUE)
biplot(penguin_pca) 

#USING ropls package, to install you can't find it under INstal Tool you need to use bioconductor
#install bioc manager first > install.packages("BiocManager")

#if (!require("BiocManager", quietly=TRUE))
#  install.packages(BiocManager)

#BiocManager::install("ropls")

#pre number of component

library(ropls)
ingradients
baked_pca <- opls(x=ingredients)
plot(baked_pca)
#what methods are available for opls, for plot function help ?plot.opls

?plot.opls

#Go to Package pane next to plots to see print.opls etc and methods specific
# parAsColFcVn = NA, is a parameter that make color and accept factors and vectors

class(baked_pca)
plot(baked_pca, typeVc ="x-score", parAsColFcVn=baked_goods$type)


#when y variable is categorical PLS is discrimant analysis PLS DA
baked_pls <-opls(ingredients, baked_goods$type) #cannot plot anything it's just onec axis, ingredients is dfataframe, baked_goods$type need to be a vactor
baked_opls <-opls(ingredients, baked_goods$type, predI = 1, orthoI=NA)
# R2X explains the variation in data, R2Y how much separation between cupcake and muffins is explained
#Q2 a number that is created to crossvalidation. Closer it is to R2Y the better, it's never larger, it gives the info about predictive power of the model
#RMSEE
#pre how many axis, ort orthogonal axis
#pR2Y it shuffle the labels and randomize that, calculate R2Y (it does it 200 times by defoult), that's our p value, a proportion of random permuted that give a greater value than your data

#INCREASE the number of permutations
baked_opls <-opls(ingredients, baked_goods$type, predI = 1, orthoI=NA, permI=999)


#TRY with penguins data yourselve
penguin_opls <- opls(x=X) #it's only one axis!
#so:
peng2<-penguins %>%
  select(bill_length_mm, bill_depth_mm, body_mass_g, flipper_length_mm, species) %>%
  drop_na()
peng2

opls(peng2 |> select(-species), peng2$species) # Score Distance SD - telling us that row 292 is a potential outlier

#loading plot with arrows - it's like weights how its loaded. it's sort of linear regrassion, correlation plot is similar but
#it's looking into what is the correlation coefficient between PC1 and other. 
#top right plot is permutation plot, black dots are q squeres, all permuted values are lot lower than true, 1 - means that randomly shuffled labels happend to be exactly the same as the true values, further to the left they get different from real data

#Q square is through validation, more observations than variables - higher q square



