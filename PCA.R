library(tidyverse)

setwd('C:/Users/clark/Documents/GitHub/kaggle-floods')

imported = read_csv('train.csv')

# PCA

X = imported %>% select(!c(FloodProbability, id))
y = imported %>% select(FloodProbability)

PCs = prcomp(X, scale = TRUE)
PCs$sdev^2 / sum(PCs$sdev^2) # first PC only explains 5% of variance :(
