library(tidyverse)

setwd('C:/Users/clark/Documents/GitHub/kaggle-floods')

imported = read_csv('train.csv')

# determine whether regularization is necessary
cor(imported)

# determine shape of outcome data
hist(imported$FloodProbability)