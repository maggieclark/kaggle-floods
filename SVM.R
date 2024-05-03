library(tidyverse)
library(e1071)

setwd('C:/Users/clark/Documents/GitHub/kaggle-floods')

imported = read_csv('train.csv')

# 5 fold CV

# divide into 5 folds
fold_assn = sample(c('fold1', 'fold2', 'fold3', 'fold4', 'fold5'), nrow(imported), replace=TRUE)
data = cbind(imported, fold_assn)
rm(fold_assn)

# use folds to create train and test
datasets = data %>% 
  select(!id) %>% 
  fold_ttsplit("fold1")

# fit  
m = svm(FloodProbability ~ ., data = datasets[[2]], kernel = "linear", cost = 10, scale = FALSE)

# predict
Xval = datasets[[1]] %>% select(!FloodProbability)
yhat <- predict(m, Xval)

ybar = mean(datasets[[2]]$FloodProbability)
results = cbind(y = datasets[[2]]$FloodProbability, yhat) %>% 
  as.data.frame() %>% 
  mutate(sq_error = (y-yhat)^2,
         sq_ref = (y-ybar)^2)
R2 = 1 - (sum(results$sq_error) / sum(results$sq_ref))
print('R2 calculated')
