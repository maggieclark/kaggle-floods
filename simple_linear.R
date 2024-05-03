library(tidyverse)

setwd('C:/Users/clark/Documents/GitHub/kaggle-floods')

imported = read_csv('train.csv')

# 5 fold CV

# divide into 5 folds
fold_assn <- sample(c('fold1', 'fold2', 'fold3', 'fold4', 'fold5'), nrow(imported), replace=TRUE)
data = cbind(imported, fold_assn)

metrics = c()
for (f in c('fold1', 'fold2', 'fold3', 'fold4', 'fold5')){
  
  datasets = fold_ttsplit(data, "SalePrice", f)
  print('datasets created')
  
  mod = lm(FloodProbability ~ ., datasets[[1]])
  print('model fitted')
  
  Xval = datasets[[2]] %>% select(!FloodProbability)
  yhat = predict(mod, Xval)
  print('yhat generated')
  
  ybar = mean(datasets[[2]]$FloodProbability)
  results = cbind(y = datasets[[2]]$FloodProbability, yhat) %>% 
    as.data.frame() %>% 
    mutate(sq_error = (y-yhat)^2,
           sq_ref = (y-ybar)^2)
  R2 = 1 - (sum(results$sq_error) / sum(results$sq_ref))
  print('R2 calculated')
  
  metrics = append(metrics, R2)
  
}

metrics

mean(metrics) # 0.8449308
