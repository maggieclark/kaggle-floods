library(tidyverse)
library(glmnet)

setwd('C:/Users/clark/Documents/GitHub/kaggle-floods')

imported = read_csv('train.csv')

# determine whether regularization is necessary
# actually not really
cor(imported)

# determine shape of outcome data
hist(imported$FloodProbability)

# CV to determine best lambda
X = imported %>% select(!c(FloodProbability, id)) %>% as.matrix()
y = imported %>% select(FloodProbability) %>% as.matrix()

### elastic net, lambda.min, gaussian, standardize ###
# note y is standardized to unit variance using 1/n
ingredients = cv.glmnet(X, 
                        y, 
                        family="gaussian", 
                        standardize=TRUE,
                        keep = TRUE)

# index of best lambda
idx = ingredients$index[1]

# calculate R2

# prevalidated array - contains y_hat from 10 folds using best lambda
y_mean = mean(y)

preval = cbind(y_hat = ingredients$fit.preval[,idx], 
               fold = ingredients$foldid,
               FloodProbability = y) %>% 
  as.data.frame() %>% 
  mutate(sq_error = (FloodProbability-y_hat)^2,
         sq_ref = (FloodProbability-y_mean)^2)

# loop through folds and calculate 10 kaggle-errors
R2s <- c()

for (i in 1:10){
  fold_dataset = preval %>% filter(fold == i) 
  R2 = 1 - (sum(fold_dataset$sq_error) / sum(fold_dataset$sq_ref))
  R2s = c(R2s, R2)
}

# validation error
mean(R2s) # 0.8449016


### elastic net, lambda.min, gaussian, non-standardized ###
# note y is standardized to unit variance using 1/n
ingredients = cv.glmnet(X, 
                        y, 
                        family="gaussian", 
                        keep = TRUE)

# index of best lambda
idx = ingredients$index[1]

# calculate R2

# prevalidated array - contains y_hat from 10 folds using best lambda
y_mean = mean(y)

preval = cbind(y_hat = ingredients$fit.preval[,idx], 
               fold = ingredients$foldid,
               FloodProbability = y) %>% 
  as.data.frame() %>% 
  mutate(sq_error = (FloodProbability-y_hat)^2,
         sq_ref = (FloodProbability-y_mean)^2)

# loop through folds and calculate 10 kaggle-errors
R2s <- c()

for (i in 1:10){
  fold_dataset = preval %>% filter(fold == i) 
  R2 = 1 - (sum(fold_dataset$sq_error) / sum(fold_dataset$sq_ref))
  R2s = c(R2s, R2)
}

# validation error
mean(R2s) # 0.8449032


### strict lasso, lambda.min, gaussian, non-standardized ###
# note y is standardized to unit variance using 1/n

ingredients = cv.glmnet(X, 
                        y,
                        alpha=1,
                        family="gaussian", 
                        keep = TRUE)

# index of best lambda
idx = ingredients$index[1]

# calculate R2

# prevalidated array - contains y_hat from 10 folds using best lambda
y_mean = mean(y)

preval = cbind(y_hat = ingredients$fit.preval[,idx], 
               fold = ingredients$foldid,
               FloodProbability = y) %>% 
  as.data.frame() %>% 
  mutate(sq_error = (FloodProbability-y_hat)^2,
         sq_ref = (FloodProbability-y_mean)^2)

# loop through folds and calculate 10 kaggle-errors
R2s <- c()

for (i in 1:10){
  fold_dataset = preval %>% filter(fold == i) 
  R2 = 1 - (sum(fold_dataset$sq_error) / sum(fold_dataset$sq_ref))
  R2s = c(R2s, R2)
}

# validation error
mean(R2s) # 0.8449012


### strict lasso, lambda.min, gaussian, standardized ### *********
# note y is standardized to unit variance using 1/n

ingredients = cv.glmnet(X, 
                        y,
                        alpha=1,
                        family="gaussian", 
                        standardize = TRUE,
                        keep = TRUE)

# index of best lambda (lambda.min)
idx = ingredients$index[1]

# calculate R2

# prevalidated array - contains y_hat from 10 folds using best lambda
y_mean = mean(y)

preval = cbind(y_hat = ingredients$fit.preval[,idx], 
               fold = ingredients$foldid,
               FloodProbability = y) %>% 
  as.data.frame() %>% 
  mutate(sq_error = (FloodProbability-y_hat)^2,
         sq_ref = (FloodProbability-y_mean)^2)

# loop through folds and calculate 10 kaggle-errors
R2s <- c()

for (i in 1:10){
  fold_dataset = preval %>% filter(fold == i) 
  R2 = 1 - (sum(fold_dataset$sq_error) / sum(fold_dataset$sq_ref))
  R2s = c(R2s, R2)
}

# validation error
mean(R2s) # 0.8449059 # ***

# predict (from best CV model - on 90%)
test_imported = read_csv('test.csv')
testset = test_imported %>% select(!id) %>% as.matrix()

final_yhat = predict(
  object = ingredients,
  testset,
  s = "lambda.min")

submission = cbind(test_imported$id, final_yhat) %>% as.data.frame()
names(submission) <- c('id', 'FloodProbability')
write_csv(submission, 'submission_5.3.csv')

### strict ridge, lambda.min, gaussian, standardized ###
# note y is standardized to unit variance using 1/n

ingredients = cv.glmnet(X, 
                        y,
                        alpha=0,
                        family="gaussian", 
                        standardize = TRUE,
                        keep = TRUE)

# index of best lambda
idx = ingredients$index[1]

# calculate R2

# prevalidated array - contains y_hat from 10 folds using best lambda
y_mean = mean(y)

preval = cbind(y_hat = ingredients$fit.preval[,idx], 
               fold = ingredients$foldid,
               FloodProbability = y) %>% 
  as.data.frame() %>% 
  mutate(sq_error = (FloodProbability-y_hat)^2,
         sq_ref = (FloodProbability-y_mean)^2)

# loop through folds and calculate 10 kaggle-errors
R2s <- c()

for (i in 1:10){
  fold_dataset = preval %>% filter(fold == i) 
  R2 = 1 - (sum(fold_dataset$sq_error) / sum(fold_dataset$sq_ref))
  R2s = c(R2s, R2)
}

# validation error
mean(R2s) # 0.8444889


### elastic net, lambda.1se, gaussian, standardized ###
# note y is standardized to unit variance using 1/n
ingredients = cv.glmnet(
  X, 
  y, 
  family="gaussian", 
  standardize=TRUE,
  keep = TRUE
  )

# calculate R2

# index of best lambda
idx = ingredients$index[2]

# prevalidated array - contains y_hat from 10 folds using best lambda
y_mean = mean(y)

preval = cbind(y_hat = ingredients$fit.preval[,idx], 
               fold = ingredients$foldid,
               FloodProbability = y) %>% 
  as.data.frame() %>% 
  mutate(sq_error = (FloodProbability-y_hat)^2,
         sq_ref = (FloodProbability-y_mean)^2)

# loop through folds and calculate 10 kaggle-errors
R2s <- c()

for (i in 1:10){
  fold_dataset = preval %>% filter(fold == i) 
  R2 = 1 - (sum(fold_dataset$sq_error) / sum(fold_dataset$sq_ref))
  R2s = c(R2s, R2)
}

# validation error
mean(R2s) # 0.8445797


### elastic net, lambda.1se, gaussian, non-standardized ###
# note y is standardized to unit variance using 1/n
ingredients = cv.glmnet(
  X, 
  y, 
  family="gaussian", 
  standardize=FALSE,
  keep = TRUE
)

# calculate R2

# index of best lambda
idx = ingredients$index[2]

# prevalidated array - contains y_hat from 10 folds using best lambda
y_mean = mean(y)

preval = cbind(y_hat = ingredients$fit.preval[,idx], 
               fold = ingredients$foldid,
               FloodProbability = y) %>% 
  as.data.frame() %>% 
  mutate(sq_error = (FloodProbability-y_hat)^2,
         sq_ref = (FloodProbability-y_mean)^2)

# loop through folds and calculate 10 kaggle-errors
R2s <- c()

for (i in 1:10){
  fold_dataset = preval %>% filter(fold == i) 
  R2 = 1 - (sum(fold_dataset$sq_error) / sum(fold_dataset$sq_ref))
  R2s = c(R2s, R2)
}

# validation error
mean(R2s) # 0.8447976
