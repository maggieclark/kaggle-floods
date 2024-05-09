library(tidyverse)

yhatlasso = read_csv('C:/Users/clark/Documents/GitHub/kaggle-floods/submission_5.3.csv')

yhatnn = read_csv('C:/Users/clark/Documents/GitHub/kaggle-floods/submission5_7.csv')

df = left_join(yhatlasso, yhatnn, by = 'id', suffix = c('.lasso', '.nn'))

yhatavg = df %>% 
  mutate(FloodProbability = (FloodProbability.lasso + FloodProbability.nn)/2) %>% 
  select(c(id, FloodProbability))

write_csv(yhatavg, 'C:/Users/clark/Documents/GitHub/kaggle-floods/nn_lasso_avg_5.9.csv')
