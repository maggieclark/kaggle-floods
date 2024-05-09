library(tidyverse)

yhatlasso = read_csv('C:/Users/clark/Documents/GitHub/kaggle-floods/submissions/submission_5.3.csv')

yhatnn = read_csv('C:/Users/clark/Documents/GitHub/kaggle-floods/submissions/submission5_7.csv')

df = left_join(yhatlasso, yhatnn, by = 'id', suffix = c('.lasso', '.nn'))

# avg

yhatavg = df %>% 
  mutate(FloodProbability = (FloodProbability.lasso + FloodProbability.nn)/2) %>% 
  select(c(id, FloodProbability))

write_csv(yhatavg, 'C:/Users/clark/Documents/GitHub/kaggle-floods/submissions/nn_lasso_avg_5.9.csv')

# piecewise

min(yhatnn$FloodProbability)
min(yhatlasso$FloodProbability)

yhatpw = df %>% 
  mutate(FloodProbability = ifelse(FloodProbability.lasso < 0.3811388,
                                   FloodProbability.lasso, 
                                   FloodProbability.nn)) %>% 
  select(c(id, FloodProbability))

write_csv(yhatpw, 'C:/Users/clark/Documents/GitHub/kaggle-floods/submissions/nn_lasso_pw1_5.9.csv')
