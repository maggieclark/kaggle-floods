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

sum(yhatlasso$FloodProbability < 0.325)
yhatpw2 = df %>% 
  mutate(FloodProbability = ifelse(FloodProbability.lasso < 0.325,
                                   FloodProbability.lasso, 
                                   FloodProbability.nn)) %>% 
  select(c(id, FloodProbability))
write_csv(yhatpw2, 'C:/Users/clark/Documents/GitHub/kaggle-floods/submissions/nn_lasso_pw2_5.9.csv')

yhatpw3 = df %>% 
  mutate(FloodProbability = ifelse(FloodProbability.lasso < 0.33,
                                   FloodProbability.lasso, 
                                   FloodProbability.nn)) %>% 
  select(c(id, FloodProbability))
write_csv(yhatpw3, 'C:/Users/clark/Documents/GitHub/kaggle-floods/submissions/nn_lasso_pw3_5.15.csv')

yhatpw4 = df %>% 
  mutate(FloodProbability = ifelse(FloodProbability.lasso < 0.34,
                                   FloodProbability.lasso, 
                                   FloodProbability.nn)) %>% 
  select(c(id, FloodProbability))
write_csv(yhatpw4, 'C:/Users/clark/Documents/GitHub/kaggle-floods/submissions/nn_lasso_pw4_5.15.csv')

yhatpw5 = df %>% 
  mutate(FloodProbability = ifelse(FloodProbability.lasso < 0.35,
                                   FloodProbability.lasso, 
                                   FloodProbability.nn)) %>% 
  select(c(id, FloodProbability))
write_csv(yhatpw5, 'C:/Users/clark/Documents/GitHub/kaggle-floods/submissions/nn_lasso_pw5_5.15.csv')

yhatpw6 = df %>% 
  mutate(FloodProbability = ifelse(FloodProbability.lasso < 0.36,
                                   FloodProbability.lasso, 
                                   FloodProbability.nn)) %>% 
  select(c(id, FloodProbability))
write_csv(yhatpw6, 'C:/Users/clark/Documents/GitHub/kaggle-floods/submissions/nn_lasso_pw6_5.15.csv')

yhatpw7 = df %>% 
  mutate(FloodProbability = ifelse(FloodProbability.lasso < 0.37,
                                   FloodProbability.lasso, 
                                   FloodProbability.nn)) %>% 
  select(c(id, FloodProbability))
write_csv(yhatpw7, 'C:/Users/clark/Documents/GitHub/kaggle-floods/submissions/nn_lasso_pw7_5.15.csv')
