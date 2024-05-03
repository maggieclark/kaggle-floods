fold_ttsplit = function(prepped_data, outcome_column, fold){
  
  train  <- prepped_data %>% filter(fold_assn == fold) %>% select(!fold_assn)
  print("train created")
  test   <- prepped_data %>% filter(fold_assn != fold) %>% select(!fold_assn)
  print('test created')
  
  return(list(train, test))
}
