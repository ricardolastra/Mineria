data$good_loan <- as.factor(
  ifelse(
    data$good_loan == 1, 
    'GoodLoan', 
    'BadLoan'
  )
)

data <- data %>% 
  mutate_all(funs(german_decode))


