#link a la funcion en otra ruta
source("utils.R")
data <- readRDS("german.rds")
head(data)

data$X2 <- as.numeric(data$X2)
data$X5 <- as.numeric(data$X5)
data$X13 <- as.numeric(data$X13)

source("metadata.R")

colnames(data) <- german_colnames

library(stringr)
colnames(data) <- str_to_lower(colnames(data))
colnames(data) <- str_replace_all(colnames(data)," ","_")
colnames(data) <- str_replace_all(colnames(data),"/","_")

data$good_loan <- as.factor(
  ifelse(
    data$good_loan == 1, 
    'GoodLoan', 
    'BadLoan'
  )
)

library(tidyverse)

data <- data %>% 
  mutate_all(funs(german_decode))

str(data)

#ya no por que ya se usaron las REGEX
colnames(data)[which(names(data) == "Credit amount")] <- "Credit_amount"
colnames(data)[which(names(data) == "Duration in month")] <- "Duration_in_month"
colnames(data)[which(names(data) == "Age in years")] <- "Age_in_years"
colnames(data)[which(names(data) == "Credit history")] <- "Credit_history"

ggplot(data, aes(Purpose)) + geom_bar()

ggplot(data, aes(Age_in_years, Credit_amount)) + geom_dotplot(method="histodot", binwidth = 1)

ggplot(data, aes(Duration_in_month, Credit_amount)) + geom_point(alpha=0.4)

ggplot(data, aes(Purpose, Credit_amount)) + geom_point(alpha=0.4)

ggplot(data, aes(Property, Credit_amount)) + geom_point(alpha=0.4)

ggplot(data, aes(Purpose, Age_in_years)) + geom_point(alpha=0.4)


ggplot(data, aes(x=Credit_history, fill=good_loan, color=good_loan)) + 
  geom_bar(position = position_stack(reverse = TRUE)) +
  coord_flip() +
  theme(legend.position = "top")

#CLASE.........

data %>%
  group_by(credit_history) %>% 
  dplyr::summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  ggplot(.) + 
  geom_bar(aes(x=reorder(credit_history, count), y = count), stat="identity", fill="gray") + 
  coord_flip() + 
  #theme_hc() + 
  ylab('casos') + 
  xlab('Historial de crédito')


#COMO LO HIZO LILIANA

plot_sorted <- data %>% group_by(credit_history) %>%
  summarise(count=n()) %>%
  arrange(count) #como haremos coord flip necesitamos ordenarlas de manera inversa a como queremos que aparezcan en el coord_flip

plot_sorted$credit_history <- factor(plot_sorted$credit_history,
                                     levels=plot_sorted$credit_history)

ggplot(plot_sorted, aes(x=credit_history, y=count), fill="gray") +
  geom_bar(stat="identity") +
  coord_flip() +
  #theme_hc() +
  ylab("casos") +
  xlab("Historial de crédito")


#SUMARY

summary(data)

#TIDY PARA SEPARAR LAS COLUMNAS DE personal_status_and_sex USAMOS UN SEPARATE PARA QUE ESTE TIDY

#CoockBook como stack en R para ggplot
