library(tidyverse)
setwd("C:/Users/FORANEA110/Desktop/MINERIA/Tareas/algas/")
source("metadata.R")
source("utils.R")
source("00-load.R")
source("01-prepare.R")
source("02-clean.R")

#Prepare
summary(data)
problems(data)
data[20,]
data[problems(data)$row,]

#Clean
problematic_rows <- problems(data)$row

data[problematic_rows,] <- data %>% 
  slice(problematic_rows) %>% 
  unite(col="all", -seq(1:6), sep = "/", remove=TRUE) %>%
  extract(all, into=c("NO3", "NH4", "resto"),
          regex="([0-9]*.[0-9]{5})([0-9]*.[0-9]*)/(.*)/NA", remove=TRUE) %>%
  separate(resto, into=names(data)[9:18], sep="/", remove=TRUE)    

data[19:20,]


data %>%
  slice(problematic_rows) %>% head()

data %>% 
  slice(problematic_rows) %>%
  unite(col="all", -seq(1:6), sep="/", remove=T)

data %>% 
  slice(problematic_rows) %>%
  unite(col="all", -seq(1:6), sep="/", remove=T) %>%
  extract(all, into=c("NO3", "NH4", "resto"),
          regex="([0-9]*.[0-9]{5})([0-9]*.[0-9]*)/(.*)/NA", remove=T)

data <- readr::type_convert(data)

glimpse(data)


#grafica 1
theme_set(theme_bw())
ggplot(data, aes(fluid_velocity, a1), remove=T) +
  geom_boxplot(outlier.shape=NA)+geom_jitter(position=position_jitter(width=.1, height=0),col = 'blue')+
  facet_wrap(~cut_number(a1, 4))

#grafica 2
ggplot(data, aes(cl, a1), remove=T) +
  geom_hex() +
  facet_wrap(~cut_number(a1, 4))

#Nice!!!
library(GGally)
ggpairs(data, aes(colour = season, alpha = 0.4))

#funcion
data <- data.frame(data)
numericvars <- character(length = ncol(data))
charvars <- character(length = ncol(data))
names <- colnames(data)
for(i in seq_len(ncol(data))) {
  if(class(data[, i]) == "numeric") {
    numericvars[i] <- names[i]
  }
  else {
    if(class(data[, i]) == "character") {
      charvars[i] <- names[i]
  }
  }
}

subset_numeric <- data %>% 
  select(one_of(numericvars))

subset_chr <- data %>% 
  select(one_of(charvars))

if(length(subset_numeric)!=0){
  print(subset_numeric %>% ggpairs(alpha = 0.6))
}

if(length(subset_chr)!=0){
  print(subset_chr %>% ggpairs())
}


pares <- combinations(length(colnames(data)),r=2,colnames(data))

for(i in 1:(length(pares[,1]))){
  if(as.character(pares[i,1])==as.character(pares[i,2])){
    next #statement para no graficar mismas vars
    }
  else{
    print(data %>% ggpairs(c(as.character(pares[i,1]),as.character(pares[i,2]))))
  }
}


class(iris)
class(data.frame(data[, 2]))
class(data[, 4])
data <- data.frame(data)



auto_plot <- function(data,f){
  
  theme_set(theme_bw())
  numericvars <- character(length = ncol(data))
  charvars <- character(length = ncol(data))
  names <- colnames(data)
  
  for(i in seq_len(ncol(data))) {
    if(class(data[, i]) == "numeric") {
      numericvars[i] <- names[i]
    }
    else {
      if(class(data[, i]) == "character") {
        charvars[i] <- names[i]
      }
    }
  }
  
  subset_numeric <- data %>% 
    select(one_of(numericvars))
  
  subset_chr <- data %>% 
    select(one_of(charvars))
  
  if(length(subset_numeric)!=0){
    ggpairs(subset_numeric, alpha = 0.6)
  }
  
  if(length(subset_chr)!=0){
    ggpairs(subset_chr)
  }
  
  pares <- combinations(length(colnames(data)),r=2,colnames(data))
  
  for(i in 1:(length(pares[,1]))){
    if(as.character(pares[i,1])==as.character(pares[i,2])){
      next #statement para no graficar mismas vars
    }
    else{
      print(data %>% ggpairs(c(as.character(pares[i,1]),as.character(pares[i,2]))))
    }
  }
  
}
  

x <- as.data.frame(abs(is.na(data))) # df es un data.frame
y <- x[which(sapply(x, sd) > 0)] 
cor(y)

######eJERCICIO 1
porcentaje <- (colSums(y)/nrow(y))*100
porcentaje

library(Amelia)
missmap(data %>% select("max_ph","min_o2","cl","no3","nh4","opo4","po4","chla"), main="Grafica de Missings", 
        col=c("red", "black"), legend=FALSE) 

######Ejercicio 2
summary(data[-grep(colnames(data),pattern = "^a[1-9]")])

data %>%
  select(-starts_with("a")) %>%
  summary()

#####Ejercicio 3

x <- airquality[, -1] # x is a regression design matrix
y <- airquality[,  1] # y is the corresponding response

stopifnot(complete.cases(y) != is.na(y))
ok <- complete.cases(x, y)
sum(!ok) # how many are not "ok" ?
x <- x[ok,]
y <- y[ok]

  
  
  