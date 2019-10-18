library(tidyverse)
setwd("C:/Users/FORANEA110/Desktop/MINERIA/Tareas/algas/tarea_nas")
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

library(stargazer)
suma <- colSums(y)
stargazer::stargazer(suma, type = "html", title = "Suma de Missings por Variable")

######eJERCICIO 1
porcentaje <- (colSums(y)/nrow(y))*100
porcentaje

library(Amelia)
missmap(data %>% select("max_ph","min_o2","cl","no3","nh4","opo4","po4","chla"), main="Grafica de Missings", 
        col=c("red", "black"), legend=FALSE) 

######Ejercicio 2
summary(data[-grep(colnames(data),pattern = "^a[1-9]")]) # Nota el uso del grep

data %>%
  select(-starts_with("a")) %>%
  summary()

nrow(data[!complete.cases(data),])

data_con_NAs <- data[!complete.cases(data),]

data_con_NAs[c("max_ph","min_o2","cl","no3","nh4","opo4","po4","chla")]  %>%
  print(n = 33)





Mode(test,na.rm = TRUE)



# ¿Cuántos NAs hay por observación?
apply(data, 1, function(x) sum(is.na(x)))

data[apply(data, 1, function(x) sum(is.na(x))) > 2,]

###SE USA LA FUNCION  indices_con_NAs

indices_con_NAs(data, 0.2)

indices_con_NAs(data, 0.8)

# Si queremos remover las que tengan más del 20% de NAs...
data <- data[-indices_con_NAs(data, 0.2),]
dim(data)

dataset$cat_with_NAs_fix <- ifelse(is.na(dataset$cat_with_NAs),
                                   "missing",
                                   ifelse(dataset$ccat_with_NAs == TRUE,    
                                          # o el valor que sea
                                          "level_1",
                                          "level_2"))

theme_set(theme_bw())
ggplot(data, aes(fluid_velocity, a1), remove=T) +
  geom_boxplot(outlier.shape=NA)+geom_jitter(position=position_jitter(width=.1, height=0),col = 'blue')+
  facet_wrap(~cut_number(a1, 4))

hist(data$max_ph, prob=TRUE, breaks=10)


# Histograms and density lines

datan <- data %>%
  select(-starts_with("a")) %>% 
  select(-which(sapply(.,is.character)))

par(mfrow=c(3, 3))
colnames <- dimnames(datan)[[2]]

for (i in 1:8) {
  boxplot(datan[,i], main=colnames[i], col="lightblue")
}

data %>%
  select(-starts_with("a")) %>%
  summary()

####FUncion imputar el valor central:


medias <- function(x,colnames){
  x %>%
    mutate(colnames = ifelse(is.na(max_ph),median(max_ph, na.rm = T),max_ph))
}


imputar_valor_central(data,"max_ph")

subset_numeric %>%
  mutate(chla=ifelse(is.na(chla),median(chla, na.rm = T),chla))

subset_chr %>% 
  mutate(season = ifelse(is.na(season),getmode(season),season))

data <- imputar_valor_central(data,{"cl"})



##### clase liliana 07/11
install.packages("feather")
library(feather)
write_feather(x = iris, path='iris.feather')
#####

summary(test)

#####Ejercicio 3

#opo4 independiente ("x")
#po4 dependiente ("y")
modelo <-lm(po4 ~ opo4, data=data)
summary(test)

imputar_valor_lm <- function(data,vary,var_independiente, modelo) { 
  if(lapply(data[,var_independiente],class)=="numeric"){
    data[is.na(data[,vary] & is.na(data[,var_independiente])==FALSE),vary]<-
      modelo$coefficients[1]+modelo$coefficients[2]*data[is.na(data[,vary]) & 
                                                           is.na(data[,var_independiente])==FALSE,var_independiente]
    
    return(data)
  }
}
  
data<-imputar_valor_lm(data,"po4","opo4",modelo)  
  
######EJERCICIO 4
library(DMwR)

test1<-imputar_por_similitud(data,10)
anyNA(test1)

  