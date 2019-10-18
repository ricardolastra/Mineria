#TAREA 6 FUNCION DE NAS

indices_con_NAs <- function (data, porcentaje = 0.2){
  n <- if (porcentaje < 1) {
    as.integer(porcentaje * ncol(data))
  }
  else {
    stop("Debes de introducir el porcentaje de columnas con NAs.")
  }
  indices <- which(apply(data, 1, function(x) sum(is.na(x))) >
                     n)
  if (!length(indices)) {
    warning("No hay observaciones con tantos NAs\\n            (la respuesta de la funcion es vacia),\\n            no se recomienda indexar el data.frame con esto")
  }
  indices
}

#Funcion para MODA
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#FUncion imputar el valor central:

imputar_valor_central <- function(data, colnames){
  
  data <- data.frame(data)
  numericvars <- character(length = ncol(data))
  charvars <- character(length = ncol(data))
  colname <- names(data)
  
  for(i in seq_len(ncol(data))){
    if(class(data[, i]) == "numeric") {
      numericvars[i] <- colname[i]
    }
    else {
      if(class(data[, i]) == "character") {
        charvars[i] <- colname[i]
      }
    }
  }
  
  subset_numeric <- data %>% 
    select(one_of(numericvars))
  subset_chr <- data %>% 
    select(one_of(charvars))
  
  if(length(subset_numeric)!=0){
    for(i in colnames){
      subset_numeric[is.na(subset_numeric[,i]), i] <- mean(subset_numeric[,i], na.rm = TRUE)
    }
  }
  else{
    for(i in colnames){
      subset_chr[is.na(subset_chr[,i]), i] <- getmode(subset_chr[,i], na.rm = TRUE)
    }
  }
  return(cbind(subset_chr,subset_numeric))
}

#IMPUTAR VALORES CON MODELO " lm "

imputar_valor_lm <- function(data,vary,var_independiente, modelo) { 
  if(lapply(data[,var_independiente],class)=="numeric"){
    data[is.na(data[,vary] & is.na(data[,var_independiente])==FALSE),vary]<-
      modelo$coefficients[1]+modelo$coefficients[2]*data[is.na(data[,vary]) & 
                                                           is.na(data[,var_independiente])==FALSE,var_independiente]
    
    return(data)
  }
}

#IMPUTAR VALORES CON MODELO " knn "
library(DMwR)

imputar_por_similitud <- function(data, num_vecinos) {
  data <- data.frame(data)
  numericvars <- character(length = ncol(data))
  charvars <- character(length = ncol(data))
  colname <- names(data)
  
  for(i in seq_len(ncol(data))){
    if(class(data[, i]) == "numeric") {
      numericvars[i] <- colname[i]
    }
    else {
      if(class(data[, i]) == "character") {
        charvars[i] <- colname[i]
      }
    }
  }
  
  subset_numeric <- data %>% 
    select(one_of(numericvars))
  subset_chr <- data %>% 
    select(one_of(charvars))
  
  if(length(subset_numeric)!=0){
    knnOutput <- knnImputation(subset_numeric, k = num_vecinos, scale = T, meth = "weighAvg", distData = NULL)  #knn imputacion
    
    return(cbind(subset_chr,knnOutput))
  }
}




