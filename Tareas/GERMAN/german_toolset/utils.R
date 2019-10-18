library(tidyverse)
library(grid)
library(vcd)
library(GGally)
library(Amelia)
library(knitr)
#FUNCION LOAD
#FUNCION LOAD
f <- "german.rds"
load <- function(f){
  
  if (!file.exists(f)){
    
    german_url <- paste0('https://archive.ics.uci.edu/ml',
                         '/machine-learning-databases/statlog',
                         '/german/german.data')
    
    german_data <- read_delim(german_url,
                              col_names=FALSE,
                              delim = " ")
    
    saveRDS(german_data,f)
    
  }else{
    
    print("El archivo german.rds ya existe")
    
  }
}

load(f)
#FUNCION DECODE
german_decode <- function(columna){
  
  if(exists("german_codes")){
    source("metadata.R") # german_codes = ON
  }
  
  if( class(columna) %in% c('character', 'factor') ){
    
    # df(data.column)
    column_ok <- data.frame(col.id = as.character(columna),
                            stringsAsFactors = F)
        # query
    code_cat <- left_join(
      # left table
      column_ok,
      # crea cat, right table (al vuelo)
      german_codes %>%
        bind_rows() %>%
        gather(col.id, col.desc), # col_name[2] = col.desc
      by = "col.id" )
    # tratamiento NA's
    code_cat <- mutate(code_cat,
                       col.modif = ifelse(
                         is.na(col.desc),
                         col.id,
                         col.desc))
    column_ok <- code_cat$col.modif
  }
  else {
    column_ok <- columna
  }
  return(column_ok)
  rm(column_ok,code_cat)
}
    
#FUNCION CLEAN

clean_text <- function(limpieza){
  colnames(data) <- str_to_lower(colnames(data))
  colnames(data) <- str_replace_all(colnames(data)," ","_")
  colnames(data) <- str_replace_all(colnames(data),"/","_")
  colnames(data)
}

#FUNCION QUE GENERE LOS TIPOS DE GRAFICA PARA CADA PAR DE VARIABLES

auto_plot <- function(data,f){
  
  data <- data.frame(data)
  theme_set(theme_bw())
  numericvars <- character(length = ncol(data))
  charvars <- character(length = ncol(data))
  names <- colnames(data)
  #Loop para sdividir la clase de la columna
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
  #no se me ocurrio de otra
  subset_numeric <- data %>% 
    select(one_of(numericvars))
  
  subset_chr <- data %>% 
    select(one_of(charvars))
  
  if(length(subset_numeric)!=0){
    print("Variables numericas")
    print(subset_numeric %>% ggpairs())
  }
  
  if(length(subset_chr)!=0){
    print("Variables Character")
    print(subset_chr %>% ggpairs())
  }
  
  pares <- combinations(length(colnames(data)),r=2,colnames(data))
  
  for(i in 1:(length(pares[,1]))){
    if(as.character(pares[i,1])==as.character(pares[i,2])){
      next #statement para no graficar mismas vars
    }
    else{
      #grafica nice
      print(data %>% ggpairs(c(as.character(pares[i,1]),as.character(pares[i,2]))))
    }
  }
  
}

