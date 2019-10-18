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


colnames(data) <- clean_text(colnames(data))
