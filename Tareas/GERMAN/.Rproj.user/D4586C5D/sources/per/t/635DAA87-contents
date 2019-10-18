#Modifica el archivo donde tengas german_colnames (puede ser utils.R o metadata.R) y sustituye (usando quizá stringr o grep) los ' ' y  '/' por '_' (ve la guía de estilo) y pasa todo a minúsculas.
colnames(data) <- clean_text(colnames(data))


#Version Tydy

data_tidy <- data %>%
  separate(personal_status_and_sex,c("sex", "personal_status"), sep=':') 

saveRDS(data_tidy,file="german-tidy.rds")