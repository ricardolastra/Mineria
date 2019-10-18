wd<-getwd()
setwd(wd)
install.packages("RSQLite")
library(RSQLite)
library(tidyverse)

berka_db <- src_sqlite(path="berka.raw", 
                       create=FALSE)
db_list_tables(berka_db$con)

accounts_tbl <- tbl(berka_db, "accounts")
clients_tbl <- tbl(berka_db, "clients")
dispositions_tbl <- tbl(berka_db, "disps")

accounts_tbl %>% group_by(district_id) %>%
  summarise(count=n()) %>% 
  arrange(desc(count)) %>%
  collect()

dispositions_tbl %>% 
  distinct(type) %>%
  collect()
  
