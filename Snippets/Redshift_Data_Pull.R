## Load required libraries
library(RPostgreSQL)

## Connect to Redshift
source("Config_Redshift.R")
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv,
                 host=config.redshift_db$db_host,
                 port=config.redshift_db$db_port,
                 dbname=config.redshift_db$db_name,
                 user=config.redshift_db$db_user,
                 password=config.redshift_db$db_pwd)

## Query 1
Query_1_Data <- dbGetQuery(con,"")

write.csv(Query_1_Data,"<some file path/file.csv>",na="NULL", row.names = FALSE)

dbDisconnect(con)
