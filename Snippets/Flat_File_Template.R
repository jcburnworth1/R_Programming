###################################     BI MEMBER TO UPDATE TEMPLATE ############################
## File name
file_name <-  paste("",Sys.Date(),sep = "")

## Dropbox Folder where the file will be dumped
destination = "<some directory>"

## Query to execute

query <- ""

## Setup libraries and connection credentials to Redshift
library(redshift)
library(rdrop2)

## Load Redshift credentials
source("~/server_config.R")

## Load Dropbox token
token <- readRDS("~/drop_token.rds") 

## Setup the redshift connection
conn <- redshift.connect(paste("jdbc:postgresql://",
                               config.redshift_db$db_host,":",
                               config.redshift_db$db_port ,"/",
                               config.redshift_db$db_name, sep=""),
                               config.redshift_db$db_user,
                               config.redshift_db$db_pwd)

## Execute the query
flat_data_query <- dbGetQuery(conn, query)

## Write to directory (root in this case)
write.csv(flat_data_query,  paste("~/",file_name,".csv",sep = ""), row.names = FALSE)

## Upload the CSV file to Dropbox
## Grab the URL from the file - There should only be one match but grab the first just in case
drop_upload(paste("~/",file_name,".csv",sep = ""),  path = destination, dtoken = token)
url <- drop_share(drop_search(file_name)$path[1])[1]

## Remove the file from the directory (root in this case)
## Ensuring script cleans up after itself
file.remove(paste("~/",file_name,".csv",sep = ""))
