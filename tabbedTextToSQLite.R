#because the census data was in access I saved the tables as a folder of tabbed text with " as a charcter marker and a heading row.
#This script takes a folder of such files and converts them to a single sqlite database
library(sqldf)
setwd("~/Documents/census2006")
db <- dbConnect(SQLite(), dbname="nzcensus.sqlite")
inputfiles  <- list.files("rawdata")
for (eachfile in inputfiles){
  tablename <- gsub("\\.txt","",eachfile)
  filepath = paste("rawdata", eachfile, sep = "/")
  myquery <- paste("CREATE TABLE",tablename, "AS SELECT * FROM file", sep = " ")
  read.csv.sql(filepath, sql = myquery, dbname = "nzcensus.sqlite",sep = "\t")
}

## This is pretty easy, but there are a few consequences that for now I can live with
## 1) because the access to tab delimited text was created on windows and shifted to a Mac, the unicode characters seem to have been scrambled
## 2) there are quotemarks around text so I will need to trim.
