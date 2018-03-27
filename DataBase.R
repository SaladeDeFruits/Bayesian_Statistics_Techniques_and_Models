library(RMySQL)
ucscDb <- dbConnect( MySQL(), 
                     user = "genomep", 
                     host = "genome-mysql.soe.ucsc.edu", 
                     password = "password",
                     central.db = "hgcentral",
                     central.host = "genome-mysql.soe.ucsc.edu",
                     central.user = "genomep",
                     central.password = "password")
result <- dbGetQuery(ucscDb, "show databases;"); dbDisconnect(ucscDb);
result

hg19 <- dbConnect(MySQL(),
                  user = "genome",
                  db = "hg19",
                  host = "genome-mysql.cse.ucsc.edu")
allTables <- dbListTables(hg19)
length(allTables)