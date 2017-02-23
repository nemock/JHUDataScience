library("RMySQL")
ucscDb <- dbConnect(MySQL(),user="genome",host="genome-mysql.cse.ucsc.edu")
result <- dbGetQuery(ucscDb,"show databases"); dbDisconnect(ucscDb);

hg19 <- dbConnect(MySQL(),user="genome", db="hg19",host="genome-mysql.cse.ucsc.edu")
allTables <- dbListTables(hg19)
length(allTables)
dbDisconnect(hg19)

source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")
