library(httr)
require(httpuv)
require(jsonlite)

# Data Cleaning Week 2 Quiz
# Question 1
# Register an application with the Github API here https://github.com/settings/applications. 
# Access the API to get information on your instructors repositories 
# (hint: this is the url you want "https://api.github.com/users/jtleek/repos"). 
# Use this data to find the time that the datasharing repo was created. 
# What time was it created?
# 
# This tutorial may be useful 
# (https://github.com/hadley/httr/blob/master/demo/oauth2-github.r). 
# You may also need to run the code in the base R package and not R studio.
# 
# 2012-06-20T18:39:06Z
# 
# 2013-11-07T13:25:07Z
# 
# 2013-08-28T18:18:50Z
# 
# 2012-06-21T17:28:38Z

# As per the suggestion from the class, I grabbed the sample code from
# https://github.com/hadley/httr/blob/master/demo/oauth2-github.r

# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/
oauth_endpoints("github")

# 2. To make your own application, register at at
#    https://github.com/settings/applications. Use any URL for the homepage URL
#    (http://github.com is fine) and  http://localhost:1410 as the callback url
#
#    Replace your key and secret below.
myapp <- oauth_app("github",
                   key = "a7c9f8ca013c4fa20997",
                   secret = "f2747f93b5315b90f2eeb1dd69f0d121093d3aba")

# 3. Get OAuth credentials
# Adding cache=FALSE to the following line was required to make it work
# Without it, I kept getting a 401 error
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp,cache=FALSE)

# 4. Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
stop_for_status(req)
# At this point, you walk the list and find the answer
# Just scrolling through the output is an eyesore
repo_list <- content(req)
temp <- c() 
for (i in 1:length(repo_list)) {
  repo <- repo_list[[i]]
  if (repo$name == "datasharing") {
    temp = repo
    break
  }
}
 #This extra error check shouldn't be necessary, but I needed it for my own debugging

if (length(temp) == 0) {
  cat(" 'datasharing' not found")
} else {
  cat("The repository 'datasharing' was created at", temp$created_at)
}

# The repository 'datasharing' was created at 2013-11-07T13:25:07Z

# Question 2
# The sqldf package allows for execution of SQL commands on R data frames. 
# We will use the sqldf package to practice the queries we might send with 
# the dbSendQuery command in RMySQL.
# 
# Download the American Community Survey data and load it into an R object called
# acs
#
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv
# 
# Which of the following commands will select only the data for the probability weights pwgtp1 with ages less than 50?
# 
# sqldf("select pwgtp1 from acs")
# 
# sqldf("select * from acs")
# 
# sqldf("select * from acs where AGEP < 50 and pwgtp1")
# 
# sqldf("select pwgtp1 from acs where AGEP < 50")

# To start, download the file and parse it as a CSV
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(fileURL,destfile="./acs.csv",method="curl")
dateDownloaded <- date()
acs <- read.csv("./acs.csv")

library("sqldf")

a2 <- sqldf("select pwgtp1 from acs where AGEP < 50")
head(a2)


# Question 3
# Using the same data frame you created in the previous problem, 
# what is the equivalent function to unique(acs$AGEP)
# 
# sqldf("select distinct AGEP from acs")
# 
# sqldf("select unique AGEP from acs")
# 
# sqldf("select AGEP where unique from acs")
# 
# sqldf("select distinct pwgtp1 from acs")

sqldf("select distinct AGEP from acs")


# Question 4
# How many characters are in the 10th, 20th, 30th and 100th lines of HTML from this page:
# 
# http://biostat.jhsph.edu/~jleek/contact.html
# 
# (Hint: the nchar() function in R may be helpful)
# 
# 45 31 7 25
# 
# 43 99 7 25
# 
# 45 31 2 25
# 
# 45 31 7 31
# 
# 45 92 7 2
# 
# 43 99 8 6
# 
# 45 0 2 2

con <- url("http://biostat.jhsph.edu/~jleek/contact.html")
html <- readLines(con)
close(con)


a4 <- c()
sapply(c(10, 20, 30, 100), function(line) {
  a4 <<- c(a4, nchar(html[line]))
})

#output 
# [[1]]
# [1] 45
# 
# [[2]]
# [1] 45 31
# 
# [[3]]
# [1] 45 31  7
# 
# [[4]]
# [1] 45 31  7 25


# Question 5
# Read this data set into R and report the sum of the numbers in the fourth of the nine columns.
# 
# https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for
# 
# Original source of the data: http://www.cpc.ncep.noaa.gov/data/indices/wksst8110.for
# 
# (Hint this is a fixed width file format)
# 
# 222243.1
# 
# 28893.3
# 
# 32426.7
# 
# 36.5
# 
# 35824.9
# 
# 101.83

# For file? Like, Fortran?
# This looks like a job for read.fwf
# (Also, looking at the file in a text editor with a fixed width font)
# read.fwf(file, widths, header = FALSE, sep = "\t",
#          skip = 0, row.names, col.names, n = -1,
#          buffersize = 2000, fileEncoding = "", ...)

file <- "Fwksst8110.for"
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"
download.file(fileURL,file,method="curl")
dateDownloaded <- date()
#set up a column sequence
seq <- c(-5,4,4)

data <- read.fwf(file, widths = c(-1,9,seq,seq,seq,seq),skip=4)

cat(sum(data[,4]))
# 32426.7