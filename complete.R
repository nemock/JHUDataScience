complete <- function(directory,id = 1:332) {
  
  # Write a function that reads a directory full of files 
  # and reports the number of completely observed cases in 
  # each data file. The function should return a data frame 
  # where the first column is the name of the file and the 
  # second column is the number of complete cases. 
  # A prototype of this function follows
  
  # 'directory' is a character vector of length 1 indicating the 
  # location of the CSV files 
  
  # 'id' is an integer vector indicating the monitor ID numbers to be used 
  # 
  # Return a data frame of the form 
  # id nobs
  # 1  117
  # 2  1041
  # ...
  # where 'id' is the monitor ID number and 'nobs' is the 
  # number of complete cases
  

  ##Initialize nobs as where the magic happens
  nobs <- numeric()
  
  ##set up a loop to walk all the files in the id vector
  for(i in id) {
    ## read each file into the vector data_read
    data_read <- read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"),
                                ".csv", sep = ""))
    nobs = c(nobs,sum(complete.cases(data_read)))
    
  }
  
  return(data.frame(id,nobs))
  

}