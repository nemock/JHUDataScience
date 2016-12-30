pollutantmean <- function(directory,pollutant,id = 1:332) {
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV file
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for whichwe will calculate the
  ## mean; either "sulfate" or "nitrate"
  
  ## Return the mean of the pollutant across all monitors listed
  ## in the 'id' vector (ignorning NA values)
  ## Note: Do not round the result!

  ##Initialize the_data as where the magic happens
  the_data <- numeric()
  
  ## pad each id to 3 digits and add the .csv extension
  ## put the results into file_id as a local scratch variable
  file_id <- formatC(id,width=3,format="d",flag="0")
  file_id <- paste(file_id,".csv",sep="" )
  
  ## capture the current working directory
  cur_dir <- getwd()

  ## set the new directory to 'directory', which all the magic happens
  setwd(file.path(cur_dir,directory))
  
  ##set up a loop to walk all the files in the directory
  for(i in file_id) {
    ## read each file into the vector data_read
    data_read <- read.csv(i,header=TRUE,na.strings=c("NA","NaN", " "))
    message (paste("read",i))
    
    #append the read data onto the vector the_data
    the_data <- c(the_data,data_read[[pollutant]])

  }
  ## return to the original working directory
  setwd(cur_dir)
  
  return(mean(the_data,na.rm = TRUE))
  

}