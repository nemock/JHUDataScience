corr <- function(directory,threshold = 0) {
  
  # Write a function that takes a directory of data files and a threshold 
  # for complete cases and calculates the correlation between sulfate and 
  # nitrate for monitor locations where the number of completely observed 
  # cases (on all variables) is greater than the threshold. The function 
  # should return a vector of correlations for the monitors that meet the 
  # threshold requirement. If no monitors meet the threshold requirement, 
  # then the function should return a numeric vector of length 0.  
  
  # 'directory' is a character vector of length 1 indicating the 
  # location of the CSV files 
  
  # 'threshold' is a numeric vector of length 1 indicating the
  # number of completely observed observations (on all
  # variables) required to compute the correlation between
  # nitrate and sulfate; the default is 0
  # 
  # Return a numeric vector of correlations
  # NOTE: Do not round the result!
  
  # Grab the number of completely observed cases and put it into data frame coc
  coc <- complete(directory)

  files_index <- coc[coc["nobs"] > threshold, ]$id

  the_data <- numeric()
  
  ##set up a loop to walk all the files in files_index 
  for(i in files_index) {
    data_read <- read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"),
                                ".csv", sep = ""))
    
    temp_buff <- data_read[complete.cases(data_read), ]
    the_data = c(the_data,cor(temp_buff$sulfate, temp_buff$nitrate))
    
  }
  
  return(the_data)
  

}