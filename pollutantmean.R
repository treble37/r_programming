pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  id = formatC(id,width=3,flag="0")
  files = paste(directory,"/",id,".csv",sep="")
  m <- 0
  x_list <- vector()  ##empty vector
  for (file in files) {
    data = read.csv(file, header=TRUE)
    x_list = c(x_list,data[[pollutant]])
  }
  z <- round(mean(x_list, na.rm=TRUE), digits=3)
  return(z)
}