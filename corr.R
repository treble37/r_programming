corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  id = 1:332
  id = formatC(id,width=3,flag="0")
  files = paste(directory,"/",id,".csv",sep="")
  x_list <- vector()  ##empty vector
  cr <- vector()
  for (file in files) {
    data = read.csv(file, header=TRUE)
    complete_data = data[complete.cases(data),]
    ##x_list = rbind(x_list,cbind(complete_data[["nitrate"]],complete_data[["sulfate"]]))
    x_list = cbind(complete_data[["nitrate"]], complete_data[["sulfate"]])
    if (nrow(x_list)>=threshold) {
      cr = rbind(cr, cor(x_list))
    }
  }
  return(cr)
}