complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  

  nobs<-vector()
  
  for (i in id){
    
    padded_id<- str_pad(i,3,side="left", pad="0")
    f <-  paste(directory,"/",padded_id,'.csv' , sep = '')
    f
    data <- read.csv(f)
   
    nobs<-c(nobs,nrow(data[!is.na(data$sulfate) & !is.na(data$nitrate),]))
  }
  complete<-data.frame(id,nobs)
  complete
}