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
  ## NOTE: Do not round the result!
  
 padded_ids<- str_pad(id,3,side="left", pad="0")
 files <-  paste(directory,"/",padded_ids,'.csv' , sep = '')
 
 p<-vector()
 
 for (f in files){
  
   #print(f)
   
   data <- read.csv(f)
   p<-c(p,data[,pollutant])
 }
 
 pollutantmean <- mean(p,na.rm = TRUE)
 pollutantmean
}