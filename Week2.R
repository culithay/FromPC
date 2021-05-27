pollutantmean <- function(directory, pollutant, id= 1:332){
  
  require(stringr)
  setwd(directory)
  
  total_data <- NA
  cname <- c("Date","sulfate","nitrate","ID")
  for(i in id){
    csvName <- paste(str_pad(i,3,pad='0'),".csv",sep='')
    my_data <- read.csv(csvName)
    total_data <- rbind(total_data,my_data)
  }
  
  colnames(total_data) <- cname
  if(pollutant =="sulfate"){
    new_data <- na.omit(total_data$sulfate)
    result <- mean(new_data)
    print(result)
  }
  else if(pollutant =="nitrate"){
    new_data <- na.omit(total_data$nitrate)
    result <- mean(new_data)
    print(result)
  }
}

complete <- function(directory, id = 1:332)
  #pollutantmean("D:\\Setup\\rprog_data_specdata\\specdata","nitrate",1:10)
  pollutantmean("D:\\Setup\\rprog_data_specdata\\specdata","sulfate",1:10)
