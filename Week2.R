pollutantmean <- function(directory, pollutant, id= 1:332){
  mylist <- list.files(path = directory, pattern = ".csv")
  result <- data.frame()
  for(i in id){
    my_data <- read.csv(paste(directory,mylist[i],sep=''))
    total_data <- rbind(total_data,my_data)
  }
  
  if(pollutant =="sulfate"){
    result <- mean(na.omit(total_data$sulfate))
  }
  else if(pollutant =="nitrate"){
    result <- mean(na.omit(total_data$nitrate))
  }
  return(result)
}

complete <- function(directory, id = 1:332){
  mylist <- list.files(path = directory, pattern = ".csv")
  result <- data.frame()
  for(i in id){
    my_data <- read.csv(paste(directory,mylist[i],sep=""))
    res <- nrow(na.exclude(my_data))  #nrow count the row of the dataset
    df <- data.frame("id" = i,"nobs" = res,  stringsAsFactors=FALSE)
    result <- rbind(result,df)
  }
  return(result)
}

corr <- function(directory, threshold = 0){
  mylist <- list.files(path = directory, pattern = ".csv")
  df <- complete(directory)
  ids <- df[df["nobs"] > threshold, ]$id
  corrr <- numeric()
  for(i in ids){
    my_data <- read.csv(paste(directory,mylist[i],sep=""))
    dff <- my_data[complete.cases(my_data),]
    corrr <- c(corrr,cor(dff$sulfate,dff$nitrate))
  }
  return(corrr)
}
  #pollutantmean("D:\\Setup\\rprog_data_specdata\\specdata","nitrate",1:10)
  # pollutantmean("D:\\Setup\\rprog_data_specdata\\specdata","sulfate",1:10)
  # complete("D:\\Setup\\rprog_data_specdata\\specdata",30:25)
  # cr <- corr("D:\\Setup\\rprog_data_specdata\\specdata\\",150)
  # head(cr)
  # summary(cr)
  