complete <- function(directory,id = 1:332) {
  id <- as.character(id)
  observations <- c()
  for(number in id){
    if(nchar(number) == 1) {
      file <- paste(directory,paste("00",number,".csv",sep = ""),sep ="/") ## add leading zeroes
      monitor <- read.csv(file) ## create data frame of specific monitor
      complete <- monitor[complete.cases(monitor),] ## fine complete cases
      observations <- c(observations,nrow(complete)) ## add number of rows which is all complete cases to list
      
    } else if(nchar(number) ==2) {
      file <- paste(directory,paste("0",number,".csv",sep = ""),sep ="/") ## add leading zeroes
      monitor <- read.csv(file) ## create data frame of specific monitor
      complete <- monitor[complete.cases(monitor),] ## fine complete cases
      observations <- c(observations,nrow(complete)) ## add number of rows which is all complete cases to list
    } else {
      file <- paste(directory,paste(number,".csv",sep = ""),sep ="/")
      monitor <- read.csv(file) ## create data frame of specific monitor
      complete <- monitor[complete.cases(monitor),] ## fine complete cases
      observations <- c(observations,nrow(complete)) ## add number of rows which is all complete cases to list
    }
  }
  print(observations)
  print(length(observations))
  data.frame("id" = id, nobs = observations)
}