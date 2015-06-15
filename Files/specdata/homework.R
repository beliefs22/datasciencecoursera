pollutantmean <- function(directory, pollutant, id = 1:322) {
  id <- as.character(id)
  IndividualMeans <- c() ## holds means for each monitory
  for(number in id){
    if(nchar(number) == 1) {
      file <- paste(directory,paste("00",number,".csv",sep = ""),sep ="/") ## add leading zeroes
      monitor <- read.csv(file) ## create data frame of specific monitor
      pollutantcolumn <- monitor[,pollutant] ## extract column for specific pollutant
      IndividualMeans[number] <- mean(pollutantcolumn[complete.cases(pollutantcolumn)]) ## add mean without NA's
    } else if(nchar(number) ==2) {
      file <- paste(directory,paste("0",number,".csv",sep = ""),sep ="/") ## add leading zeroes
      monitor <- read.csv(file) ## create data frame of specific monitor
      pollutantcolumn <- monitor[,pollutant] ## extract column for specific pollutant
      IndividualMeans[number] <- mean(pollutantcolumn[complete.cases(pollutantcolumn)]) ## add mean without NA's
    } else {
      file <- paste(directory,paste(number,".csv",sep = ""),sep ="/")
      monitor <- read.csv(file) ## create data frame of specific monitor
      pollutantcolumn <- monitor[,pollutant] ## extract column for specific pollutant
      IndividualMeans[number] <- mean(pollutantcolumn[complete.cases(pollutantcolumn)]) ## add mean without NA's
    }
  }
  mean(IndividualMeans[complete.cases(IndividualMeans)]) ## Remove NaN instances
}