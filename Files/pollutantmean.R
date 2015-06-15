pollutantmean <- function(directory, pollutant, id = 1:332) {
  id <- as.character(id)
  IndividualCounts <- c() ## holds pollutant values for each monitor
  for(number in id){
    if(nchar(number) == 1) {
      file <- paste(directory,paste("00",number,".csv",sep = ""),sep ="/") ## add leading zeroes
      monitor <- read.csv(file) ## create data frame of specific monitor
      pollutantcolumn <- monitor[,pollutant] ## extract column for specific pollutant
      IndividualCounts <- c(IndividualCounts,pollutantcolumn) ## add valeus for monitor to counts
    } else if(nchar(number) ==2) {
      file <- paste(directory,paste("0",number,".csv",sep = ""),sep ="/") ## add leading zeroes
      monitor <- read.csv(file) ## create data frame of specific monitor
      pollutantcolumn <- monitor[,pollutant] ## extract column for specific pollutant
      IndividualCounts <- c(IndividualCounts,pollutantcolumn) ## add valeus for monitor to counts
    } else {
      file <- paste(directory,paste(number,".csv",sep = ""),sep ="/")
      monitor <- read.csv(file) ## create data frame of specific monitor
      pollutantcolumn <- monitor[,pollutant] ## extract column for specific pollutant
      IndividualCounts <- c(IndividualCounts,pollutantcolumn) ## add valeus for monitor to counts
    }
  }
  print(length(IndividualCounts))
  mean(IndividualCounts[complete.cases(IndividualCounts)]) ## Remove NAs instances
}