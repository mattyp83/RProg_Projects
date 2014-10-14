getwd()
files <- list.files()
files[1:9]
pollutantmean <- function(directory, pollutant,id) {
        #if(length(directory) = 1 && length(pollutant) = 1 {
            #setwd(directory)
            #getpollutant <- c(pollutant)        
        for(i in id){
            files <- list.files()
            readfile <- read.csv(files[i])
            meancol <- mean(readfile[, pollutant], na.rm = TRUE)
            print(meancol)
        }
}

pollutantmean <- function(directory, pollutant, id) {      
        fullfiles <- list.files(directory, full.names = TRUE)
        for(i in id) {
               readfile <- read.csv(fullfiles[i])
               meancol <- round(mean(readfile[, pollutant], na.rm = TRUE), 3)
               print(meancol)
            }
 }

