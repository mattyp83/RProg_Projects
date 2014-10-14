#pollutantmean.R
pollutantmean <- function(directory, pollutant, id = 1:332) {      
    # assigns fullfiles with the full path of directory
    fullfiles <- list.files(directory, full.names = TRUE)
    # initialize readfile as empty data frame
    readfile <- data.frame() 
    # runs through each id file and one by one binds the rows to readfile until
    # the loop ends
    for(i in id) {                                          
        readfile <- rbind(readfile, read.csv(fullfiles[i]))
        }
    # takes the mean of pollutant column in readfile, sans NAs, rounds it to 3 decimals
    # and assigns it to meancol
    meancol <- round(mean(readfile[, pollutant], na.rm = TRUE), 3)
    #prints meancol
    print(meancol)
}

