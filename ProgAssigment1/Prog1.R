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

complete <- function(directory, id = 1:332) {      
    # assigns fullfiles with the full path of directory
    fullfiles <- list.files(directory, full.names = TRUE)
    # initialize readfile as empty data frame
    dframe <- data.frame()
    # this loop reads each monitor id file, finds the complete cases, assigns the complete cases 
    #to a new matrix "goodfile" and then counts the number of rows of good file. it also
    # creates a new matrix dframe2 that identifies each id and the amount of complete cases in that id
    # as id increments, the complete cases is added to dframe2 as a new row
    for(i in id) {
        readfile <- read.csv(fullfiles[i])
        good <- complete.cases(readfile)
        goodfile <- readfile[good, ]
        comcases <- nrow(goodfile)
        dframe2 <- c(i, comcases)
        dframe <- rbind(dframe, dframe2)
        colnames(dframe) <- c("id", "nobs")
        }
    print(dframe)
}


#corr.R
fullfiles <- list.files(directory, full.names = TRUE)
count <- length(fullfiles)
dframe <- data.frame()
for (i in count) {
    readfile <- read.csv(fullfiles[i])
    good <- complete.cases(readfile)
    goodfile <- readfile[good, ]
    dframe <- rbind(dframe, goodfile)
}
subset(dframe, select = c(sulfate, nitrate))