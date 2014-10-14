#complete.R
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
    return(dframe)
    #print(dframe)
}