corr <- function(directory, threshold = 0)
{
    fullfiles <- list.files(directory, full.names = TRUE)
    cr <- c()
    #print("star")
    #empty <- c()
    for (i in 1:332)
    { 
        thresh <- complete(directory, i)
        if(thresh$nobs > threshold)
        {
            readfile <- read.csv(fullfiles[thresh$id])
            good <- complete.cases(readfile)
            goodfile <- readfile[good, ]
            sulcol <- goodfile$sulfate 
            nitcol <- goodfile$nitrate
            final_data <- cor(sulcol, y=nitcol)
            cr <- c(cr, final_data)
            #print(z)
            
        }
        #else {print(empty)}
    } 
    print(cr)
}