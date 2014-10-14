corr <- function(directory, threshold = 0)
{
fullfiles <- list.files(directory, full.names = TRUE)
cr <- c()
empty <- c()
for (i in 1:332)
{ 
    
     readfile <- read.csv(fullfiles[i])
     good <- complete.cases(readfile)
     goodfile <- readfile[good, ]
     #count how many are in goodfile
     compcases <- nrow(goodfile)
     #if count is greater than threshold do this...
     if(compcases > threshold)
     {
        sulcol <- goodfile$sulfate 
        nitcol <- goodfile$nitrate
        final_data <- cor(sulcol, y=nitcol)
        cr <- c(cr, final_data)
        #print(z)
        print(cr)
     }
     #else {print(empty)}
 }   
}