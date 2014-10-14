#corr.R
corr <- function(directory, threshold = 0) 
{
    files <- complete(directory)
    y <- subset(files, select = nobs)
    #x <- nrow(files)
    for(i in 1:332) 
    {
        
        if (y[i]) > threshold) 
        {
            idz <- files[i,1]
            z <- cbind(goodfile[idz,"sulfate"], goodfile[idz, "nitrate"])
            z2 <- cor(z)
            return(z2)
        }
        else {print(0)}
    }

}