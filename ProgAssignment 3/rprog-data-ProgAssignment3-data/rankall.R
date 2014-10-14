rankall <- function(outcome, num = "best") {

#checks valid outcome
out <-c("heart attack", "heart failure", "pneumonia")
if(!outcome %in% out) stop ("invalid outcome")
outcome

data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

if(outcome == "heart attack") {
    colname = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
}
if(outcome == "heart failure") {
    colname = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
}
if(outcome == "pneumonia") {
    colname = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
}

rank <- data.frame()
for(i in 1:50) {
    threshold <- "Not Available"
    state <- state.abb[i]
    sdata <- subset(data, State == state) 
    sdata <- subset(sdata, sdata[,colname] != threshold) 
    ordersdata <- sdata[order(as.numeric(sdata[,colname]), sdata[,2]),]
    if(num == "best") {
        num2 <- 1
    }
    if(num == "worst") {
        num2 <- as.numeric(nrow(ordersdata))
    }
    if(num > nrow(ordersdata)) {
        NA
    }
    rank <- rbind(rank, ordersdata[num2,c(2,7)])
}
colnames(rank) <- c("hospital", "state")
rank
}