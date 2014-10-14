best <- function(state, outcome) {
    st <- state.abb
    if(!state %in% st) stop ("invalid state")
    state
    
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
    sdata <- subset(data, State == state)
    orderdata <- sdata[order(sdata[,2], decreasing = FALSE),]
    orderdata2 <- as.numeric(orderdata[,colname])
    x <- min(orderdata2, na.rm = TRUE)
    y <- match(x, orderdata[,colname])
    rate <- orderdata[y,2]
    rate
}