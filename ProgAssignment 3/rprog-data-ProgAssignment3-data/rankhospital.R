rankhospital <- function(state, outcome, num) {
    threshold <- "Not Available"
    #checks valid state
    st <- state.abb
    if(!state %in% st) stop ("invalid state")
    state
    #checks valid outcome
    out <-c("heart attack", "heart failure", "pneumonia")
    if(!outcome %in% out) stop ("invalid outcome")
    outcome
    #read data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    #subset data by user entered state
    sdata <- subset(data, State == state) 
    if(outcome == "heart attack") {
        colname = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    }
    if(outcome == "heart failure") {
        colname = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    }
    if(outcome == "pneumonia") {
        colname = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    }

    #remove not available rows from column pertaining to user entered outcome
    sdata <- subset(sdata, sdata[,colname] != threshold) 
    #first order the data by the outcome input by the user, if there are ties
    #in outcome values then sort by the 2-column (Hospital.Name alphabetically)
    ordersdata <- sdata[order(as.numeric(sdata[,colname]), sdata[,2]),]
    if(num == "best") {
        num <- 1
    }
    if(num == "worst") {
        num <- as.numeric(nrow(ordersdata))
    }
    if(num > nrow(ordersdata)) {
        NA
    }
    rank <- ordersdata[num,2]
    rank    
}