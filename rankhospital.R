rankhospital <- function(state, outcome, num) {
    ##Read outcome data
    data <- read.csv("outcome-of-care-measures.csv")
    ##Check that state and outcome are valid
    states <- data[["State"]]
    if (!state %in% states) {
        stop("invalid state")
    }
    if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
        stop("invalid outcome")
    }
    ##Return hospital name in that state with the given rank
    stateData <- subset(data, State == state)
    if (outcome == "heart attack") {
        tdata <- stateData[,c(2,11)]
    } else if (outcome == "heart failure") {
        tdata <- stateData[,c(2,17)]
    } else {
        tdata <- stateData[,c(2,23)]
    }
    ##Next, we remove the missing values from tdata to get cdata
    c <- (tdata[,2] != "Not Available")
    cdata <- tdata[c,]
    ##order cdata
    ao <- order(as.numeric(cdata[,2]), cdata[,1])
    odata <- cdata[ao,]
    ##now that the data table is correctly ordered
    ##time to give back the rank!
    if(num == "best") {
        return(odata[1,1])
    } else if(num == "worst") {
        return(odata[length(odata[,1]),1])
    } else if(num > length(odata[,1])) {
        return(NA)
    } else {
        return(odata[num,1])
    }
}