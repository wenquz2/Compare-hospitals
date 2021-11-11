rankall <- function(outcome, num = "best") {
    ##Read outcome data
    data <- read.csv("outcome-of-care-measures.csv")
    ## Check that the outcome is valid
    if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
        stop("invalid outcome")
    }
    ## For each state, find the hospital of the given rank
    hospital <- vector(mode = "character")
    state <- vector(mode = "character")
    states <- data[["State"]]
    result <- data.frame()
    for(s in unique(states)) {
        stateData <- subset(data, State == s)
        if (outcome == "heart attack") {
            tdata <- stateData[,c(2,11)]
        } else if (outcome == "heart failure") {
            tdata <- stateData[,c(2,17)]
        } else {
            tdata <- stateData[,c(2,23)]
        }
        c <- (tdata[,2] != "Not Available")
        cdata <- tdata[c,]
        ao <- order(as.numeric(cdata[,2]), cdata[,1])
        odata <- cdata[ao,]
        if(num == "best") {
            hospital <- append(hospital, odata[1,1])
            state <- append(state, s)
        } else if(num == "worst") {
            hospital <- append(hospital,odata[length(odata[,1]),1])
            state <- append(state, s)
        } else if(num > length(odata[,1])) {
            hospital <- append(hospital, NA)
            state <- append(state, s)
        } else {
            hospital <- append(hospital, odata[num, 1])
            state <- append(state, s)
        }
    }
    result <- data.frame(hospital, state)
    o <- order(result[,2])
    result <- result[o,]
    result
}
