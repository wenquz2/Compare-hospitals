best <- function(state, outcome) {
    ##Read in the data
    data <- read.csv("outcome-of-care-measures.csv")
    ##Check that state and outcome are valid
    states <- data[["State"]]
    if (!state %in% states) {
        stop("invalid state")
    }
    if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
        stop("invalid outcome")
    }
    ## Return hospital name in that state with lowest 30-day death
    hospitalData <- subset(data, State == state)
    hosList <- vector(mode = "character")
    if (outcome == "heart attack") {
        tdata <- hospitalData[,c(2,11)]
    } else if (outcome == "heart failure") {
        tdata <- hospitalData[,c(2,17)]
    } else {
        tdata <- hospitalData[,c(2,23)]
    }
    ##remove the NA from the data
    c <- (tdata[,2] != "Not Available")
    cdata <- tdata[c,]
    min <- min(as.numeric(cdata[,2]))
    for (m in 1:length(cdata[,1])) {
        if(min == as.numeric(cdata[m,2])) {
            hosList <- append(hosList, cdata[m,1])
        }
    }
    ##Find out what is the best Hospital using the tie breaker
    best <- min(hosList)
    best
}