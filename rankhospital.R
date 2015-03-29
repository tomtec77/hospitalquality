rankhospital <- function(state, outcome, num="best") {
    ## Read outcome data
    df <- read.csv("data/outcome-of-care-measures.csv", colClasses="character")
    
    ## Check that state and outcome are valid
    if (!(state %in% df$State)) {
        stop("invalid state")
    }
    
    possible.outcomes <- c("heart attack", "heart failure", "pneumonia")
    column.index <- c(11, 17, 23)
    if (!(outcome %in% possible.outcomes)) {
        stop("invalid outcome")
    }
    column.select <- column.index[outcome==possible.outcomes]
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    dfstate <- df[df$State==state & df[,column.select] != "Not Available",
                  c(2,column.select)]
    names(dfstate) <- c("Hospital.Name","Mortality.Rate")
    dfstate$Mortality.Rate <- as.numeric(dfstate$Mortality.Rate)
    ordered.results.list <- dfstate$Hospital.Name[order(dfstate$Mortality.Rate,
                                                        dfstate$Hospital.Name)]
    
    if (num=="best") {
        return(ordered.results.list[1])
    }
    else if (num=="worst") {
        return(ordered.results.list[length(ordered.results.list)])
    }
    else {
        return(ordered.results.list[num])
    }
}