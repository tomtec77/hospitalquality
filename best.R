best <- function(state, outcome) {
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
    
    ## Return hospital name in that state with lowest 30.day death
    ## rate
    dfstate <- df[df$State==state & df[,column.select] != "Not Available",
                  c(2,column.select)]
    names(dfstate) <- c("Hospital.Name","Mortality.Rate")
    dfstate$Mortality.Rate <- as.numeric(dfstate$Mortality.Rate)
    
    best <- min(dfstate$Mortality.Rate)
    sort(dfstate$Hospital.Name[dfstate$Mortality.Rate==best])[1]
}