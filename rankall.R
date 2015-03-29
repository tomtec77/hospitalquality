rankall <- function(outcome, num="best") {
    ## Read outcome data
    df <- read.csv("data/outcome-of-care-measures.csv", colClasses="character")
    
    ## Check that state and outcome are valid
    if (!(state %in% df$State)) {
        stop("invalid state")
    }
    
    possible.outcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!(outcome %in% possible.outcomes)) {
        stop("invalid outcome")
    }
    
    ## For each state, find the hospital of the given rank
    state.list <- sort(unique(df$State))
    dfresult <- data.frame(
        hospital=rep(NA,length(state.list)),
        state=state.list)
    
    source("rankhospital.R")
    
    for (ist in 1:length(state.list)) {
        dfresult$hospital[ist] <- rankhospital(state.list[ist], outcome, num)
    }
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    dfresult
}