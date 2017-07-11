rm(list = ls())

## Read the outcome of care measures file to outcome 
outcome <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available",
                    stringsAsFactors=FALSE)

## create a new data frame with only the required columns
resizedOutcome <- data.frame(outcome$"Hospital.Name", outcome$"State", 
                             outcome$"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                             outcome$"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                             outcome$"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")

## set the names of the columns.
names(resizedOutcome) <- c("hospital", "state", "heart attack", 
                           "heart failure", "pneumonia")

## Identify best hospital in a state for a specified outcome.

best <- function(state, outcome) {
      
      # Change cases of state to all caps, outcome to all lower case to ensure 
      # conditionals work (Thanks RR)
      state <- toupper(state)
      outcome <- tolower(outcome)
      
      ## Reads the state and outcome data to determine validity of the data.
      if (state %in% resizedOutcome$state) {
            if (outcome %in% c("heart attack", "heart failure",
                               "pneumonia")) {
                  out <- which(resizedOutcome[, "state"] == state)
                  test1 <- resizedOutcome[out,]
                  test2 <- as.numeric(test1[,outcome])
                  test3 <- min(test2, na.rm = TRUE)
                  test4 <- subset(test1, test2==test3)
            } else {
                  stop('Invalid outcome entered') ## Prints error message if invalid
            }
            
      } else {
            stop('Invalid state entered') ## Prints error message if invalid
            
      }
      print(test4["hospital"]) 
      
}
