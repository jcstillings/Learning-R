rm(list = ls())

rankhospital <- function(state, outcome, num = "best") {
## Read the outcome of care measures file to rank 

rank <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available",
                    stringsAsFactors=FALSE)

## Check validity of state and outcome
if(!state %in% unique(rank[,7])) {
      stop('Invalid state')
}
switch(outcome, 'heart attack' = {
      col = 11
}, 'heart failure' = {
      col = 17
}, 'pneumonia' = {
      col = 23
}, stop("Invalid outcome"))

df = rank[rank[, 7] == state, c(2,col)]
df = na.omit(df)
nhospital = nrow(df)

switch(num, best = {
      num = 1
}, worst = {
      nhospital
})

if (num > nhospital) {
      return(NA)
}

## Return hospital name in that state with the given rank for 30 day death rate

odr = order(df[, 2], df[, 1])
df[odr, ][num, 1]

}
      