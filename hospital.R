best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Define a variable to store actual dataset column name for given outcome
  outcome.columnname <- NULL
  
  ## Check that state and outcome are valid
  if (!(state %in% data$State)) {
    stop("invalid state")
  }
  if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("invalid outcome")
    
  } else if (outcome == "heart attack") {
    outcome.columnname <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    
  } else if (outcome == "heart failure") {
    outcome.columnname <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    
  } else if (outcome == "pneumonia") {
    outcome.columnname <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    
  }
  
  ## Select all hospitals in given state
  state.data <- subset(data, State == state, select = c("Hospital.Name", outcome.columnname))
  
  ## Calculate best (i.e. minimal) outcome among given state hospitals
  best.outcome <- suppressWarnings(min(as.numeric(state.data[[outcome.columnname]]), na.rm=TRUE))
  
  ## Select only hospitals with best (i.e. minimal) outcome among given state hospitals
  best.hospitals <- state.data[state.data[[outcome.columnname]] == sprintf("%.1f", best.outcome),]
  
  ## Return selected hospital name(s) sorted alphabetically
  sort(best.hospitals$Hospital.Name)
}


rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Define a variable to store actual dataset column name for given outcome
  outcome.columnname <- NULL
  
  ## Check that state and outcome are valid
  if (!(state %in% data$State)) {
    stop("invalid state")
  }
  if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("invalid outcome")
    
  } else if (outcome == "heart attack") {
    outcome.columnname <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    
  } else if (outcome == "heart failure") {
    outcome.columnname <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    
  } else if (outcome == "pneumonia") {
    outcome.columnname <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    
  }
  
  ## Select all hospitals in given state
  state.data <- subset(data, State == state, select = c("Hospital.Name", outcome.columnname))
    
  ## Make outcomes numeric
  state.data[, 2] <- suppressWarnings(sapply(state.data[, 2], as.numeric))
  
  ## Remove NAs
  state.data <- na.omit(state.data)
  
  ## Make num numeric
  if (num == "best") {
    num = 1
  } else if (num == "worst") {
    num = nrow(state.data)
  } else if (num > nrow(state.data)) {
    return(NA)
  } 

  ## Order list of hospitals in given state by outcome, then by name
  state.data.ordered <- state.data[order(state.data[[outcome.columnname]], state.data$Hospital.Name),]
  
  ## Add rank column
  #state.data.ordered["Rank"] <- seq(1:nrow(state.data.ordered))
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  state.data.ordered[num, 1]
  
}


rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Define a variable to store actual dataset column name for given outcome
  outcome.columnname <- NULL
  
  # Define a variable to store result 2-column data frame
  result <- data.frame(hospital = character(), state = character())

  ## Check that state and outcome are valid
  if (!(state %in% data$State)) {
    stop("invalid state")
  }
  if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("invalid outcome")
    
  } else if (outcome == "heart attack") {
    outcome.columnname <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    
  } else if (outcome == "heart failure") {
    outcome.columnname <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    
  } else if (outcome == "pneumonia") {
    outcome.columnname <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    
  }
  
  ## Select all hospitals in given state
  states.data <- subset(data, select = c("Hospital.Name", "State", outcome.columnname))
  rm(data)
  
  ## Make outcomes numeric
  states.data[, 3] <- suppressWarnings(sapply(states.data[, 3], as.numeric))
  
  ## Remove NAs
  states.data <- na.omit(states.data)
  
  ## Split the data frame into 54 smaller ones by State
  states = split(states.data, f=states.data[, "State"])
  
  ## For each state, find the hospital of the given rank
  for(name in names(states)) {
    i <- num
    state.data <- states[[name]]
    state.data.ordered <- state.data[order(state.data[[outcome.columnname]], state.data$Hospital.Name),]
    
    ## Fix index 
    if (i == "best") {
      i <- 1
    } else if (i == "worst") {
      i <- nrow(state.data)
    } else if (i > nrow(state.data)) {
      i <- NaN
    } 
    df <- data.frame(hospital = state.data.ordered[i, 1], state = name)
    result <- rbind(result, df)
  }

  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  return(result)
}
