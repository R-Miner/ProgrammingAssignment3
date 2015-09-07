rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if(!state %in% unique(hospital_data[,7])) {stop("Invalid State")}
  
  if(outcome == "heart attack"){
    col = 11
  } else if(outcome == "heart failure"){
    col = 17
  } else if (outcome == "pneumonia"){
    col = 23
  } else{ 
    stop("Invalid outcome")
  }
  
 
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  req_data<- subset(outcome_data,State  == state, select =c(2,col))
  req_data[ ,2]<- as.numeric(req_data[ ,2])
  req_data<- req_data[complete.cases(req_data),]
  req_data<- req_data[order(req_data[,2], req_data$Hospital.Name), ]
  
  ## Validating and assigning the rank to which the result 
  ## should be made.

  if(num == "best"){
    num <- 1
  }
  else if(num == "worst"){
    num <- nrow(req_data)
  }
  else if(is.numeric(num)){
    if((num < 1) | (num > nrow(req_data))){
      return(NA)
    }
  }
  else{
    stop("invalid num")
  } 
  
 return(req_data[num,]$Hospital.Name)
  
  
 
  
}