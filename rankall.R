rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  all_states = levels(factor(outcome_data$State))
  
  result <- data.frame(matrix(NA,length(all_states),2 ))
  colnames(result)[1] <- "hospital"
  colnames(result)[2] <- "state"
  
  if(outcome == "heart attack"){
    col = 11
  } else if(outcome == "heart failure"){
    col = 17
  } else if (outcome == "pneumonia"){
    col = 23
  } else{ 
    stop("Invalid outcome")
  }
  
for (i in seq_along(all_states)) {
    state_order <- subset(outcome_data, outcome_data$State == all_states[[i]],select =c(2,7,col))
    state_order[,3] <- as.numeric(state_order[ ,3])
    state_order<- state_order[complete.cases(state_order),]
    state_order<- state_order[order(state_order[,3],state_order[,1]), ]
    print(state_order)
    if(num == "best"){
      num1 <- 1
    }
    else if(num == "worst"){
      num1 <- nrow(state_order)
    }
    else if(is.numeric(num)){ 
      num1 <- num
    }
    else{
      stop("invalid num")
    } 
print(num1)
    result[i,1]<- state_order[num1,]$Hospital.Name
    result[i,2]<- all_states[[i]]
    rownames(result)[i]<-all_states[[i]]
  } 

 return(result)
}