#Finding the best hospital in the state
rankhospital <- function(state, outcome, num = "best") {
  #Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",na.strings="Not Available", stringsAsFactors=FALSE)
  
  #Subset the data
  outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  if (!outcome %in% names(outcomes)) stop("invalid outcome")
  
  sd <- data[, c(2,7,outcomes[outcome])]
  
  ## Check that state and outcome are valid
  if (!state %in% sd$State) stop("invalid state")
  
  
  #Remove NA values
  cd <- na.omit(sd)
  
  #Sort all the data by state -> by outcome -> hospital
  st <- cd[order(cd$"State", cd[,3], cd$"Hospital.Name"), ]
  
  #Split the data by state to get a list of data frames ordered by state
  sp <- split(st, st$State)
  
  
  #Execute lapply function
  function_for_lapply(sp) {
    
    if(num = "best") lapply(sp, data$Hospital.Name[1])

  }
  
  
  results_of_lapply <- lapply(sp, function(data) data$Hospital.Name)
  
  len <- length(results_of_lapply[[state]])
  
  unlisted_values <- unlist(results_of_lapply)
                                             
  list_names <- names(results_of_lapply)
  
  if(num == "best") {
    return(unlisted_values[[paste0(state,1)]])
  } else if(num == "worst") {
    return(unlisted_values[[paste0(state,len)]])
  } else if(num > len) {
    return(NA)
  } else
    return(unlisted_values[[paste0(state,num)]])
    
  data.frame(hospital=unlisted_values, state=list_names, row.names=list_names)
  
}