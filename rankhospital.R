rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  rates <- as.data.frame(cbind(outcomes[, 2],  # hospital 
                               outcomes[, 7],   # state
                               outcomes[, 11],  # heart attack 
                               outcomes[, 17],  # heart failure 
                               outcomes[, 23]), # pneumonia
                         stringsAsFactors = FALSE)
  
  colnames(rates) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if(!state %in% rates[,"state"]){
    stop('invalid state')
  }
  
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  }
  
  ## Return hospital name in that state with the given rank
  # Get only the hospitals in chosen state
  hRates <- rates[(rates[, "state"] == state), ]
  
  ## Convert outcome rate to numberic
  hRates[, outcome] <- as.numeric(hRates[, outcome])
  
  ## Remove NA values
  hRates <- hRates[!is.na(hRates[, outcome]), ]
  
  ## Order by outcome rate
  hRates <- hRates[order(hRates[, outcome], hRates[, "hospital"]), ]
  
  ## Get names of hosptial with the lowest rate
  # hNames <- hRates[hRates[, outcome] == min(hRates[,outcome]),1]
  if(num == "best") {
    hNames <- hRates[1, 1]
  }
  else if(num == "worst") {
    hNames <- hRates[nrow(hRates), 1]
  }
  else if(num > nrow(hRates)) {
    return(NA)
  }
  else {
    hNames <- hRates[num, 1]
  }
  sort(hNames)[1]
}
