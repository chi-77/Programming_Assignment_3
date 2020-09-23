best <- function(state, outcome) {
  ## Read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  rates <- as.data.frame(cbind(outcomes[, 2],  # hospital 
                               outcomes[, 7],   # state
                               outcomes[, 11],  # heart attack 
                               outcomes[, 17],  # heart failure 
                               outcomes[, 23]), # pneumonia
                         stringsAsFactors = FALSE)
  
  colnames(rates) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  if(!state %in% rates[,"state"]){
    stop('invalid state')
  }
  
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  
  ## Get only the hospitals in chosen state
  hRates <- rates[(rates[, "state"] == state), ]
  
  ## Convert outcome rate to numberic
  hRates[, outcome] <- as.numeric(hRates[, outcome])
  
  ## Remove NA values
  hRates <- hRates[!is.na(hRates[, outcome]), ]
  
  ## Order by outcome rate
  hRates <- hRates[order(hRates[, outcome]), ]
  
  ## Get names of hosptial with the lowest rate
  hNames <- hRates[hRates[, outcome] == min(hRates[,outcome]),1]
  
  ## Sort by hospital name if tie
  sort(hNames)[1]

}