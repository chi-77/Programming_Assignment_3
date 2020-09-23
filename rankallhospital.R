rankall <- function(outcome, num = "best") {
  hospital_rank <- data.frame()
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
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  }
  
  ## Convert outcome rate to numberic
  rates[, outcome] <- as.numeric(rates[, outcome])
  
  ## Remove NA values
  rates <- rates[!is.na(rates[, outcome]), ]
  
  ## Split dataset by state name
  split_rates <- split(rates, rates$state)
  
  for(i in 1:length(split_rates)) {
    hRates <- split_rates[[i]][order(split_rates[[i]][, outcome], split_rates[[i]]$hospital),]
    
    if(num == "best") {
      rnum <- 1
    }
    else if(num == "worst") {
      rnum <- nrow(split_rates[[i]])
    }
    else {
      rnum <- num
    }
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    hNames <- hRates[rnum, 1:2]
    
    hospital_rank <- rbind(hospital_rank, hNames)
  }
  
  hospital_rank
}