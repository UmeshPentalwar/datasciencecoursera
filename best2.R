

best <- function(state, outcome){
  
  library(dplyr)
  
  df <- read.csv('outcome-of-care-measures.csv' , colClasses = 'character')
  
  req.df <- sapply(df ,function(x){ x[grepl('Not Available', x )] <- NA ; x })
  req.df <- as.data.frame(req.df)
  
  dist.state <- unique(req.df$State)
  outcomes <- c('heart attack','heart failure','pneumonia')
  
  if ( !any(dist.state == state) & !any(outcomes == outcome) )
    stop ('invalide state and outcome')
  else if (!any(dist.state == state))
    stop('invalide state')
  else if (!any(outcomes == outcome))
    stop('invalide outcome')
  
  

  if (outcome == 'heart attack')
  slice(select(arrange(filter(req.df, State == state),Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.Name),Hospital.Name ) , 1)
  else if (outcome == 'heart failure')
    slice(select(arrange(filter(req.df, State == state),Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure ,Hospital.Name),Hospital.Name ) , 1) 
  else 
  slice(select(arrange(filter(req.df, State == state),Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia ,Hospital.Name),Hospital.Name ) , 1)
  
}