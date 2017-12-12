library(readxl)
library(dplyr)

complete <- function(directory,id = 1:332) {
  
  file.names <- vector('character', length(id))  
  id.vector <- vector('integer',length(id))
  
  m <- 1
  
  for (n in id ){
    
    
    if(n < 10)
      
      file.names[m] <- paste0('00',n,'.csv')
    
    else if (n < 100)
      
      file.names[m] <- paste0('0',n,'.csv')
    
    else 
      
    {file.names[m] <- paste0(n,'.csv') }
    
    id.vector[m] <- n 
    
    m <- m+ 1 
    
  }
  
  setwd(directory)    
  df.list <- lapply(file.names,read.csv) 
  nobs <- sapply(df.list, function(df){nrow(df[complete.cases(df),])} )
  setwd('..')  ## move back to previous working directory 
  
  return(data.frame(id = id.vector,nobs))
  
  
}       