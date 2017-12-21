
rankall <- function(outcome , num = 'best'){
  
  df <- read.csv('outcome-of-care-measures.csv' , colClasses = 'character')
  
  req.df <- sapply(df ,function(x){ x[grepl('Not Available', x )] <- NA ; x })
  req.df <- as.data.frame(req.df)
  
  outcomes <- c('heart attack','heart failure','pneumonia')
      if (!any(outcomes == outcome))
          stop('invalide outcome')
  
    df.list <- split(req.df , req.df$State)
    
    if(num == 'worst'){
    
    rank.fun <-  function(x){
      
      if(outcome == 'heart attack'){
        x[order( -as.numeric(paste(x[,11])),x[,2] ,na.last = NA),c(2,7)][1,]
      } else if (outcome == 'heart failure'){
        x[order(-as.numeric(paste(x[,17])),x[,2],na.last = NA),c(2,7)][1,]
      }else {
        x[order(-as.numeric(paste(x[,23])),x[,2],na.last = NA),c(2,7)][1,]
      }
      
    }}
    
    else if(num == 'best'){
      
      rank.fun <-  function(x){
        
        if(outcome == 'heart attack'){
          x[order( as.numeric(paste(x[,11])),x[,2] ,na.last = NA),c(2,7)][1,]
        } else if (outcome == 'heart failure'){
          x[order(as.numeric(paste(x[,17])),x[,2],na.last = NA),c(2,7)][1,]
        }else {
          x[order(as.numeric(paste(x[,23])),x[,2],na.last = NA),c(2,7)][1,]
        }
        
      }}
      
      else {
        
        rank.fun <-  function(x){
          
          if(outcome == 'heart attack'){
            x[order( as.numeric(paste(x[,11])),x[,2] ,na.last = NA),c(2,7)][num,]
          } else if (outcome == 'heart failure'){
            x[order(as.numeric(paste(x[,17])),x[,2],na.last = NA),c(2,7)][num,]
          }else {
            x[order(as.numeric(paste(x[,23])),x[,2],na.last = NA),c(2,7)][num,]
          }
          
        }}
    
    
    
      list.2 <- lapply(df.list, rank.fun )
      
      do.call(rbind, list.2)
      
     
 }