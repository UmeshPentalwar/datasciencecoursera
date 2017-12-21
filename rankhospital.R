

rankhospital <- function(state, outcome ,num = 'best' ){
  
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
  
  req.df <- req.df[req.df$State == state ,]
  
  if(num == 'worst'){
  
  if(outcome == 'heart attack'){
    req.df[order( -as.numeric(paste(req.df[,11])),req.df[,2] ,na.last = NA),2][1]
  } else if (outcome == 'heart failure'){
    req.df[order(-as.numeric(paste(req.df[,17])),req.df[,2],na.last = NA),2][1]
  }else {
    req.df[order(-as.numeric(paste(req.df[,23])),req.df[,2],na.last = NA),2][1]
  }
  }
  
  else if (num == 'best'){
    
    if(outcome == 'heart attack'){
      req.df[order( as.numeric(paste(req.df[,11])),req.df[,2] ,na.last = NA),2][1]
    } else if (outcome == 'heart failure'){
      req.df[order(as.numeric(paste(req.df[,17])),req.df[,2],na.last = NA),2][1]
    }else {
      req.df[order(as.numeric(paste(req.df[,23])),req.df[,2],na.last = NA),2][1]
    }
    
  }else{
    if(outcome == 'heart attack'){
      req.df[order( as.numeric(paste(req.df[,11])),req.df[,2] ,na.last = NA),2][num]
    } else if (outcome == 'heart failure'){
      req.df[order(as.numeric(paste(req.df[,17])),req.df[,2],na.last = NA),2][num]
    }else {
      req.df[order(as.numeric(paste(req.df[,23])),req.df[,2],na.last = NA),2][num]
    }
    
  }
  
  
  
}
