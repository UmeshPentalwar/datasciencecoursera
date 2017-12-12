library(readxl)
library(dplyr)

pollutantmean2 <- function(directory , polutant , id = 1:332){
  
        file.names <- vector('character', length(id))  
        m <- 1
        
        for (n in id ){
          
          
          if(n < 10)
            
            file.names[m] <- paste0('00',n,'.csv')
          
          else if (n < 100)
            
            file.names[m] <- paste0('0',n,'.csv')
          
          else 
            
          {file.names[m] <- paste0(n,'.csv') }
          
          
          m <- m+ 1 
          
        }
        setwd(directory)  
        
        df.list <- list()    
        
        L <- 1
        for ( k in file.names ){
          df.list[[L]] <- read.csv(k)
          L <- L+1 
        } 
        df <- bind_rows(df.list) 
        setwd('..')
        
        return(colMeans(df[,polutant,drop = F],na.rm = T))
        
     
        } 