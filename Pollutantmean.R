
library(readxl)
library(dplyr)

pollutantmean <- function(directory , pollutant , id = 1:332) {
  
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
           df.list <- lapply(file.names,read.csv) 
           
           
           
           df <- bind_rows(df.list)
           setwd('..')  ## move back to previous working directory 
           
           return(colMeans(df[,pollutant,drop = F] , na.rm = T))
           
           
          }       
  