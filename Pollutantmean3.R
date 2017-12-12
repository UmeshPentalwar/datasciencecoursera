library(readxl)
library(dplyr)

pollutantmean3 <- function(directory , polutant , id = 1:332){
  
  file.names <- list.files(directory , pattern = '*.csv')
  
  setwd(directory)  
  
  df.list <- lapply(file.names,read.csv)  
  req.list <- df.list[id]
  df <- bind_rows(req.list)
  
  setwd('..')
  
  return(colMeans(df[,polutant,drop = F],na.rm = T))
  
  
} 