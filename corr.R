
library(readxl)
library(dplyr)

corr <- function(directory,threshold = 0) {
  
   
  id.vector <- 1:332 
  file.names <- list.files(directory , pattern = '*.csv')
  
  setwd(directory) 
  
  df.list <- lapply(file.names,read.csv) 
  
  nobs <- sapply(df.list, function(df){nrow(df[complete.cases(df),])} )
  req.df <- data.frame(id.vector,nobs)
  v <- req.df[req.df$nobs >= threshold ,'id.vector',drop = T]
  co.rel <- sapply(v, function(ele) { cor(df.list[[ele]]$sulfate,df.list[[ele]]$nitrate,use = 'na.or.complete')} )
  
  setwd('..')  ## move back to previous working directory 
  
  return(co.rel)
  
}       