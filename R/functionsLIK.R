
constructLIK <- function(pos,neg = NULL){
  
  # Get a list of the relevant keys
  keys <- c(pos,neg)
  
  dat_all <- na.omit(do.call(cbind, data[keys]))
  
  # Compute growth rates with the last base year as base
  series <- lapply(keys, function(x){
    
    dat <- dat_all[,x]
    
    # Initialize values
    base_value <- dat[1]
    result <- 1
    
    for(i in 1:length(dat)){
      
      # Each time after the weights change, a new base value is set
      if((isTRUE(all.equal(round(time(dat)[i],3) %% 1, 0)) & round(time(dat)[i]) > 2001) |
         isTRUE(all.equal(time(dat)[i], 2000 + 5/12))){
        
        base_value <- dat[i-1]
        
      }
      
      result[i] <- dat[i]/base_value
      
    }
    
    result <- ts(data = result,
                 start = time(dat)[1], 
                 frequency = 12)
    
    if(x %in% neg){
      
      result <- result * weights_ts[[x]] * -1
      
    } else {
      
      result <- result * weights_ts[[x]]
      
    }
    
  })
  
  names(series) <- keys
  
  # Sum the weighted indices and divided by the sum of their weights
  if(is.null(neg)){
    
    series_reduced <- Reduce("+", series) / Reduce("+", weights_ts[pos])
    
  } else {
    
    series_reduced <- Reduce("+", series) / (Reduce("+", weights_ts[pos]) - Reduce("+", weights_ts[neg]))
    
  }
  
  
  # Reconstruct the index from the growth rates
  series_reduced <- na.omit(series_reduced)
  result <- 100
  base_value <- 100
  
  
  for(i in 1:length(series_reduced)){
    
    # Each time that the weights change, a new base value is set
    if((isTRUE(all.equal(round(time(series_reduced)[i],3) %% 1, 0)) & round(time(series_reduced)[i]) > 2001) |
       isTRUE(all.equal(time(series_reduced)[i], 2000 + 5/12))){
      
      base_value <- result[i-1]
      
    }
    
    result[i] <- series_reduced[i] * base_value
    
  }
  
  result <- ts(data = result,
               start = time(series_reduced)[1], 
               frequency = 12)
  
  # Rebase series
  result <- result/coredata(window(result,base_year,base_year))*100
  
  return(result)
  
}

