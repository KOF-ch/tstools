#' Strip Leading / Trailing NAs from a Time Series Object
#' 
#' Removes NAs to begin with and starts time series index at the first non-NA value.
#' 
#' 
#' @param s an object of class ts.
#' @rdname stripNAsFromTs
#' @export 
stripLeadingNAsFromTs <- function(s){
  if(!is.na(s[1])){
    s
  } else{
    nas <- which(is.na(s))
    # when all difference are zero, just take the last 
    # NA in line, otherwise only use the first to go beyond 1
    if(all(diff(nas) == 1)){
      end <- nas[length(nas)]+1
    } else{
      end <- min(which(diff(nas) > 1))+1  
    }
    
    if(end == Inf){
      start_time <- time(s)[which(!is.na(s))[1]]
      end_time <- time(s)[length(s)]
    } else{
      start_time <- time(s)[end]
      end_time <- time(s)[length(s)]  
    }
    
    window(s,start = start_time, end = end_time)
  }
}

#' @rdname stripNAsFromTs
#' @export
stripTrailingNAsFromTs <- function(ts){
  if(is.null(dim(ts))){
    ntf <- is.na(ts)
  } else{
    ntf <- apply(ts,1,function(x) all(is.na(x)))
  }
  na_pos <- which(ntf)
  sqntl <- length(ntf)-na_pos
  if(rev(sqntl)[1] != 0){
    return(ts)
  } else {
    rmv <- na_pos[sqntl-1 <= 1]
    if(is.null(dim(ts))){
      ts(ts[-rmv],start = start(ts),frequency = frequency(ts))  
    } else {
      ts(ts[-rmv,],start = start(ts),frequency = frequency(ts))  
    }
    
  }
}  
