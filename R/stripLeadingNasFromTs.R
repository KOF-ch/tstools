#' Strip Leading NAs from a Time Series Object
#' 
#' Removes NAs to begin with and starts time series index at the first non-NA value.
#' 
#' @param s an object of class ts.
#' @export 
stripLeadingNAsFromTs <- function(s){
  if(!is.na(s[1])){
    s
  } else{
    nas <- which(is.na(s))
    end <- min(which(diff(nas) > 1))+1
    start_time <- time(s)[end]
    end_time <- time(s)[length(s)]
    window(s,start = start_time, end = end_time)
  }
}
