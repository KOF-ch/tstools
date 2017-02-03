#' Fill Up a Time Series with NAs 
#' 
#' When plotting a time series you might want set the range of the plot a little wider than just the start and end date of the original series. This function add fills up the current period (typically year) with NA.
#' 
#' @param ts object of class ts
#' @export
fillUpYearWithNAs <- function(x,add_periods = 1,
                              fill_up_start = FALSE){
  frq <- frequency(x)
  de <- frq-end(x)[2]
  ds <- start(x)[2]-1
  new_start <- c(start(x)[1],1)
  if(fill_up_start){
    ts(c(rep(NA,ds),x,rep(NA,de+add_periods)),
       start = new_start,
       frequency = frq
    )
  } else {
    ts(c(x,rep(NA,de+add_periods)),
       start = start(x),
       frequency = frq
    )
  }
  
  
}






