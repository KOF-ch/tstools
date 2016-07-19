#' Fill Up a Time Series with NAs 
#' 
#' When plotting a time series you might want set the range of the plot a little wider than just the start and end date of the original series. This function add fills up the current period (typically year) with NA.
#' 
#' @param ts object of class ts
#' @export
fillUpYearWithNAs <- function(x){
  frq <- frequency(x)
  y <- floor(round(max(time(x)),digits=5))
  ymin <- min(time(x))
  yna <- ts(rep(NA,frq),start = ymin,end=y+1,frequency = frq)
  ts(c(x,yna), start = ymin, end = y+1, frequency = frq)
}
