#' Turn monthly series with regular NAs to quarter
#' 
#' Monthly series with NAs in non-quarter months are turned to quarterly series.
#' Series without NAs are just returned. 
#' 
#' @param series an object of class ts with monthly frequency
#' @importFrom stats start
#' 
#' @export
m_to_q <- function(series){
  stopifnot(frequency(series) == 12)
  if(!any(is.na(series))) return(series)
  period <- start(series)[2]
  q_start <- c(start(series)[1],rep(1:4,each=3)[period])
  no_nas <- na.omit(as.numeric(series))
  ts(no_nas, start = q_start, frequency = 4)
}
