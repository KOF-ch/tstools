#' Percentage functions
#' 
#' Computes the percentage change at each time point.
#' 
#' @param x object of class time series
#' @examples 
#' ts_1 <- ts(rnorm(12), frequency=4, start=c(2004,1))
#' pct(ts_1)
#' @author Florian Eckert and Heiner Mikosch, KOF, ETH Zurich 
#' @export
pct <- function(x){
  
  # Period over period percentage change at each time point
  # Time shift is one time point
  result <- ((x/lag(x,-1))-1)*100
  colnames(result) <- colnames(x)
  result
  
}
#' Percentage function
#'
#' Computes the annual percentage change at each time point.
#' 
#' @param x object of class time series
#' @examples
#' ts_1 <- ts(rnorm(12), frequency=4, start=c(2004,1))
#' annpct(ts_1)
#' @author Florian Eckert and Heiner Mikosch, KOF, ETH Zurich 
#' @export
annpct <- function(x){
  
  # Annual percentage change at each time point
  # Time shift is one time point
  result <- ((x/lag(x,-1))^frequency(x)-1)*100
  colnames(result) <- colnames(x)
  result
  
}
#' Percentage function
#'
#' Computes the year-over-year percentage change at each time point of the time series.
#' 
#' @param x object of class time series
#' @examples 
#' ts_1 <- ts(rnorm(12), frequency=4, start=c(2004,1))
#' yoypct(ts_1)
#' @author Florian Eckert and Heiner Mikosch, KOF, ETH Zurich 
#' @export
yoypct <- function(x){
  
  # Year-over-year percentage change at each time point
  # Time shift is the frequency of the time series
  result <- ((x/lag(x,-frequency(x)))-1)*100
  colnames(result) <- colnames(x)
  result
  
}