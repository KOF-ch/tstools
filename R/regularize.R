#' Turn an Irregular Time Series to a Regular, Yearly ts-Based Series 
#' 
#' Adds missing values to turn an irregular time series into a regular one. This function is currently experimental and only works for irregular with gaps of one year or more. 
#' 
#' @param x an irregular time series object of class zoo or xts.
#' @param df character denoting the interval in date notation. Currently only works for "\%Y".
#' @examples 
#' ts1 <- rnorm(5)
#' dv <- c(seq(as.Date("2010-01-01"), length = 3, by="3 years"),
#' seq(as.Date("2018-01-01"), length = 2, by="2 years"))
#' library(zoo)
#' xx <- zoo(ts1,dv)
#' regularize(xx)
#' @export
regularize <- function(x, df = "%Y"){
  reg <- as.numeric(format(index(x), df))
  full_r <- seq(range(reg)[1],range(reg)[2])
  full_r[!(full_r %in% reg)]
  nas <- rep(NA,length(full_r))
  nas[(full_r %in% reg)] <- x
  tx <- ts(nas,start = range(full_r)[1],freq = 1)
  tx
}
