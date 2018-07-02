#' Turn an Irregular Time Series to a Regular, ts-Based Series 
#' 
#' Adds missing values to turn an irregular time series into a regular one. This function is currently experimental. Only works or target frequencies 1,2,4,12.
#' 
#' @param x an irregular time series object of class zoo or xts.
#' @examples 
#' ts1 <- rnorm(5)
#' dv <- c(seq(as.Date("2010-01-01"), length = 3, by="3 years"),
#' seq(as.Date("2018-01-01"), length = 2, by="2 years"))
#' library(zoo)
#' xx <- zoo(ts1,dv)
#' regularize(xx)
#' 
#' dv2 <- c(seq(as.Date("2010-01-01"), length = 20, by = "1 months"))
#' dv2 <- dv2[c(1:10, 14:20)]
#' xx2 <- zoo(rnorm(length(dv2)), dv2)
#' regularize(xx2)
#' 
#' @importFrom zoo as.yearmon
#' @export
regularize <- function(x){
  idx <- index(x)
  # difference in days in order to guess frequency
  dt <- as.numeric(diff(idx))/365
  freqs <- c(12, 4, 2, 1)
  frq <- freqs[which.min(abs(min(dt) - 1/freqs))]
  full_r <- seq(min(idx), max(idx), by = sprintf("%d months", 12/frq))
  val <- rep(NA, length(full_r))
  val[(full_r %in% idx)] <- x
  tx <- ts(val, start = as.yearmon(min(idx)), frequency = frq)
  tx
}
