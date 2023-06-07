#' Start a Time Series after the Last Internal NA
#'
#' Internal NAs can cause trouble for time series operations such as
#' X-13-ARIMA SEATS seasonal adjustment. Often, internal NAs only occur at
#' at the beginning of a time series. Thus an easy solution to the problem
#' is to discard the initial part of the data which contains the NA values.
#' This way only a small part of the information is lost as opposed to
#' not being able to seasonally adjust an entire series.
#'
#' @param series on object of class ts
#' @seealso \code{\link{stripLeadingNAsFromTs}}, \code{\link{stripTrailingNAsFromTs}}
#' @importFrom stats window
#' @export
#' @examples
#' ts1 <- 1:30
#' ts1[c(3, 6)] <- NA
#' ts1 <- ts(ts1, start = c(2000, 1), frequency = 4)
#' start_ts_after_internal_nas(ts1)
start_ts_after_internal_nas <- function(series) {
  # returning the series right away saves time
  # if there are no nas
  if (!any(is.na(series))) {
    return(series)
  }
  na_pos <- which(is.na(series))
  freq <- frequency(series)
  new_start_pos <- max(na_pos) + 1
  window(series,
    start = time(series)[new_start_pos],
    frequency = freq
  )
}
