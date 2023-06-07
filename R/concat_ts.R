#' Concatenate to Non-Overlapping Time Series
#'
#' Append one time series to another. This only works for non-overlapping time series of the same frequency.
#' For overlapping time series please see \code{\link{resolveOverlap}}.
#'
#' @param ts1 object of class ts1, typically the older of two time series.
#' @param ts2 object of class ts1, typically the younger of two time series.
#' @export
#' @importFrom stats frequency time ts
concat_ts <- function(ts1, ts2) {
  stopifnot(frequency(ts1) == frequency(ts2))
  if (any(time(ts1) %in% time(ts2))) {
    stop("time series are not allowed to overlap use resolveOverlap for overlapping series!")
  }
  s <- min(time(ts1))
  f <- frequency(ts1)
  ts(c(ts1, ts2), start = s, frequency = f)
}
