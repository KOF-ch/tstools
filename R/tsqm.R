#' Interpolate quarterly time series into monthly
#'
#' Repeat quarterly variables two times to generate a monthly variable.
#'
#' @param qts quarterly time series
#' @examples
#' tsq <- ts(1:20, start = c(1990, 1), frequency = 4)
#' tsqm(tsq)
#'
#' @importFrom stats start
#' @export
tsqm <- function(qts) {
  if (frequency(qts) == 12) {
    return(qts)
  }
  start_date <- start(qts)
  mts <- as.vector(t(qts %*% t(c(1, 1, 1))))
  mts <- ts(mts, start = quarter_to_month(start_date), frequency = 12)
  mts
}

quarter_to_month <- function(sdate) {
  y <- sdate[1]
  qms <- c(1, 4, 7, 10)
  out <- qms[sdate[2]]
  if (is.na(out)) {
    return(c(y, 1))
  }
  c(y, out)
}
