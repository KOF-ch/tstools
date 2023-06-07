#' Flexible Function to Create Time Series Dummy Variables
#'
#' Generate time series with a default value that is changed within a certain subperiod.
#' The function allows for additional convenience when specifying single period dummies and dummies that go from a certain point in time to the end of the series.
#'
#' @param end_basic numeric vector of form c(yyyy,p) defining the end of the time series.
#' @param dummy_start numeric vector of form c(yyyy,p) defining the beginning of the period with different value.
#' @param dummy_end numeric vector of form c(yyyy,p) defining the end of the period with different value. Defaults to NULL, using the end_date of the series.
#' @param sp logical should NULL value for dummy_end lead to a single period dummy (TRUE) or to alternative values until the end.
#' @param start_basic numeric vector of form c(yyyy,p) defining the start of the time series. Defaults to c(1980,1)
#' @param basic_value default value of the time series, defaults to 0.
#' @param dummy_value the alternative value, defaults to 1.
#' @param frequency integer frequency of the regular time series, defaults to 4 (quarterly).
#' @author Matthias Bannert
#' @export
#' @importFrom stats ts
create_dummy_ts <- function(end_basic,
                            dummy_start,
                            dummy_end = NULL,
                            sp = T,
                            start_basic = c(1980, 1),
                            basic_value = 0,
                            dummy_value = 1,
                            frequency = 4) {
  basic <- ts(basic_value, start_basic,
    end_basic,
    frequency = frequency
  )
  if (is.null(dummy_end)) {
    if (sp) {
      dummy_end <- dummy_start
    } else {
      dummy_end <- end_basic
    }
  }
  dummy <- ts(dummy_value, dummy_start,
    dummy_end,
    frequency = frequency
  )
  resolve_ts_overlap(basic, dummy)
}
