# Helper to turn api-returned json (lists) into lists of ts objects

#' @importFrom stats as.ts start end
json_to_ts <- function(json_data) {
  xt <- xts(json_data$value, order.by=as.yearmon(json_data$date))
  
  if(frequency(xt) < Inf) {
    as.ts(xt, start=start(xt), end=end(xt))
  } else {
    xt
  }
}

#' Transform a long format data.frame of time series to a tslist
#' 
#' The data.frame must have three columns "date", "value" and "series" (identifying the time series)
#' @param data data.frame The data.frame to be transformed 
#' @importFrom data.table dcast
#' @importFrom zoo na.trim
#' @export
long_to_ts <- function(data) {
  data_dt <- as.data.table(data)
  
  # Strip series consisting only of NAs
  empty_series <- data_dt[, list(is_empty = all(is.na(value))), by = series]
  
  if(empty_series[, any(is_empty)]) {
    warning(sprintf("Some series contained only NAs and were stripped:\n%s",
                    paste(empty_series[is_empty == TRUE, series], collapse = "\n")))
  }
  
  data_dt <- data_dt[!(series %in% empty_series[is_empty == TRUE, series])]
  
  data_dt[, `:=`(date_zoo = as.numeric(as.yearmon(date)), frq = 12), by = series]
  
  data_dt[is.na(date_zoo), `:=`(date_zoo = as.numeric(as.yearqtr(date)), frq = 4)]
  
  dt_of_lists <- data_dt[, {
    dT <- diff(date_zoo)
    if(any(diff(dT) > 1e-6)) {
      if(frq[1] == 4) {
        list(ts_object = list(xts(value, order.by = as.yearqtr(date_zoo))))
      } else {
        list(ts_object = list(xts(value, order.by = as.yearmon(date_zoo))))
      }
    } else {
      list(ts_object = list(ts(value, start = .SD[1, date_zoo], end = .SD[.N, date_zoo], deltat = dT[1])))
    }
  }, by = series]
  
  tslist <- dt_of_lists[, ts_object]
  tslist <- lapply(tslist, na.trim)
  names(tslist) <- dt_of_lists[, series]
  
  tslist
}

utils::globalVariables(c("date_zoo", "series", "ts_object", "value", "frq", "is_empty"))

#' Transform a wide format data.frame into a tslist
#' 
#' The time series in the data.frame may be stored either rowwise or columnswise.
#' The identifying column must be called date (for columnwise) or series (for rowwise)
#' @param data data.frame The data.frame to be transformed
#' @importFrom xts xts
#' @importFrom zoo as.yearqtr as.yearmon
#' @export
wide_to_ts <- function(data) {
  if(!("date" %in% names(data))) {
    # Data was written in transposed format
    long_to_ts(melt(data, id.vars = "series", variable.name = "date")) 
  } else {
    long_to_ts(melt(data, id.vars = "date", variable.name = "series"))
  }
}
