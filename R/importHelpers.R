# Helper to turn api-returned json (lists) into lists of ts objects
json_to_ts <- function(json_data) {
  xt <- xts(json_data$value, order.by=as.yearmon(json_data$date))
  
  if(frequency(xt) < Inf) {
    as.ts(xt, start=start(xt), end=end(xt))
  } else {
    xt
  }
}

#' @importFrom data.table dcast
long_to_ts <- function(data) {
  wide_to_ts(dcast(data, date ~ series))
}

#' @importFrom xts xts
#' @importFrom zoo as.yearqtr as.yearmon
wide_to_ts <- function(data) {
  t <- as.yearmon(data$date)
  if(any(is.na(t))) {
    t <- as.yearqtr(data$date)
  }
  data$date <- NULL
  lapply(data, function(x) {
    nas <- is.na(x)
    xt <- xts(x[!nas], order.by=t[!nas])
    if(frequency(xt) < Inf) {
      as.ts(xt, start=start(xt), end=end(xt))
    } else {
      xt
    }
  })
}