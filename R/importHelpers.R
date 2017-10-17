# Helper to turn api-returned json (lists) into lists of ts objects
json_to_ts <- function(json_data) {
  xt <- xts(json_data$value, order.by=as.yearmon(json_data$date))
  
  if(frequency(xt) < Inf) {
    as.ts(xt, start=start(xt), end=end(xt))
  } else {
    xt
  }
}

#' @importFrom reshape2 dcast
long_to_ts <- function(data) {
  wide_to_ts(dcast(data, date ~ series))
}

wide_to_ts <- function(data) {
  t <- as.yearmon(data$date)
  data$date <- NULL
  fata <- lapply(data, function(x) {
    xt <- xts(x, order.by=t)
    if(frequency(xt) < Inf) {
      as.ts(xt, start=start(xt), end=end(xt))
    } else {
      xt
    }
  })
}