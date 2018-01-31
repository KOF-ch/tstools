#' @importFrom stats frequency
dateFromTs <- function(ts) {
  freq <- frequency(ts)
  start <- start(ts)
  
  quarterToMonth <- function(q) {
    switch(q, "1"=1, "2"=4, "3"=7, "4"=10)
  }
  
  if(freq == 4) {
    # TODO: There has to be a better way of converting c(year, quarter) into a Date
    seq(as.Date(sprintf("%d-%d-%d", start[1], quarterToMonth(start[2]), 1)), by = "quarter", length.out = length(ts))
  } else if(freq == 12) {
    seq(as.Date(sprintf("%d-%d-%d", start[1], start[2], 1)), by = "month", length.out = length(ts))
  }
}
