#' Fill Up a Time Series with NAs 
#' 
#' When plotting a time series you might want set the range of the plot a little wider than just the start and end date of the original series. This function add fills up the current period (typically year) with NA.
#' 
#' @param x object of class ts
#' @param add_periods integer periods to add.
#' @param fill_up_start logical should start year be filled up? Defaults to FALSE.
#' @export
#' @importFrom stats start end
fill_year_with_nas <- function(x, add_periods = 1,
                              fill_up_start = FALSE){
  UseMethod("fill_year_with_nas")
}

#' @export
fill_year_with_nas.ts <- function(x, add_periods = 1,
                              fill_up_start = FALSE){
  frq <- frequency(x)
  de <- frq-end(x)[2]
  ds <- start(x)[2]-1
  new_start <- c(start(x)[1],1)
  if(fill_up_start){
    ts(c(rep(NA,ds),x,rep(NA,de+add_periods)),
       start = new_start,
       frequency = frq
    )
  } else {
    ts(c(x,rep(NA,de+add_periods)),
       start = start(x),
       frequency = frq
    )
  }
}

#' @export
fill_year_with_nas.xts <- function(x, add_periods = 1,
                                  fill_up_start = FALSE){
  
  stop("xts support for filling up NAs not supported yet.")
  
  
}


#' @export
fill_year_with_nas.zoo <- function(x, add_periods = 1,
                                  fill_up_start = FALSE){
  
  stop("zoo support for filling up NAs not supported yet.")
}



