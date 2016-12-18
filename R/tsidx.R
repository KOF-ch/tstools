#' Get an index
#' 
#' Compare time series to extracted value at given time point (year and quarter e.g.).
#' 
#' @param x object of class time series
#' @param byear year of observation
#' @param bperiod month or quarter of observation, NULL by default (if yearly data). E.g. 2nd quarter.
#' @examples 
#' ts_1 <- ts(rnorm(20), start=c(2004,1), frequency=4)
#' tsidx(ts_1,2005,4)
#' @author Florian Eckert and Heiner Mikosch, KOF, ETH Zurich 
#' @export
tsidx <- function(x, byear, bperiod = NULL){
  # Get Index
  
  if(is.null(bperiod)){
    
    # Temporal aggregation of time series - annual average
    var_1 <- ta(x, conversion = "average", to = "annual")
    # Extract the annual average at time of observation
    base_value <- window(var_1, 
                         start = byear, 
                         end = byear)
    
  } else {
    
    # Extract value of time series at time of observation
    base_value <- window(x, 
                         start = byear + (bperiod-1)/frequency(x), 
                         end = byear + (bperiod-1)/frequency(x))
  }
  
  # Multiply extracted value in time series object
  base_value_ext <- ts(data = base_value,
                       start = time(x)[1],
                       end = time(x)[length(time(x))],
                       frequency = frequency(x))
  
  # Compare time series to extracted value at given time of observation
  res <- x/base_value_ext*100
  
  colnames(res) <- colnames(x)
  
  res
  
}
