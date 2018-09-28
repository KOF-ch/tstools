#' Create an Overview data.table of (last) observations
#'
#' Create a data.table that shows the i-th obsersvation of 
#' several time series. 
#'
#'@param list_of_rows list of time series names
#'@param col_labels character list of column labels
#'@param tsl list of time series object to select from
#'@param selected_period numeric date as in defining ts objects.
#'@import data.table
#'@export
#'@examples
#' tsl <- generate_random_ts(10,lengths = 20)
#' list_of_rows <- list("group 1" = c("ts1","ts2","ts3","ts4"),
#' "group 2" = c("ts5","ts6","ts7","ts10"))
#' # These are no real +,=,- values just random data.
#' create_cross_sec_overview(list_of_rows,
#'                          c("+","=","-","random"),
#'                          tsl,c(1988,12))
create_cross_sec_overview <- function(list_of_rows,
                                      col_labels,
                                      tsl,
                                      selected_period){
  all_series <- tsl[unlist(list_of_rows)]
  single_obs <- lapply(all_series, window,
                       start = selected_period,
                       end = selected_period)
  
  by_rows <- lapply(list_of_rows,function(x){
    data.table(t(unlist((single_obs[x]))))
  })
  
  out <- rbindlist(by_rows)
  # this could be done more efficient, 
  # yet performance is not a factor here and this
  # seems straight forward
  out <- cbind(names(list_of_rows),out)
  setnames(out,names(out),c("",col_labels))
  out
}












