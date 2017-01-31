.getValueRange <- function(li,manual_value_range = NULL){
  if(is.null(manual_value_range)){
    value_range <- range(unlist(li),na.rm=T)
    value_range <- trunc(value_range)
    value_range <- c((floor(value_range[1]/10)-1)*10,
                     (ceiling(value_range[2]/10)+1)*10)
    value_range  
  } else {
    manual_value_range
  }
  
}

.getDateRange <- function(li, manual_date_range = NULL){
  if(is.null(manual_date_range)){
    ts_time <- unique(unlist(lapply(li,time)))
    ts_time <- round(ts_time,digits = 5)
    date_range <- range(ts_time)
  } else{
    manual_date_range
  }
}
