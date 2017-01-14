.getValueRange <- function(li){
  value_range <- range(unlist(li),na.rm=T)
  value_range <- trunc(value_range)
  value_range <- c((floor(value_range[1]/10)-1)*10,
                   (ceiling(value_range[2]/10)+1)*10)
  value_range
}

.getDateRange <- function(li){
  value_range <- range(unlist(li),na.rm=T)
  value_range <- trunc(value_range)
  value_range <- c((floor(value_range[1]/10)-1)*10,
                   (ceiling(value_range[2]/10)+1)*10)
  value_range
}
