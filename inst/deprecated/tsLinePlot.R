#' @export
tsLinePlot <- function(tsl,
                       manual_value_range = NULL,
                       manual_date_range = NULL,
                       theme = NULL,
                       blank = F){
  # move the ts to list to the higher level later on:
  if(!is.list(tsl)) tsl <- as.list(tsl)
  
  if(is.null(theme)) theme <- initDefaultLineTheme()
  # determine xlim, ylim
  d <- list()
  ts_time <- unique(unlist(lapply(tsl,time)))
  d$ts_time <- round(ts_time,digits = 5)
  if(is.null(manual_date_range)){
    d$date_range <- .getDateRange(tsl,manual_date_range)  
  } else {
    d$date_range <- manual_date_range
  }
  if(is.null(manual_value_range)){
    d$value_range <- .getValueRange(tsl,manual_value_range)  
  } else {
    d$value_range <- manual_value_range
  }
  
  
  # empty plot with xlim and ylim
  plot(NULL,
       xlim = d$date_range,
       ylim = d$value_range,
       axes = F,
       xlab = "",
       ylab = "",
       yaxs = theme$yaxs,
       xaxs = theme$xaxs
  )
  # add lines
  for (i in 1:length(tsl)){
    lines(tsl[[i]],
          col = theme$line_colors[[i]],
          lwd = ifelse(length(theme$lwd) > 1,
                       theme$lwd[i],
                       theme$lwd),
          lty = ifelse(length(theme$lty) > 1,
                       theme$lty[i],
                       theme$lty)
    )
  }
  
  
  d
}
