.lowLevelBarPlot <- function(tsl,
                            theme = NULL){
  if(is.null(theme)) theme <- initLowLevelBarTheme()
  # split nega and pos values
  # do this for one time series
  # and build an S3 method for mts as well... 
  neg_0 <- tsl
  pos_0 <- tsl
  neg_0[tsl < 0] <- 0
  pos_0[!tsl < 0] <- 0
  
  frq <- frequency(tsl)
  ts_time <- time(tsl)
  
  # positive values
  rect(ts_time,0,
       ts_time+1/frq,
       neg_0,
       border = theme$border,
       col = theme$fill_color
  )
  
  # negative values
  rect(ts_time,pos_0,
       ts_time+1/frq,
       0,
       border = theme$border,
       col = theme$fill_color
  )
}
