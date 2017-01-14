
drawBlankTsPlot <- function(l_series,
                            r_series = NULL,
                            manual_date_range = NULL,
                            manual_value_range = NULL,
                            theme = NULL){
  if(is.null(theme)) theme <- initDefaultTsTheme()
  # determine value range #########
  # for both y-axes
  # because plots from both input lists need to 
  # fit to the standard canvas
  t_date_range <- .getDateRange(l_series)
  t_value_range <- .getValueRange(l_series)
  
  if(!is.null(r_series)){
    r_value_range <- .getValueRange(r_series)
    r_date_range <- .getDateRange(r_series)
    t_series <- c(l_series,r_series)
    t_value_range <- .getValueRange(t_series)
    t_date_range <- .getDateRange(t_series)
    }
  
  if(!is.null(manual_value_range)) t_value_range <- 
    manual_value_range

  if(!is.null(manual_date_range)) t_date_range <- 
    manual_date_range
  
  # draw standard canvas #########
  plot(NULL,
       xlim = t_date_range,
       ylim = t_value_range,
       xlab="",
       ylab="",
       axes = F,
       xaxs="i",
       yaxs="i")
# draw box #########
  if(theme$box) box()
# draw axes  ####
  # Always draw left y-axis
  ygrid_lines <- .addYAxis(value_range = t_value_range,
                           theme = theme)
  # optional right y-axis
  if(!is.null(r_series)){
    .addYAxis(right = T,
              value_range = r_value_range,
              theme = theme)    
  }
  # add a time axis
  #.addXAxis()
  
  
# draw y grids ##########
if(theme$use_ygrid){
  for (hl in ygrid_lines){
    abline(h = hl,
           col = theme$ygrid_color)
  } 
}

}



.addXAxis <- function(quarter_ticks,
                      ts_time,
                      date_range,
                      at_pos = NULL,
                      theme){
  if(quarter_ticks){
    ext_qtr <- ts_time[abs(ts_time * 4 -
                             floor(ts_time * 4)) < 0.001]
    ext_label <- ifelse(ext_qtr - floor(ext_qtr) == 0.5,
                        as.character(floor(ext_qtr)), NA)
    # x-axis, for charts with a true time series axis, 
    # this should be NULL, argument is basically used for 
    # bar charts
    if(is.null(at_pos)) at_pos <- ext_qtr
    axis(1, at = at_pos, labels = ext_label,
         tcl = theme$tcl_1,
         cex.axis = theme$cex.axis_1,
         padj = theme$padj_1)
    axis(1, at = at_pos[which(!is.na(ext_label))+2],
         tcl = theme$tcl_2,
         lwd.ticks = theme$lwd_ticks_1,
         labels = FALSE) # thick tick marks
    
  } else{
    axis(1, at = min_date_value:max_date_value,
         tcl = theme$tcl_1,
         cex.axis = theme$cex.axis_1,
         padj = theme$padj_1)
  }
}




# http://stackoverflow.com/questions/19581721/changing-x-axis-position-in-r-barplot
