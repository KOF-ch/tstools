# seperate helper function to add X-axis because we can use this
# for different plots
# note that barplots don't have a time x-axis, they just 
# get labels based on sequence... 
# this is a potential problem with irregular time series
# however, R's ts does not support irregular series anyway...
.addTsAxis <- function(quarter_ticks,
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
