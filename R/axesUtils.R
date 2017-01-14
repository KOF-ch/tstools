# http://stackoverflow.com/questions/29182758/how-to-get-the-x-coordinate-of-bars-in-barplot-in-r
# seperate helper function to add X-axis because we can use this
# for different plots
# note that barplots don't have a time x-axis, they just 
# get labels based on sequence... 
# this is a potential problem with irregular time series
# however, R's ts does not support irregular series anyway...



.addYAxis <- function(right = F,
                      value_range,
                      theme){
  stps <- abs(value_range[1] -
                value_range[2])/theme$ygrid_factor
  ygrid_labels <- seq(from = value_range[1],
                      to = value_range[2],
                      by = trunc(stps))
  ygrid_lines <- ygrid_labels[-c(1,length(ygrid_labels))]
  axis(ifelse(right,4,2),
       at = ygrid_labels,
       tcl = theme$tcl_1,
       cex.axis = theme$cex.axis_2,
       padj = theme$padj_2,
       las = theme$axis_las_2,
       labels = theme$yaxis_labels,
       tick = theme$yaxis_tick)
  ygrid_lines
}

