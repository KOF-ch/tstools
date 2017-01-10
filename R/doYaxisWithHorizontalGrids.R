.doYAxisWithHorizontalGrids <- function(theme,value_range,ygrid_factor,ygrid = T,
                                        print_y_right = F){
  stps <- abs(value_range[1] -
                value_range[2])/ygrid_factor
  ygrid_labels <- seq(from = value_range[1],
                      to = value_range[2],
                      by = trunc(stps))
  ygrid_lines <- ygrid_labels[-c(1,length(ygrid_labels))]
  axis(ifelse(print_y_right,4,2),
       at = ygrid_labels,
       tcl = theme$tcl_1,
       cex.axis = theme$cex.axis_2,
       padj = theme$padj_2,
       las = theme$axis_las_2,
       labels = theme$yaxis_labels,
       tick = theme$yaxis_tick)
  if(ygrid){
    for (hl in ygrid_lines)  abline(h = hl, col = theme$ygrid_color)
  }
}
