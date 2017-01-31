.plotPosNeg <- function(p0, n0,
                        print_y_axis,
                        print_x_axis,
                        value_range,
                        ts_time,
                        date_range){
  pos_part <- barplot(t(n0),
                      ylim = value_range,
                      axes = F,
                      col = theme$line_colors)
  
  
  neg_part <- barplot(t(p0),
                      ylim = value_range,
                      axes = F,
                      col = theme$line_colors,
                      add = T)
  
  if(print_y_axis){
    .doYAxisWithHorizontalGrids(theme,
                                value_range,
                                ygrid_factor = ygrid_factor,
                                print_y_right = print_y_right)
  }
  
  pos_part <- barplot(t(n0),
                      ylim = value_range,
                      axes = F,
                      col = theme$line_colors,
                      add = T)
  
  neg_part <- barplot(t(p0),
                      ylim = value_range,
                      axes = F,
                      col = theme$line_colors,
                      add = T)
  
  if(print_x_axis){
    .addTsAxis(quarter_ticks = quarter_ticks,
               ts_time = ts_time,
               date_range = date_range,
               at = pos_part,
               theme = theme)
  } 
  
}
