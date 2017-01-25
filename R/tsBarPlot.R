#' @export
tsBarPlot <- function(tsl,manual_date_range = NULL,
                      manual_value_range = NULL,
                      sum_as_line = TRUE,
                      theme = NULL,
                      quiet = T){
  if(is.null(theme)) theme <- initDefaultBarTheme()
  
  
  # fill up year with NAs before the series are being passed 
  # on to other functions, but check whether freq is either 
  # quarterly or monthly, this makes quarterly ticks look 
  # better... 
  all_q_or_m <- all(sapply(tsl,function(x) frequency(x) %in% c(4,12)))
  if(all(theme$fillYearWithNA & all_q_or_m)){
    tsl <- lapply(tsl,fillUpYearWithNAs)
  }
  
  date_range <- manual_date_range
  value_range <- manual_value_range
  
  # determine axis but draw an empty plot first so
  # grids can be set up before drawing on them.
  b_pos <- drawTsBars(tsl,
                      manual_value_range = value_range,
                      sum_as_line = sum_as_line,
                      no_plot = T)

  plot(NULL,
       xlim = range(b_pos$bar_pos),
       ylim = b_pos$value_range,
       axes = F,
       xlab = "",
       ylab = "",
       yaxs = theme$yaxs
  )

  
  if(theme$print_y){
    y_ticks <- addYAxis(b_pos,
                        y_grd_steps = theme$ygrid_steps,
                        manual_value_range = value_range)
    if(theme$print_y_grid) addYGrids(y_ticks,theme = theme)
  }

  par(new = T)
  b_pos <- drawTsBars(tsl, manual_value_range = value_range,
                      sum_as_line = sum_as_line)
  
  # it's massively important to invoke x-axis after bar plot
  # is really plotting, otherwise you don't get the ticks
  # in the middle of the bar!!!!
  if(theme$print_x){
    addXAxis(b_pos,isBar = T,
             theme = theme)
  }
  
  
  
  if(theme$box) box()
  if(!quiet) b_pos
}
