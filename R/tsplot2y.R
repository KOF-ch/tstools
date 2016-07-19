################# Make sure to check out the plotrix package too ##########
# particularly twoord.stackplot
# see also http://moderndata.plot.ly/macroeconomic-charts-by-the-fed-using-r-and-plotly/
# https://plot.ly/r/offline/
# https://plot.ly/python/graphing-multiple-chart-types/
# http://blog.plot.ly/post/69647810381/multiple-axes-scales-with-old-faithful-data

tsplot2y <- function(x,y,theme = NULL,plot.title = NULL,plot.subtitle = NULL,
                     theme_out = F,pmar = c(5,4,4,5)+.1,...){
  # set margin a little different
  # to free space for another y-axis
  par(mar = pmar)
  
  if(is.null(theme)){
    theme <- initDefaultTheme()
  }
  
  # left theme will be KOF theme... 
  # but the x-axis will be manipulated to total range of all plots
  full_li <- c(list(x),list(y))
  ll <- length(full_li)
  x_axis_range <- range(unlist(unique(lapply(full_li,time))))
  theme$ygrid <- seq(-20,60,10)
  theme_left <- theme
  theme_left$xlim <- x_axis_range
  y_axis_range <- range(unlist(full_li))
  theme_right <- theme
  theme_right$yaxt = 'n'
  
  
  tsplot(x,theme = theme_left, manual_date_range = x_axis_range,
         manual_value_range = y_axis_range)
  par(new=TRUE)
  tsplot(y,theme = theme_right,print_x_axis = F,
         print_y_axis = F,
         manual_date_range = x_axis_range)
  axis(4)
}

