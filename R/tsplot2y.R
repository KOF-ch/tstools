################# Make sure to check out the plotrix package too ##########
# particularly twoord.stackplot
# see also http://moderndata.plot.ly/macroeconomic-charts-by-the-fed-using-r-and-plotly/
# https://plot.ly/r/offline/
# https://plot.ly/python/graphing-multiple-chart-types/
# http://blog.plot.ly/post/69647810381/multiple-axes-scales-with-old-faithful-data

# strategy:
# par(new = TRUE) is key!!
# x and y need to be lists
# check length of lists, split up theme info
# number of ticks is important for grids... 
# maybe add a yAsBars option

#' Create Time Series Plots With Two Ordinates (Y-axes)
#' 
#' @param x list of time series, related to the right Y-axis
#' @param y list of time series, related to the left Y-axis
#' @param plot.title character
#' @param plot.subtitle character
#' @param theme list 
#' @param ... list of additional adhoc plot options.
tsplot2y <- function(x,y,theme_2y = NULL,
                     plot.title = NULL,
                     plot.subtitle = NULL,
                     theme_out = F,
                     ...){
  
  # set margin a little different
  # to free space for another y-axis
  
  if(is.null(theme)){
    theme_2y <- initPrint2YTheme()
  }
  
  # information for both plots
  #par(mar = theme_2y$par)
  x_axis_range <- range(unlist(x))
  
  # left Y axis plot 
  # determine length to say how many colors are needed
  lx <- length(x)
  theme_left <- theme_2y
  theme_left$line_colors <- theme_left$line_colors[1:lx]
  
  tsplot(x,theme = theme_left)
  # left theme will be KOF theme... 
  # but the x-axis will be manipulated to total range of all plots
  
}




############# DEPRECATED ####################
# full_li <- c(list(x),list(y))
# ll <- length(full_li)
# x_axis_range <- range(unlist(unique(lapply(full_li,time))))
# theme$ygrid <- seq(-20,60,10)
# theme_left <- theme
# theme_left$xlim <- x_axis_range
# y_axis_range <- range(unlist(full_li))
# theme_right <- theme
# theme_right$yaxt = 'n'
# 
# 
# tsplot(x,theme = theme_left, manual_date_range = x_axis_range,
#        manual_value_range = y_axis_range)
# par(new=TRUE)
# tsplot(y,theme = theme_right,print_x_axis = F,
#        print_y_axis = F,
#        manual_date_range = x_axis_range)
# axis(4)
# 
# 
# 
# 
# 
# 
