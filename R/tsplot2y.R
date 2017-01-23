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
#' @export
tsplot2y <- function(x,y,...,
                     theme = NULL,
                     plot.title = NULL,
                     plot.subtitle = NULL,
                     l_manual_y_range = NULL,
                     r_manual_y_range = NULL){
  # sanity checks
  if(!(is.ts(x) | is.list(x))){
    stop("x has to be a time series or a list of time series")
  }
  
  if(!(is.ts(y) | is.list(y))){
    stop("y has to be a time series or a list of time series")
  }
  
  # lists can be handled by the 
  # flexible tsplot.list method
  # hence we turn all single series into 
  # a list of time series with one element
  if(is.ts(x)) x <- list(x)
  if(is.ts(y)) y <- list(y)

  if(is.null(theme)){
    theme <- initDefaultLineTheme()
  }

  tsplot(x,manual_value_range = l_manual_y_range)
  par(new = T)
  r_range <- tsLinePlot(y)
  addYAxis(r_range,
           right=T,
           y_grd_steps = theme$ygrid_steps
           ,manual_value_range = r_manual_y_range)
  
  
  
}




