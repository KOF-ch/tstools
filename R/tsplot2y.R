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
tsplot2y <- function(x,y,...,
                     theme_2y = NULL,
                     plot.title = NULL,
                     plot.subtitle = NULL,
                     lgnd = NULL,
                     highlight_window = NULL,
                     theme_out = F,
                     ygrid_factor = 4,
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

  if(is.null(theme_2y)){
    theme_2y <- initPrint2YTheme()
  }
  
  
  
  # information for both plots
  #par(mar = theme_2y$par)
  x_axis_range <- range(unlist(x))
  
  # left Y axis plot 
  # determine length to say how many colors are needed
  lx <- length(x)
  theme_left <- theme_2y
  theme_left$line_colors <- 
    theme_left$line_colors[1:lx]
  theme_left$lty <- theme_left$lty[1:lx]
  theme_left$lwd <- theme_left$lwd[1:lx]
  
  tsplot(x,theme = theme_left,
         ygrid_factor = ygrid_factor,
         highlight_window = highlight_window,
         manual_value_range = l_manual_y_range)
  
  
  
  
  
  # right Y axis plot
  ly <- length(y)
  theme_right <- theme_2y
  # get what's left, make sure to not use 
  # a color, lty or lwd twice unless there's 
  # only a single value
  theme_right$line_colors <- 
    theme_2y$line_colors[-c(1:lx)]
  
  if(length(theme_2y$lty) == 1) 
    theme_right$lty <- theme_2y$lty
  else
    theme_right$lty <- theme_2y$lty[-c(1:lx)]
  
  if(length(theme_2y$lwd) == 1) 
    theme_right$lwd <- theme_2y$lwd
  else
    theme_right$lwd <- theme_2y$lwd[-c(1:lx)]

  
  par(new=T)
  
  tsplot(y,
         plot.title = plot.title,
         plot.subtitle = plot.subtitle,
         theme = theme_right,
         ygrid_factor = ygrid_factor,
         print_y_axis = T,
         print_y_right = T,
         manual_value_range = r_manual_y_range)
  
  # legend position
  ts_time <- unique(unlist(lapply(x,time)))
  # floating problems when comparing stuff, set it to 
  # 5 digits ... 
  ts_time <- round(ts_time,digits = 5)
  date_range <- range(ts_time)
  value_range <- trunc(range(unlist(y),na.rm=T))
  
  if(!is.null(lgnd)){
    # http://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics
    legend("bottomleft",
           inset=c(0,-0.35),
           legend = lgnd,
           y.intersp = 1.6,
           xpd = theme_2y$lgnd_offset,
           box.col = NA, 
           lty = theme_2y$lty,
           lwd = theme_2y$lwd,
           cex = theme_2y$lgnd_cex_label,
           col = theme_2y$line_colors)
  }
  
  
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
