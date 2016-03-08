#' @param series object of class ts, or list of time series series.
#' @param ... objects of class ts
#' @param theme list holding extra style arguments to be passed to plot.
#' @param ygrid_dynamic logical, defaults to FALSE. Should ygrids be dynamic?
#' If not, the ygrid configuration of the theme is used. 
#' @param ygrid_factor numeric, defaults to 5. Factor by which the difference 
#' between the maximum and the minimum y value should be divided. This parameter
#' determines the number of horizontal grid lines when dynamic ygrid_dynamic is 
#' TRUE.
#' 
#' @export
tsplot <- function(series,...,
                   theme = NULL,
                   plot.title = NULL,
                   plot.subtitle = NULL,
                   ygrid_dynamic,
                   ygrid_factor,
                   yaxis_factor,
                   theme_out,
                   print_x_axis) UseMethod("tsplot")


#' @rdname tsplot
#' @export
tsplot.ts <- function(series,...,
                      theme = NULL,
                      ygrid_dynamic = F,
                      plot.title = NULL,
                      plot.subtitle = NULL,
                      ygrid_factor = 5,
                      yaxis_factor = 20,
                      quarter_ticks = T,
                      theme_out = F,
                      print_x_axis = T){
  
  li <- list(...)
  
  # list of time series
  tl <- c(list(series),li)
  
  if(!all(unlist(lapply(li,is.ts)))) 
    stop("all list elements must be of class ts!")
  
  # basically pass it all on to the list method of tsplot
  tsplot(tl,theme = theme, ygrid_dynamic = ygrid_dynamic,
         ygrid_factor = ygrid_factor,
         yaxis_factor = yaxis_factor,
         plot.title = plot.title,
         plot.subtitle = plot.subtitle,
         theme_out = theme_out,
         print_x_axis = print_x_axis)  
  
}

#' @rdname tsplot
#' @export
tsplot.list <- function(series,sel=NULL,
                        theme = NULL,
                        plot.title = NULL,
                        plot.subtitle = NULL,
                        ygrid_dynamic = F,
                        ygrid_factor = 5,
                        yaxis_factor = 20,
                        quarter_ticks = T,
                        theme_out = F,
                        print_x_axis = T,
                        ...){
  
  # definition of a default theme
  if(is.null(theme)){
    kof_theme <- list()
    kof_theme$ygrid <- seq(-60, 60, 30)
    kof_theme$xlab <- NA
    kof_theme$ylab <- NA
    kof_theme$xaxs <- 'i'
    kof_theme$yaxs <- 'i'  
    kof_theme$xaxt <- 'n'  
    kof_theme$yaxt <- 'n'  
    kof_theme$lwd <- 1.5
    kof_theme$title_adj <- 0
    kof_theme$title_line <- 1.5
    kof_theme$subtitle_line <- .3
    kof_theme$title_cex.main <- 1
    kof_theme$subtitle_cex.main <- 1
    kof_theme$grid_color <- "#00000022"
    kof_theme$line_colors <- c(ETH7 = "#a8322d",
                               ETH5 = "#91056a",
                               ETH8 = "#007a92",
                               ETH8_60 = "#66b0c2",
                               ETH5_60 = "#cc67a7",
                               ETH7_50 = "#e19794")
    theme <- kof_theme
  }
  
  
  # some sanity checks
  if(!all(unlist(lapply(series,is.ts))))
    stop("all elements of the list need to be objects of class ts.")
  
  # select the entire series if there is no particular selection
  if(!is.null(sel)){
    series <- series[sel]  
  }
  # don't have default colors for more than 6 lines
  if(length(series) > 6) stop("This convenience plot function does not
                              support more than 6 series in one plot.
                              Don't use this theme / template in case
                              you need more, just use basic plotting
                              and build such a plot on your own.")

  ts_time <- unique(unlist(lapply(series,time)))
  date_range <- range(ts_time)
  value_range <- range(unlist(series))
  
    
  # # get the window:
  # # min date
  # min_date <- unlist(lapply(series,function(x) min(time(x))))
  # min_date_nm <- names(min_date)[which.min(min_date)]
  # min_date_value <- min_date[which.min(min_date)]
  # # min values
  # min_value <- unlist(lapply(series,function(x) min(x,na.rm = T)))
  # min_value_nm <- names(min_value)[which.min(min_value)]
  # min_value_value <- min_value[which.min(min_value)]
  # 
  # # max dates
  # max_date <- unlist(lapply(series,function(x) max(time(x))))
  # max_date_nm <- names(max_date)[which.max(max_date)]
  # max_date_value <- max_date[which.max(max_date)]
  # # min values
  # max_value <- unlist(lapply(series,function(x) max(x,na.rm = T)))
  # max_value_nm <- names(max_value)[which.max(max_value)]
  # max_value_value <- max_value[which.max(max_value)]
  # 
  # ts_time <- unique(unlist(lapply(series,function(x) time(x))))
  
  
  # Define Plot ###############
  plot(series[[1]],
       xlim = date_range,
       ylim = value_range*1.04,
       col = theme$line_colors[[1]],
       lwd = theme$lwd,
       xlab = theme$xlab,
       ylab = theme$ylab,
       xaxs = theme$xaxs,
       yaxs = theme$yaxs,
       xaxt = theme$xaxt,
       yaxt = theme$yaxt,
       ...)
  
  # this param is always true in stand alone plots, 
  # might be useful if multiple tsplots are plotted 
  # top of each other
  if(print_x_axis){
    # axis and ticks defintion
    if(quarter_ticks){
      ext_qtr <- ts_time[abs(ts_time * 4 - floor(ts_time * 4)) < 0.001]
      ext_label <- ifelse(ext_qtr - floor(ext_qtr) == 0.5, as.character(floor(ext_qtr)), NA)
      # x-axis
      axis(1, at = ext_qtr, labels = ext_label,
           tcl = -0.5, cex.axis = 1, padj = 0.25)
      axis(1, at = date_range[1]:date_range[2], tcl = -0.75,
           lwd.ticks = 2, labels = FALSE) # thick tick marks
      
    } else{
      axis(1, at = min_date_value:max_date_value,
           tcl = -0.5, cex.axis = 1, padj = 0.25)
    }
  }
  
  
  
  # y-axis

  # horizontal grid lines ######
  if(ygrid_dynamic){
    ygrid <- seq(value_range[1],value_range[2],
                 (value_range[2] - value_range[1])/ygrid_factor)
    for (hl in ygrid)  abline(h = hl, col = theme$grid_color)
    yaxis_main_ticks <- round(seq(value_range[1]*1.04,
                                  value_range[2]*1.04,
                                  yaxis_factor))
  } else {
    for (hl in theme$ygrid)  abline(h = hl, col = theme$grid_color)
    yaxis_main_ticks <- theme$ygrid
  }
  
  axis(2, at = yaxis_main_ticks,
       tcl = -0.5, cex.axis = 1, padj = 0.25)

  # add multiple series to the plot #####
  if(length(series) > 1){
    for (i in 2:length(series)){
      lines(series[[i]],
            col=theme$line_colors[[i]],
            lwd = 1.5)
    }
  }
  
  # Add Title to plot #####
  # title
  if(!is.null(plot.title)){
    title(main = plot.title, adj = theme$title_adj,
          line = theme$title_line,
          cex.main = theme$title_cex.main)  
  }
  
  # subtitle
  if(!is.null(plot.subtitle)){
    mtext(plot.subtitle, adj = theme$title_adj,
          line = theme$subtitle_line,
          cex = theme$subtitle_cex)  
  }
  
  if(theme_out) theme
  
}




