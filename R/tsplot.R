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
tsplot <- function(series,...,theme = kof_theme,ygrid_dynamic,ygrid_factor) UseMethod("tsplot")


#' @rdname tsplot
#' @export
tsplot.ts <- function(series,...,
                      theme=NULL,
                      ygrid_dynamic=F,
                      ygrid_factor = 5){
  
  li <- list(...)
  
  # list of time series
  tl <- c(list(series),li)
  
  if(!all(unlist(lapply(li,is.ts)))) 
    stop("all list elements must be of class ts!")
  
  # basically pass it all on to the list method of tsplot
  tsplot(tl,theme = theme, ygrid_dynamic = ygrid_dynamic,
         ygrid_factor = ygrid_factor)  
  
}

#' @rdname tsplot
#' @export
tsplot.list <- function(series,sel=NULL,theme = NULL, ygrid_dynamic = F,
                        ygrid_factor = 5){
  
  # use an ETH / KOF default theme if no other theme is specified
  if(is.null(theme)){
    kof_theme <- list()
    kof_theme$ygrid <- seq(-60, 60, 30)
    kof_theme$xlab <- NA
    kof_theme$ylab <- NA
    kof_theme$xaxs <- 'i'
    kof_theme$yaxs <- 'i'  
    kof_theme$xaxt <- 'n'  
    kof_theme$yaxt <- 'n'  
    kof_theme$title_adj <- 0
    kof_theme$title_line <- 3.75
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
  
  # get the window:
  # min date
  min_date <- unlist(lapply(series,function(x) min(time(x))))
  min_date_nm <- names(min_date)[which.min(min_date)]
  min_date_value <- min_date[which.min(min_date)]
  # min values
  min_value <- unlist(lapply(series,function(x) min(x,na.rm = T)))
  min_value_nm <- names(min_value)[which.min(min_value)]
  min_value_value <- min_value[which.min(min_value)]
  
  # max dates
  max_date <- unlist(lapply(series,function(x) max(time(x))))
  max_date_nm <- names(max_date)[which.max(max_date)]
  max_date_value <- max_date[which.max(max_date)]
  # min values
  max_value <- unlist(lapply(series,function(x) max(x,na.rm = T)))
  max_value_nm <- names(max_value)[which.max(max_value)]
  max_value_value <- max_value[which.max(max_value)]
  
  
  plot(series[[1]],main="test",
       xlim = c(min_date_value,max_date_value),
       ylim = c(min_value_value*1.1,max_value_value*1.1),
       col = theme$line_colors[[1]],
       lwd = 1.5,
       xlab = theme$xlab,
       ylab = theme$ylab,
       xaxs = theme$xaxs,
       yaxs = theme$yaxs,
       xaxt = theme$xaxt,
       yaxt = theme$yaxt
  )
  
  # add horizontal grid lines
  
  if(ygrid_dynamic){
    ygrid <- seq(min_value_value,max_value_value,
                 (max_value_value - min_value_value)/ygrid_factor)
    for (hl in ygrid)  abline(h = hl, col = theme$grid_color)
  } else {
    for (hl in theme$ygrid)  abline(h = hl, col = theme$grid_color)
  }
  
  if(length(series) > 1){
    for (i in 2:length(series)){
      lines(series[[i]],
            col=theme$line_colors[[i]],
            lwd = 1.5)
    }
  }
  
}


