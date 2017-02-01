#' @export
tsplot <- function(...,
                   tsr = NULL,
                   left_as_bar = FALSE,
                   overall_xlim = NULL,
                   overall_ylim = NULL,
                   manual_date_ticks = NULL,
                   manual_value_ticks_l = NULL,
                   manual_value_ticks_r = NULL,
                   theme = NULL){
  UseMethod("tsplot")
} 

#' @export
tsplot.ts <- function(...,tsr = NULL,
                      left_as_bar = FALSE,
                      overall_xlim = NULL,
                      overall_ylim = NULL,
                      manual_date_ticks = NULL,
                      manual_value_ticks_l = NULL,
                      manual_value_ticks_r = NULL,
                      theme = NULL){
  tsl <- list(...)
  tsplot(tsl,tsr = tsr,
         manual_date_ticks = manual_date_ticks,
         left_as_bar = left_as_bar,
         overall_xlim = overall_xlim,
         overall_ylim = overall_ylim,
         theme = theme)
}

#' @export
tsplot.mts <- function(...,
                       tsr = NULL,
                       left_as_bar = FALSE,
                       overall_xlim = NULL,
                       overall_ylim = NULL,
                       manual_date_ticks = NULL,
                       manual_value_ticks_l = NULL,
                       manual_value_ticks_r = NULL,
                       theme = NULL){
  li <- list(...)
  if(length(li) > 1){
    stop("If you use multivariate time series objects (mts), make sure to pass only one object per axis. Place all time series you want to plot on one y-axis in one mts object or list of time series.")
  } else{
    tsplot(as.list(li[[1]]),
           tsr = tsr,
           manual_date_ticks = manual_date_ticks,
           left_as_bar = left_as_bar,
           overall_xlim = overall_xlim,
           overall_ylim = overall_ylim,
           theme = theme)
  }
}

#' @export
tsplot.list <- function(...,
                        tsr = NULL,
                        left_as_bar = FALSE,
                        overall_xlim = NULL,
                        overall_ylim = NULL,
                        manual_date_ticks = NULL,
                        manual_value_ticks_l = NULL,
                        manual_value_ticks_r = NULL,
                        theme = NULL){
  
  tsl <- (...)
  tsr <- .sanitizeTsr(tsr)
  
  if(is.null(theme)) theme <- initDefaultTheme()
  if(theme$fillYearWithNAs){
    tsl <- lapply(tsl,fillUpYearWithNAs)
    if(!is.null(tsr)) tsr <- lapply(tsr,fillUpYearWithNAs)
  }
  
  global_x <- .getDateInfo(tsl,tsr,
                           theme,manual_date_ticks)
  
  global_y <- list(y_range = c(-100,100))
  
  # global_y <- .getValueInfo(tsl,tsr,
  #                           theme, 
  #                           manual_value_ticks_l,
  #                           manual_value_ticks_r)
  
  # BASE CANVAS 
  plot(NULL,
       xlim = global_x$x_range,
       ylim = global_y$y_range,
       axes = F,
       xlab = "",
       ylab = "",
       xaxs = theme$xaxs,
       yaxs = theme$yaxs
  )
  
  # add X-Axis (always the same, no matter how many lines or 
  # or whether bar or line)
  axis(1)
  
  
  if(left_as_bar){
    # draw barplot
    return(drawTsBars(tsl,theme=theme))
    
  } else {
    # draw lineplot
    drawTsLines(tsl,theme=theme)
    
    
  }
  
  # Add a right axis line plot
  if(is.null(tsr)){
    
  }
}


# o <- diff(r)*theme$y_offset_pct
# d$x_range <- r + c(-o,o)
#' determine ticks and grid position
#' theme contains which grids should be drawn etc.
.getDateInfo <- function(tsr,tsl,theme,
                         manual_date_ticks){
  d <- list()
  if(!is.null(manual_date_ticks)){
    d$x_ticks <- manual_date_ticks
    d$x_range <- range(manual_date_ticks)
    return(d)
  } else{
    # NO MANUAL X-AXIS given
    all_series <- c(tsl,tsr)
    d$x_range <- range(time(unlist(all_series)))
    if(theme$yearly_ticks){
      
    }
    if(theme$quarterly_ticks){
      if(theme$year_labels_mid){
        
      }
    }
  }
}



#' Make sure right axis object is of appropriate class.
.sanitizeTsr <- function(tsr){
  if(is.null(tsr)){
    return(tsr)
  } else if(inherits(tsr,"mts")){
    as.list(tsr)
  } else if(inherits(tsr,"ts")){
    list(tsr)
  } else if(inherits(tsr,"list")){
    tsr
  } else {
    stop("Time series object to be plotted on the right axis, 
         has to be either of class ts, mts or list.")
  }
}

.getXAxisInfo <- function(tsl,tsr,theme){
  unlist(c(tsr,tsl))
}






