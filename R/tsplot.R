#' @export
tsplot <- function(...,
                   tsr = NULL,
                   left_as_bar = FALSE,
                   fill_up_start = FALSE,
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
                      fill_up_start = fill_up_start,
                      overall_xlim = NULL,
                      overall_ylim = NULL,
                      manual_date_ticks = NULL,
                      manual_value_ticks_l = NULL,
                      manual_value_ticks_r = NULL,
                      theme = NULL){
  tsl <- list(...)
  tsplot(tsl,tsr = tsr,
         fill_up_start = fill_up_start,
         manual_date_ticks = manual_date_ticks,
         left_as_bar = left_as_bar,
         overall_xlim = overall_xlim,
         overall_ylim = overall_ylim,
         manual_date_ticks = manual_date_ticks,
         manual_value_ticks_l = manual_value_ticks_l,
         manual_value_ticks_r = manual_value_ticks_r,
         theme = theme)
}

#' @export
tsplot.mts <- function(...,
                       tsr = NULL,
                       left_as_bar = FALSE,
                       fill_up_start = NULL,
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
           fill_up_start = fill_up_start,
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
                        fill_up_start = F,
                        overall_xlim = NULL,
                        overall_ylim = NULL,
                        manual_date_ticks = NULL,
                        manual_value_ticks_l = NULL,
                        manual_value_ticks_r = NULL,
                        theme = NULL,
                        quiet = TRUE){
  
  tsl <- unlist(list(...),recursive = F)
  tsr <- .sanitizeTsr(tsr)
  
  if(is.null(theme)) theme <- initDefaultTheme()
  
  
  global_x <- getGlobalXInfo(tsl,tsr,fill_up_start = fill_up_start)
  
  # y can't be global in the first place, cause 
  # tsr and tsl have different scales.... 
  left_y <- list(y_range = c(-100,100))
  right_y <- list(y_range = c(-3,3))
  
  # global_y <- .getValueInfo(tsl,tsr,
  #                           theme, 
  #                           manual_value_ticks_l,
  #                           manual_value_ticks_r)
  
  # BASE CANVAS 
  plot(NULL,
       xlim = global_x$x_range,
       ylim = left_y$y_range,
       axes = F,
       xlab = "",
       ylab = "",
       xaxs = theme$xaxs,
       yaxs = theme$yaxs
  )
  
  if(theme$highlight_window){
    if(!is.null(theme$highlight_window_start)){
      xl <- theme$highlight_window_start
    } else{
      xl <- global_x$x_range[2]-2
    }
    
    if(!is.null(theme$highlight_window_end)){
      xr <- theme$highlight_window_end
    } else{
      xr <- global_x$x_range[2]
    }
    rect(xl,left_y$y_range[1],xr,left_y$y_range[2],
         col = theme$highlight_color,
         border = NA)
      
      
  }
  
  # Global X-Axis ###################
  if(theme$yearly_ticks){
    if(theme$label_pos == "start"){
      axis(1,global_x$yearly_tick_pos,labels = global_x$yearly_tick_pos,
           lwd.ticks = theme$lwd_yearly_ticks,
           tcl = theme$tcl_yearly_tick)    
    } else{
      axis(1,global_x$yearly_tick_pos,labels = F,
           lwd.ticks = theme$lwd_yearly_ticks,
           tcl = theme$tcl_yearly_tick)
    }
  }
  
  if(theme$quarterly_ticks){
    if(theme$label_pos == "mid"){
      axis(1,global_x$quarterly_tick_pos,labels = global_x$year_labels_middle_q,
           lwd.ticks = theme$lwd_quarterly_ticks)    
    } else{
      axis(1,global_x$quarterly_tick_pos,labels = F,
           lwd.ticks = theme$lwd_quarterly_ticks)
    }
  }
  
  
  
  if(left_as_bar){
    # draw barplot
    drawTsBars(tsl,theme=theme)
    
  } else {
    # draw lineplot
    drawTsLines(tsl,theme=theme)
  }
  
  # Add a right axis line plot
  if(!is.null(tsr)){
    par(new = T)
    plot(NULL,
         xlim = global_x$x_range,
         ylim = right_y$y_range,
         axes = F,
         xlab = "",
         ylab = "",
         yaxs = theme$yaxs,
         xaxs = theme$xaxs
    )
    drawTsLines(tsr,theme=theme)
  }
  
  # return axes and tick info, as well as theme maybe? 
  if(!quiet){
    
  } else{
    return()
  }
  
}


