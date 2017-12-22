#' @export
tsplot <- function(...,
                   tsr = NULL,
                   left_as_bar = FALSE,                    
                   group_bar_chart = NULL,
                   plot_title = NULL,
                   plot_subtitle = NULL,              
                   plot_subtitle_r = NULL,
                   find_ticks_function = "findTicks",
                   fill_up_start = FALSE,
                   overall_xlim = NULL,
                   overall_ylim = NULL,
                   manual_date_ticks = NULL,
                   manual_value_ticks_l = NULL,
                   manual_value_ticks_r = NULL,
                   theme = NULL,
                   quiet = TRUE,
                   auto_legend = TRUE){
  UseMethod("tsplot")
} 

#' @export
tsplot.ts <- function(...,
                      tsr = NULL,
                      left_as_bar = FALSE,                
                      group_bar_chart = NULL,
                      plot_title = NULL,
                      plot_subtitle = NULL,
                      plot_subtitle_r = NULL,
                      find_ticks_function = "findTicks",
                      fill_up_start = fill_up_start,
                      overall_xlim = NULL,
                      overall_ylim = NULL,
                      manual_date_ticks = NULL,
                      manual_value_ticks_l = NULL,
                      manual_value_ticks_r = NULL,
                      theme = NULL,
                      quiet = TRUE,
                      auto_legend = TRUE
){
  li <- list(...)
  tsplot(li,
         tsr = tsr,
         left_as_bar = left_as_bar,
         group_bar_chart = group_bar_chart,
         find_ticks_function = find_ticks_function,
         manual_date_ticks = manual_date_ticks,
         quiet = quiet,
         auto_legend = auto_legend,
         overall_xlim = overall_xlim,
         overall_ylim = overall_ylim,
         manual_value_ticks_l = manual_value_ticks_l,
         manual_value_ticks_r = manual_value_ticks_r,
         theme = theme)
}

#' @export
tsplot.mts <- function(...,
                       tsr = NULL,
                       left_as_bar = FALSE,                                       group_bar_chart = NULL,
                       plot_title = NULL,
                       plot_subtitle = NULL,                                      plot_subtitle_r = NULL,
                       find_ticks_function = "findTicks",
                       fill_up_start = NULL,
                       overall_xlim = NULL,
                       overall_ylim = NULL,
                       manual_date_ticks = NULL,
                       manual_value_ticks_l = NULL,
                       manual_value_ticks_r = NULL,
                       theme = NULL,
                       quiet = TRUE,
                       auto_legend = TRUE){
  li <- list(...)
  if(length(li) > 1){
    stop("If you use multivariate time series objects (mts), make sure to pass only one object per axis. Place all time series you want to plot on one y-axis in one mts object or list of time series.")
  } else{
    tsplot(as.list(li[[1]]),
           tsr = tsr,
           fill_up_start = fill_up_start,
           manual_date_ticks = manual_date_ticks,
           left_as_bar = left_as_bar,
           group_bar_chart = group_bar_chart,
           find_ticks_function = find_ticks_function,
           overall_xlim = overall_xlim,
           overall_ylim = overall_ylim,
           theme = theme)
  }
}

#' @export
tsplot.list <- function(tsl,
                        tsr = NULL,
                        left_as_bar = FALSE,
                        group_bar_chart = NULL,
                        plot_title = NULL,
                        plot_subtitle = NULL,
                        plot_subtitle_r = NULL,
                        find_ticks_function = "findTicks",
                        tick_function_args = list(tsl_r,
                                                  theme$y_grid_count),
                        fill_up_start = F,
                        overall_xlim = NULL,
                        overall_ylim = NULL,
                        manual_date_ticks = NULL,
                        manual_value_ticks_l = NULL,
                        manual_value_ticks_r = NULL,
                        theme = NULL,
                        quiet = TRUE,
                        auto_legend = TRUE
){
  
  if(is.null(theme)) theme <- initDefaultTheme()
  # thanks to @christophsax for that snippet.
  # I been looking for this for while..
  op <- par(no.readonly = T)
  par(no.readonly = T,
      mar = theme$margins)
  # restore par on exit
  on.exit(par(op))
  
  
  cnames <- names(tsl)
  # if(!is.null(tsr)) cnames <- names(tsr) 
  
  tsl_r <- as.numeric(range(unlist(tsl),na.rm = T))
  tsr <- .sanitizeTsr(tsr)
  if(!is.null(tsr)) tsr_r <- range(unlist(tsr))
  
  
  
  
  # CANVAS OPTIONS START #########################################
  # so far manual date ticks are ignored.
  global_x <- getGlobalXInfo(tsl,tsr,fill_up_start = fill_up_start)
  
  # y can't be global in the first place, cause 
  # tsr and tsl have different scales.... 
  # time series left
  if(!is.null(manual_value_ticks_l)){
    left_y <- list(y_range = range(manual_value_ticks_l),
                   y_ticks = manual_value_ticks_l)  
  } else{
    if(left_as_bar) return("Finding ticks automatically when stacking values is not supported yet. Please use manual_value_ticks.")
    
    left_y <- list(y_range = range(do.call(find_ticks_function,
                                           list(tsl_r,theme$y_grid_count)
    )),
    y_ticks = do.call(find_ticks_function,
                      list(tsl_r,theme$y_grid_count)
    ))
    # return("Only works with manual value ticks...")
  }
  # time series right 
  if(!is.null(tsr)){
    if(!is.null(manual_value_ticks_r)){
      if(length(manual_value_ticks_r) != length(left_y$y_ticks)){
        return("When using to manual tick position vectors, both need to be of same length! (Otherwise grids look ugly)")
      }
      right_y <- list(y_range = range(manual_value_ticks_r),
                      y_ticks = manual_value_ticks_r)  
    } else {
      right_y <- list(y_range = 
                        range(do.call(find_ticks_function,
                                      list(tsr_r,
                                           length(left_y$y_ticks)))
                        ),
                      y_ticks = do.call(find_ticks_function,
                                        list(tsr_r,length(left_y$y_ticks))))
    }
    
    
    
  }
  
  
  # CANVAS OPTIONS END #########################################
  
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
    if(!any(is.na(theme$highlight_window_start))){
      xl <- computeDecimalTime(theme$highlight_window_start,
                               theme$highlight_window_freq)
      
    } else{
      xl <- global_x$x_range[2]-2
    }
    
    if(!any(is.na(theme$highlight_window_end))){
      xr <- computeDecimalTime(theme$highlight_window_end,
                               theme$highlight_window_freq)
      
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
  
  # LEFT Y-AXIS
  if(theme$show_left_y_axis){
    axis(2,left_y$y_ticks,las = theme$y_las)
  }
  
  if(theme$show_y_grids){
    addYGrids(left_y$y_ticks,theme = theme)
  }
  
  
  if(left_as_bar){
    # draw barplot
    drawTsBars(tsl,
               group_bar_chart = group_bar_chart,
               theme = theme)
    if(theme$sum_as_line){
      reduced <- Reduce("+",tsl)
      .drawSumAsLine(reduced, theme)
    }
    
  } else {
    # draw lineplot
    drawTsLines(tsl,theme=theme)
  }
  
  # RIGHT PLOT #######################
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
    total_le <- length(tsl) + length(tsr)
    start_r <- (total_le - (length(tsr)-1)):total_le
    
    
    tt_r <- theme
    tt_r$line_colors <- tt_r$line_colors[start_r]
    if(!all(is.na(tt_r$lwd[start_r]))) tt_r$lwd <- na.omit(tt_r$lwd[start_r])
    if(!all(is.na(tt_r$lwd[start_r]))) tt_r$lty <- na.omit(tt_r$lty[start_r])
    
    drawTsLines(tsr,theme = tt_r)
    
    # RIGHT Y-Axis
    if(theme$show_right_y_axis){
      axis(4,right_y$y_ticks,las = theme$y_las)
    }
  }
  
  if(theme$use_box) box()
  
  # add legend
  if(auto_legend){
    if(is.null(names(tsl))){
      names(tsl) <- paste0("series_",1:length(tsl))
    }
    if(is.null(names(tsr)) & !is.null(tsr)){
      names(tsr) <- paste0("series_",1:length(tsr))
    }
    
    addLegend(names(tsl), names(tsr),
              theme = theme, left_as_bar = left_as_bar)
  }
  
  # add title and subtitle
  if(!is.null(plot_title)){
    if(!any(is.na(theme$title_transform))){
      plot_title <- do.call(theme$title_transform,
                            list(plot_title))
    } 
    title(main = plot_title, adj = theme$title_adj,
          line = theme$title_line,
          outer = theme$title_outer,
          cex.main = theme$title_cex.main)    
  }
  
  if(!is.null(plot_subtitle)){
    if(!is.null(theme$subtitle_transform)){
      plot_subtitle <- do.call(theme$subtitle_transform,
                               list(plot_subtitle))
    } 
    mtext(plot_subtitle, adj = theme$title_adj,
          line = theme$subtitle_line,
          outer = theme$subtitle_outer,
          cex = theme$subtitle_cex)    
  }
  
  
  if(!is.null(plot_subtitle_r)){
    if(!is.null(theme$subtitle_transform)){
      plot_subtitle_r <- do.call(theme$subtitle_transform,
                                 list(plot_subtitle_r))
    } 
    mtext(plot_subtitle_r,
          adj = theme$subtitle_adj_r,
          line = theme$subtitle_line,
          outer = theme$subtitle_outer,
          cex = theme$subtitle_cex)    
  }
  # return axes and tick info, as well as theme maybe? 
  if(!quiet){
    output <- list(left_range = tsl_r,
                   right_range = tsl_l)
  } 
}


