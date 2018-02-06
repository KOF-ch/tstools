#' Plot Time Series
#' 
#' Conveniently plot time series. 
#' 
#' @param ... multiple objects of class ts or a list of time series. All objects passed through the ... parameter relate to the standard left y-axis.
#' @param tsr list of time series objects of class ts.
#' @param left_as_bar logical should the series that relate to the left bar be drawn as (stacked) bar charts?
#' @param group_bar_chart logical should a bar chart be grouped instead of stacked?
#' @param plot_title character title to be added to the plot
#' @param plot_subtitle character subtitle to be added to the plot 
#' @param plot_subtitle_r character second subtitle to be added at the top right
#' @param find_ticks_function function to compute ticks.
#' @param fill_up_start logical should the start year be filled up? 
#' @param overall_xlim integer overall x-axis limits, defaults to NULL. 
#' @param overall_ylim integer overall y-axis limits, defaults to NULL.
#' @param manual_date_ticks character vector of manual date ticks.
#' @param manual_value_ticks_l numeric vector, forcing ticks to the left y-axis 
#' @param manual_value_ticks_r numeric vector, forcing ticks to the right y-axis 
#' @param theme list of default plot output parameters. Defaults to NULL, which leads to \code{\link{init_tsplot_theme}} being called. Please see the vignette for details about tweaking themes.
#' @param quiet logical suppress output, defaults to TRUE.
#' @param auto_legend logical should legends be printed automatically, defaults to TRUE.
#'
#' @importFrom graphics rect axis box title mtext
#'
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
tsplot.zoo <- function(...,
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
                       auto_legend = TRUE) {
  stop("zoo objets are not supported yet. Please convert your data to ts!")
}

#' @export
tsplot.xts <- function(...,
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
                       auto_legend = TRUE) {
  stop("xts objects are not supported yet. Please convert your data to ts if possible!")
}

#' @export
tsplot.list <- function(...,
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
  
  tsl <- c(...)
  
  if(is.null(theme)) theme <- init_tsplot_theme()
  # thanks to @christophsax for that snippet.
  # I been looking for this for while..
  op <- par(no.readonly = T)
  par(no.readonly = T,
      mar = theme$margins)
  # restore par on exit
  on.exit(par(op))
  
  
  cnames <- names(tsl)
  # if(!is.null(tsr)) cnames <- names(tsr) 
  
  if(left_as_bar) {
    # Combine ts
    tsmat <- do.call("cbind", tsl)
    
    if(!is.null(dim(tsmat)) && dim(tsmat)[2] > 1) {
      # Set all NAs to 0 so range() works properly
      tsmat[is.na(tsmat)] <- 0
      ranges <- apply(tsmat, 1, function(r) {
        range(c(sum(r[r < 0]), sum(r[r >= 0])))
      })
      tsl_r <- c(min(ranges[1,]), max(ranges[2,]))
    } else {
      # tsmat is still a single ts
      tsl_r <- range(tsmat)
    }
  } else {
    tsl_r <- range(as.numeric(unlist(tsl)),na.rm = T)
  }
  
  if(!is.null(theme$y_range_min_size)) {
    # Restrict y range to at least theme$y_range_min_size
    tsl_r_size <- diff(tsl_r)
    if(tsl_r_size < theme$y_range_min_size) {
      tsl_r_mid <- 0.5*tsl_r_size + tsl_r[1]
      half_min_range_size <- 0.5*theme$y_range_min_size
      tsl_r <- c(tsl_r_mid - half_min_range_size, tsl_r_mid + half_min_range_size)
    }
  }
  
  if(!is.null(tsr)) {
    tsr <- sanitizeTsr(tsr)
    tsr_r <- range(unlist(tsr))
    
    if(!is.null(theme$y_range_min_size)) {
      tsr_r_size <- diff(tsr_r)
      if(tsr_r_size < theme$y_range_min_size) {
        tsr_r_mid <- 0.5*tsr_r_size + tsr_r[1]
        half_min_range_size <- 0.5*theme$y_range_min_size
        tsr_r <- c(tsr_r_mid - half_min_range_size, tsr_r_mid + half_min_range_size)
      }
    }
  }
  
  
  
  
  # CANVAS OPTIONS START #########################################
  # so far manual date ticks are ignored.
  global_x <- getGlobalXInfo(tsl,tsr,fill_up_start = fill_up_start)
  
  # y can't be global in the first place, cause 
  # tsr and tsl have different scales.... 
  # time series left
  if(!is.null(manual_value_ticks_l)){
    left_ticks <- manual_value_ticks_l
    left_y <- list(y_range = range(manual_value_ticks_l),
                   y_ticks = manual_value_ticks_l)  
  } else{
    left_ticks <- do.call(find_ticks_function, list(tsl_r, theme$y_grid_count, theme$preferred_y_gap_sizes, theme$range_must_not_cross_zero, left_as_bar))
    left_y <- list(y_range = range(left_ticks), y_ticks = left_ticks)
    # return("Only works with manual value ticks...")
  }
  # time series right 
  if(!is.null(tsr)){
    if(!is.null(manual_value_ticks_r)){
      if(length(manual_value_ticks_r) != length(left_y$y_ticks)){
        return("When using to manual tick position vectors, both need to be of same length! (Otherwise grids look ugly)")
      }
      right_ticks <- manual_value_ticks_r
      right_y <- list(y_range = range(manual_value_ticks_r),
                      y_ticks = manual_value_ticks_r)  
    } else {
      right_ticks <- do.call(find_ticks_function, list(tsr_r, length(left_ticks), theme$preferred_y_gap_sizes, theme$range_must_not_cross_zero, FALSE))
      right_y <- list(y_range = range(right_ticks), y_ticks = right_ticks)
    }
  } else {
    # define right_ticks anyway to avoid having to check for it further down
    right_ticks <- 1
  }
  
  if(!theme$y_grid_count_strict) {
    left_diff <- diff(left_ticks)
    left_d <- left_diff[1]
    left_ub <- left_ticks[length(left_ticks)]
    left_lb <- left_ticks[1]
    
    if(!is.null(tsr)) {
      right_diff <- diff(right_ticks)
      right_d <- right_diff[1]
      right_ub <- right_ticks[length(left_ticks)]
      right_lb <- right_ticks[1]
    }
    
    left_needs_extra_tick_top <- tsl_r[2] > left_ub - left_d*theme$y_tick_margin
    
    if(left_needs_extra_tick_top) {
      left_ticks <- c(left_ticks, left_ub + left_d)
      if(!is.null(tsr)) {
        right_ticks <- c(right_ticks, right_ub + right_d)
      }
    }
  
    if(!left_as_bar) {
      left_needs_exta_tick_bottom <- tsl_r[1] < left_lb + left_d*theme$y_tick_margin
      
      if(left_needs_exta_tick_bottom) {
        left_ticks <- c(left_lb - left_d, left_ticks)
        if(!is.null(tsr)) {
          right_ticks <- c(right_lb - right_d, right_ticks)
        }
      }
    }
    
    if(!is.null(tsr)) {
      right_needs_extra_tick_top <- tsr_r[2] > right_ub - right_d*theme$y_tick_margin
      
      if(right_needs_extra_tick_top) {
        left_ticks <- c(left_ticks, left_ub + left_d)
        right_ticks <- c(right_ticks, right_ub + right_d)
      }
      
      right_needs_extra_tick_bottom <- tsr_r[1] < right_lb + right_d*theme$y_tick_margin
      
      if(right_needs_extra_tick_bottom) {
        left_ticks <- c(left_lb - left_d, left_ticks)
        right_ticks <- c(right_lb - right_d, right_ticks)
      }
    }
    
    # Technically we could save ourselves all that correcting if manual ticks are not null.
    # This is just a convenient place to check.
    
    if(!theme$range_must_not_cross_zero || (sign(min(left_ticks)) == sign(max(left_ticks)) && sign(min(right_ticks)) == sign(max(right_ticks)))) {
      if(is.null(manual_value_ticks_l)) {
        left_y <- list(y_range = range(left_ticks), y_ticks = left_ticks)
      }
      
      if(is.null(manual_value_ticks_r) && !is.null(tsr)) {
        right_y <- list(y_range = range(right_ticks), y_ticks = right_ticks)
      }
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
      xl <- compute_decimal_time(theme$highlight_window_start,
                               theme$highlight_window_freq)
      
    } else{
      xl <- global_x$x_range[2]-2
    }
    
    if(!any(is.na(theme$highlight_window_end))){
      xr <- compute_decimal_time(theme$highlight_window_end,
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
    draw_ts_bars(tsl,
               group_bar_chart = group_bar_chart,
               theme = theme)
    if(theme$sum_as_line){
      reduced <- Reduce("+",tsl)
      draw_sum_as_line(reduced, theme)
    }
    
  } else {
    # draw lineplot
    draw_ts_lines(tsl,theme=theme)
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
    
    draw_ts_lines(tsr,theme = tt_r)
    
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
    
    add_legend(names(tsl), names(tsr),
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
                   right_range = tsr_r)
  } 
}


