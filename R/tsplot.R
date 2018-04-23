#' Plot Time Series
#' 
#' Conveniently plot time series. 
#' 
#' @param ... multiple objects of class ts or a list of time series. All objects passed through the ... parameter relate to the standard left y-axis.
#' @param tsr list of time series objects of class ts.
#' @param left_as_bar logical should the series that relate to the left bar be drawn as (stacked) bar charts?
#' @param group_bar_chart logical should a bar chart be grouped instead of stacked?
#' @param relative_bar_chart logical Should time series be normalized such that bars range from 0 to 1?
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
#' @importFrom graphics rect axis box title mtext strheight
#'
#' @export
tsplot <- function(...,
                   tsr = NULL,
                   left_as_bar = FALSE,                    
                   group_bar_chart = FALSE,
                   relative_bar_chart = FALSE,
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
                      group_bar_chart = FALSE,
                      relative_bar_chart = FALSE,
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
                      auto_legend = TRUE
){
  li <- list(...)
  tsplot(li,
         tsr = tsr,
         left_as_bar = left_as_bar,
         group_bar_chart = group_bar_chart,
         relative_bar_chart = relative_bar_chart,
         plot_title = plot_title,
         plot_subtitle = plot_subtitle,
         plot_subtitle_r = plot_subtitle_r,
         find_ticks_function = find_ticks_function,
         fill_up_start = fill_up_start,
         manual_date_ticks = manual_date_ticks,
         overall_xlim = overall_xlim,
         overall_ylim = overall_ylim,
         manual_value_ticks_l = manual_value_ticks_l,
         manual_value_ticks_r = manual_value_ticks_r,
         theme = theme,
         quiet = quiet,
         auto_legend = auto_legend)
}

#' @export
tsplot.mts <- function(...,
                       tsr = NULL,
                       left_as_bar = FALSE,
                       group_bar_chart = FALSE,
                       relative_bar_chart = FALSE,
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
  li <- list(...)
  if(length(li) > 1){
    stop("If you use multivariate time series objects (mts), make sure to pass only one object per axis. Place all time series you want to plot on one y-axis in one mts object or list of time series.")
  } else{
    tsplot(as.list(li[[1]]),
           tsr = tsr,
           left_as_bar = left_as_bar,
           group_bar_chart = group_bar_chart,
           relative_bar_chart = relative_bar_chart,
           plot_title = plot_title,
           plot_subtitle = plot_subtitle,
           plot_subtitle_r = plot_subtitle_r,
           find_ticks_function = find_ticks_function,
           fill_up_start = fill_up_start,
           overall_xlim = overall_xlim,
           overall_ylim = overall_ylim,
           manual_date_ticks = manual_date_ticks,
           manual_value_ticks_l = manual_value_ticks_l,
           manual_value_ticks_r = manual_value_ticks_r,
           theme = theme,
           quiet = quiet,
           auto_legend = auto_legend)
  }
}

#' @export
tsplot.zoo <- function(...,
                       tsr = NULL,
                       left_as_bar = FALSE,
                       group_bar_chart = FALSE,
                       relative_bar_chart = FALSE,
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
                       auto_legend = TRUE) {
  stop("zoo objets are not supported yet. Please convert your data to ts!")
}

#' @export
tsplot.xts <- function(...,
                       tsr = NULL,
                       left_as_bar = FALSE,
                       group_bar_chart = FALSE,
                       relative_bar_chart = FALSE,
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
                       auto_legend = TRUE) {
  stop("xts objects are not supported yet. Please convert your data to ts if possible!")
}

#' @export
tsplot.list <- function(...,
                        tsr = NULL,
                        left_as_bar = FALSE,
                        group_bar_chart = FALSE,
                        relative_bar_chart = FALSE,
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
                        auto_legend = TRUE
){
  
  tsl <- c(...)
  
  if(inherits(tsr, "ts")) {
    tsr <- list(tsr)
  }
  
  if(any(sapply(tsl, length) == 1) || (!is.null(tsr) && any(sapply(tsr, length) == 1))) {
    stop("Time series of length 1 are not supported!")
  }
  
  if(is.null(theme)) theme <- init_tsplot_theme()
  
  # Expand per-line parameters for recycling
  total_n_ts <- length(tsl) + length(tsr)
  expand_param <- function(theme, param_name) {
    rep(theme[[param_name]], ceiling(total_n_ts/length(theme[[param_name]])))
  }
  
  
  theme$line_colors <- expand_param(theme, "line_colors")
  theme$lwd <- expand_param(theme, "lwd")
  theme$lty <- expand_param(theme, "lty")
  theme$show_points <- expand_param(theme, "show_points")
  theme$point_symbol <- expand_param(theme, "point_symbol")
  theme$NA_continue_line <- expand_param(theme, "NA_continue_line")
  
  
  if(left_as_bar && relative_bar_chart) {
    # Normalize ts
    if(group_bar_chart) {
      m <- Reduce('max', tsl)
    } else {
      sums <- Reduce('+', tsl)
      m <- max(sums)
    }
    tsl <- lapply(tsl, '/', m)
  }
 
  # Set default names for legend if none provided (moved here for measuring margin)
  right_name_start <- 0
  if(is.null(names(tsl))){
    names(tsl) <- paste0("series_",1:length(tsl))
    right_name_start <- length(tsl)
  }
  if(is.null(names(tsr)) & !is.null(tsr)){
    if(is.list(tsr)){
      names(tsr) <- paste0("series_", 1:length(tsr) + right_name_start)  
    } else{
      tsr <- list(tsr)
      names(tsr) <- paste0("series_", right_name_start + 1)  
    }
    
  }
  
  if(is.na(theme$margins[1])) {
    if(theme$auto_bottom_margin || auto_legend) {
      line_height_in <- par("csi") # Miami. YEEEAAAAAAHHHHH!
      
      legend_left <- names(tsl)
      if(theme$sum_as_line && !is.null(theme$sum_legend)) {
        legend_left <- c(legend_left, theme$sum_legend)
      }
      legend_height_in <- strheight(paste(legend_left, collapse = "\n"), units = "inches", cex = theme$legend_font_size)
      if(!is.null(tsr)) {
        legend_height_in <- max(legend_height_in, strheight(paste(names(tsr), collapse = "\n"), units = "inches", cex = theme$legend_font_size))
      }
      # TODO: theme$legend_intersp_y
      # Also: a single multiline legend changes the height of ALL of them (in add_legends>legend)
      
      theme$margins[1] <- (legend_height_in)/(line_height_in*theme$legend_col) + theme$legend_margin_top/line_height_in + 1.2
    } else {
      theme$margins[1] <- theme$default_bottom_margin
    }
  }
      
  par(mar = theme$margins)
  
  cnames <- names(tsl)
  # if(!is.null(tsr)) cnames <- names(tsr) 
  
  if(left_as_bar) {
    # Combine ts
    tsmat <- do.call("cbind", tsl)
    
    if(!is.null(dim(tsmat)) && dim(tsmat)[2] > 1) {
      # Set all NAs to 0 so range() works properly
      tsmat[is.na(tsmat)] <- 0
      ranges <- apply(tsmat, 1, function(r) {
        if(group_bar_chart) {
          range(r)
        } else {
          range(c(sum(r[r < 0]), sum(r[r >= 0])))
        }
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
    left_ticks <- do.call(find_ticks_function, list(tsl_r, theme$y_grid_count, theme$preferred_y_gap_sizes, theme$range_must_not_cross_zero))
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
      right_ticks <- do.call(find_ticks_function, list(tsr_r, length(left_ticks), theme$preferred_y_gap_sizes, theme$range_must_not_cross_zero))
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
    
    left_needs_exta_tick_bottom <- tsl_r[1] < left_lb + left_d*theme$y_tick_margin
    
    if(left_needs_exta_tick_bottom) {
      left_ticks <- c(left_lb - left_d, left_ticks)
      if(!is.null(tsr)) {
        right_ticks <- c(right_lb - right_d, right_ticks)
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
    
    left_sign_ok = sign(left_ticks[1]) == sign(left_y$y_ticks[1]) && sign(max(left_ticks)) == sign(max(left_y$y_ticks))
    
    right_sign_ok = is.null(tsr) || (sign(right_ticks[1]) == sign(right_y$y_ticks[1]) && sign(max(right_ticks)) == sign(max(right_y$y_ticks)))
    
    if(is.null(manual_value_ticks_l) && (!theme$range_must_not_cross_zero || left_sign_ok)) {
      left_y <- list(y_range = range(left_ticks), y_ticks = left_ticks)
    }
    
    if(is.null(manual_value_ticks_r) && !is.null(tsr) && (!theme$range_must_not_cross_zero || right_sign_ok)) {
      right_y <- list(y_range = range(right_ticks), y_ticks = right_ticks)
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
    hlw_start <- theme$highlight_window_start
    if(!any(is.na(hlw_start))){
      if(!is.list(hlw_start)) {
        hlw_start <- list(hlw_start)
      }
      xl <- sapply(hlw_start, compute_decimal_time, theme$highlight_window_freq)
    } else{
      xl <- global_x$x_range[2]-2
    }
    
    hlw_end <- theme$highlight_window_end
    if(!any(is.na(hlw_end))){
      if(!is.list(hlw_end)) {
        hlw_end <- list(hlw_end)
      }
      xr <- sapply(hlw_end, compute_decimal_time, theme$highlight_window_freq) + 1/theme$highlight_window_freq
    } else{
      xr <- global_x$x_range[2]
    }
    
    n_start <- length(xl)
    n_end <- length(xr)
    
    if(n_start != n_end) {
      warning(sprintf("%s highlight start points than end points specified! Dropping excess ones.", ifelse(n_start > n_end, "More", "Fewer")))
    }
    
    for(i in seq_along(xl)) {
      rect(xl[i],left_y$y_range[1],xr[i],left_y$y_range[2],
           col = theme$highlight_color,
           border = NA)
    }
    
    
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
    overlap <- global_x$quarterly_tick_pos %in% global_x$yearly_tick_pos
    q_ticks <- global_x$quarterly_tick_pos[!overlap]
    q_labels <- global_x$year_labels_middle_q[!overlap]
    if(theme$label_pos == "mid"){
      axis(1, q_ticks,labels = q_labels,
           lwd.ticks = theme$lwd_quarterly_ticks,
           tcl = theme$tcl_quarterly_ticks)    
    } else{
      axis(1, q_ticks, labels = F,
           lwd.ticks = theme$lwd_quarterly_ticks,
           tcl = theme$tcl_quarterly_ticks)
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
    
    tt_r <- theme
    # Make sure we do not reuse line specs for the right axis (if left is not bars)
    if(!left_as_bar) {
      total_le <- length(tsl) + length(tsr)
      start_r <- (total_le - (length(tsr)-1)):total_le
      
      tt_r$line_colors <- tt_r$line_colors[start_r]
      tt_r$lwd <- tt_r$lwd[start_r]
      tt_r$lty <- tt_r$lty[start_r]
      tt_r$show_points <- tt_r$show_points[start_r]
      tt_r$point_symbol <- tt_r$point_symbol[start_r]
      tt_r$NA_continue_line <- tt_r$NA_continue_line[start_r]
    }
    draw_ts_lines(tsr,theme = tt_r)
    
    # RIGHT Y-Axis
    if(theme$show_right_y_axis){
      axis(4,right_y$y_ticks,las = theme$y_las)
    }
  }
  
  if(theme$use_box) box()
  
  # add legend
  if(auto_legend){
    add_legend(names(tsl), names(tsr),
               theme = theme, left_as_bar = left_as_bar)
  }
  
  # add title and subtitle
  add_title(plot_title, plot_subtitle, plot_subtitle_r, theme)
  
  # return axes and tick info, as well as theme maybe? 
  if(!quiet){
    output <- list(left_range = tsl_r,
                   right_range = tsr_r)
  } 
}


