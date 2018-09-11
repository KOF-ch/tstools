#' Plot Time Series
#' 
#' Conveniently plot time series. 
#' 
#' @param ... multiple objects of class ts or a list of time series. All objects passed through the ... parameter relate to the standard left y-axis.
#' @param tsr list of time series objects of class ts.
#' @param ci list of confidence intervals for time series
#' @param left_as_bar logical should the series that relate to the left bar be drawn as (stacked) bar charts?
#' @param group_bar_chart logical should a bar chart be grouped instead of stacked?
#' @param relative_bar_chart logical Should time series be normalized such that bars range from 0 to 1? Defaults to FALSE. That way every sub bar (time series) is related to the global max. Hence do not expect every single bar to reach 1. This works for stacked and grouped charts and does not change anything but the scale of the chart. 
#' @param left_as_band logical Should the time series assigned to the left axis be displayed as stacked area charts?
#' @param plot_title character title to be added to the plot
#' @param plot_subtitle character subtitle to be added to the plot 
#' @param plot_subtitle_r character second subtitle to be added at the top right
#' @param find_ticks_function function to compute ticks.
#' @param overall_xlim integer overall x-axis limits, defaults to NULL. 
#' @param overall_ylim integer overall y-axis limits, defaults to NULL.
#' @param manual_date_ticks character vector of manual date ticks.
#' @param manual_value_ticks_l numeric vector, forcing ticks to the left y-axis 
#' @param manual_value_ticks_r numeric vector, forcing ticks to the right y-axis 
#' @param manual_ticks_x numeric vector, forcing ticks on the x axis
#' @param theme list of default plot output parameters. Defaults to NULL, which leads to \code{\link{init_tsplot_theme}} being called. Please see the vignette for details about tweaking themes.
#' @param quiet logical suppress output, defaults to TRUE.
#' @param auto_legend logical should legends be printed automatically, defaults to TRUE.
#' @param output_format character Should the plot be drawn on screen or written to a file? Possible values are "plot" for screen output and "pdf". Default "plot"
#' @param filename character Path to the file to be written if \code{output_format} is "pdf". Default "tsplot.pdf"
#' @param close_graphics_device logical Should the graphics device of the output file be closed after \code{tsplot}? Set this to FALSE to be able to make modifications to the plot after \code{tsplot} finishes. Default TRUE
#'
#' @details 
#' The ci parameter is a 3-level list of the form
#' list(
#'  ts1 = list(
#'   ci_value_1 = list(
#'    ub = upper_bound_ts_object,
#'    lb = lower_bound_ts_object
#'   ),
#'   ...
#'  ),
#'  ...
#' )
#'
#' See \code{vignette("tstools")} for details.
#'
#' @importFrom graphics rect axis box title mtext strheight
#' @importFrom grDevices dev.off pdf
#'
#' @export
tsplot <- function(...,
                   tsr = NULL,
                   ci = NULL,
                   left_as_bar = FALSE,                    
                   group_bar_chart = FALSE,
                   relative_bar_chart = FALSE,
                   left_as_band = FALSE,
                   plot_title = NULL,
                   plot_subtitle = NULL,              
                   plot_subtitle_r = NULL,
                   find_ticks_function = "findTicks",
                   overall_xlim = NULL,
                   overall_ylim = NULL,
                   manual_date_ticks = NULL,
                   manual_value_ticks_l = NULL,
                   manual_value_ticks_r = NULL,
                   manual_ticks_x = NULL,
                   theme = NULL,
                   quiet = TRUE,
                   auto_legend = TRUE,
                   output_format = "plot",
                   filename = "tsplot",
                   close_graphics_device = TRUE){
  UseMethod("tsplot")
} 

#' @export
tsplot.ts <- function(...,
                      tsr = NULL,
                      ci = NULL,
                      left_as_bar = FALSE,                
                      group_bar_chart = FALSE,
                      relative_bar_chart = FALSE,
                      left_as_band = FALSE,
                      plot_title = NULL,
                      plot_subtitle = NULL,
                      plot_subtitle_r = NULL,
                      find_ticks_function = "findTicks",
                      overall_xlim = NULL,
                      overall_ylim = NULL,
                      manual_date_ticks = NULL,
                      manual_value_ticks_l = NULL,
                      manual_value_ticks_r = NULL,
                      manual_ticks_x = NULL,
                      theme = NULL,
                      quiet = TRUE,
                      auto_legend = TRUE,
                      output_format = "plot",
                      filename = "tsplot",
                      close_graphics_device = TRUE
){
  li <- list(...)
  tsplot(li,
         tsr = tsr,
         ci = ci,
         left_as_bar = left_as_bar,
         group_bar_chart = group_bar_chart,
         relative_bar_chart = relative_bar_chart,
         left_as_band = left_as_band,
         plot_title = plot_title,
         plot_subtitle = plot_subtitle,
         plot_subtitle_r = plot_subtitle_r,
         find_ticks_function = find_ticks_function,
         manual_date_ticks = manual_date_ticks,
         overall_xlim = overall_xlim,
         overall_ylim = overall_ylim,
         manual_value_ticks_l = manual_value_ticks_l,
         manual_value_ticks_r = manual_value_ticks_r,
         manual_ticks_x = manual_ticks_x,
         quiet = quiet,
         auto_legend = auto_legend,
         theme = theme,
         output_format = output_format,
         filename = filename,
         close_graphics_device = close_graphics_device)
}

#' @export
tsplot.mts <- function(...,
                       tsr = NULL,
                       ci = NULL,
                       left_as_bar = FALSE,
                       group_bar_chart = FALSE,
                       relative_bar_chart = FALSE,
                       left_as_band = FALSE,
                       plot_title = NULL,
                       plot_subtitle = NULL,
                       plot_subtitle_r = NULL,
                       find_ticks_function = "findTicks",
                       overall_xlim = NULL,
                       overall_ylim = NULL,
                       manual_date_ticks = NULL,
                       manual_value_ticks_l = NULL,
                       manual_value_ticks_r = NULL,
                       manual_ticks_x = NULL,
                       theme = NULL,
                       quiet = TRUE,
                       auto_legend = TRUE,
                       output_format = "plot",
                       filename = "tsplot",
                       close_graphics_device = TRUE){
  li <- list(...)
  if(length(li) > 1){
    stop("If you use multivariate time series objects (mts), make sure to pass only one object per axis. Place all time series you want to plot on one y-axis in one mts object or list of time series.")
  } else{
    data <- li[[1]]
    
    if(nrow(data) == 1) {
      warning("mts contains only a single row! This means it contains multiple time series of length 1, did you
create a ts out of a row of a data.frame? Converting to single ts.")
      data <- ts(data[1, ], start = start(data), frequency = frequency(data))
    }
    
    tsplot(as.list(data),
           tsr = tsr,
           ci = ci,
           left_as_bar = left_as_bar,
           group_bar_chart = group_bar_chart,
           relative_bar_chart = relative_bar_chart,
           left_as_band = left_as_band,
           plot_title = plot_title,
           plot_subtitle = plot_subtitle,
           plot_subtitle_r = plot_subtitle_r,
           find_ticks_function = find_ticks_function,
           overall_xlim = overall_xlim,
           overall_ylim = overall_ylim,
           manual_date_ticks = manual_date_ticks,
           manual_value_ticks_l = manual_value_ticks_l,
           manual_value_ticks_r = manual_value_ticks_r,
           manual_ticks_x = manual_ticks_x,
           quiet = quiet,
           auto_legend = auto_legend,
           theme = theme,
           output_format = output_format,
           filename = filename,
           close_graphics_device = close_graphics_device)
  }
}

#' @export
tsplot.zoo <- function(...,
                       tsr = NULL,
                       ci = NULL,
                       left_as_bar = FALSE,
                       group_bar_chart = FALSE,
                       relative_bar_chart = FALSE,
                       left_as_band = FALSE,
                       plot_title = NULL,
                       plot_subtitle = NULL,
                       plot_subtitle_r = NULL,
                       find_ticks_function = "findTicks",
                       overall_xlim = NULL,
                       overall_ylim = NULL,
                       manual_date_ticks = NULL,
                       manual_value_ticks_l = NULL,
                       manual_value_ticks_r = NULL,
                       manual_ticks_x = NULL,
                       theme = NULL,
                       quiet = TRUE,
                       auto_legend = TRUE,
                       output_format = "plot",
                       filename = "tsplot",
                       close_graphics_device = TRUE) {
  stop("zoo objets are not supported yet. Please convert your data to ts!")
}

#' @export
tsplot.xts <- function(...,
                       tsr = NULL,
                       ci = NULL,
                       left_as_bar = FALSE,
                       group_bar_chart = FALSE,
                       relative_bar_chart = FALSE,
                       left_as_band = FALSE,
                       plot_title = NULL,
                       plot_subtitle = NULL,
                       plot_subtitle_r = NULL,
                       find_ticks_function = "findTicks",
                       overall_xlim = NULL,
                       overall_ylim = NULL,
                       manual_date_ticks = NULL,
                       manual_value_ticks_l = NULL,
                       manual_value_ticks_r = NULL,
                       manual_ticks_x = NULL,
                       theme = NULL,
                       quiet = TRUE,
                       auto_legend = TRUE,
                       output_format = "plot",
                       filename = "tsplot",
                       close_graphics_device = TRUE) {
  stop("xts objects are not supported yet. Please convert your data to ts if possible!")
}

#' @export
tsplot.list <- function(...,
                        tsr = NULL,
                        ci = NULL,
                        left_as_bar = FALSE,
                        group_bar_chart = FALSE,
                        relative_bar_chart = FALSE,
                        left_as_band = FALSE,
                        plot_title = NULL,
                        plot_subtitle = NULL,
                        plot_subtitle_r = NULL,
                        find_ticks_function = "findTicks",
                        overall_xlim = NULL,
                        overall_ylim = NULL,
                        manual_date_ticks = NULL,
                        manual_value_ticks_l = NULL,
                        manual_value_ticks_r = NULL,
                        manual_ticks_x = NULL,
                        theme = NULL,
                        quiet = TRUE,
                        auto_legend = TRUE,
                        output_format = "plot",
                        filename = "tsplot",
                        close_graphics_device = TRUE
){
  
  tsl <- c(...)
  
  if(inherits(tsr, "ts")) {
    tsr <- list(tsr)
  }
  
  class_l <- sapply(tsl, "class")
  non_ts_l <- class_l != "ts"
  if(any(non_ts_l)) {
    warning(
      sprintf("Ignoring non-ts objects in list: %s\nCheck if those belong in the theme!",
              paste(names(class_l[non_ts_l]), collapse = ", ")
      )
    )
    tsl <- tsl[!non_ts_l]
  }
  
  tsl_lengths <- sapply(tsl, length)
  if(any(tsl_lengths == 1)) {
    warning("tsl contains series of length 1! Omitting those.")
    tsl <- tsl[tsl_lengths > 1]
    if(length(tsl) == 0) {
      stop("No series with length greater 1 left, stopping!")
    }
  } 
  
  if(!is.null(tsr)) {
    tsr_lengths <- sapply(tsr, length)
    if(any(tsr_lengths == 1)) {
      warning("tsr contains series of length 1! omitting those.")
      tsr <- tsr[tsr_lengths > 1]
      if(length(tsr) == 0) {
        tsr <- NULL
      }
    }
  }
  
  # Sanity check for band plots
  if(left_as_band) {
    all_signs <- sign(unlist(tsl))
    signs_consistent <- (any(all_signs < 0) & all(all_signs <= 0)) | (any(all_signs > 0) & all(all_signs >= 0))
    if(!signs_consistent) {
      warning("Found both positive and negative contributions in tsl!\nAre you sure a band plot is what you want?")
    }
  }
  
  if(any(sapply(tsl, length) == 1) || (!is.null(tsr) && any(sapply(tsr, length) == 1))) {
    stop("Time series of length 1 are not supported!")
  }
  
  if(is.null(theme)) {
    if(output_format != "plot") {
      theme <- init_tsplot_print_theme()
    } else {
      theme <- init_tsplot_theme()
    }
  }
  
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
  theme$ci_colors <- expand_param(theme, "ci_colors")
  
  # OPEN CORRECT GRAPHICS DEVICE
  
  if(output_format != "plot") {
    if(!grepl(sprintf("[.]%s$", output_format), filename)) {
      filename = sprintf("%s.%s", filename, output_format)
    }
    
    output_dim <- `if`(theme$output_wide, c(10+2/3, 6), c(8, 6))
    
    if(output_format == "pdf") {
      pdf(filename, width = output_dim[1], height = output_dim[2])
    }
    # } else if(output_format == "bmp") {
    #   bmp(filename, width = output_dim[1], height = output_dim[2], units = "in", res = theme$resolution)
    # } else if(output_format == "jpeg" || output_format == "jpg") {
    #   jpeg(filename, width = output_dim[1], height = output_dim[2], units = "in", res = theme$resolution, quality = theme$jpeg_quality)
    # } else if(output_format == "png") {
    #   png(filename, width = output_dim[1], height = output_dim[2], units = "in", res = theme$resolution)
    # } else if(output_format == "tiff") {
    #   
    # }
    
    if(close_graphics_device) {
      on.exit(dev.off())
    }
  }

  # Set pointsize and mex pars 
  par(ps = theme$pointsize,
      mex = ifelse(output_format == "plot", 1, scale_theme_param_for_print(1, dev.size())),
      lwd = ifelse(output_format == "plot", 1, scale_theme_param_for_print(1, dev.size())))
  
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
  
  margins <- theme$margins
  
  if(is.na(margins[1])) {
    if(theme$auto_bottom_margin || auto_legend) {
      
      length_l <- length(tsl)
      length_r <- length(tsr)
      
      names_l <- names(tsl)
      names_r <- names(tsr)
      
      n_ci_l <- `if`(any(names_l %in% names(ci)), sum(sapply(ci[names_l], length)), 0)
      n_ci_r <- `if`(any(names_r %in% names(ci)), sum(sapply(ci[names_r], length)), 0)
      
      n_newline_ci <- lengths(regmatches(theme$ci_legend_label, gregexpr("\n", theme$ci_legend_label)))
      
      n_newline_l <- max(lengths(regmatches(names_l, gregexpr("\n", names_l))))
      n_newline_r <- `if`(is.null(tsr), 0, max(lengths(regmatches(names_r, gregexpr("\n", names_r)))))
      
      if(n_ci_l > 0) {
        n_newline_l <- max(n_newline_ci, n_newline_l)
      }
      
      if(n_ci_r > 0) {
        n_newline_r <- max(n_newline_ci, n_newline_r)
      }
      
      n_legends_l_r <- c(
        # Add length_x*n_legend_x since the height of all legend entries is determined by the tallest one
        length_l + n_ci_l + (left_as_bar && theme$sum_as_line) + (length_l + n_ci_l)*n_newline_l, 
        length_r + n_ci_r + (length_r + n_ci_r)*n_newline_r
      )
      
      bigger_legend <- 1
      if(theme$legend_all_left) {
        n_legends <- sum(n_legends_l_r)
      } else {
        n_legends <- max(n_legends_l_r)
        bigger_legend <- which.max(n_legends_l_r)
      }
      
      n_legend_lines <- ceiling(n_legends/theme$legend_col)
      n_legend_entries <- `if`(bigger_legend == 1, length_l + n_ci_l, length_r + n_ci_r)
      
      # strheight only really considers the number of newlines in the text to be measured
      legend_height_in_in <- strheight(
        paste(rep("\n", n_legend_lines - 1), collapse = ""),
        units = "inches",
        cex = theme$legend_font_size) + 
        # space between legends
        (n_legend_entries - 1)*theme$legend_font_size*(theme$legend_intersp_y - 1)*par("cin")[2]
      
      single_line_height_in_in <- strheight("", units = "inches", cex = par("cex"))
      
      # Add the height of a single line to account for the x ticks (more or less)
      margins[1] <- 100*(single_line_height_in_in + legend_height_in_in)/dev.size()[2] + theme$legend_margin_top + theme$legend_margin_bottom
    } else {
      margins[1] <- theme$default_bottom_margin
    }
  }
  
  margins[c(1, 3)] <- margins[c(1, 3)]*dev.size()[2]/100
  margins[c(2, 4)] <- margins[c(2, 4)]*dev.size()[1]/100
  
  par(mai = margins)
  
  cnames <- names(tsl)
  # if(!is.null(tsr)) cnames <- names(tsr) 
  
  if(left_as_bar || left_as_band) {
    # Combine ts
    tsmat <- do.call("cbind", tsl)
    
    if(!is.null(dim(tsmat)) && dim(tsmat)[2] > 1) {
      # Set all NAs to 0 so range() works properly
      tsmat[is.na(tsmat)] <- 0
      ranges <- apply(tsmat, 1, function(r) {
        if(group_bar_chart && !left_as_band) {
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
    
    # Ensure 0 is part of the range when plotting bars
    tsl_r[1] <- min(tsl_r[1], 0)
    tsl_r[2] <- max(0, tsl_r[2])
  } else {
    # Determine range of tsl plus any potential confidence bands
    tsl_r <- range(as.numeric(unlist(c(tsl, ci[names(tsl)]))), na.rm = TRUE)
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
    tsr_r <- range(unlist(c(tsr, ci[names(tsr)])), na.rm = TRUE)
    
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
  global_x <- getGlobalXInfo(tsl,tsr, fill_up = theme$fill_year_with_nas, fill_up_start = theme$fill_up_start, theme$x_tick_dt, manual_ticks_x)
  
  # y can't be global in the first place, cause 
  # tsr and tsl have different scales.... 
  # time series left
  if(!is.null(manual_value_ticks_l)){
    left_ticks <- manual_value_ticks_l
    left_y <- list(y_range = range(manual_value_ticks_l),
                   y_ticks = manual_value_ticks_l)  
  } else{
    left_ticks <- do.call(find_ticks_function, list(tsl_r, theme$y_grid_count, theme$preferred_y_gap_sizes, theme$y_tick_force_integers, theme$range_must_not_cross_zero))
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
      right_ticks <- do.call(find_ticks_function, list(tsr_r, length(left_ticks), theme$preferred_y_gap_sizes, theme$y_tick_force_integers, theme$range_must_not_cross_zero))
      right_y <- list(y_range = range(right_ticks), y_ticks = right_ticks)
    }
  } else {
    # define right_ticks anyway to avoid having to check for it further down
    right_ticks <- 1
  }
  
  if(!theme$y_grid_count_strict && is.null(manual_value_ticks_l) && is.null(manual_value_ticks_r)) {
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
        right_ub <- right_ub + right_d
      }
    }
    
    left_needs_exta_tick_bottom <- tsl_r[1] < left_lb + left_d*theme$y_tick_margin
    
    if(left_needs_exta_tick_bottom) {
      left_ticks <- c(left_lb - left_d, left_ticks)
      if(!is.null(tsr)) {
        right_ticks <- c(right_lb - right_d, right_ticks)
        right_lb <- right_lb - right_d
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
    
    left_sign_ok = (
      sign(left_ticks[1]) == sign(left_y$y_ticks[1]) || sign(left_ticks[1]) == 0
    ) && (
      sign(max(left_ticks)) == sign(max(left_y$y_ticks)) || sign(max(left_ticks)) == 0
    )
    
    right_sign_ok = 
      is.null(tsr) || (
        (
          sign(right_ticks[1]) == sign(right_y$y_ticks[1]) || sign(right_ticks[1]) == 0
        ) && (
          sign(max(right_ticks)) == sign(max(right_y$y_ticks)) || sign(max(right_ticks)) == 0
        )
      )
    
    # Only touch ticks if both sides are ok
    if(!theme$range_must_not_cross_zero || (left_sign_ok && right_sign_ok)) {
      left_y <- list(y_range = range(left_ticks), y_ticks = left_ticks)
      
      if(!is.null(tsr)) {
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
  
  if(theme$show_y_grids){
    addYGrids(left_y$y_ticks, global_x$x_range, theme = theme)
  }
  # Split theme into left/right
  tt_r <- theme
  # Make sure we do not reuse line specs for the right axis (if left is not bars)
  if(!(left_as_bar || left_as_band) ) {
    total_le <- length(tsl) + length(tsr)
    start_r <- (total_le - (length(tsr)-1)):total_le
    
    tt_r$line_colors <- tt_r$line_colors[start_r]
    tt_r$lwd <- tt_r$lwd[start_r]
    tt_r$lty <- tt_r$lty[start_r]
    tt_r$show_points <- tt_r$show_points[start_r]
    tt_r$point_symbol <- tt_r$point_symbol[start_r]
    tt_r$NA_continue_line <- tt_r$NA_continue_line[start_r]
    tt_r$ci_colors <- tt_r$ci_colors[start_r]
  }
  
  # Draw all confidence bands here (so they don't overlap lines later)
  # Or should they be drawn left first, then right as before? cf especially with left_as_bar == TRUE and 
  # CI somewhere in the middle of the series. How common a case is that though?
  if(!left_as_bar) {
    par(new = TRUE)
    plot(NULL,
         xlim = global_x$x_range,
         ylim = left_y$y_range,
         axes = F,
         xlab = "",
         ylab = "",
         xaxs = theme$xaxs,
         yaxs = theme$yaxs
    )
    
    ci_left <- ci[names(ci) %in% names(tsl)]
    draw_ts_ci(ci_left, theme)
  }
  
  if(!is.null(tsr)) {
    par(new = TRUE)
    plot(NULL,
         xlim = global_x$x_range,
         ylim = right_y$y_range,
         axes = F,
         xlab = "",
         ylab = "",
         yaxs = theme$yaxs,
         xaxs = theme$xaxs
    )
    
    ci_right <- ci[names(ci) %in% names(tsr)]
    draw_ts_ci(ci_right, tt_r)
  }
  
  par(new = TRUE)
  plot(NULL,
       xlim = global_x$x_range,
       ylim = left_y$y_range,
       axes = F,
       xlab = "",
       ylab = "",
       xaxs = theme$xaxs,
       yaxs = theme$yaxs
  )
  
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
    draw_ts_lines(tsl, theme=theme, bandplot = left_as_band)
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
    
    draw_ts_lines(tsr,theme = tt_r)
    
    # RIGHT Y-Axis
    if(theme$show_right_y_axis){
      axis(4,right_y$y_ticks,las = theme$y_las,
           lwd = theme$lwd_y_axis,
           lwd.ticks = theme$lwd_y_ticks, tcl = theme$tcl_y_ticks)
    }
  }
  
  # DRAW AXES
  par(new = TRUE)
  plot(NULL,
       xlim = global_x$x_range,
       ylim = left_y$y_range,
       axes = F,
       xlab = "",
       ylab = "",
       xaxs = theme$xaxs,
       yaxs = theme$yaxs
  )
  
  # Global X-Axis ###################
  if(theme$show_x_axis) {
    if(theme$yearly_ticks){
      if(theme$label_pos == "start" || theme$x_tick_dt != 1 || !is.null(manual_ticks_x)){
        axis(1,global_x$yearly_tick_pos,labels = global_x$yearly_tick_pos,
             lwd = theme$lwd_x_axis,
             lwd.ticks = theme$lwd_yearly_ticks,
             tcl = theme$tcl_yearly_tick,
             padj = 0)    
      } else{
        axis(1,global_x$yearly_tick_pos,labels = F,
             lwd = theme$lwd_x_axis,
             lwd.ticks = theme$lwd_yearly_ticks,
             tcl = theme$tcl_yearly_tick)
      }
    }
    
    if(theme$quarterly_ticks && theme$x_tick_dt == 1 && is.null(manual_ticks_x)){
      overlap <- global_x$quarterly_tick_pos %in% global_x$yearly_tick_pos
      q_ticks <- global_x$quarterly_tick_pos[!overlap]
      q_labels <- global_x$year_labels_middle_q[!overlap]
      if(theme$label_pos == "mid"){
        axis(1, q_ticks,labels = q_labels,
             lwd = theme$lwd_x_axis,
             lwd.ticks = theme$lwd_quarterly_ticks,
             tcl = theme$tcl_quarterly_ticks,
             padj = 0)    
      } else{
        axis(1, q_ticks, labels = F,
             lwd = theme$lwd_x_axis,
             lwd.ticks = theme$lwd_quarterly_ticks,
             tcl = theme$tcl_quarterly_ticks)
      }
    }
  }
  
  # LEFT Y-AXIS
  if(theme$show_left_y_axis){
    axis(2,left_y$y_ticks,las = theme$y_las,
         lwd = theme$lwd_y_axis,
         lwd.ticks = theme$lwd_y_ticks, tcl = theme$tcl_y_ticks)
  }
  
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
    
    # RIGHT Y-Axis
    if(theme$show_right_y_axis){
      axis(4,right_y$y_ticks,las = theme$y_las,
           lwd = theme$lwd_y_axis,
           lwd.ticks = theme$lwd_y_ticks, tcl = theme$tcl_y_ticks)
    }
  }
  
  # RESET USER COORDINATES TO LEFT SIDE
  par(new = TRUE)
  plot(NULL,
       xlim = global_x$x_range,
       ylim = left_y$y_range,
       axes = F,
       xlab = "",
       ylab = "",
       xaxs = theme$xaxs,
       yaxs = theme$yaxs
  )
  
  if(theme$use_box) {
    box(lwd = theme$lwd_box)
  }
  
  # add legend
  if(auto_legend){
    ci_names <- lapply(names(ci), function(x) {
      y <- gsub("%series%", x, theme$ci_legend_label)
      if(grepl("%ci_value%", y)) {
        parts <- strsplit(y, "%ci_value%")[[1]]
        # in case %ci_value% is at the very end (see ?split)
        if(length(parts) == 1) {
          parts <- c(parts, "")
        }
        y <- paste0(parts[1], names(ci[[x]]), parts[2])
      } else {
        y <- rep(y, length(ci[[x]]))
      }
      y
    })
    names(ci_names) <- names(ci)
    
    add_legend(names(tsl), names(tsr), ci_names,
               theme = theme, left_as_bar = left_as_bar,
               left_as_band = left_as_band)
  }
  
  # add title and subtitle
  add_title(plot_title, plot_subtitle, plot_subtitle_r, theme)
  
  # return axes and tick info, as well as theme maybe? 
  if(!quiet){
    output <- list(left_range = tsl_r,
                   right_range = tsr_r)
  } 
}


