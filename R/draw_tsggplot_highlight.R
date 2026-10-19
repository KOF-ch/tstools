#' @importFrom ggplot2 aes geom_rect .data
draw_tsggplot_highlight <- function(p, global_x, left_y, theme) {
  x_range <- global_x$x_range
  is_posix <- inherits(x_range, "POSIXct")
  is_date <- inherits(x_range, "Date")
  is_date_scale <- is_posix || is_date

  # Convert a user-supplied highlight_window_start/end value to the same
  # x-axis unit as x_range (Date for daily/weekly, POSIXct for hourly);
  # accepts anything as.Date()/as.POSIXct() does, e.g. a "YYYY-MM-DD" string.
  to_axis_unit <- function(v) {
    if (is_posix) {
      return(as.POSIXct(v, tz = attr(x_range, "tzone")))
    }
    as.Date(v)
  }

  hlw_start <- theme$highlight_window_start
  if (!any(is.na(hlw_start))) {
    if (is_date_scale) {
      xl <- to_axis_unit(hlw_start)
    } else {
      if (!is.list(hlw_start)) {
        hlw_start <- list(hlw_start)
      }
      xl <- sapply(hlw_start, compute_decimal_time, theme$highlight_window_freq)
    }
  } else {
    # Default: highlight the last ~2 years of the plotted range, converted
    # to whichever unit x_range is actually in.
    default_pad <- if (is_posix) {
      2 * 365.25 * 86400
    } else if (is_date) {
      round(2 * 365.25)
    } else {
      2
    }
    xl <- x_range[2] - default_pad
  }

  hlw_end <- theme$highlight_window_end
  if (!any(is.na(hlw_end))) {
    if (is_date_scale) {
      # A Date/POSIXct value is already a precise instant -- unlike a
      # ts-style c(year, period) pair, there's no following period
      # boundary to round up to.
      xr <- to_axis_unit(hlw_end)
    } else {
      if (!is.list(hlw_end)) {
        hlw_end <- list(hlw_end)
      }
      xr <- sapply(hlw_end, compute_decimal_time, theme$highlight_window_freq) + 1 / theme$highlight_window_freq
    }
    # highlight window can maximally extend to the end of the x-axis
    xr[xr > x_range[2]] <- x_range[2]
  } else {
    xr <- x_range[2]
  }

  n_start <- length(xl)
  n_end <- length(xr)

  if (n_start != n_end) {
    warning(sprintf("%s highlight start points than end points specified! Dropping excess ones.", ifelse(n_start > n_end, "More", "Fewer")))
  }

  rect_df <- data.frame(
    xmin = xl,
    xmax = xr,
    ymin = left_y$y_range[1],
    ymax = left_y$y_range[2]
  )
  # marks the layer for tsggplotly()
  attr(rect_df, "tsggplot_highlight") <- TRUE

  p <- p + geom_rect(
    data = rect_df,
    aes(
      xmin = .data$xmin,
      xmax = .data$xmax,
      ymin = .data$ymin,
      ymax = .data$ymax
    ),
    fill = theme$highlight_window_color,
    alpha = theme$highlight_window_alpha,
    color = NA,
    inherit.aes = FALSE,
    show.legend = FALSE
  )
}
