#' @importFrom stats ts.union
draw_ts_lines <- function(x, theme = NULL, bandplot = FALSE) {
  nts <- length(x)
  op <- rep(theme$show_points, ceiling(nts / length(theme$show_points)))
  ops <- rep(theme$point_symbol, ceiling(nts / length(theme$point_symbol)))

  # "harmonize" all ts, range wise
  if (bandplot) {
    x_mat <- do.call(ts.union, x)
    x_mat[is.na(x_mat)] <- 0
    x <- as.list(x_mat)
  }

  band_low <- rep(0, length(x[[1]]))

  for (i in 1:nts) {
    xx <- as.numeric(time(x[[i]]))
    yy <- x[[i]]
    frq <- frequency(x[[i]])

    if (theme$line_to_middle) {
      xx <- xx + (1 / frq) / 2
    }

    if (theme$NA_continue_line[i]) {
      yy_na <- is.na(yy)
      xx <- xx[!yy_na]
      yy <- yy[!yy_na]
    }

    if (!bandplot) {
      lines(xx, yy,
        col = theme$line_colors[i],
        lwd = theme$lwd[i],
        lty = theme$lty[i],
        type = ifelse(theme$show_points[i], "o", "l"),
        pch = theme$point_symbol[i]
      )
    } else {
      band_high <- band_low + yy
      polygon(c(xx, rev(xx)), c(band_low, rev(band_high)), border = NA, col = theme$band_fill_color[i])
      band_low <- band_high
    }
  }
}


#' @importFrom ggplot2 aes geom_line geom_ribbon geom_point .data
#' @importFrom stats setNames time frequency
draw_tsggplot_lines <- function(p, x, theme, bandplot = FALSE, true_values = NULL, use_date_scale = FALSE) {
  nts <- length(x)
  series <- names(x)

  # "harmonize" all ts, range wise
  if (bandplot) {
    x_mat <- do.call(merge, x)
    x_mat[is.na(x_mat)] <- 0
    x_names <- names(x)
    x <- as.list(x_mat)
    x <- setNames(x, x_names)
  }

  band_low <- rep(0, length(x[[1]]))

  for (i in 1:nts) {
    xx <- getNumericTimeIndex(x[[i]], use_date_scale = use_date_scale)
    yy <- as.numeric(x[[i]])

    # Right-axis series are rescaled into the left axis's numeric range for
    # plotting (see the sec_axis() trick in tsggplot()), so the true value
    # for hover text has to come from the pre-rescale series instead of
    # from yy, which by this point holds the rescaled number.
    hover_yy <- if (!is.null(true_values)) as.numeric(true_values[[i]]) else yy

    if (theme$line_to_middle) xx <- xx + getLineToMiddleShift(x[[i]], use_date_scale = use_date_scale)

    if (theme$NA_continue_line[i]) {
      yy_na <- is.na(yy)
      xx <- xx[!yy_na]
      yy <- yy[!yy_na]
      hover_yy <- hover_yy[!yy_na]
    }

    df <- data.frame(
      time = xx,
      value = yy,
      line_colors = rep(theme$line_colors[i], each = length(xx)),
      series = factor(series[i], levels = series)
    )

    if (!bandplot) {
      # Custom hover text, kept as a plain data column rather than an aes()
      # mapping -- tsggplotly() picks it up from the layer data directly
      # (see text_by_series there) and injects it into the plotly trace,
      # instead of mapping it here where it would trigger a ggplot2
      # "Ignoring unknown aesthetics: text" warning on every ordinary
      # (non-plotly) tsggplot() call.
      df$text <- paste("value:", hover_yy)
      p <- p + geom_line(
        data = df,
        aes(
          x = .data$time,
          y = .data$value,
          color = .data$series
        ),
        linewidth = theme$linewidth[i],
        linetype = theme$linetype[i]
      )

      # Optionally add points
      if (theme$show_points[i]) {
        p <- p + geom_point(
          data = df,
          aes(
            x = .data$time,
            y = .data$value
          ),
          shape = theme$point_symbol[i]
        )
      }
    } else {
      band_high <- band_low + yy
      df_band <- data.frame(
        time = xx,
        ymin = band_low,
        ymax = band_high,
        series = factor(series[i], levels = series)
      )

      p <- p + geom_ribbon(
        data = df_band,
        aes(
          x = .data$time,
          fill = .data$series,
          ymin = .data$ymin,
          ymax = .data$ymax
        )
      )
      band_low <- band_high # Update band_low for cumulative stacking
    }
  }
  p
}

#' @importFrom graphics lines
draw_sum_as_line <- function(x, theme = NULL) {
  xx <- as.numeric(time(x))
  yy <- x
  frq <- frequency(x)
  if (theme$line_to_middle) xx <- xx + (1 / frq) / 2
  lines(xx, yy,
    col = theme$sum_line_color,
    lwd = theme$sum_line_lwd,
    lty = theme$sum_line_lty
  )
}

#' @importFrom ggplot2 aes geom_line .data
draw_sum_as_ggline <- function(p, x, theme = NULL, use_date_scale = FALSE) {
  df <- data.frame(
    xx = getNumericTimeIndex(x, use_date_scale = use_date_scale),
    yy = as.numeric(x)
  )
  if (theme$line_to_middle) df$xx <- df$xx + getLineToMiddleShift(x, use_date_scale = use_date_scale)

  if (is.null(theme$sum_legend)) {
    # no label -> constant colour, no legend entry
    return(p + geom_line(
      data = df,
      aes(
        x = .data$xx,
        y = .data$yy
      ),
      color = theme$sum_line_color,
      linewidth = theme$sum_line_linewidth,
      linetype = theme$sum_line_linetype
    ))
  }

  # mapped colour gives the line a legend entry; the value is set by the
  # caller's scale_color_manual() in tsggplot.list()
  df$series <- theme$sum_legend
  p + geom_line(
    data = df,
    aes(
      x = .data$xx,
      y = .data$yy,
      color = .data$series
    ),
    linewidth = theme$sum_line_linewidth,
    linetype = theme$sum_line_linetype
  )
}
