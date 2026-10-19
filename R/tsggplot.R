#' Plot Time Series
#'
#' Conveniently plot time series.
#'
#' @param ... multiple objects of class ts or a list of time series. All objects
#'        passed through the ... parameter relate to the standard left y-axis.
#' @param tsr list of time series objects of class ts.
#' @param ci list of confidence intervals for time series
#' @param left_as_bar logical should the series that relate to the left bar be
#'        drawn as (stacked) bar charts?
#' @param group_bar_chart logical should a bar chart be grouped instead of
#'        stacked?
#' @param relative_bar_chart logical Should time series be normalized such that
#'        bars range from 0 to 1? Defaults to FALSE. That way every sub bar
#'        (time series) is related to the global max. Hence do not expect every
#'        single bar to reach 1. This works for stacked and grouped charts and
#'        does not change anything but the scale of the chart.
#' @param left_as_band logical Should the time series assigned to the left axis
#'        be displayed as stacked area charts?
#' @param labs A named list containing plot text elements. Valid elements are:
#'        \code{title} for the main title, \code{subtitle} for the subtitle
#'        below the title, \code{caption} for the text in the bottom-right
#'        corner, \code{tag} for the label at the top-left of the plot,
#'        \code{alt} and \code{alt_insight} for alt-text generation (see
#'        \code{\link[ggplot2]{get_alt_text}} for examples). You may also
#'        provide additional name-value pairs corresponding to aesthetics. See
#'        \code{\link[ggplot2]{labs}} for further details.
#'        Use \code{y_right} to set the label of the right-side y-axis
#'        (secondary axis), if present.
#' @param find_ticks_function function to compute ticks.
#' @param overall_xlim integer overall x-axis limits, defaults to NULL.
#' @param overall_ylim integer overall y-axis limits, defaults to NULL.
#' @param manual_date_ticks character vector of manual date ticks.
#' @param manual_value_ticks_l numeric vector, forcing ticks to the left y-axis
#' @param manual_value_ticks_r numeric vector, forcing ticks to the right y-axis
#' @param manual_ticks_x numeric vector, forcing ticks on the x axis
#' @param theme list of default plot output parameters. Defaults to NULL, which
#'        leads to \code{\link{init_tsggplot_theme}} being called. Please see
#'        the vignette for details about tweaking themes.
#' @param quiet logical should only the plot be returned (default TRUE)? If
#'        FALSE, returns a list with the plot (\code{plot}) and the axis
#'        metadata used to build it (\code{global_x}, \code{left_y},
#'        \code{right_y}, \code{y_right_label}).
#' @param auto_legend logical should legends be printed automatically, defaults
#'        to TRUE.
#' @param output_format character Should the plot be drawn on screen or written
#'        to a file? Possible values are "plot" for screen output and "pdf".
#'        Default "plot"
#' @param save list containing filename (default tsplot) used when output_format
#'        is not "plot". See [ggplot2::ggsave]] for list of arguments.
#'
#' @importFrom ggplot2 aes coord_cartesian element_blank element_line element_text geom_rect geom_segment ggplot
#' @importFrom ggplot2 ggplot_build ggsave guides guide_axis guide_legend margin sec_axis scale_color_manual
#' @importFrom ggplot2 scale_fill_manual scale_x_continuous scale_y_continuous waiver .data
#' @importFrom ggplot2 scale_x_date scale_x_datetime
#'
#' @seealso [ggplot2::labs()] for information on labels (title, subtitle,
#'          caption, tag)
#'
#'
#' @author Merlin Scherer
#' @export
tsggplot <- function(...,
                     tsr = NULL,
                     ci = NULL,
                     left_as_bar = FALSE,
                     group_bar_chart = FALSE,
                     relative_bar_chart = FALSE,
                     left_as_band = FALSE,
                     labs = NULL,
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
                     save = list(
                       filename = "tsplot",
                       height = 210,
                       width = 297,
                       units = "mm"
                     )) {
  UseMethod("tsggplot")
}

#' @export
tsggplot.ts <- function(...,
                        tsr = NULL,
                        ci = NULL,
                        left_as_bar = FALSE,
                        group_bar_chart = FALSE,
                        relative_bar_chart = FALSE,
                        left_as_band = FALSE,
                        labs = NULL,
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
                        save = list(
                          filename = "tsplot",
                          height = 210,
                          width = 297,
                          units = "mm"
                        )) {
  li <- list(...)
  tsggplot(li,
    tsr = tsr,
    ci = ci,
    left_as_bar = left_as_bar,
    group_bar_chart = group_bar_chart,
    relative_bar_chart = relative_bar_chart,
    left_as_band = left_as_band,
    labs = labs,
    find_ticks_function = find_ticks_function,
    manual_date_ticks = manual_date_ticks,
    overall_xlim = overall_xlim,
    overall_ylim = overall_ylim,
    manual_value_ticks_l = manual_value_ticks_l,
    manual_value_ticks_r = manual_value_ticks_r,
    manual_ticks_x = manual_ticks_x,
    auto_legend = auto_legend,
    theme = theme,
    quiet = quiet,
    output_format = output_format,
    save = save
  )
}

#' @export
tsggplot.mts <- function(...,
                         tsr = NULL,
                         ci = NULL,
                         left_as_bar = FALSE,
                         group_bar_chart = FALSE,
                         relative_bar_chart = FALSE,
                         left_as_band = FALSE,
                         labs = NULL,
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
                         save = list(
                           filename = "tsplot",
                           height = 210,
                           width = 297,
                           units = "mm"
                         )) {
  li <- list(...)
  if (length(li) > 1) {
    stop("If you use multivariate time series objects (mts), make sure to pass only one object per axis. Place all time series you want to plot on one y-axis in one mts object or list of time series.")
  } else {
    data <- li[[1]]

    if (nrow(data) == 1) {
      warning("mts contains only a single row! This means it contains multiple time series of length 1, did you
create a ts out of a row of a data.frame? Converting to single ts.")
      data <- ts(data[1, ], start = start(data), frequency = frequency(data))
    }

    tsggplot(as.list(data),
      tsr = tsr,
      ci = ci,
      left_as_bar = left_as_bar,
      group_bar_chart = group_bar_chart,
      relative_bar_chart = relative_bar_chart,
      left_as_band = left_as_band,
      labs = labs,
      find_ticks_function = find_ticks_function,
      overall_xlim = overall_xlim,
      overall_ylim = overall_ylim,
      manual_date_ticks = manual_date_ticks,
      manual_value_ticks_l = manual_value_ticks_l,
      manual_value_ticks_r = manual_value_ticks_r,
      manual_ticks_x = manual_ticks_x,
      auto_legend = auto_legend,
      theme = theme,
      quiet = quiet,
      output_format = output_format,
      save = save
    )
  }
}

#' @export
tsggplot.xts <- function(...,
                         tsr = NULL,
                         ci = NULL,
                         left_as_bar = FALSE,
                         group_bar_chart = FALSE,
                         relative_bar_chart = FALSE,
                         left_as_band = FALSE,
                         labs = NULL,
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
                         save = list(
                           filename = "tsplot",
                           height = 210,
                           width = 297,
                           units = "mm"
                         )) {
  li <- list(...)
  tsggplot(li,
    tsr = tsr,
    ci = ci,
    left_as_bar = left_as_bar,
    group_bar_chart = group_bar_chart,
    relative_bar_chart = relative_bar_chart,
    left_as_band = left_as_band,
    labs = labs,
    find_ticks_function = find_ticks_function,
    manual_date_ticks = manual_date_ticks,
    overall_xlim = overall_xlim,
    overall_ylim = overall_ylim,
    manual_value_ticks_l = manual_value_ticks_l,
    manual_value_ticks_r = manual_value_ticks_r,
    manual_ticks_x = manual_ticks_x,
    auto_legend = auto_legend,
    theme = theme,
    quiet = quiet,
    output_format = output_format,
    save = save
  )
}

#' @export
tsggplot.zoo <- function(...,
                         tsr = NULL,
                         ci = NULL,
                         left_as_bar = FALSE,
                         group_bar_chart = FALSE,
                         relative_bar_chart = FALSE,
                         left_as_band = FALSE,
                         labs = NULL,
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
                         save = list(
                           filename = "tsplot",
                           height = 210,
                           width = 297,
                           units = "mm"
                         )) {
  li <- list(...)
  li <- lapply(li, xts::as.xts)
  tsggplot(li,
    tsr = tsr,
    ci = ci,
    left_as_bar = left_as_bar,
    group_bar_chart = group_bar_chart,
    relative_bar_chart = relative_bar_chart,
    left_as_band = left_as_band,
    labs = labs,
    find_ticks_function = find_ticks_function,
    manual_date_ticks = manual_date_ticks,
    overall_xlim = overall_xlim,
    overall_ylim = overall_ylim,
    manual_value_ticks_l = manual_value_ticks_l,
    manual_value_ticks_r = manual_value_ticks_r,
    manual_ticks_x = manual_ticks_x,
    auto_legend = auto_legend,
    theme = theme,
    output_format = output_format,
    save = save
  )
}

#' @export
tsggplot.list <- function(...,
                          tsr = NULL,
                          ci = NULL,
                          left_as_bar = FALSE,
                          group_bar_chart = FALSE,
                          relative_bar_chart = FALSE,
                          left_as_band = FALSE,
                          labs = NULL,
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
                          save = list(
                            filename = "tsplot",
                            height = 210,
                            width = 297,
                            units = "mm"
                          )) {
  tsl <- c(...)
  tsl <- lapply(tsl, xts::as.xts)

  if (inherits(tsr, "ts")) {
    tsr <- list(tsr)
  }

  class_l <- lapply(tsl, class)
  non_xts_l <- !vapply(class_l, function(cl) "xts" %in% cl, logical(1))
  if (any(non_xts_l)) {
    warning(sprintf(
      "Ignoring non-xts objects in list: %s\nCheck if those belong in the theme!",
      paste(names(tsl)[non_xts_l], collapse = ", ")
    ))
    tsl <- tsl[!non_xts_l]
  }


  tsl_lengths <- sapply(tsl, length)
  if (any(tsl_lengths == 1) && !left_as_bar) {
    warning("tsl contains series of length 1! Omitting those.")
    tsl <- tsl[tsl_lengths > 1]
    if (length(tsl) == 0) {
      stop("No series with length greater 1 left, stopping!")
    }
  }

  if (!is.null(tsr)) {
    tsr_lengths <- sapply(tsr, length)
    if (any(tsr_lengths == 1)) {
      warning("tsr contains series of length 1! omitting those.")
      tsr <- tsr[tsr_lengths > 1]
      if (length(tsr) == 0) {
        tsr <- NULL
      }
    }

    tsr <- lapply(tsr, xts::as.xts)
  }

  if (!is.null(ci)) {
    ci <- lapply(ci, function(x) {
      lapply(x, function(y) {
        lapply(y, xts::as.xts)
      })
    })
  }


  # Sanity check for band plots
  if (left_as_band) {
    all_signs <- sign(unlist(tsl))
    signs_consistent <- (any(all_signs < 0) & all(all_signs <= 0)) | (any(all_signs > 0) & all(all_signs >= 0))
    if (!signs_consistent) {
      warning("Found both positive and negative contributions in tsl!\nAre you sure a band plot is what you want?")
    }
  }

  if (is.null(theme)) {
    if (output_format != "plot") {
      # theme <- init_tsplot_print_theme()
      theme <- init_tsggplot_theme()
    } else {
      theme <- init_tsggplot_theme()
    }
  } else {
    if (!inherits(theme, "tsggplot_theme")) {
      stop("Invalid theme: please pass a theme created via init_tsggplot_theme.")
    }
  }

  # Expand per-line parameters for recycling
  total_n_ts <- length(tsl) + length(tsr)
  expand_param <- function(theme, param_name) {
    rep(theme[[param_name]], ceiling(total_n_ts / length(theme[[param_name]])))
  }

  theme$line_colors <- expand_param(theme, "line_colors")
  theme$linewidth <- expand_param(theme, "linewidth")
  theme$linetype <- expand_param(theme, "linetype")
  theme$show_points <- expand_param(theme, "show_points")
  theme$point_symbol <- expand_param(theme, "point_symbol")
  theme$NA_continue_line <- expand_param(theme, "NA_continue_line")
  theme$ci_colors <- expand_param(theme, "ci_colors")

  if (left_as_bar && relative_bar_chart) {
    # Normalize ts
    if (group_bar_chart) {
      m <- Reduce("max", tsl)
    } else {
      sums <- Reduce("+", tsl)
      m <- max(sums)
    }
    tsl <- lapply(tsl, "/", m)
  }

  # Set default names for legend if none provided
  right_name_start <- 0
  if (is.null(names(tsl))) {
    names(tsl) <- paste0("series_", seq_along(tsl))
    right_name_start <- length(tsl)
  }
  if (is.null(names(tsr)) && !is.null(tsr)) {
    if (is.list(tsr)) {
      names(tsr) <- paste0("series_", seq_along(tsr) + right_name_start)
    } else {
      tsr <- list(tsr)
      names(tsr) <- paste0("series_", right_name_start + 1)
    }
  }

  if (left_as_bar || left_as_band) {
    # Combine ts
    tsmat <- do.call("cbind", tsl)

    if (!is.null(dim(tsmat)) && dim(tsmat)[2] > 1) {
      # Set all NAs to 0 so range() works properly
      tsmat[is.na(tsmat)] <- 0
      ranges <- apply(tsmat, 1, function(r) {
        if (group_bar_chart && !left_as_band) {
          range(r)
        } else {
          range(c(sum(r[r < 0]), sum(r[r >= 0])))
        }
      })
      tsl_r <- c(min(ranges[1, ]), max(ranges[2, ]))
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


  if (!is.null(tsr)) {
    tsr <- sanitizeTsr(tsr)
    tsr_r <- range(unlist(c(tsr, ci[names(tsr)])), na.rm = TRUE)

    if (!is.null(theme$y_range_min_size)) {
      tsr_r_size <- diff(tsr_r)
      if (tsr_r_size < theme$y_range_min_size) {
        tsr_r_mid <- 0.5 * tsr_r_size + tsr_r[1]
        half_min_range_size <- 0.5 * theme$y_range_min_size
        tsr_r <- c(tsr_r_mid - half_min_range_size, tsr_r_mid + half_min_range_size)
      }
    } else if (tsr_r[1] == tsr_r[2]) {
      level <- tsr_r[1]
      offset <- level %% 10
      tsr_r <- c(level - 10 - offset, level + 10 - offset)
    }
  }

  global_x <- getGlobalXInfo_tsggplot(
    tsl, tsr,
    fill_up = theme$fill_year_with_nas,
    fill_up_start = theme$fill_up_start,
    tick_dt = theme$axis_x_tick_dt,
    label_dt = theme$axis_x_label_dt,
    manual_ticks_x,
    pad = theme$axis_x_pad
  )

  # daily/weekly xts data is plotted on a Date-based x-axis (scale_x_date),
  # hourly on a POSIXct-based one (scale_x_datetime); everything else (ts,
  # monthly/quarterly/annual xts) is plotted on a numeric decimal-year
  # x-axis (scale_x_continuous) -- draw functions need to know which, so
  # their data lines up with the axis built below.
  use_date_scale <- global_x$dominant_freq %in% c("daily", "weekly", "hourly")

  # y can't be global in the first place, cause
  # tsr and tsl have different scales....
  # time series left
  if (!is.null(manual_value_ticks_l)) {
    left_ticks <- manual_value_ticks_l
    left_y <- list(
      y_range = range(manual_value_ticks_l),
      y_ticks = manual_value_ticks_l
    )
  } else {
    left_ticks <- do.call(find_ticks_function, list(tsl_r, theme$grids_x_count, theme$preferred_y_gap_sizes, theme$y_tick_force_integers, theme$range_must_not_cross_zero))
    left_y <- list(y_range = range(left_ticks), y_ticks = left_ticks)
  }
  # time series right
  if (!is.null(tsr)) {
    if (!is.null(manual_value_ticks_r)) {
      if (length(manual_value_ticks_r) != length(left_y$y_ticks)) {
        return("When using to manual tick position vectors, both need to be of same length! (Otherwise grids look ugly)")
      }
      right_ticks <- manual_value_ticks_r
      right_y <- list(
        y_range = range(manual_value_ticks_r),
        y_ticks = manual_value_ticks_r
      )
    } else {
      right_ticks <- do.call(find_ticks_function, list(tsr_r, length(left_ticks), theme$preferred_y_gap_sizes, theme$y_tick_force_integers, theme$range_must_not_cross_zero))
      right_y <- list(y_range = range(right_ticks), y_ticks = right_ticks)
    }
  } else {
    # define right_ticks anyway to avoid having to check for it further down
    right_ticks <- 1
  }

  if (!theme$grids_x_count_strict && is.null(manual_value_ticks_l) && is.null(manual_value_ticks_r)) {
    left_diff <- diff(left_ticks)
    left_d <- left_diff[1]
    left_ub <- left_ticks[length(left_ticks)]
    left_lb <- left_ticks[1]

    if (!is.null(tsr)) {
      right_diff <- diff(right_ticks)
      right_d <- right_diff[1]
      right_ub <- right_ticks[length(left_ticks)]
      right_lb <- right_ticks[1]
    }

    left_needs_extra_tick_top <- tsl_r[2] > left_ub - left_d * theme$y_tick_margin

    if (left_needs_extra_tick_top) {
      left_ticks <- c(left_ticks, left_ub + left_d)
      if (!is.null(tsr)) {
        right_ticks <- c(right_ticks, right_ub + right_d)
        right_ub <- right_ub + right_d
      }
    }

    left_needs_exta_tick_bottom <- tsl_r[1] < left_lb + left_d * theme$y_tick_margin

    if (left_needs_exta_tick_bottom) {
      left_ticks <- c(left_lb - left_d, left_ticks)
      if (!is.null(tsr)) {
        right_ticks <- c(right_lb - right_d, right_ticks)
        right_lb <- right_lb - right_d
      }
    }

    if (!is.null(tsr)) {
      right_needs_extra_tick_top <- tsr_r[2] > right_ub - right_d * theme$y_tick_margin

      if (right_needs_extra_tick_top) {
        left_ticks <- c(left_ticks, left_ub + left_d)
        right_ticks <- c(right_ticks, right_ub + right_d)
      }

      right_needs_extra_tick_bottom <- tsr_r[1] < right_lb + right_d * theme$y_tick_margin

      if (right_needs_extra_tick_bottom) {
        left_ticks <- c(left_lb - left_d, left_ticks)
        right_ticks <- c(right_lb - right_d, right_ticks)
      }
    }

    # Technically we could save ourselves all that correcting if manual ticks are not null.
    # This is just a convenient place to check.

    left_sign_ok <- (
      sign(left_ticks[1]) == sign(left_y$y_ticks[1]) || sign(left_ticks[1]) == 0
    ) && (
      sign(max(left_ticks)) == sign(max(left_y$y_ticks)) || sign(max(left_ticks)) == 0
    )

    right_sign_ok <-
      is.null(tsr) || (
        (
          sign(right_ticks[1]) == sign(right_y$y_ticks[1]) || sign(right_ticks[1]) == 0
        ) && (
          sign(max(right_ticks)) == sign(max(right_y$y_ticks)) || sign(max(right_ticks)) == 0
        )
      )

    # Only touch ticks if both sides are ok
    if (!theme$range_must_not_cross_zero || (left_sign_ok && right_sign_ok)) {
      left_y <- list(y_range = range(left_ticks), y_ticks = left_ticks)

      if (!is.null(tsr)) {
        right_y <- list(y_range = range(right_ticks), y_ticks = right_ticks)
      }
    }
  }

  # Set below (Global X-Axis section) if a quarterly-tick decoration layer
  # (geom_segment(), not a real ggplot2 axis element) gets added -- kept
  # around so tsggplotly() can find and strip the resulting trace, which
  # ggplotly() has no way to tell apart from real plotted data on its own.
  quarterly_tick_marks <- NULL

  # Extract valid theme elements from the provided theme listents]
  # by matching their names with the formal arguments of ggplot2::theme
  valid_theme_elements <- names(formals(ggplot2::theme))
  # Filter the theme list to include only valid theme elements
  theme_args <- theme[names(theme) %in% valid_theme_elements]

  if (is.null(labs$x)) {
    theme_args$axis.title.x <- element_blank()
  }
  # [[ ]]: labs$y would partial-match y_right
  if (is.null(labs[["y"]])) {
    theme_args$axis.title.y <- element_blank()
  }
  if (!is.null(labs$y_right)) {
    theme_args$axis.title.y.right <- element_text()
  }
  if (is.null(labs$color)) {
    theme_args$legend.title <- element_blank()
  }
  if (!auto_legend) {
    theme_args$legend.position <- "none"
  }

  if (!is.null(tsr) && !isTRUE(theme$legend_all_left)) {
    # Left- and right-axis series get their own, separately grouped guide-box
    # below the plot. ggplot2 places guide-boxes right next to each other by
    # default, which reads as one continuous legend -- add spacing so the two
    # groups are visually distinguishable, matching tsplot's topleft/topright
    # placement.
    theme_args$legend.spacing.x <- unit(2, "cm")
  }

  if (!inherits(theme$axis.line.y, "element_blank")) {
    # If the y-axis line theme is not identical to the default ggplot2
    # element_line
    if (!identical(theme$axis.line.y, element_line())) {
      # Assign the y-axis line theme to both left and right y-axis line
      # arguments
      theme_args$axis.line.y.left <- theme$axis.line.y
      theme_args$axis.line.y.right <- theme$axis.line.y
    } else {
      theme_args$axis.line.y.left <- theme$axis.line.y.left
      theme_args$axis.line.y.right <- theme$axis.line.y.right
    }
  } else {
    element_blank()
  }

  if (!inherits(theme$axis.text, "element_blank")) {
    if (identical(theme$axis.text, element_text())) {
      theme_args$axis.text.x <- theme$axis.text.x
      theme_args$axis.text.y.left <- theme$axis.text.y.left
      theme_args$axis.text.y.right <- theme$axis.text.y.right
    }

    if (theme$axis.text.x.pos == "mid") {
      # To position the labels between major tick marks, we repurpose minor
      # ticks as the actual tick marks, and use major
      # breaks only for positioning labels (hiding their tick lines).
      # segments are used to draw the minor ticks
      theme_args$axis.minor.ticks.length <- theme$axis.ticks.length
      theme_args$axis.minor.ticks.x.bottom <- theme$axis.ticks.x.bottom
      theme_args$axis.ticks.length <- unit(0, "cm") # hide major ticks as they are used only for labels
      theme_args$axis.minor.ticks.length <- -theme$axis.ticks.length # draw minor ticks inward
      segment_length <- as.numeric(theme$axis.minor.ticks.length)
      segment_x_bottom <- theme$axis.minor.ticks.x.bottom
      if (is.null(theme$axis.text.x$margin)) {
        theme_args$axis.text.x <- theme$axis.text.x
        theme_args$axis.text.x$margin <- margin(t = 10, unit = "pt")
      }
    }
  } else {
    # Explicitly set all axis text elements to blank
    theme_args$axis.text.x <- element_blank()
    theme_args$axis.text.y.left <- element_blank()
    theme_args$axis.text.y.right <- element_blank()
  }

  p <- ggplot() +
    do.call(ggplot2::theme, theme_args)

  if (theme$highlight_window) {
    p <- draw_tsggplot_highlight(p, global_x, left_y, theme)
  }

  # Split theme into left/right
  tt_r <- theme
  # Make sure we do not reuse line specs for the right axis (if left is not bars)
  if (!(left_as_bar || left_as_band)) {
    total_le <- length(tsl) + length(tsr)
    start_r <- (total_le - (length(tsr) - 1)):total_le

    tt_r$line_colors <- tt_r$line_colors[start_r]
    tt_r$linewidth <- tt_r$linewidth[start_r]
    tt_r$linetype <- tt_r$linetype[start_r]
    tt_r$show_points <- tt_r$show_points[start_r]
    tt_r$point_symbol <- tt_r$point_symbol[start_r]
    tt_r$NA_continue_line <- tt_r$NA_continue_line[start_r]
    tt_r$ci_colors <- tt_r$ci_colors[start_r]
  }

  # Whether the left- and right-axis series get two separately grouped
  # legends below the plot (one per axis) instead of one legend merged from
  # both. Only relevant when both axes are plain lines and thus share a
  # single "colour" aesthetic -- left_as_bar/left_as_band already put the
  # left series on a separate "fill" aesthetic, so those get a split legend
  # "for free" further down without needing this. Static (print/save) output
  # only: tsggplotly() still renders one merged legend for tsr charts.
  # The sum line of a bar chart is a colour-aesthetic entry too, so with a
  # tsr it needs the same treatment to stay in the left-axis group.
  sum_own_scale <- left_as_bar && isTRUE(theme$sum_as_line) && !is.null(theme$sum_legend) &&
    !is.null(tsr) && !isTRUE(theme$legend_all_left)
  split_legend <- sum_own_scale ||
    (!left_as_bar && !left_as_band && !is.null(tsr) && !isTRUE(theme$legend_all_left))

  # tsggplotly() can't convert the split legend (ggnewscale's scale-renaming
  # trick breaks plotly::ggplotly()'s geom conversion), so build the merged-
  # legend equivalent here too and stash it for tsggplotly() to use
  # transparently instead of erroring. Recursing with legend_all_left = TRUE
  # makes the inner call's own split_legend FALSE, so this doesn't recurse
  # further.
  merged_legend_fallback <- NULL
  if (split_legend) {
    fallback_args <- as.list(environment())
    fallback_args <- fallback_args[intersect(names(formals(tsggplot.list)), names(fallback_args))]
    fallback_args$theme <- theme
    fallback_args$theme$legend_all_left <- TRUE
    fallback_args$quiet <- TRUE
    fallback_args$output_format <- "plot"
    merged_legend_fallback <- do.call(tsggplot.list, c(list(tsl), fallback_args))
  }

  # Each CI's shaded band is its own fill scale (scale_fill_manual(), keyed
  # by CI group id) -- ggplot2 only allows one scale per aesthetic, so
  # without ggnewscale a second fill consumer (the other axis' CI, or the
  # band/bar fill scale added further down) would silently replace it,
  # leaving the replaced band's fill unmapped/invisible (#14).
  # new_scale_fill() starts a fresh "fill" generation for whatever comes
  # next, but is only added when something actually follows -- calling it
  # unconditionally would rename even a lone CI's scale away from plain
  # "fill", breaking e.g. plot$scales$get_scales("fill") for a chart that
  # never collides in the first place.
  ci_left <- if (!left_as_bar) ci[names(ci) %in% names(tsl)] else NULL
  ci_right <- if (!is.null(tsr)) ci[names(ci) %in% names(tsr)] else NULL
  band_or_bar_fill <- left_as_band || left_as_bar

  if (!is.null(ci_left)) {
    p <- draw_tsggplot_ci(p, ci_left, theme, use_date_scale = use_date_scale)
    if (!is.null(ci_right) || band_or_bar_fill) {
      p <- p + ggnewscale::new_scale_fill()
    }
  }

  if (!is.null(ci_right)) {
    p <- draw_tsggplot_ci(p, ci_right, tt_r, use_date_scale = use_date_scale)
    if (band_or_bar_fill) {
      p <- p + ggnewscale::new_scale_fill()
    }
  }

  if (left_as_bar) {
    ## draw barplot
    p <- draw_tsggplot_bars(p, tsl,
      group_bar_chart = group_bar_chart,
      theme = theme,
      use_date_scale = use_date_scale
    )
    if (theme$sum_as_line) {
      reduced <- Reduce("+", tsl)
      p <- draw_sum_as_ggline(p, reduced, theme, use_date_scale = use_date_scale)
      if (sum_own_scale) {
        # the sum belongs to the left axis: own colour scale/guide, so it
        # isn't listed with the right-axis lines (cf. the split legend below)
        p <- p + scale_color_manual(
          values = setNames(unname(theme$sum_line_color), theme$sum_legend)
        )
        if (auto_legend) {
          p <- p + guides(color = guide_legend(ncol = theme$legend_col, position = "bottom", order = 2, override.aes = list(fill = NA)))
        }
        p <- p + ggnewscale::new_scale_color()
      }
    }
  } else {
    # draw lineplot
    p <- draw_tsggplot_lines(p, tsl, theme = theme, bandplot = left_as_band, use_date_scale = use_date_scale)

    if (split_legend) {
      # tsl and tsr would otherwise share one "colour" aesthetic and get
      # merged by ggplot2 into a single legend. Give the left-axis series
      # their own colour scale/guide now, then start a fresh colour scale
      # via new_scale_color() before the right-axis lines are drawn below,
      # so each side ends up with its own, independently positioned guide.
      # This has to happen right here, interleaved with the geoms it
      # applies to -- ggnewscale tracks which "generation" of the colour
      # aesthetic a scale belongs to by the order components are added to
      # the plot, not by aesthetic name, so this scale/guide pair can't be
      # deferred to the end the way the merged-legend case does below.
      p <- p + scale_color_manual(
        values = setNames(theme$line_colors[seq_along(tsl)], names(tsl))
      )
      if (auto_legend) {
        p <- p + guides(color = guide_legend(ncol = theme$legend_col, position = "bottom", override.aes = list(fill = NA)))
      }
      p <- p + ggnewscale::new_scale_color()
    }
  }

  # RIGHT PLOT #######################
  if (!is.null(tsr)) {
    scale <- function(x, min1, max1, min2, max2) {
      ((x - min1) / (max1 - min1)) * (max2 - min2) + min2
    }
    # Define the ranges
    left_min <- min(left_y$y_range)
    left_max <- max(left_y$y_range)
    right_min <- min(right_y$y_range)
    right_max <- max(right_y$y_range)

    # Apply the scaling function to the secondary data
    scaled_tsr <- lapply(tsr, scale, right_min, right_max, left_min, left_max)

    # Add `tsr` as secondary time series
    p <- draw_tsggplot_lines(p, scaled_tsr, theme = tt_r, bandplot = FALSE, true_values = tsr, use_date_scale = use_date_scale)
  }

  if (!inherits(theme$axis.line.y, "element_blank")) {
    # Compute y-axis limits in a separate variable
    y_lim <- if (!inherits(theme$axis.line.y.left, "element_blank") &&
      inherits(theme$axis.line.y.right, "element_blank") &&
      !is.null(tsr)) {
      range(left_y$y_range, scaled_tsr, 0)
    } else if (!inherits(theme$axis.line.y.left, "element_blank")) {
      left_y$y_range
    } else {
      NULL
    }

    # Remove limits from scale_y_continuous and add coord_cartesian
    p <- p + scale_y_continuous(
      labels = if (!inherits(theme$axis.line.y.left, "element_blank")) {
        left_y$y_ticks
      } else {
        NULL
      },
      breaks = if (!inherits(theme$axis.line.y.left, "element_blank")) {
        left_y$y_ticks
      } else {
        NULL
      },
      minor_breaks = NULL,
      sec.axis = if (!inherits(theme$axis.line.y.right, "element_blank") && !is.null(tsr)) {
        sec_axis(
          transform = ~ scale(., left_min, left_max, right_min, right_max),
          name = labs$y_right
        )
      } else {
        waiver()
      },
      expand = c(0, 0)
    ) + coord_cartesian(ylim = y_lim)
  } else {
    p <- p +
      scale_y_continuous(
        minor_breaks = NULL,
        expand = c(0, 0)
      )
  }

  # Global X-Axis ###################
  if (!inherits(theme$axis.line.x, "element_blank")) {
    if (exists("segment_length")) {
      brks <- global_x$yearly_tick_pos
      is_date_scale <- global_x$dominant_freq %in% c("daily", "weekly", "hourly")

      # Label mid-points (labels centered within each year's bin) are only
      # used by the numeric (ts/monthly/quarterly/annual) scale below --
      # date-based scales (daily/weekly/hourly) hand tick placement to
      # ggplot2's own scale_x_date()/scale_x_datetime() instead, and their
      # yearly_tick_pos/quarterly_tick_pos are Date/POSIXct, for which this
      # arithmetic (e.g. summing two Date/POSIXct values) isn't meaningful.
      if (!is_date_scale) {
        if (length(brks) < 2) {
          # A single tick (e.g. a series short enough that only one yearly/
          # quarterly boundary falls within its padded range) has no "next"
          # tick to center a mid-point label between -- and the usual
          # mid-point formula's result would fall outside [min(brks),
          # max(brks)] and get filtered out anyway, leaving no breaks/labels
          # at all. Label the single tick directly at its own position
          # instead.
          mid_pts <- brks
          labs_pt <- global_x$year_labels_start
        } else {
          tick_spacing <- diff(brks)[1]
          mid_all <- c(
            (brks[-length(brks)] + brks[-1]) / 2,
            brks[length(brks)] + tick_spacing / 2
          )
          is_valid <- mid_all >= min(brks) & mid_all <= max(brks)
          mid_pts <- mid_all[is_valid]
          labs_pt <- global_x$year_labels_start[is_valid]
        }
      }

      # choose scale type depending on frequency
      if (global_x$dominant_freq %in% c("daily", "weekly")) {
        date_scale_args <- list(
          limits = as.Date(global_x$x_range),
          expand = c(0, 0)
        )
        if (identical(theme$axis_x_date_ticks, "years")) {
          # Fixed year-based spacing, regardless of the series' own span --
          # matches the numeric (ts/monthly/quarterly/annual) x-axis'
          # always-year-based convention, but produces an NA tick for
          # anything shorter than a year (the "auto" default's whole
          # reason for existing).
          date_scale_args$date_breaks <- paste0(theme$axis_x_label_dt, " years")
          date_scale_args$date_labels <- "%Y"
        }
        p <- p +
          do.call(scale_x_date, date_scale_args) +
          guides(x = guide_axis(check.overlap = TRUE))
      } else if (global_x$dominant_freq == "hourly") {
        datetime_scale_args <- list(
          limits = global_x$x_range,
          expand = c(0, 0)
        )
        if (identical(theme$axis_x_date_ticks, "years")) {
          datetime_scale_args$date_breaks <- paste0(theme$axis_x_label_dt, " years")
          datetime_scale_args$date_labels <- "%Y"
        }
        p <- p +
          do.call(scale_x_datetime, datetime_scale_args) +
          guides(x = guide_axis(check.overlap = TRUE))
      } else {
        p <- p +
          scale_x_continuous(
            breaks = mid_pts,
            minor_breaks = global_x$yearly_tick_pos,
            labels = labs_pt,
            limits = c(global_x$x_range[1], global_x$x_range[2]),
            expand = c(0, 0)
          ) +
          # check.overlap drops whichever yearly labels would collide, the
          # same fallback base R's axis() applies automatically in tsplot().
          guides(x = guide_axis(minor.ticks = TRUE, check.overlap = TRUE))
      }

      if (theme$quarterly_ticks && is.null(manual_ticks_x) && !is.null(global_x$quarterly_tick_pos)) {
        # Filter out overlapping quarterly ticks -- a very short series (or
        # a small axis_x_pad) may leave none beyond the yearly ones already
        # drawn
        q_ticks <- setdiff(global_x$quarterly_tick_pos, brks)

        if (length(q_ticks) > 0) {
          # Build a df of segment endpoints plotted in data coordinates
          panel <- ggplot_build(p)$layout$panel_params[[1]]
          y_min <- panel$y.range[1]
          y_rng <- diff(panel$y.range)
          # cheating here a bit because we neeed to translate grid units
          # (axis.minor.ticks.length) that make sense in the drawing coordinate
          # to data coordinates
          tick_h <- y_rng * segment_length / 100

          tick_df <- data.frame(
            x    = q_ticks,
            xend = q_ticks,
            y    = y_min,
            yend = y_min + tick_h
          )
          quarterly_tick_marks <- list(
            x = q_ticks,
            y_range = range(y_min, y_min + tick_h),
            # tick height as a fraction of the panel, so tsggplotly() can
            # redraw the marks in plotly's own "y domain" units
            height_frac = tick_h / y_rng
          )

          geom_args <- c(
            list(
              data = tick_df,
              mapping = aes(
                x = .data$x, y = .data$y,
                xend = .data$xend, yend = .data$yend
              ),
              inherit.aes = FALSE
            ),
            segment_x_bottom
          )

          p <- p + do.call(geom_segment, geom_args)
        }
      }
    } else {
      # no segment_length case
      if (global_x$dominant_freq %in% c("daily", "weekly")) {
        p <- p +
          scale_x_date(
            limits = as.Date(global_x$x_range, origin = "1970-01-01"),
            expand = c(0, 0)
          ) +
          guides(x = guide_axis(check.overlap = TRUE))
      } else if (global_x$dominant_freq == "hourly") {
        p <- p +
          scale_x_datetime(
            limits = global_x$x_range,
            expand = c(0, 0)
          ) +
          guides(x = guide_axis(check.overlap = TRUE))
      } else {
        p <- p +
          scale_x_continuous(
            breaks = global_x$yearly_tick_pos,
            limits = c(global_x$x_range[1], global_x$x_range[2]),
            expand = c(0, 0)
          ) +
          guides(x = guide_axis(check.overlap = TRUE))
      }
    }
  } else {
    p <- p +
      scale_x_continuous(
        breaks = NULL, labels = NULL, minor_breaks = NULL
      )
  }

  # Set the colors
  if (left_as_band || left_as_bar) {
    line_names <- names(tsr)
    fill_colors <- if (left_as_band) {
      theme$band_fill_color
    } else {
      theme$bar_fill_color
    }
    p <- p + scale_fill_manual(
      values = setNames(fill_colors, names(tsl)),
    )
    color_values <- setNames(theme$line_colors[seq_along(line_names)], line_names)
    if (left_as_bar && isTRUE(theme$sum_as_line) && !is.null(theme$sum_legend) && !sum_own_scale) {
      color_values <- c(color_values, setNames(unname(theme$sum_line_color), theme$sum_legend))
    }
    p <- p + scale_color_manual(values = color_values)
    if (auto_legend) {
      if (!is.null(tsr) && !isTRUE(theme$legend_all_left)) {
        # left series (bars/bands) and right series (lines) are already on
        # separate aesthetics (fill vs colour), so they already render as
        # two separate guide boxes below the plot, no new_scale_color()
        # needed.
        p <- p + guides(
          fill = guide_legend(ncol = theme$legend_col, position = "bottom", order = 1),
          color = guide_legend(ncol = theme$legend_col, position = "bottom", order = 3, override.aes = list(fill = NA))
        )
      } else {
        p <- p + guides(
          fill = guide_legend(ncol = theme$legend_col),
          color = guide_legend(ncol = theme$legend_col, override.aes = list(fill = NA))
        )
      }
    }
  } else if (split_legend) {
    # The left-axis colour scale/guide was already added above, right
    # before new_scale_color() and the right-axis lines were drawn.
    p <- p + scale_color_manual(
      values = setNames(tt_r$line_colors, names(tsr))
    )
    if (auto_legend) {
      p <- p + guides(color = guide_legend(ncol = theme$legend_col, position = "bottom", override.aes = list(fill = NA)))
    }
  } else {
    line_names <- c(names(tsl), names(tsr))
    p <- p + scale_color_manual(
      values = setNames(theme$line_colors, line_names)
    )
    if (auto_legend) {
      p <- p + guides(
        color = guide_legend(
          ncol = theme$legend_col,
          override.aes = list(fill = NA)
        )
      )
    }
  }

  if (!is.null(labs)) {
    lab_args <- labs[!vapply(labs, is.null, logical(1))]
    # y_right isn't a real ggplot2 label; passing it to labs() only
    # triggers an "Ignoring unknown labels" message on every build.
    lab_args$y_right <- NULL
    p <- p + do.call(ggplot2::labs, lab_args)
  }

  # Attach the axis metadata already computed above, so tsggplotly() can
  # build a real plotly yaxis2 for the secondary axis instead of guessing.
  attr(p, "tsggplot_meta") <- list(
    global_x = global_x,
    left_y = left_y,
    right_y = if (!is.null(tsr)) right_y else NULL,
    y_right_label = labs$y_right,
    # tsggplotly() can't convert the ggnewscale-based split legend directly
    # (ggplotly() drops the split-off geom's data), so it converts
    # merged_legend_fallback instead and re-splits the resulting plotly
    # traces into two plotly legends using these series names -- computed
    # here (not on merged_legend_fallback, whose own split_legend is always
    # FALSE) since it's the same tsl/tsr split either way.
    split_legend = split_legend,
    legend_groups = list(
      left = c(names(tsl), if (sum_own_scale) theme$sum_legend),
      right = names(tsr)
    ),
    merged_legend_fallback = merged_legend_fallback,
    quarterly_tick_marks = quarterly_tick_marks
  )

  if (output_format != "plot") {
    if (!grepl(sprintf("[.]%s$", output_format), save$filename)) {
      save$filename <- sprintf("%s.%s", save$filename, output_format)
    }
    do.call(ggsave, save)
  } else if (quiet) {
    p
  } else {
    c(list(plot = p), attr(p, "tsggplot_meta"))
  }
}
