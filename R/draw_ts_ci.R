#' @importFrom graphics polygon
draw_ts_ci <- function(ci, theme) {
  if (!is.null(ci)) {
    ci_colors <- namedColor2Hex(theme$ci_colors, theme$ci_alpha)
    for (ci_series_i in 1:length(ci)) {
      ci_series <- ci[[ci_series_i]]

      for (ci_level_i in 1:length(ci_series)) {
        ci_level <- ci_series[[ci_level_i]]
        xx <- as.numeric(time(ci_level$lb))

        frq <- frequency(ci_level$lb)

        if (theme$line_to_middle) {
          xx <- xx + (1 / frq) / 2
        }

        yy_low <- ci_level$lb
        yy_high <- ci_level$ub

        polygon(c(xx, rev(xx)), c(yy_low, rev(yy_high)), border = NA, col = ci_colors[ci_series_i])
      }
    }
  }
}

#' @importFrom ggplot2 aes geom_polygon scale_fill_manual .data
draw_tsggplot_ci <- function(p, ci, theme, use_date_scale = FALSE) {
  if (!is.null(ci)) {
    ci_names <- lapply(names(ci), function(x) {
      y <- gsub("%series%", x, theme$ci_legend_label)
      if (grepl("%ci_value%", y)) {
        parts <- strsplit(y, "%ci_value%")[[1]]
        # in case %ci_value% is at the very end (see ?split)
        if (length(parts) == 1) {
          parts <- c(parts, "")
        }
        y <- paste0(parts[1], names(ci[[x]]), parts[2])
      } else {
        y <- rep(y, length(ci[[x]]))
      }
      y
    })

    ci_colors <- namedColor2Hex(theme$ci_colors, theme$ci_alpha)

    group_ids <- character()

    for (ci_series_i in seq_along(ci)) {
      ci_series <- ci[[ci_series_i]]

      for (ci_level_i in seq_along(ci_series)) {
        ci_level <- ci_series[[ci_level_i]]
        xx <- getNumericTimeIndex(ci_level$lb, use_date_scale = use_date_scale)
        if (theme$line_to_middle) xx <- xx + getLineToMiddleShift(ci_level$lb, use_date_scale = use_date_scale)

        yy_low <- as.numeric(ci_level$lb)
        yy_high <- as.numeric(ci_level$ub)

        group_id <- interaction(ci_series_i, ci_level_i, drop = TRUE)
        group_ids <- c(group_ids, as.character(group_id))

        ci_df <- data.frame(
          x = c(xx, rev(xx)),
          y = c(yy_low, rev(yy_high)),
          group = group_id
        )

        p <- p +
          geom_polygon(
            data = ci_df,
            aes(
              x = .data$x,
              y = .data$y,
              group = .data$group,
              fill = .data$group
            ),
            show.legend = TRUE
          )
      }
    }

    group_labels <- unlist(ci_names)

    p <- p +
      scale_fill_manual(
        breaks = group_ids,
        labels = group_labels,
        values = setNames(ci_colors, group_ids)
      )

    p
  }
}
