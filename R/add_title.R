add_title <- function(plot_title, plot_subtitle, plot_subtitle_r, theme) {
  dev_size <- dev.size()
  if (!is.null(plot_title)) {
    if (!any(is.na(theme$title_transform))) {
      plot_title <- do.call(
        theme$title_transform,
        list(plot_title)
      )
    }

    # Transform title line from % of device height to "lines outside the plot"
    # the height of such a line is strheight("\n") - strheight("") i.e. 1 line plus interline spacing
    # because reasons I guess.

    title_line_height <- strheight("\n",
      units = "inches",
      cex = theme$title_cex.main
    ) -
      strheight("", units = "inches", cex = theme$title_cex.main)
    title_line <- (theme$title_margin * dev_size[2]) / (100 * title_line_height)

    title(
      main = plot_title, adj = theme$title_adj,
      line = title_line,
      outer = theme$title_outer,
      cex.main = theme$title_cex.main
    )
  }

  # Transform subtitle line from % of device height to "lines outside the plot"
  sub_line_height <- strheight("\n",
    units = "inches",
    cex = theme$subtitle_cex
  ) -
    strheight("", units = "inches", cex = theme$subtitle_cex)
  sub_line <- (theme$subtitle_margin * dev_size[2]) / (100 * sub_line_height)

  # See R source src/library/graphics/src/graphics.c:3325
  # where they add a ("visually tuned") offset to the mtext line
  sub_line <- sub_line - 0.2 / par("mex")

  if (!is.null(plot_subtitle)) {
    if (!is.null(theme$subtitle_transform)) {
      plot_subtitle <- do.call(
        theme$subtitle_transform,
        list(plot_subtitle)
      )
    }
    mtext(plot_subtitle,
      adj = theme$subtitle_adj,
      line = sub_line,
      outer = theme$subtitle_outer,
      cex = theme$subtitle_cex
    )
  }


  if (!is.null(plot_subtitle_r)) {
    if (!is.null(theme$subtitle_transform)) {
      plot_subtitle_r <- do.call(
        theme$subtitle_transform,
        list(plot_subtitle_r)
      )
    }
    mtext(plot_subtitle_r,
      adj = theme$subtitle_adj_r,
      line = sub_line,
      outer = theme$subtitle_outer,
      cex = theme$subtitle_cex
    )
  }
}
