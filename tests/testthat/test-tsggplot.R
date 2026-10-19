test_that("tsggplot sets custom labels correctly", {
  tsl <- list(AirPassengers = AirPassengers, JohnsonJohnson = JohnsonJohnson)
  labs <- list(
    x = "Engine displacement (litres)",
    y = "Highway miles per gallon",
    y_right = "Medication per kilogram",
    title = "Air Passengers",
    subtitle = "In thousands",
    caption = "(based on data from ...)",
    tag = "A"
  )
  theme <- init_tsggplot_theme(
    plot.title = element_text(size = 30, face = "bold", family = "sans"),
    plot.subtitle = element_text(size = 20),
    plot.caption = element_text(size = 25),
    plot.tag = element_text(size = 15)
  )
  p <- tsggplot(tsl, labs = labs, theme = theme)

  # Check if the labels are set correctly
  expect_equal(p$labels$x, labs$x)
  expect_equal(p$labels$y, labs$y)
  # y_right isn't a real ggplot2 label; it lives in tsggplot_meta instead.
  expect_equal(attr(p, "tsggplot_meta")$y_right_label, labs$y_right)
  expect_equal(p$labels$title, labs$title)
  expect_equal(p$labels$subtitle, labs$subtitle)
  expect_equal(p$labels$caption, labs$caption)
  expect_equal(p$labels$tag, labs$tag)

  expect_equal(p$theme$plot.title$face, "bold")
  expect_equal(p$theme$plot.title$size, 30)
  expect_equal(p$theme$plot.title$family, "sans")
  expect_false(isTRUE(p$theme$plot.title$inherit.blank))

  expect_equal(p$theme$plot.subtitle$size, 20)

  expect_equal(p$theme$plot.caption$size, 25)

  expect_equal(p$theme$plot.tag$size, 15)
})

test_that("tsggplot, grids", {
  theme <- init_tsggplot_theme(
    grids_x_count = c(5, 6, 8, 10),
    grids_x_count_strict = FALSE,
    panel.grid.major.x = element_line(
      color = "red",
      linewidth = 4,
      linetype = "dashed"
    ),
    panel.grid.major.y = element_line(
      color = "blue",
      linewidth = 5,
    )
  )
  p <- tsggplot(AirPassengers,
    theme = theme
  )

  # Check if the theme attributes are applied correctly
  expect_equal(p$theme$panel.grid.major.x$colour, "red")
  expect_equal(p$theme$panel.grid.major.x$linewidth, 4)
  expect_equal(p$theme$panel.grid.major.x$linetype, "dashed")
  expect_null(p$theme$panel.grid.major.x$lineend)
  expect_false(p$theme$panel.grid.major.x$arrow)
  expect_false(p$theme$panel.grid.major.x$inherit.blank)

  expect_equal(p$theme$panel.grid.major.y$colour, "blue")
  expect_equal(p$theme$panel.grid.major.y$linewidth, 5)
  expect_null(p$theme$panel.grid.major.y$linetype)
  expect_null(p$theme$panel.grid.major.y$lineend)
  expect_false(p$theme$panel.grid.major.y$arrow)
  expect_false(p$theme$panel.grid.major.y$inherit.blank)
})

test_that("tsggplot tick labels centered", {
  tsl <- list(AirPassengers = AirPassengers)
  theme <- init_tsggplot_theme(
    axis.minor.ticks.length = ggplot2::unit(5, "pt"),
    axis.minor.ticks.x.bottom = aes(
      color = "blue", linewidth = 2, linetype = "dashed"
    ),
    axis.ticks.length = ggplot2::unit(50, "pt"),
    axis.ticks.x.bottom = element_line(color = "red", linewidth = 3),
    axis.text.x.pos = "mid"
  )
  p <- tsggplot(list(tsl$AirPassengers),
    left_as_bar = TRUE,
    theme = theme
  )

  # Check if the theme attributes are applied correctly
  expect_equal(p$theme$axis.minor.ticks.length, ggplot2::unit(-50, "pt"))

  expect_equal(p$theme$axis.minor.ticks.x.bottom$colour, "red")
  expect_equal(p$theme$axis.minor.ticks.x.bottom$linewidth, 3)
  expect_null(p$theme$axis.minor.ticks.x.bottom$linetype)
  expect_null(p$theme$axis.minor.ticks.x.bottom$lineend)
  expect_false(p$theme$axis.minor.ticks.x.bottom$arrow)
  expect_false(p$theme$axis.minor.ticks.x.bottom$inherit.blank)

  # main ticks are set to 0, only labels are showing
  expect_equal(p$theme$axis.ticks.length, ggplot2::unit(0, "cm"))

  seg_layers <- which(
    sapply(p$layers, function(l) inherits(l$geom, "GeomSegment"))
  )
  seg_layer <- p$layers[[seg_layers]]

  expected_segment <- structure(
    list(
      x = c(
        1949.25, 1949.5, 1949.75, 1950.25, 1950.5, 1950.75, 1951.25, 1951.5,
        1951.75, 1952.25, 1952.5, 1952.75, 1953.25, 1953.5, 1953.75, 1954.25,
        1954.5, 1954.75, 1955.25, 1955.5, 1955.75, 1956.25, 1956.5, 1956.75,
        1957.25, 1957.5, 1957.75, 1958.25, 1958.5, 1958.75, 1959.25, 1959.5,
        1959.75, 1960.25, 1960.5, 1960.75
      ),
      xend = c(
        1949.25, 1949.5, 1949.75, 1950.25, 1950.5, 1950.75, 1951.25, 1951.5,
        1951.75, 1952.25, 1952.5, 1952.75, 1953.25, 1953.5, 1953.75, 1954.25,
        1954.5, 1954.75, 1955.25, 1955.5, 1955.75, 1956.25, 1956.5, 1956.75,
        1957.25, 1957.5, 1957.75, 1958.25, 1958.5, 1958.75, 1959.25, 1959.5,
        1959.75, 1960.25, 1960.5, 1960.75
      ),
      y = rep(0, 36),
      yend = rep(40, 36)
    ),
    class = "data.frame",
    row.names = c(NA, -36L)
  )
  expect_equal(seg_layer$data, expected_segment)

  expect_equal(
    seg_layer$aes_params,
    list(colour = "blue", linewidth = 2, linetype = "dashed")
  )
})

test_that("tsggplot ticks", {
  tsl <- list(AirPassengers = AirPassengers)
  tsg <- list(AirPassengers = diff(log(AirPassengers)) * 100)
  theme <- init_tsggplot_theme(
    axis.minor.ticks.length = ggplot2::unit(15, "pt"),
    axis.minor.ticks.x.bottom = element_line(color = "blue", linewidth = 2),
    axis.ticks.length = ggplot2::unit(20, "pt"),
    axis.ticks.x.bottom = element_line(color = "red", linewidth = 3),
    axis.text.x.pos = "start"
  )
  p <- tsggplot(list(tsl$AirPassengers),
    tsr = list(tsg$AirPassengers),
    left_as_bar = TRUE,
    theme = theme
  )

  # Check if the theme attributes are applied correctly
  expect_equal(p$theme$axis.minor.ticks.length, ggplot2::unit(15, "pt"))

  expect_equal(p$theme$axis.minor.ticks.x.bottom$colour, "blue")
  expect_equal(p$theme$axis.minor.ticks.x.bottom$linewidth, 2)
  expect_null(p$theme$axis.minor.ticks.x.bottom$linetype)
  expect_null(p$theme$axis.minor.ticks.x.bottom$lineend)
  expect_false(p$theme$axis.minor.ticks.x.bottom$arrow)
  expect_false(p$theme$axis.minor.ticks.x.bottom$inherit.blank)

  expect_equal(p$theme$axis.ticks.length, ggplot2::unit(20, "pt"))

  expect_equal(p$theme$axis.ticks.x.bottom$colour, "red")
  expect_equal(p$theme$axis.ticks.x.bottom$linewidth, 3)
  expect_null(p$theme$axis.ticks.x.bottom$linetype)
  expect_null(p$theme$axis.ticks.x.bottom$lineend)
  expect_false(p$theme$axis.ticks.x.bottom$arrow)
  expect_false(p$theme$axis.ticks.x.bottom$inherit.blank)
})

test_that("tsggplot x-axis guide drops overlapping yearly labels (#13)", {
  # guide_axis(check.overlap = TRUE) makes ggplot2 drop whichever x-axis
  # labels would collide at draw time, the same fallback base R's axis()
  # gives tsplot() for free. Exercise all four Global X-Axis branches
  # (numeric vs. Date scale, crossed with the "mid"/"start" label position,
  # which decides whether the segment_length/minor-ticks code path is used).
  x_check_overlap <- function(p) {
    p$guides$guides[["x"]]$params$check.overlap
  }

  ts_long <- window(AirPassengers, start = c(1949, 1), end = c(1960, 12))
  weekly_long <- xts::xts(
    seq_len(261),
    order.by = seq(as.Date("2010-01-01"), by = "week", length.out = 261)
  )

  # "mid" label position (default) -> segment_length/minor-ticks branch
  expect_true(x_check_overlap(tsggplot(list(ts_long))))
  expect_true(x_check_overlap(tsggplot(list(weekly_long))))

  # "start" label position -> no segment_length branch
  start_theme <- init_tsggplot_theme(axis.text.x.pos = "start")
  expect_true(x_check_overlap(tsggplot(list(ts_long), theme = start_theme)))
  expect_true(x_check_overlap(tsggplot(list(weekly_long), theme = start_theme)))
})

test_that("tsggplot axis", {
  tsl <- list(AirPassengers = AirPassengers, JohnsonJohnson = JohnsonJohnson)
  theme <- init_tsggplot_theme(
    axis.line.x = element_line(color = "blue", linewidth = 2),
    axis.line.y.left = element_line(color = "red", linewidth = 1),
    axis.line.y.right = element_line(color = "green", linewidth = 3)
  )
  p <- tsggplot(list(tsl$AirPassengers),
    tsr = list(tsl$JohnsonJohnson),
    theme = theme
  )

  # Check if the theme attributes are applied correctly
  expect_equal(p$theme$axis.line.x$colour, "blue")
  expect_equal(p$theme$axis.line.x$linewidth, 2)
  expect_null(p$theme$axis.line.x$linetype)
  expect_null(p$theme$axis.line.x$lineend)
  expect_false(p$theme$axis.line.x$arrow)
  expect_false(p$theme$axis.line.x$inherit.blank)

  expect_equal(p$theme$axis.line.y.left$colour, "red")
  expect_equal(p$theme$axis.line.y.left$linewidth, 1)
  expect_null(p$theme$axis.line.y.left$linetype)
  expect_null(p$theme$axis.line.y.left$lineend)
  expect_false(p$theme$axis.line.y.left$arrow)
  expect_false(p$theme$axis.line.y.left$inherit.blank)

  expect_equal(p$theme$axis.line.y.right$colour, "green")
  expect_equal(p$theme$axis.line.y.right$linewidth, 3)
  expect_null(p$theme$axis.line.y.right$linetype)
  expect_null(p$theme$axis.line.y.right$lineend)
  expect_false(p$theme$axis.line.y.right$arrow)
  expect_false(p$theme$axis.line.y.right$inherit.blank)
})

test_that("tsggplot hide y axis", {
  tsl <- list(AirPassengers = AirPassengers, JohnsonJohnson = JohnsonJohnson)
  theme <- init_tsggplot_theme(
    axis.line.y = element_blank()
  )
  p <- tsggplot(list(tsl$AirPassengers),
    tsr = list(tsl$JohnsonJohnson),
    theme = theme
  )

  expect_true(inherits(p$theme$axis.line.y, "ggplot2::element_blank"))
})

test_that("tsggplot hide x axis", {
  tsl <- list(AirPassengers = AirPassengers, JohnsonJohnson = JohnsonJohnson)
  theme <- init_tsggplot_theme(
    axis.line.x = element_blank()
  )
  p <- tsggplot(list(tsl$AirPassengers),
    tsr = list(tsl$JohnsonJohnson),
    theme = theme
  )

  expect_true(inherits(p$theme$axis.line.x, "ggplot2::element_blank"))
})

test_that("tsggplot y axis overrides left and right", {
  tsl <- list(AirPassengers = AirPassengers, JohnsonJohnson = JohnsonJohnson)
  theme <- init_tsggplot_theme(
    axis.line.y = element_line(color = "red")
  )
  p <- tsggplot(list(tsl$AirPassengers),
    tsr = list(tsl$JohnsonJohnson),
    theme = theme
  )

  expect_equal(p$theme$axis.line.y.left$colour, "red")
  expect_null(p$theme$axis.line.y.left$linewidth)
  expect_null(p$theme$axis.line.y.left$linetype)
  expect_null(p$theme$axis.line.y.left$lineend)
  expect_false(p$theme$axis.line.y.left$arrow)
  expect_false(p$theme$axis.line.y.left$inherit.blank)
})

test_that("tsggplot axis text", {
  tsl <- list(AirPassengers = AirPassengers, JohnsonJohnson = JohnsonJohnson)
  theme <- init_tsggplot_theme(
    axis.text = element_text(color = "blue", size = 10),
  )
  p <- tsggplot(list(tsl$AirPassengers),
    tsr = list(tsl$JohnsonJohnson),
    theme = theme
  )

  expect_equal(p$theme$axis.text$colour, "blue")
  expect_equal(p$theme$axis.text$size, 10)
  expect_null(p$theme$axis.text$family)
  expect_null(p$theme$axis.text$face)
  expect_null(p$theme$axis.text$hjust)
  expect_null(p$theme$axis.text$vjust)
  expect_null(p$theme$axis.text$angle)
  expect_null(p$theme$axis.text$lineheight)
  expect_null(p$theme$axis.text$margin)
  expect_null(p$theme$axis.text$debug)
  expect_false(p$theme$axis.text$inherit.blank)
})

test_that("tsggplot hide axis text", {
  tsl <- list(AirPassengers = AirPassengers, JohnsonJohnson = JohnsonJohnson)
  theme <- init_tsggplot_theme(
    axis.text = element_blank()
  )
  p <- tsggplot(list(tsl$AirPassengers),
    tsr = list(tsl$JohnsonJohnson),
    theme = theme
  )

  expect_true(inherits(p$theme$axis.text, "element_blank"))
  expect_true(inherits(p$theme$axis.text.x, "element_blank"))
  expect_true(inherits(p$theme$axis.text.y.left, "element_blank"))
  expect_true(inherits(p$theme$axis.text.y.right, "element_blank"))
})

test_that("tsggplot x and y axis text", {
  tsl <- list(AirPassengers = AirPassengers, JohnsonJohnson = JohnsonJohnson)
  theme <- init_tsggplot_theme(
    axis.text.x = element_text(color = "blue", size = 10),
    axis.text.x.pos = "mid",
    axis.text.y.left = element_text(color = "green", size = 15),
    axis.text.y.right = element_text(color = "red", size = 20),
  )
  p <- tsggplot(list(tsl$AirPassengers),
    tsr = list(tsl$JohnsonJohnson),
    theme = theme
  )

  expect_equal(p$theme$axis.text.x$colour, "blue")
  expect_equal(p$theme$axis.text.x$size, 10)
  expect_equal(c(p$theme$axis.text.x$margin), c(10, 0, 0, 0))
  expect_null(p$theme$axis.text.x$family)
  expect_null(p$theme$axis.text.x$face)
  expect_null(p$theme$axis.text.x$hjust)
  expect_null(p$theme$axis.text.x$vjust)
  expect_null(p$theme$axis.text.x$angle)
  expect_null(p$theme$axis.text.x$lineheight)
  expect_null(p$theme$axis.text.x$debug)
  expect_false(p$theme$axis.text.x$inherit.blank)

  expect_equal(p$theme$axis.text.y.left$colour, "green")
  expect_equal(p$theme$axis.text.y.left$size, 15)
  expect_null(p$theme$axis.text.y.left$family)
  expect_null(p$theme$axis.text.y.left$face)
  expect_null(p$theme$axis.text.y.left$hjust)
  expect_null(p$theme$axis.text.y.left$vjust)
  expect_null(p$theme$axis.text.y.left$angle)
  expect_null(p$theme$axis.text.y.left$lineheight)
  expect_null(p$theme$axis.text.y.left$debug)
  expect_false(p$theme$axis.text.y.left$inherit.blank)

  expect_equal(p$theme$axis.text.y.right$colour, "red")
  expect_equal(p$theme$axis.text.y.right$size, 20)
  expect_null(p$theme$axis.text.y.right$family)
  expect_null(p$theme$axis.text.y.right$face)
  expect_null(p$theme$axis.text.y.right$hjust)
  expect_null(p$theme$axis.text.y.right$vjust)
  expect_null(p$theme$axis.text.y.right$angle)
  expect_null(p$theme$axis.text.y.right$lineheight)
  expect_null(p$theme$axis.text.y.right$debug)
  expect_false(p$theme$axis.text.y.right$inherit.blank)
})

test_that("tsggplot legend", {
  # Plot with left and right axes - lines
  theme <- init_tsggplot_theme()
  p <- tsggplot(list(JohnsonJohnson = JohnsonJohnson, JohnsonJohnson2 = JohnsonJohnson * 2),
    tsr = list(AirPassengers = AirPassengers, AirPassengers2 = AirPassengers * 2),
    labs = list(color = "Legend Title"),
    theme = theme
  )

  expect_equal(p$labels$colour, "Legend Title")

  pb <- ggplot_build(p)
  # line 1
  colour_1 <- unique(pb$data[[1]]$colour)
  expect_equal(colour_1, unname(theme$line_colors[1]))
  # line 2
  colour_2 <- unique(pb$data[[2]]$colour)
  expect_equal(colour_2, unname(theme$line_colors[2]))

  # Plot with left as band
  theme <- init_tsggplot_theme()
  p <- tsggplot(list(JohnsonJohnson = JohnsonJohnson, JohnsonJohnson2 = JohnsonJohnson),
    tsr = list(AirPassengers = AirPassengers, AirPassengers2 = AirPassengers * 2),
    left_as_band = TRUE,
    theme = theme
  )

  pb <- ggplot_build(p)
  # band
  colour_band_1 <- unique(pb$data[[1]]$fill)
  expect_equal(colour_band_1, unname(theme$band_fill_color[1]))
  colour_band_2 <- unique(pb$data[[2]]$fill)
  expect_equal(colour_band_2, unname(theme$band_fill_color[2]))
  # line
  colour_line_1 <- unique(pb$data[[3]]$colour)
  expect_equal(colour_line_1, unname(theme$line_colors[1]))
  colour_line_2 <- unique(pb$data[[4]]$colour)
  expect_equal(colour_line_2, unname(theme$line_colors[2]))

  # Plot with left as bar
  theme <- init_tsggplot_theme()
  p <- tsggplot(list(JohnsonJohnson = JohnsonJohnson, JohnsonJohnson2 = JohnsonJohnson),
    tsr = list(AirPassengers = AirPassengers, AirPassengers2 = AirPassengers * 2),
    left_as_bar = TRUE,
    theme = theme
  )

  pb <- ggplot_build(p)
  # bar
  colour_bar <- unique(pb$data[[1]]$fill)
  expect_equal(colour_bar, unname(theme$bar_fill_color[1:2]))
  # line
  colour_line_1 <- unique(pb$data[[2]]$colour)
  expect_equal(colour_line_1, unname(theme$line_colors[1]))
  colour_line_2 <- unique(pb$data[[3]]$colour)
  expect_equal(colour_line_2, unname(theme$line_colors[2]))
})

test_that("tsggplot splits the tsr legend left/right axis by default", {
  # Plain lines on both axes: split into two colour scales via ggnewscale,
  # each getting its own guide instead of one merged legend.
  p <- tsggplot(list(a = AirPassengers), tsr = list(b = JohnsonJohnson))
  expect_true(attr(p, "tsggplot_meta")$split_legend)
  scale_aes <- vapply(p$scales$scales, function(s) paste(s$aesthetics, collapse = ","), character(1))
  expect_true(any(grepl("^colour_ggnewscale_", scale_aes)))
  expect_true("colour" %in% scale_aes)

  # legend_all_left opts back into a single merged legend/colour scale.
  p_merged <- tsggplot(list(a = AirPassengers),
    tsr = list(b = JohnsonJohnson),
    theme = init_tsggplot_theme(legend_all_left = TRUE)
  )
  expect_false(attr(p_merged, "tsggplot_meta")$split_legend)
  merged_scale_aes <- vapply(p_merged$scales$scales, function(s) paste(s$aesthetics, collapse = ","), character(1))
  expect_false(any(grepl("^colour_ggnewscale_", merged_scale_aes)))
  expect_true("colour" %in% merged_scale_aes)

  # No tsr at all: unaffected, single ordinary colour scale.
  p_no_tsr <- tsggplot(list(a = AirPassengers))
  expect_false(attr(p_no_tsr, "tsggplot_meta")$split_legend)

  # left_as_bar + tsr: left (fill) and right (colour) are already on
  # separate aesthetics, so no ggnewscale split is needed there.
  p_bar <- tsggplot(list(a = AirPassengers), tsr = list(b = JohnsonJohnson), left_as_bar = TRUE)
  expect_false(attr(p_bar, "tsggplot_meta")$split_legend)
})

test_that("tsggplot adds spacing between the left/right legend guide-boxes so they read as two groups", {
  # ggplot2 places separate guide-boxes right next to each other by default,
  # which looks like one continuous legend even though they're structurally
  # distinct -- legend.spacing.x is the only thing that visually tells them
  # apart, so it must actually be set whenever there are two guide-boxes to
  # separate.
  p_split <- tsggplot(list(a = AirPassengers), tsr = list(b = JohnsonJohnson))
  expect_equal(p_split$theme$legend.spacing.x, unit(2, "cm"))

  p_bar <- tsggplot(list(a = AirPassengers), tsr = list(b = JohnsonJohnson), left_as_bar = TRUE)
  expect_equal(p_bar$theme$legend.spacing.x, unit(2, "cm"))

  # Merged legend and no-tsr cases only ever render a single guide-box, so
  # the extra spacing would have no visible effect there -- confirm it's
  # left at ggplot2's default instead of being set unconditionally.
  p_merged <- tsggplot(list(a = AirPassengers),
    tsr = list(b = JohnsonJohnson),
    theme = init_tsggplot_theme(legend_all_left = TRUE)
  )
  expect_null(p_merged$theme$legend.spacing.x)

  p_no_tsr <- tsggplot(list(a = AirPassengers))
  expect_null(p_no_tsr$theme$legend.spacing.x)
})

test_that("tsggplotly converts a split-legend plot via its merged-legend fallback", {
  skip_if_not_installed("plotly")
  p <- tsggplot(list(a = AirPassengers), tsr = list(b = JohnsonJohnson))
  meta <- attr(p, "tsggplot_meta")
  expect_true(meta$split_legend)
  expect_s3_class(meta$merged_legend_fallback, "ggplot")

  expect_no_error(fig <- tsggplotly(p))
  built <- plotly::plotly_build(fig)
  traces <- setNames(built$x$data, sapply(built$x$data, function(d) if (is.null(d$name)) "" else d$name))
  # both series' full data made it through, not just one (what "the
  # split-off geom silently loses its data" would otherwise look like)
  expect_equal(length(traces[["a"]]$x), length(AirPassengers))
  expect_equal(length(traces[["b"]]$x), length(JohnsonJohnson))

  p_merged <- tsggplot(list(a = AirPassengers),
    tsr = list(b = JohnsonJohnson),
    theme = init_tsggplot_theme(legend_all_left = TRUE)
  )
  expect_no_error(tsggplotly(p_merged))
})

test_that("tsggplotly re-splits the merged-legend fallback into two plotly legends (#16)", {
  skip_if_not_installed("plotly")
  # Plain lines on both axes.
  p <- tsggplot(list(a = AirPassengers), tsr = list(b = JohnsonJohnson))
  built <- plotly::plotly_build(tsggplotly(p))
  traces <- setNames(built$x$data, vapply(built$x$data, function(d) d$name %||% "", ""))
  expect_null(traces[["a"]][["legend"]]) # default "legend"
  expect_equal(traces[["b"]][["legend"]], "legend2")
  expect_true(!is.null(built$x$layout$legend2))
  expect_lt(built$x$layout$legend$x, built$x$layout$legend2$x)
  # no stray "series" (the colour aes name) shown as either box's title
  expect_equal(built$x$layout$legend$title$text, "")
  expect_equal(built$x$layout$legend2$title$text, "")

  # Bar chart with a tsr and a sum line: the sum line gets its own colour
  # scale (see #14/legend ordering commit), so this also goes through the
  # split-legend path even though the bars themselves are on a separate,
  # untouched fill aesthetic.
  tsb1 <- ts(runif(20, 0, 50), start = c(2010, 1), frequency = 4)
  tsb2 <- ts(runif(20, 0, 50), start = c(2010, 1), frequency = 4)
  tsb3 <- ts(runif(20, 0, 5), start = c(2010, 1), frequency = 4)
  p_bar <- tsggplot(list(a = tsb1, b = tsb2),
    tsr = list(c = tsb3), left_as_bar = TRUE,
    theme = init_tsggplot_theme(sum_as_line = TRUE)
  )
  expect_true(attr(p_bar, "tsggplot_meta")$split_legend)
  built_bar <- plotly::plotly_build(tsggplotly(p_bar))
  traces_bar <- setNames(built_bar$x$data, vapply(built_bar$x$data, function(d) d$name %||% "", ""))
  expect_null(traces_bar[["a"]][["legend"]])
  expect_null(traces_bar[["b"]][["legend"]])
  expect_null(traces_bar[["sum"]][["legend"]])
  expect_equal(traces_bar[["c"]][["legend"]], "legend2")
})

test_that("tsggplot modify the legend", {
  # Modify the legend title
  theme <- init_tsggplot_theme(
    legend.title = element_text(size = 20, face = "bold"),
    legend.title.position = "top",
    legend.justification = "left"
  )
  p <- tsggplot(list(AirPassengers = AirPassengers),
    tsr = list(JohnsonJohnson = JohnsonJohnson),
    labs = list(color = "Legend Title"),
    theme = theme
  )

  expect_equal(p$theme$legend.title$size, 20)
  expect_equal(p$theme$legend.title$face, "bold")
  expect_null(p$theme$legend.title$colour)
  expect_null(p$theme$legend.title$family)
  expect_null(p$theme$legend.title$hjust)
  expect_null(p$theme$legend.title$vjust)
  expect_null(p$theme$legend.title$angle)
  expect_null(p$theme$legend.title$lineheight)
  expect_null(p$theme$legend.title$debug)
  expect_false(p$theme$legend.title$inherit.blank)
})

test_that("tsggplot hide legend", {
  theme <- init_tsggplot_theme(legend.position = "none")
  p <- tsggplot(list(AirPassengers = AirPassengers),
    tsr = list(JohnsonJohnson = JohnsonJohnson),
    theme = theme
  )

  expect_equal(p$theme$legend.position, "none")

  # alternative way
  p <- tsggplot(
    AirPassengers = AirPassengers,
    auto_legend = FALSE
  )
  expect_equal(p$theme$legend.position, "none")
})

test_that("tsggplot wrong theme passed", {
  # fails gracefully with tsplot theme
  theme <- init_tsplot_theme()
  expect_error(tsggplot(AirPassengers, theme = theme), "Invalid theme")

  # works with ggplot theme
  theme <- init_tsggplot_theme()
  expect_no_error(tsggplot(AirPassengers, theme = theme))
})

test_that("tsggplot with highlight window", {
  theme <- init_tsggplot_theme(
    highlight_window = TRUE,
    highlight_window_color = "red",
    highlight_window_alpha = 0.2
  )
  # translucent geom_rect fill is composited by grid regardless of the
  # device's bitmapType, unlike base graphics' rect(), so no cairo warning
  expect_no_warning(tsggplot(
    list(AirPassengers = AirPassengers),
    theme = theme
  ))

  p <- tsggplot(list(AirPassengers = AirPassengers),
    theme = theme
  )

  # find the geom_rect layer
  ix <- which(sapply(
    p$layers,
    function(l) inherits(l$geom, "GeomRect")
  ))

  rect <- p$layers[[ix]]
  expect_false(rect$inherit.aes)
  expect_equal(rect$aes_params$fill, "red")
  expect_true(is.na(rect$aes_params$colour))

  # Highlight window with start and end date
  theme <- init_tsggplot_theme(
    highlight_window = TRUE,
    highlight_window_start = c(1959, 1),
    highlight_window_end = c(1971, 1)
  )
  p <- tsggplot(list(AirPassengers = AirPassengers),
    theme = theme
  )
  # find the geom_rect layer
  ix <- which(sapply(
    p$layers,
    function(l) inherits(l$geom, "GeomRect")
  ))

  rect <- p$layers[[ix]]
  expect_false(rect$inherit.aes)
  expect_equal(rect$aes_params$fill, theme$highlight_window_color)
  expect_true(is.na(rect$aes_params$colour))
})

test_that("tsggplot highlight window on a date/datetime x-axis (daily/hourly)", {
  get_rect_data <- function(p) {
    ix <- which(sapply(p$layers, function(l) inherits(l$geom, "GeomRect")))
    p$layers[[ix]]$data
  }

  # daily: default (NA start/end) highlights ~2 years before the end of the
  # plotted range, not 2 days (the same "2" used for the numeric x-axis,
  # applied literally to a Date would be almost invisible)
  dates <- seq(as.Date("2015-01-01"), as.Date("2020-01-01"), by = "day")
  daily_xts <- xts::xts(seq_along(dates), order.by = dates)
  p_daily_default <- tsggplot(list(A = daily_xts), theme = init_tsggplot_theme(highlight_window = TRUE))
  rect_daily_default <- get_rect_data(p_daily_default)
  expect_true(inherits(rect_daily_default$xmin, "Date"))
  expect_true(diff(c(rect_daily_default$xmin, rect_daily_default$xmax)) > 300)

  # daily: explicit Date (or a parseable string) values are used directly,
  # instead of being (mis)interpreted as a ts-style c(year, period) pair
  p_daily_explicit <- tsggplot(
    list(A = daily_xts),
    theme = init_tsggplot_theme(
      highlight_window = TRUE,
      highlight_window_start = "2018-06-01",
      highlight_window_end = as.Date("2018-12-31")
    )
  )
  rect_daily_explicit <- get_rect_data(p_daily_explicit)
  expect_equal(rect_daily_explicit$xmin, as.Date("2018-06-01"))
  expect_equal(rect_daily_explicit$xmax, as.Date("2018-12-31"))

  # hourly: same idea, but in POSIXct/seconds
  hourly_idx <- seq(as.POSIXct("2020-01-01", tz = "UTC"), as.POSIXct("2023-01-01", tz = "UTC"), by = "hour")
  hourly_xts <- xts::xts(seq_along(hourly_idx), order.by = hourly_idx)
  p_hourly_default <- tsggplot(list(A = hourly_xts), theme = init_tsggplot_theme(highlight_window = TRUE))
  rect_hourly_default <- get_rect_data(p_hourly_default)
  expect_true(inherits(rect_hourly_default$xmin, "POSIXct"))
  expect_true(diff(c(rect_hourly_default$xmin, rect_hourly_default$xmax)) > 300) # days

  p_hourly_explicit <- tsggplot(
    list(A = hourly_xts),
    theme = init_tsggplot_theme(
      highlight_window = TRUE,
      highlight_window_start = as.POSIXct("2022-06-01", tz = "UTC"),
      highlight_window_end = as.POSIXct("2022-12-01", tz = "UTC")
    )
  )
  rect_hourly_explicit <- get_rect_data(p_hourly_explicit)
  expect_equal(as.numeric(rect_hourly_explicit$xmin), as.numeric(as.POSIXct("2022-06-01", tz = "UTC")))
  expect_equal(as.numeric(rect_hourly_explicit$xmax), as.numeric(as.POSIXct("2022-12-01", tz = "UTC")))
})

test_that("tsggplot, with series starting not at start of year", {
  theme <- init_tsggplot_theme(fill_up_start = TRUE)
  p <- tsggplot(list(JohnsonJohnson = window(JohnsonJohnson, start = c(1960, 3))),
    theme = theme
  )

  # Plotted x-positions are numeric decimal-year values shifted by half a
  # quarter (line_to_middle, the theme default). Previously this stayed
  # yearqtr-classed and unshifted, because zoo's yearqtr arithmetic treats
  # "+" as whole quarters and silently absorbed the fractional (0.125)
  # shift -- i.e. quarterly xts data was never actually centered despite
  # line_to_middle = TRUE. See getNumericTimeIndex()/getLineToMiddleShift().
  expect_equal(
    range(p$layers[[1]]$data$time),
    c(1960.625, 1980.875)
  )

  # check axis x labels
  pb <- ggplot_build(p)
  txts <- pb$layout$panel_params[[1]]$x$get_labels()
  expect_equal(range(txts), c(1960, 1980))
})

test_that("tsggplot, confidence intervals", {
  theme <- init_tsggplot_theme(
    ci_alpha = 0.2,
    ci_colors = c("red", "blue"),
    ci_legend_label = "%ci_value%% ci for %series% TEST"
  )

  # Define confidence intervals
  ci <- list(
    "KOF Barometer" = list(
      "80" = list(
        lb = KOF$baro_lo_80,
        ub = KOF$baro_hi_80
      ),
      "95" = list(
        lb = KOF$baro_lo_95,
        ub = KOF$baro_hi_95
      )
    )
  )

  p <- tsggplot(list("KOF Barometer" = KOF$baro_point_fc),
    ci = ci,
    theme = theme
  )

  expected_ci_colors <- namedColor2Hex(theme$ci_colors, theme$ci_alpha)

  bd <- ggplot_build(p)$data
  ci_colors <- unique(c(bd[[1]]$fill, bd[[2]]$fill))

  expect_equal(
    ci_colors,
    expected_ci_colors
  )

  fill_scale <- p$scales$get_scales("fill")

  expect_equal(
    fill_scale$labels,
    c(
      "80% ci for KOF Barometer TEST",
      "95% ci for KOF Barometer TEST"
    )
  )
})

test_that("tsggplot doesn't let a band/bar or second-axis CI collide with the CI fill scale (#14)", {
  ts1 <- ts(1:8, start = c(2020, 1), frequency = 4)
  ts2 <- ts(8:1, start = c(2020, 1), frequency = 4)

  # left_as_band + ci on the band's own series: without its own scale
  # generation, the band's fill scale added afterwards would silently
  # replace the CI's, leaving the CI polygon's fill unmapped/invisible
  ci <- list(A = list("80" = list(lb = ts1 - 1, ub = ts1 + 1)))
  expect_no_message(p <- tsggplot(list(A = ts1, B = ts2), left_as_band = TRUE, ci = ci))
  bd <- ggplot2::ggplot_build(p)
  is_ci <- vapply(bd$plot$layers, function(l) inherits(l$geom, "GeomPolygon"), logical(1))
  is_band <- vapply(bd$plot$layers, function(l) inherits(l$geom, "GeomRibbon"), logical(1))
  ci_fill <- unique(bd$data[is_ci][[1]]$fill)
  band_fills <- unique(unlist(lapply(bd$data[is_band], `[[`, "fill")))
  expect_length(ci_fill, 1)
  expect_false(anyNA(ci_fill))
  expect_false(ci_fill %in% band_fills)

  # ci on both axes: each axis' CI used to collide with the other's fill
  # scale, and both ended up rendered with whichever scale was added last
  tsr <- list(C = ts2 + 10)
  ci2 <- list(
    A = list("80" = list(lb = ts1 - 1, ub = ts1 + 1)),
    C = list("80" = list(lb = ts2 + 9, ub = ts2 + 11))
  )
  expect_no_message(p2 <- tsggplot(list(A = ts1), tsr = tsr, ci = ci2))
  bd2 <- ggplot2::ggplot_build(p2)
  is_ci2 <- vapply(bd2$plot$layers, function(l) inherits(l$geom, "GeomPolygon"), logical(1))
  ci_fills2 <- vapply(bd2$data[is_ci2], function(d) unique(d$fill)[1], character(1))
  expect_length(ci_fills2, 2)
  expect_length(unique(ci_fills2), 2)
  expect_false(anyNA(ci_fills2))
})

test_that("tsggplot, confidence intervals don't add a stray fill box to the colour legend key", {
  # ggplot2 merges the CI's "fill" legend (draw_tsggplot_ci()) into the
  # series' "colour" legend since there's no separate colour scale for the
  # CI groups -- without override.aes, that merge makes every colour key
  # (including the plain line's, which has no real fill value) draw with
  # the fill geom's default background swatch, showing as a solid black
  # box behind the line.
  ci <- list("KOF Barometer" = list("80" = list(lb = KOF$baro_lo_80, ub = KOF$baro_hi_80)))
  p <- tsggplot(list("KOF Barometer" = KOF$baro_point_fc), ci = ci)

  expect_equal(p$guides$guides$colour$params$override.aes, list(fill = NA))
})

test_that("daily xts", {
  data("sample_matrix", package = "xts")
  x <- xts::as.xts(sample_matrix)

  theme <- init_tsggplot_theme()

  p <- tsggplot(list("KOF Barometer" = x$Open),
    theme = theme
  )

  expect_s3_class(p, "ggplot")
  b <- ggplot2::ggplot_build(p)
  line_x <- b$data[[1]]$x
  expect_equal(length(line_x), length(x$Open))
  expect_equal(length(unique(line_x)), length(x$Open))
  expect_false(anyNA(line_x))
  # the actual values plotted, not just their count/uniqueness -- the
  # sub-day line_to_middle shift is absorbed by scale_x_date(), so each
  # point still lands on its own observation's calendar day
  expect_equal(b$data[[1]]$y, as.numeric(x$Open))
  idx <- zoo::index(x$Open)
  expect_equal(as.Date(line_x, origin = "1970-01-01"), as.Date(idx, tz = attr(idx, "tzone") %||% ""))
})

test_that("weekly xts (#2)", {
  idx <- seq(as.Date("2020-01-06"), as.Date("2020-06-01"), by = "week")
  x <- xts::xts(seq_along(idx), order.by = idx)

  p <- tsggplot(list("Weekly" = x))

  expect_s3_class(p, "ggplot")
  b <- ggplot2::ggplot_build(p)
  line_x <- b$data[[1]]$x
  expect_equal(length(line_x), length(idx))
  expect_equal(length(unique(line_x)), length(idx))
  expect_false(anyNA(line_x))
  expect_equal(b$data[[1]]$y, as.numeric(x))
  # line_to_middle centers each point mid-week (half the 7-day spacing)
  expect_equal(as.Date(line_x, origin = "1970-01-01"), idx + 3.5)
})

test_that("yearly xts (#6)", {
  idx <- as.Date(paste0(2010:2015, "-01-01"))
  x <- xts::xts(1:6, order.by = idx)

  expect_no_error(p <- tsggplot(list("Yearly" = x)))

  expect_s3_class(p, "ggplot")
  b <- ggplot2::ggplot_build(p)
  line_x <- b$data[[1]]$x
  expect_equal(length(line_x), 6)
  expect_equal(length(unique(line_x)), 6)
  expect_false(anyNA(line_x))
  expect_equal(b$data[[1]]$y, as.numeric(x))
  # "annual" isn't a Date-scale frequency (unlike daily/weekly/hourly), so
  # this plots on the numeric decimal-year axis -- centered mid-year
  # (+0.5), same as an annual ts
  expect_equal(line_x, as.numeric(format(idx, "%Y")) + 0.5)
  # x positions fall within the built panel's x range
  panel_range <- b$layout$panel_params[[1]]$x.range
  expect_true(all(line_x >= panel_range[1] & line_x <= panel_range[2]))
})

test_that("hourly xts (#7)", {
  idx <- seq(as.POSIXct("2020-01-01 05:00:00", tz = "UTC"),
    as.POSIXct("2020-01-02 10:00:00", tz = "UTC"),
    by = "hour"
  )
  x <- xts::xts(seq_along(idx), order.by = idx)

  expect_no_error(p <- tsggplot(list("Hourly" = x)))

  expect_s3_class(p, "ggplot")
  b <- ggplot2::ggplot_build(p)
  line_x <- b$data[[1]]$x
  expect_equal(length(line_x), length(idx))
  # every hourly observation gets its own distinct x position (not
  # collapsed onto the same day, which was the #7 bug)
  expect_equal(length(unique(line_x)), length(idx))
  expect_false(anyNA(line_x))
  expect_equal(b$data[[1]]$y, as.numeric(x))
  # hourly uses a real datetime axis (scale_x_datetime, no day-level
  # truncation), so the half-period (30 min = 1800s) shift stays visible --
  # getLineToMiddleShift() must get this right, not just "sub-day and
  # therefore irrelevant" like the Date-scale cases above
  expect_equal(line_x, as.numeric(idx) + 1800)
})

test_that("xts", {
  data("sample_matrix", package = "xts")
  x <- xts::as.xts(sample_matrix)

  theme <- init_tsggplot_theme(
    ci_alpha = 0.2,
    ci_colors = c("red", "blue"),
    ci_legend_label = "%ci_value%% ci for %series% TEST",
    fill_year_with_nas = TRUE
  )

  # Define confidence intervals
  ci <- list(
    "KOF Barometer" = list(
      "95" = list(
        lb = x$Low,
        ub = x$High
      )
    )
  )

  p <- tsggplot(list("KOF Barometer" = x$Open),
    ci = ci,
    theme = theme
  )

  expect_s3_class(p, "ggplot")
  b <- ggplot2::ggplot_build(p)
  is_ci <- vapply(b$plot$layers, function(l) inherits(l$geom, "GeomPolygon"), logical(1))
  is_line <- vapply(b$plot$layers, function(l) inherits(l$geom, "GeomLine"), logical(1))
  line_data <- b$data[is_line][[1]]
  ci_data <- b$data[is_ci][[1]]

  expect_equal(line_data$y, as.numeric(x$Open))
  idx <- zoo::index(x$Open)
  expect_equal(as.Date(line_data$x, origin = "1970-01-01"), as.Date(idx, tz = attr(idx, "tzone") %||% ""))

  # the CI polygon is xmin/xmax as c(x, rev(x)), ymin/ymax as
  # c(lb, rev(ub)) -- its first half should be the lower bound, in order,
  # and its second half the upper bound, reversed
  n <- nrow(x)
  expect_equal(nrow(ci_data), 2 * n)
  expect_equal(ci_data$y[1:n], as.numeric(x$Low))
  expect_equal(ci_data$y[(n + 1):(2 * n)], rev(as.numeric(x$High)))
})

test_that("zoo (#5)", {
  idx <- as.Date("2020-01-01") + 0:23
  x_zoo <- zoo::zoo(1:24, idx)

  expect_no_error(p_zoo <- tsggplot(list("Zoo" = x_zoo)))
  expect_s3_class(p_zoo, "ggplot")

  # should match the equivalent xts input exactly
  p_xts <- tsggplot(list("Zoo" = xts::as.xts(x_zoo)))
  b_zoo <- ggplot2::ggplot_build(p_zoo)
  b_xts <- ggplot2::ggplot_build(p_xts)
  expect_equal(b_zoo$data[[1]]$x, b_xts$data[[1]]$x)
  expect_equal(b_zoo$data[[1]]$y, b_xts$data[[1]]$y)
})

test_that("tsggplot honours highlight_window_alpha", {
  built_alpha <- function(alpha) {
    theme <- init_tsggplot_theme(highlight_window = TRUE, highlight_window_alpha = alpha)
    p <- tsggplot(list(AirPassengers = AirPassengers), theme = theme)
    ix <- which(sapply(p$layers, function(l) inherits(l$geom, "GeomRect")))
    unique(ggplot2::ggplot_build(p)$data[[ix]]$alpha)
  }
  expect_equal(built_alpha(0.1), 0.1)
  expect_equal(built_alpha(0.9), 0.9)
})

test_that("tsggplot applies legend_col to bar legends", {
  tsl <- list(
    a = ts(1:8, start = c(2020, 1), frequency = 4),
    b = ts(8:1, start = c(2020, 1), frequency = 4),
    c = ts(rep(2, 8), start = c(2020, 1), frequency = 4)
  )
  theme <- init_tsggplot_theme(legend_col = 2)

  p <- tsggplot(tsl, left_as_bar = TRUE, theme = theme)
  expect_equal(p$guides$guides$fill$params$ncol, 2)

  p_r <- tsggplot(tsl[1:2], tsr = tsl[3], left_as_bar = TRUE, theme = theme)
  expect_equal(p_r$guides$guides$fill$params$ncol, 2)
})

test_that("tsggplot gives the sum line a legend entry", {
  tsl <- list(
    a = ts(1:8, start = c(2020, 1), frequency = 4),
    b = ts(8:1, start = c(2020, 1), frequency = 4)
  )
  theme <- init_tsggplot_theme(sum_as_line = TRUE, sum_legend = "Total")
  p <- tsggplot(tsl, left_as_bar = TRUE, theme = theme)
  b <- ggplot2::ggplot_build(p)
  sum_layer <- Filter(function(d) "colour" %in% names(d) && !"fill" %in% names(d), b$data)[[1]]
  expect_equal(unique(sum_layer$colour), unname(theme$sum_line_color))
  expect_equal(b$plot$scales$get_scales("colour")$get_labels(), "Total")

  # NULL sum_legend: line stays, no legend entry
  theme_none <- init_tsggplot_theme(sum_as_line = TRUE, sum_legend = NULL)
  p_none <- tsggplot(tsl, left_as_bar = TRUE, theme = theme_none)
  expect_true(any(vapply(p_none$layers, function(l) inherits(l$geom, "GeomLine"), logical(1))))
  expect_length(ggplot2::ggplot_build(p_none)$plot$scales$get_scales("colour")$get_labels(), 0)
})

test_that("tsggplot keeps the sum line out of the right-axis legend", {
  tsl <- list(
    a = ts(1:8, start = c(2020, 1), frequency = 4),
    b = ts(8:1, start = c(2020, 1), frequency = 4)
  )
  tsr <- list(c = ts(c(2, 1, 3, 2, 4, 3, 5, 4), start = c(2020, 1), frequency = 4))
  theme <- init_tsggplot_theme(sum_as_line = TRUE, sum_legend = "Total")
  p <- tsggplot(tsl, tsr = tsr, left_as_bar = TRUE, theme = theme)
  scales <- ggplot2::ggplot_build(p)$plot$scales$scales
  colour_labels <- lapply(
    Filter(function(s) any(grepl("colou?r", s$aesthetics)), scales),
    function(s) s$get_labels()
  )
  expect_length(colour_labels, 2)
  expect_true(any(vapply(colour_labels, identical, logical(1), "Total")))
  expect_true(any(vapply(colour_labels, identical, logical(1), "c")))

  # tsggplotly() converts the merged-legend equivalent, sum line included
  skip_if_not_installed("plotly")
  fig <- plotly::plotly_build(tsggplotly(p))
  expect_true("Total" %in% vapply(fig$x$data, function(d) d$name %||% "", ""))
})

test_that("tsggplot only titles the left axis when labs$y is set, not y_right", {
  tsl <- list(a = ts(1:8, start = c(2020, 1), frequency = 4))
  tsr <- list(b = ts(8:1, start = c(2020, 1), frequency = 4))

  p <- tsggplot(tsl, tsr = tsr, labs = list(y_right = "Right"))
  expect_s3_class(p$theme$axis.title.y, "ggplot2::element_blank")
  expect_s3_class(p$theme$axis.title.y.right, "ggplot2::element_text")

  p_y <- tsggplot(tsl, tsr = tsr, labs = list(y = "Left", y_right = "Right"))
  expect_false(inherits(p_y$theme$axis.title.y, "ggplot2::element_blank"))

  skip_if_not_installed("plotly")
  fig <- plotly::plotly_build(tsggplotly(p, axis_titles = TRUE))
  expect_equal(fig$x$layout$yaxis$title$text, "")
  expect_equal(fig$x$layout$yaxis2$title$text, "Right")
})

test_that("tsggplotly leaves the axes unnamed unless axis_titles = TRUE", {
  skip_if_not_installed("plotly")
  tsl <- list(a = ts(1:8, start = c(2020, 1), frequency = 4))
  tsr <- list(b = ts(8:1, start = c(2020, 1), frequency = 4))
  p <- tsggplot(tsl, tsr = tsr, labs = list(x = "Time", y = "Left", y_right = "Right"))

  layout <- plotly::plotly_build(tsggplotly(p))$x$layout
  expect_equal(layout$xaxis$title$text, "")
  expect_equal(layout$yaxis$title$text, "")
  expect_null(layout$yaxis2$title$text)

  layout_on <- plotly::plotly_build(tsggplotly(p, axis_titles = TRUE))$x$layout
  expect_equal(layout_on$xaxis$title$text, "Time")
  expect_equal(layout_on$yaxis$title$text, "Left")
  expect_equal(layout_on$yaxis2$title$text, "Right")
})
