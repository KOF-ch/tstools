test_that("draw_tsggplot_lines, ts", {
  x <- list(AirPassengers = AirPassengers, JohnsonJohnson = JohnsonJohnson)
  x <- lapply(x, xts::as.xts)
  p <- ggplot()
  theme <- init_tsggplot_theme()
  theme$NA_continue_line <- rep(theme$NA_continue_line, 2)
  theme$show_points <- rep(theme$show_points, 2)
  out <- draw_tsggplot_lines(p, x, theme = theme)

  # one GeomLine layer per series (show_points is FALSE, so no geom_point)
  expect_length(out$layers, 2)
  expect_true(all(vapply(out$layers, function(l) inherits(l$geom, "GeomLine"), logical(1))))

  bd <- ggplot_build(out)$data
  for (i in seq_along(x)) {
    # each layer's own raw data is mapped to its own series -- the actual
    # colour value comes from a scale_color_manual() the caller adds later,
    # not from draw_tsggplot_lines() itself, so what's checked here is the
    # mapping (one series per layer), not a rendered colour
    expect_equal(unique(as.character(out$layers[[i]]$data$series)), names(x)[i])
    # x/y actually reflect the series' own values, shifted to the middle of
    # its period like tsplot (line_to_middle = TRUE by default)
    expect_equal(bd[[i]]$x, getNumericTimeIndex(x[[i]]) + getLineToMiddleShift(x[[i]]))
    expect_equal(bd[[i]]$y, as.numeric(x[[i]]))
  }
})

test_that("draw_tsggplot_lines, xts", {
  data("sample_matrix", package = "xts")
  x <- list(x = xts::as.xts(sample_matrix)[, "Open"])
  p <- ggplot()
  theme <- init_tsggplot_theme()
  out <- draw_tsggplot_lines(p, x, theme = theme)

  expect_length(out$layers, 1)
  expect_true(inherits(out$layers[[1]]$geom, "GeomLine"))

  bd <- ggplot_build(out)$data[[1]]
  expect_equal(nrow(bd), length(x[[1]]))
  expect_equal(bd$x, getNumericTimeIndex(x[[1]]) + getLineToMiddleShift(x[[1]]))
  expect_equal(bd$y, as.numeric(x[[1]]))
})
