test_that("getGlobalXInfo_tsggplot, ts", {
  theme <- init_tsggplot_theme()
  tsl <- list(AirPassengers = AirPassengers)
  out <- getGlobalXInfo_tsggplot(
    tsl, NULL,
    theme$fill_year_with_nas,
    theme$fill_up_start,
    theme$axis_x_tick_dt,
    theme$axis_x_label_dt,
    NULL
  )

  expected <- list(
    x_range = c(1949, 1961),
    yearly_tick_pos = c(
      1949, 1950, 1951, 1952, 1953, 1954, 1955,
      1956, 1957, 1958, 1959, 1960, 1961
    ),
    year_labels_start = c(
      1949, 1950, 1951, 1952, 1953, 1954, 1955,
      1956, 1957, 1958, 1959, 1960, 1961
    ),
    min_year = 1949,
    max_year = 1961,
    quarterly_tick_pos = c(
      1949, 1949.25, 1949.5, 1949.75, 1950,
      1950.25, 1950.5, 1950.75, 1951, 1951.25, 1951.5, 1951.75,
      1952, 1952.25, 1952.5, 1952.75, 1953, 1953.25, 1953.5, 1953.75,
      1954, 1954.25, 1954.5, 1954.75, 1955, 1955.25, 1955.5, 1955.75,
      1956, 1956.25, 1956.5, 1956.75, 1957, 1957.25, 1957.5, 1957.75,
      1958, 1958.25, 1958.5, 1958.75, 1959, 1959.25, 1959.5, 1959.75,
      1960, 1960.25, 1960.5, 1960.75, 1961
    ), dominant_freq = "ts"
  )
  expect_equal(out, expected)

  # with monthly xts instead of ts
  tsl <- list(AirPassengers = xts::as.xts(AirPassengers))
  out <- getGlobalXInfo_tsggplot(
    tsl, NULL,
    fill_up = theme$fill_year_with_nas,
    fill_up_start = theme$fill_up_start,
    tick_dt = theme$axis_x_tick_dt,
    label_dt = theme$axis_x_label_dt,
    manual_ticks = NULL
  )
  expected$dominant_freq <- "monthly"
  expect_equal(out, expected)
})

test_that("getGlobalXInfo_tsggplot, xts", {
  theme <- init_tsggplot_theme(fill_year_with_nas = TRUE)
  data("sample_matrix", package = "xts")
  tsl <- list(sample = xts::as.xts(sample_matrix))
  out <- getGlobalXInfo_tsggplot(
    tsl, NULL,
    fill_up = theme$fill_year_with_nas,
    fill_up_start = theme$fill_up_start,
    tick_dt = theme$axis_x_tick_dt,
    label_dt = theme$axis_x_label_dt,
    manual_ticks = NULL
  )

  expected <-
    list(
      x_range = structure(c(13515, 13879), class = "Date"),
      yearly_tick_pos = structure(c(13514, 13879), class = "Date"),
      year_labels_start = c(2007, 2008), min_year = 2007, max_year = 2008,
      quarterly_tick_pos = structure(c(
        13514, 13604, 13695, 13787,
        13879
      ), class = "Date"), dominant_freq = "daily"
    )

  expect_equal(out, expected)
})

test_that("getGlobalXInfo_tsggplot, xts, don't fill year", {
  theme <- init_tsggplot_theme(fill_year_with_nas = FALSE)
  data("sample_matrix", package = "xts")
  tsl <- list(sample = xts::as.xts(sample_matrix))
  out <- getGlobalXInfo_tsggplot(
    tsl, NULL,
    fill_up = theme$fill_year_with_nas,
    fill_up_start = theme$fill_up_start,
    tick_dt = theme$axis_x_tick_dt,
    label_dt = theme$axis_x_label_dt,
    manual_ticks = NULL
  )

  expected <-
    list(
      x_range = structure(c(13515, 13739), class = "Date"),
      yearly_tick_pos = structure(13514, class = "Date"),
      year_labels_start = 2007, min_year = 2007, max_year = 2007,
      quarterly_tick_pos = structure(c(13514, 13604, 13695), class = "Date"),
      dominant_freq = "daily"
    )

  expect_equal(out, expected)
})

test_that("getGlobalXInfo_tsggplot, xts", {
  theme <- init_tsggplot_theme()
  tsl <- list(
    AirPassengers = xts::as.xts(AirPassengers),
    JohnsonJohnson = xts::as.xts(JohnsonJohnson)
  )
  out <- getGlobalXInfo_tsggplot(
    tsl, NULL,
    theme$fill_year_with_nas,
    theme$fill_up_start,
    theme$axis_x_tick_dt,
    theme$axis_x_label_dt,
    NULL
  )

  expected <- list(
    x_range = c(1949, 1981),
    yearly_tick_pos = c(
      1949, 1950, 1951, 1952, 1953, 1954, 1955, 1956, 1957, 1958, 1959, 1960,
      1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, 1971, 1972,
      1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981
    ),
    year_labels_start = c(
      1949,
      1950, 1951, 1952, 1953, 1954, 1955, 1956, 1957, 1958, 1959, 1960,
      1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, 1971,
      1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981
    ),
    min_year = 1949,
    max_year = 1981,
    quarterly_tick_pos = c(
      1949, 1949.25, 1949.5,
      1949.75, 1950, 1950.25, 1950.5, 1950.75, 1951, 1951.25, 1951.5,
      1951.75, 1952, 1952.25, 1952.5, 1952.75, 1953, 1953.25, 1953.5,
      1953.75, 1954, 1954.25, 1954.5, 1954.75, 1955, 1955.25, 1955.5,
      1955.75, 1956, 1956.25, 1956.5, 1956.75, 1957, 1957.25, 1957.5,
      1957.75, 1958, 1958.25, 1958.5, 1958.75, 1959, 1959.25, 1959.5,
      1959.75, 1960, 1960.25, 1960.5, 1960.75, 1961, 1961.25, 1961.5,
      1961.75, 1962, 1962.25, 1962.5, 1962.75, 1963, 1963.25, 1963.5,
      1963.75, 1964, 1964.25, 1964.5, 1964.75, 1965, 1965.25, 1965.5,
      1965.75, 1966, 1966.25, 1966.5, 1966.75, 1967, 1967.25, 1967.5,
      1967.75, 1968, 1968.25, 1968.5, 1968.75, 1969, 1969.25, 1969.5,
      1969.75, 1970, 1970.25, 1970.5, 1970.75, 1971, 1971.25, 1971.5,
      1971.75, 1972, 1972.25, 1972.5, 1972.75, 1973, 1973.25, 1973.5,
      1973.75, 1974, 1974.25, 1974.5, 1974.75, 1975, 1975.25, 1975.5,
      1975.75, 1976, 1976.25, 1976.5, 1976.75, 1977, 1977.25, 1977.5,
      1977.75, 1978, 1978.25, 1978.5, 1978.75, 1979, 1979.25, 1979.5,
      1979.75, 1980, 1980.25, 1980.5, 1980.75, 1981
    ),
    dominant_freq = "monthly"
  )

  expect_equal(out, expected)
})

test_that("getGlobalXInfo_tsggplot adapts the trailing pad to the series span (axis_x_pad)", {
  theme <- init_tsggplot_theme(fill_year_with_nas = FALSE)

  # a short series gets a small, proportional pad rather than the old
  # fixed one-quarter margin (which would dwarf a few days of data)
  short_ts <- ts(1:5, start = c(2010, 1), frequency = 365)
  out_short <- getGlobalXInfo_tsggplot(
    list(A = short_ts), NULL,
    theme$fill_year_with_nas, theme$fill_up_start,
    theme$axis_x_tick_dt, theme$axis_x_label_dt, NULL
  )
  span <- diff(range(stats::time(short_ts)))
  expect_true(diff(out_short$x_range) > span)
  expect_true(diff(out_short$x_range) < 0.25)

  # a long series still gets ~the classic one-quarter margin
  out_long <- getGlobalXInfo_tsggplot(
    list(A = AirPassengers), NULL,
    theme$fill_year_with_nas, theme$fill_up_start,
    theme$axis_x_tick_dt, theme$axis_x_label_dt, NULL
  )
  expect_equal(out_long$x_range[2] - max(stats::time(AirPassengers)), 0.25, tolerance = 0.01)

  # an explicit axis_x_pad overrides the automatic scaling entirely
  out_pad0 <- getGlobalXInfo_tsggplot(
    list(A = short_ts), NULL,
    theme$fill_year_with_nas, theme$fill_up_start,
    theme$axis_x_tick_dt, theme$axis_x_label_dt, NULL,
    pad = 0
  )
  expect_equal(out_pad0$x_range[2], max(stats::time(short_ts)))

  # same adaptive behavior for the daily/weekly (Date-scale) branch, which
  # otherwise recomputes x_range from the raw index and would drop the pad
  short_daily <- xts::xts(1:5, order.by = seq(as.Date("2023-01-01"), by = "day", length.out = 5))
  out_daily <- getGlobalXInfo_tsggplot(
    list(A = short_daily), NULL,
    theme$fill_year_with_nas, theme$fill_up_start,
    theme$axis_x_tick_dt, theme$axis_x_label_dt, NULL
  )
  expect_true(out_daily$x_range[2] > as.Date("2023-01-05"))
  expect_true(out_daily$x_range[2] < as.Date("2023-01-05") + 30)
})

test_that("getGlobalXInfo_tsggplot adds no trailing pad once the year is filled up (like tsplot)", {
  theme <- init_tsggplot_theme()
  expect_true(theme$fill_year_with_nas)

  for (x in list(
    ts(rnorm(24), end = c(2023, 4), frequency = 4),
    ts(rnorm(24), end = c(2023, 2), frequency = 4),
    ts(rnorm(60), end = c(2023, 6), frequency = 12)
  )) {
    out <- getGlobalXInfo_tsggplot(
      list(A = x), NULL,
      theme$fill_year_with_nas, theme$fill_up_start,
      theme$axis_x_tick_dt, theme$axis_x_label_dt, NULL
    )
    expected <- getGlobalXInfo(
      list(x), NULL,
      theme$fill_year_with_nas, theme$fill_up_start, 1, NULL
    )
    expect_equal(out$x_range, expected$x_range)
    expect_equal(out$x_range[2], 2024)
  }
})

test_that("tsggplot doesn't error when the axis is too short for a quarterly tick", {
  short_daily <- xts::xts(1:5, order.by = seq(as.Date("2023-01-01"), by = "day", length.out = 5))
  expect_no_error(tsggplot(
    list(A = short_daily),
    theme = init_tsggplot_theme(fill_year_with_nas = FALSE)
  ))
})

test_that("tsggplot still shows an x-axis label when only one yearly tick fits (short monthly series)", {
  # a genuinely numeric-scale (non date/datetime) series short enough that
  # its padded range contains only one yearly tick
  short_monthly <- ts(1:3, start = c(2023, 1), frequency = 12)
  p <- tsggplot(
    list(A = short_monthly),
    theme = init_tsggplot_theme(fill_year_with_nas = FALSE)
  )

  meta <- attr(p, "tsggplot_meta")
  expect_equal(length(meta$global_x$yearly_tick_pos), 1)

  b <- ggplot2::ggplot_build(p)
  x_scale <- b$layout$panel_params[[1]]$x
  expect_false(anyNA(x_scale$breaks))
  expect_false(anyNA(x_scale$get_labels()))
  expect_equal(x_scale$get_labels(), 2023)
})

test_that("tsggplot plots hourly xts series on a real datetime x-axis", {
  hourly_idx <- seq(as.POSIXct("2023-01-01", tz = "UTC"), by = "hour", length.out = 5 * 24)
  hourly_xts <- xts::xts(seq_along(hourly_idx), order.by = hourly_idx)
  p <- tsggplot(
    list(A = hourly_xts),
    theme = init_tsggplot_theme(fill_year_with_nas = FALSE)
  )

  meta <- attr(p, "tsggplot_meta")
  expect_true(inherits(meta$global_x$x_range, "POSIXct"))
  expect_true(inherits(meta$global_x$yearly_tick_pos, "POSIXct"))

  # real day-level labels (e.g. "Jan 01"), not a single decimal-year label
  b <- ggplot2::ggplot_build(p)
  x_scale <- b$layout$panel_params[[1]]$x
  labels <- x_scale$get_labels()
  expect_false(anyNA(labels))
  expect_true(all(grepl("^[A-Z][a-z]{2} \\d{2}$", labels)))
  expect_true(length(labels) >= 3)

  # the actual data lines up correctly with that datetime axis (line_to_
  # middle off here so the plotted x isn't shifted by half a period)
  p_no_shift <- tsggplot(
    list(A = hourly_xts),
    theme = init_tsggplot_theme(fill_year_with_nas = FALSE, line_to_middle = FALSE)
  )
  built_data <- ggplot2::ggplot_build(p_no_shift)$data[[1]]
  expect_equal(as.numeric(built_data$x), as.numeric(hourly_idx), tolerance = 1e-6)
})

test_that("tsggplot's daily/weekly x-axis picks sensible tick spacing for its span (axis_x_date_ticks)", {
  dates <- seq(as.Date("2023-01-01"), by = "day", length.out = 5)
  daily_xts <- xts::xts(seq_along(dates), order.by = dates)

  # "auto" (the default) lets ggplot2 pick day-level breaks for a short
  # series instead of the always-year-based spacing, which produces an NA
  # tick for anything shorter than a year
  p_auto <- tsggplot(list(A = daily_xts), theme = init_tsggplot_theme(fill_year_with_nas = FALSE))
  b_auto <- ggplot2::ggplot_build(p_auto)
  x_auto <- b_auto$layout$panel_params[[1]]$x
  expect_false(anyNA(x_auto$breaks))
  expect_true(length(x_auto$breaks) >= 5)

  # "years" keeps the old fixed year-based spacing as an explicit opt-in
  p_years <- tsggplot(
    list(A = daily_xts),
    theme = init_tsggplot_theme(fill_year_with_nas = FALSE, axis_x_date_ticks = "years")
  )
  b_years <- ggplot2::ggplot_build(p_years)
  x_years <- b_years$layout$panel_params[[1]]$x
  expect_equal(x_years$get_labels()[!is.na(x_years$get_labels())], "2023")
})
