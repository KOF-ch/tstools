test_that("ts, adds 1 period", {
  x <- AirPassengers
  out <- fill_year_with_nas(x)

  expect_equal(max(time(out)), 1961)
})

test_that("ts, fills year and adds 1 period", {
  x <- AirPassengers
  x <- stats::window(x, end = c(1960, 10))
  out <- fill_year_with_nas(x)

  expect_equal(max(time(out)), 1961)
})

test_that("ts, fill up start", {
  x <- AirPassengers
  x <- stats::window(x, start = c(1949, 4))
  out <- fill_year_with_nas(x, add_periods = 0, fill_up_start = TRUE)

  expect_equal(min(time(out)), 1949)
  expect_equal(out[1:3], rep(NA_real_, 3))
})

test_that("xts, fills year and adds 1 period", {
  # daily
  data("sample_matrix", package = "xts")
  x <- xts::as.xts(sample_matrix)
  out <- fill_year_with_nas(x, fill_up_start = FALSE)

  expect_s3_class(out, "xts")

  expect_equal(
    xts::periodicity(out)$scale,
    xts::periodicity(x)$scale
  )

  # It should not fill up start of the year
  first_idx <- zoo::index(utils::head(out, 1))
  expect_equal(as.Date(first_idx, tz = ""), as.Date("2007-01-02"))

  # It should fill to the end of the year + 1 extra period
  last_idx <- zoo::index(utils::tail(out, 1))
  expected_last <- seq(
    from = as.Date("2007-12-31"),
    by = "day", length.out = 2
  )[2]

  expect_equal(as.Date(last_idx, tz = ""), expected_last)

  # There should be NA rows beyond original data
  expect_true(any(is.na(utils::tail(out, 2))))
})

test_that("xts, fills up start", {
  # monthly
  x <- stats::window(AirPassengers, start = c(1949, 4))
  x <- xts::as.xts(x)
  out <- fill_year_with_nas(x, add_periods = 0, fill_up_start = TRUE)

  expect_s3_class(out, "xts")

  expect_equal(
    xts::periodicity(out)$scale,
    xts::periodicity(x)$scale
  )

  # It should fill up start of the year
  first_idx <- zoo::index(utils::head(out, 1))
  expect_equal(zoo::as.Date(first_idx, tz = ""), as.Date("1949-01-01"))

  # It should fill to the end of the year
  last_idx <- zoo::index(utils::tail(out, 1))
  expect_equal(zoo::as.Date(last_idx, tz = ""), as.Date("1960-12-01"))

  # There should be NA rows beyond original data
  expect_true(any(is.na(utils::head(out, 2))))
})

test_that("xts, weekly, fills year and adds 1 period", {
  idx <- seq(as.Date("2020-01-06"), as.Date("2020-06-01"), by = "week")
  x <- xts::xts(seq_along(idx), order.by = idx)
  out <- fill_year_with_nas(x, add_periods = 1, fill_up_start = FALSE)

  expect_s3_class(out, "xts")
  expect_equal(xts::periodicity(out)$scale, "weekly")

  first_idx <- zoo::index(utils::head(out, 1))
  expect_equal(as.Date(first_idx, tz = ""), as.Date("2020-01-06"), ignore_attr = TRUE)

  last_idx <- zoo::index(utils::tail(out, 1))
  expected_last <- seq(from = as.Date("2020-12-28"), by = "week", length.out = 2)[2]
  expect_equal(as.Date(last_idx, tz = ""), expected_last, ignore_attr = TRUE)

  expect_true(any(is.na(utils::tail(out, 2))))
})

test_that("xts, yearly, fills year and adds 1 period (#6)", {
  idx <- as.Date(paste0(2010:2015, "-01-01"))
  x <- xts::xts(1:6, order.by = idx)

  expect_equal(xts::periodicity(x)$scale, "yearly")
  expect_no_error(out <- fill_year_with_nas(x, add_periods = 1, fill_up_start = FALSE))

  expect_s3_class(out, "xts")
  expect_equal(length(out), 7)

  first_idx <- zoo::index(utils::head(out, 1))
  expect_equal(as.Date(first_idx, tz = ""), as.Date("2010-01-01"), ignore_attr = TRUE)

  last_idx <- zoo::index(utils::tail(out, 1))
  expect_equal(as.Date(last_idx, tz = ""), as.Date("2016-01-01"), ignore_attr = TRUE)

  expect_true(is.na(utils::tail(out, 1)))
})

test_that("xts, yearly, fill up start", {
  idx <- as.Date(paste0(2011:2015, "-01-01"))
  x <- xts::xts(1:5, order.by = idx)
  out <- fill_year_with_nas(x, add_periods = 0, fill_up_start = TRUE)

  first_idx <- zoo::index(utils::head(out, 1))
  expect_equal(as.Date(first_idx, tz = ""), as.Date("2011-01-01"), ignore_attr = TRUE)
})

test_that("xts, hourly, fills year and adds periods, preserving hour-level index (#7)", {
  idx <- seq(as.POSIXct("2020-01-01 05:00:00", tz = "UTC"),
    as.POSIXct("2020-01-02 10:00:00", tz = "UTC"),
    by = "hour"
  )
  x <- xts::xts(seq_along(idx), order.by = idx)

  expect_equal(xts::periodicity(x)$scale, "hourly")
  expect_no_error(out <- fill_year_with_nas(x, add_periods = 2, fill_up_start = FALSE))

  expect_s3_class(out, "xts")
  # index must stay at hourly (not daily/midnight) resolution
  expect_equal(length(unique(zoo::index(out))), length(out))
  expect_true(all(diff(as.numeric(zoo::index(out))) == 3600))

  first_idx <- zoo::index(utils::head(out, 1))
  expect_equal(first_idx, idx[1], ignore_attr = TRUE)

  # filled to the end of the year (Dec 31, 23:00) plus 2 extra hourly periods
  last_idx <- zoo::index(utils::tail(out, 1))
  expect_equal(
    last_idx,
    as.POSIXct("2021-01-01 01:00:00", tz = "UTC"),
    ignore_attr = TRUE
  )

  expect_true(any(is.na(utils::tail(out, 3))))
})

test_that("zoo, delegates to xts (#5)", {
  idx <- as.Date("2020-01-01") + 0:9
  x_zoo <- zoo::zoo(1:10, idx)
  x_xts <- xts::as.xts(x_zoo)

  out_zoo <- fill_year_with_nas(x_zoo, add_periods = 1, fill_up_start = FALSE)
  out_xts <- fill_year_with_nas(x_xts, add_periods = 1, fill_up_start = FALSE)

  expect_s3_class(out_zoo, "xts")
  expect_equal(zoo::index(out_zoo), zoo::index(out_xts), ignore_attr = TRUE)
  expect_equal(as.numeric(out_zoo), as.numeric(out_xts))
})
