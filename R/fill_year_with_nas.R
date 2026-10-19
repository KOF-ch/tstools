#' Fill Up a Time Series with NAs
#'
#' When plotting a time series you might want set the range of the plot a little wider than just the start and end date of the original series. This function add fills up the current period (typically year) with NA.
#'
#' @param x object of class ts
#' @param add_periods integer periods to add.
#' @param fill_up_start logical should start year be filled up? Defaults to FALSE.
#' @export
#' @importFrom stats start end
fill_year_with_nas <- function(x, add_periods = 1,
                               fill_up_start = FALSE) {
  UseMethod("fill_year_with_nas")
}

#' @export
fill_year_with_nas.ts <- function(x, add_periods = 1,
                                  fill_up_start = FALSE) {
  frq <- frequency(x)
  de <- frq - end(x)[2]
  ds <- start(x)[2] - 1
  new_start <- c(start(x)[1], 1)
  if (fill_up_start) {
    ts(c(rep(NA, ds), x, rep(NA, de + add_periods)),
      start = new_start,
      frequency = frq
    )
  } else {
    ts(c(x, rep(NA, de + add_periods)),
      start = start(x),
      frequency = frq
    )
  }
}

#' @export
fill_year_with_nas.xts <- function(x, add_periods = 1,
                                   fill_up_start = FALSE) {
  if (!xts::is.xts(x)) stop("Input must be an xts object.")
  if (add_periods < 0) stop("add_periods must be >= 0.")

  tz <- attr(x, "tzone")
  if (is.null(tz)) {
    tz <- attr(attr(x, "index"), "tzone")
  }
  if (is.null(tz)) {
    tz <- ""
  }

  per <- xts::periodicity(x)$scale

  # Sub-daily data (e.g. hourly) can't be represented by the Date-based
  # sequence below (a Date has no hour-of-day), so it is handled separately,
  # staying in POSIXct throughout instead of snapping to midnight.
  if (per %in% c("hourly")) {
    start_dt <- as.POSIXct(start(x), tz = tz)
    end_dt <- as.POSIXct(end(x), tz = tz)

    year_start_dt <- as.POSIXct(paste0(format(start_dt, "%Y"), "-01-01 00:00:00"), tz = tz)
    year_end_dt <- as.POSIXct(paste0(format(end_dt, "%Y"), "-12-31 23:00:00"), tz = tz)

    seq_start_dt <- if (fill_up_start) year_start_dt else start_dt
    full_idx <- seq(seq_start_dt, year_end_dt, by = "hour")

    if (add_periods > 0) {
      extra_idx <- seq(
        from = utils::tail(full_idx, 1),
        by = "hour", length.out = add_periods + 1
      )[-1]
      full_idx <- c(full_idx, extra_idx)
    }

    return(merge(x, full_idx))
  }

  step <- switch(per,
    "daily" = "day",
    "weekly" = "week",
    "monthly" = "month",
    "quarterly" = "quarter",
    "yearly" = "year",
    stop("Unsupported frequency: ", per)
  )

  start_d <- switch(per,
    "monthly"   = zoo::as.Date(zoo::as.yearmon(start(x)), tz = tz),
    "quarterly" = zoo::as.Date(zoo::as.yearqtr(start(x)), tz = tz),
    "weekly"    = as.Date(start(x), tz = tz),
    "daily"     = as.Date(start(x), tz = tz),
    "yearly"    = as.Date(start(x), tz = tz),
    stop("Unsupported frequency: ", per)
  )
  end_d <- switch(per,
    "monthly"   = zoo::as.Date(zoo::as.yearmon(end(x)), tz = tz),
    "quarterly" = zoo::as.Date(zoo::as.yearqtr(end(x)), tz = tz),
    "weekly"    = as.Date(end(x), tz = tz),
    "daily"     = as.Date(end(x), tz = tz),
    "yearly"    = as.Date(end(x), tz = tz),
    stop("Unsupported frequency: ", per)
  )

  year_start <- as.Date(paste0(format(start_d, "%Y"), "-01-01"))
  year_end <- as.Date(paste0(format(end_d, "%Y"), "-12-31"))

  seq_start <- if (fill_up_start) year_start else start_d
  full_idx <- seq(seq_start, year_end, by = step)

  if (add_periods > 0) {
    extra_idx <- seq(
      from = utils::tail(full_idx, 1),
      by = step, length.out = add_periods + 1
    )[-1]
    full_idx <- c(full_idx, extra_idx)
  }

  full_idx <- as.POSIXct(paste(full_idx, "00:00:00"), format = "%Y-%m-%d %H:%M:%S", tz = tz)

  merge(x, full_idx)
}


#' @export
fill_year_with_nas.zoo <- function(x, add_periods = 1,
                                   fill_up_start = FALSE) {
  fill_year_with_nas(xts::as.xts(x),
    add_periods = add_periods,
    fill_up_start = fill_up_start
  )
}
