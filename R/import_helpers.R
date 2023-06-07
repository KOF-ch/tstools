# Helper to turn api-returned json (lists) into lists of ts objects

#' @importFrom stats as.ts start end
json_to_ts <- function(json_data) {
  xt <- xts(json_data$value, order.by = as.yearmon(json_data$date))

  if (frequency(xt) < Inf) {
    as.ts(xt, start = start(xt), end = end(xt))
  } else {
    xt
  }
}

#' Transform a long format data.frame of time series to a tslist
#'
#' The data.frame must have three columns "date", "value" and "series" (identifying the time series)
#' @param data data.frame The data.frame to be transformed
#' @param keep_last_freq_only in case there is a frequency change in a time series,
#' should only the part of the series be returned that has the same frequency as
#' the last observation. This is useful when data start out crappy and then stabilize
# after a while. Defaults to FALSE. Hence only the last part of the series is returned.
#' @param force_xts logical
#' @param strip_nas logical should NAs be stripped (no leading and trailing nas) ?
#' @importFrom data.table dcast
#' @importFrom zoo na.trim
#' @export
long_to_ts <- function(data, keep_last_freq_only = FALSE, force_xts = FALSE,
                       strip_nas = TRUE) {
  data_dt <- as.data.table(data)

  # Strip series consisting only of NAs
  empty_series <- data_dt[, list(is_empty = all(is.na(value))), by = series]

  if (empty_series[, any(is_empty)]) {
    warning(sprintf(
      "Some series contained only NAs and were stripped:\n%s",
      paste(empty_series[is_empty == TRUE, series], collapse = "\n")
    ))
  }

  data_dt <- data_dt[!(series %in% empty_series[is_empty == TRUE, series])]
  # this helps to read in yearly data, otherwise the fact that date is
  # a character would break zoo's as.yearmon below.
  # not an optimal solution but a good fix to read in yearly data w/o complaints.
  if (all(grepl("^[0-9]{4}\\s*$", data_dt$date))) {
    data_dt[, date := as.numeric(date)]
  }

  data_dt[, `:=`(date_zoo = as.numeric(as.yearmon(date)), frq = 12), by = series]

  data_dt[is.na(date_zoo), `:=`(date_zoo = as.numeric(as.yearqtr(date)), frq = 4)]

  dt_of_lists <- data_dt[,
    {
      if (.N == 1) {
        NULL
      } else {
        dT <- diff(date_zoo)
        if (any(abs(diff(dT)) > 1e-6) || force_xts) {
          if (keep_last_freq_only && !force_xts) {
            # find last frequency shift in order to keep only
            # the data which has the same frequency as the end of the series
            # this is useful when data start out crappy and then stabilize
            # after a while.
            l <- length(dT)
            use_only <- (max(which(dT != dT[l])) + 1):(l + 1)
            list(ts_object = list(ts(value[use_only],
              start = .SD[use_only[1], date_zoo],
              end = .SD[.N, date_zoo], deltat = dT[use_only[1]]
            )))
          } else {
            if (any(dT == 0)) {
              # Daily series
              list(ts_object = list(xts(value, order.by = as.Date(date))))
            } else if (frq[1] == 4) {
              list(ts_object = list(xts(value, order.by = as.yearqtr(date_zoo))))
            } else {
              list(ts_object = list(xts(value, order.by = as.yearmon(date_zoo))))
            }
          }
        } else {
          list(ts_object = list(ts(value,
            start = .SD[1, date_zoo],
            end = .SD[.N, date_zoo], deltat = dT[1]
          )))
        }
      }
    },
    by = series
  ]

  dropped <- setdiff(data_dt$series, dt_of_lists$series)
  if (length(dropped) > 0) {
    message(
      "dropped: \n", paste(dropped, collapse = " \n"),
      "\n\nFrequency cannot be detected in time series of length 1!"
    )
  }

  tslist <- dt_of_lists[, ts_object]
  if (strip_nas) {
    tslist <- lapply(tslist, function(x) {
      strip_ts_of_leading_nas(strip_ts_of_trailing_nas(x))
    })
  }

  names(tslist) <- dt_of_lists[, series]
  tslist
}



utils::globalVariables(c("date_zoo", "series", "ts_object", "value", "frq", "is_empty"))

#' Transform a wide format data.frame into a tslist
#'
#' The time series in the data.frame may be stored either rowwise or columnswise.
#' The identifying column must be called date (for columnwise) or series (for rowwise)
#' @param data data.frame The data.frame to be transformed
#' @param keep_last_freq_only in case there is a frequency change in a time series,
#' should only the part of the series be returned that has the same frequency as
#' the last observation. This is useful when data start out crappy and then stabilize
#' after a while. Defaults to FALSE. Hence only the last part of the series is returned.
#' @param force_xts boolean force xts format? Defaults to FALSE.
#' @importFrom xts xts
#' @importFrom zoo as.yearqtr as.yearmon
#' @export
wide_to_ts <- function(data, keep_last_freq_only = FALSE, force_xts = FALSE) {
  if (!("date" %in% names(data))) {
    # Data was written in transposed format
    long_to_ts(melt(data, id.vars = "series", variable.name = "date"),
      keep_last_freq_only = keep_last_freq_only,
      force_xts = force_xts
    )
  } else {
    long_to_ts(melt(data, id.vars = "date", variable.name = "series"),
      keep_last_freq_only = keep_last_freq_only,
      force_xts = force_xts
    )
  }
}
