#' Turn data.frame to Regular Monthly or Quarterly Time Series
#'
#' Turn a data.frame with date columns to a regular time series object
#' if possible. Design to work with quarterly and monthly data.
#'
#' @param dframe data.frame input
#' @param var_cols columns that contain variables as opposed to date index.
#' @param year_col integer, logical or character vector indicating the year
#' position within the data.frame.
#' @param period_col integer, logical or character vector indicating the period
#' position within the data.frame.
#' @param freq integer indicating the frequency of new time series.
#' @param return_ts logical should a (list of) time series be returned? Defaults to TRUE.
#' FALSE returns data.frame.
#' @param by character overwrite automatically detected (from freq) by parameter.
#' e.g. '1 day'. Defaults to NULL.
#' @examples
#' start_m <- as.Date("2017-01-01")
#' df_missing <- data.frame(
#'   date = seq(start_m, by = "2 months", length = 6),
#'   value = 1:6,
#'   another_value = letters[1:6],
#'   yet_another_col = letters[6:1]
#' )
#' df_to_reg_ts(df_missing, c("value", "another_value"))
#' df_to_reg_ts(df_missing, c("value", "another_value"), return_ts = FALSE)
#' @export
#' @importFrom data.table year quarter month
#' @importFrom stats ts
df_to_reg_ts <- function(dframe,
                         var_cols,
                         year_col = "year",
                         period_col = "month",
                         freq = 12,
                         return_ts = T,
                         by = NULL) {
  n_vars <- length(var_cols)
  if (!is.null(by)) by_period <- by
  if (freq == 12) by_period <- "1 month"
  if (freq == 4) by_period <- "1 quarter"

  if ("date" %in% names(dframe)) {
    d_s <- dframe[order(dframe[, "date"]), ]
    d_s[, year_col] <- year(d_s[, "date"])
    if (freq == 12) {
      periods <- month(d_s[, "date"])
    } else if (freq == 4) {
      periods <- quarter(d_s[, "date"])
    }
    d_s[, period_col] <- periods
  } else {
    # sort the data.frame, so all periods within a year are in the right order.
    d_s <- dframe[order(
      dframe[, year_col],
      dframe[, period_col]
    ), ]
    # append a date column in order to compare with a full date column.
    d_s$date <- as.Date(paste(d_s[, year_col], d_s[, period_col], "01", sep = "-"))
  }

  # start and end end date to construct a full date vector
  start_period <- c(d_s[1, year_col], d_s[1, period_col])
  end_period <- c(d_s[nrow(d_s), year_col], d_s[nrow(d_s), period_col])
  full_dates <- seq(
    from = as.Date(d_s$date[1]), to = as.Date(d_s$date[nrow(d_s)]),
    by = by_period
  )

  # create a full matrix of NAs to have the maximum amount of observations
  # observations that are not missing are replaced with the actual values
  # later on
  na_m <- matrix(rep(NA, n_vars * length(full_dates)), ncol = n_vars)
  # combine na matrix with date in order to use indexing
  na_df <- data.frame(date = full_dates, na_m)
  # TRUE / FALSE matrix
  tf_m <- matrix(rep(full_dates %in% d_s$date, n_vars), ncol = n_vars)
  # use matrix indexing to replace all NAs
  na_df[, -1][tf_m] <- as.matrix(d_s[, var_cols])
  names(na_df)[-1] <- var_cols

  if (return_ts) {
    lapply(na_df[, -1], ts, start = as.numeric(start_period), end = as.numeric(end_period), frequency = freq)
  } else {
    na_df
  }
}
