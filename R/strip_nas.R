#' Strip Leading / Trailing NAs from a Time Series Object
#'
#' Removes NAs to begin with and starts time series index at the first non-NA value.
#'
#'
#' @param s an object of class ts.
#' @rdname strip_nas
#'
#' @importFrom stats window
#' @export
strip_ts_of_leading_nas <- function(s) {
  if (!is.na(s[1])) {
    s
  } else {
    nas <- which(is.na(s))
    # when all difference are zero, just take the last
    # NA in line, otherwise only use the first to go beyond 1
    if (all(diff(nas) == 1)) {
      end <- nas[length(nas)] + 1
    } else {
      end <- min(which(diff(nas) > 1)) + 1
    }

    if (end == Inf) {
      start_time <- time(s)[which(!is.na(s))[1]]
      end_time <- time(s)[length(s)]
    } else {
      start_time <- time(s)[end]
      end_time <- time(s)[length(s)]
    }

    window(s, start = start_time, end = end_time)
  }
}

#' @rdname strip_nas
#' @importFrom stats start
#' @export
strip_ts_of_trailing_nas <- function(s) {
  if (is.null(dim(s))) {
    ntf <- is.na(s)
  } else {
    ntf <- apply(s, 1, function(x) all(is.na(x)))
  }

  if (!any(ntf)) {
    return(s)
  }

  na_pos <- which(ntf)
  sqntl <- length(ntf) - na_pos
  if (rev(sqntl)[1] != 0) {
    return(s)
  } else {
    rmv <- na_pos[sqntl - 1 <= 1]
    if (is.null(dim(s))) {
      ts(s[-rmv], start = start(s), frequency = frequency(s))
    } else {
      ts(s[-rmv, ], start = start(s), frequency = frequency(s))
    }
  }
}
