find_group_coords <- function(x, theme, i) {
  ts_time <- time(x)
  COLS <- ncol(x)
  FRQ <- frequency(x)
  T_SPACE <- 1 / FRQ

  T_MARGIN_SPACE <- theme$total_bar_margin_pct * T_SPACE
  MARGIN_SPACE <- T_MARGIN_SPACE / 2
  T_BAR_SPACE <- (1 - theme$total_bar_margin_pct) * T_SPACE
  BAR_SPACE <- T_BAR_SPACE / COLS
  # XL vector
  coords <- list()
  coords$xl <- cumsum(c(0, rep(BAR_SPACE, COLS - 1))) +
    MARGIN_SPACE +
    ts_time[i]

  # YB vector
  coords$yb <- rep(0, COLS)

  # XR vector
  coords$xr <- coords$xl + BAR_SPACE

  # YT vector
  coords$yt <- apply(x, 1, function(x) x)[, i]
  coords
}
