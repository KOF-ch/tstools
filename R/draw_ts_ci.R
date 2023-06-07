#' @importFrom graphics polygon
draw_ts_ci <- function(ci, theme) {
  if (!is.null(ci)) {
    ci_colors <- namedColor2Hex(theme$ci_colors, theme$ci_alpha)
    for (ci_series_i in 1:length(ci)) {
      ci_series <- ci[[ci_series_i]]

      for (ci_level_i in 1:length(ci_series)) {
        ci_level <- ci_series[[ci_level_i]]
        xx <- as.numeric(time(ci_level$lb))

        frq <- frequency(ci_level$lb)

        if (theme$line_to_middle) {
          xx <- xx + (1 / frq) / 2
        }

        yy_low <- ci_level$lb
        yy_high <- ci_level$ub

        polygon(c(xx, rev(xx)), c(yy_low, rev(yy_high)), border = NA, col = ci_colors[ci_series_i])
      }
    }
  }
}
