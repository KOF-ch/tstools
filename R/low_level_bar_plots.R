#' @importFrom graphics rect
draw_ts_bars <- function(x, group_bar_chart = FALSE, theme = NULL) {
  n_ts <- length(x)

  # Matrixify tslist
  x <- do.call(cbind, x)

  # "Remove" NAs (basically rect omits them anyway. Might even be better because of the borders)
  x[is.na(x)] <- 0

  # Base rect coordinates
  ts_time <- time(x)
  frq <- frequency(x)
  positives <- x
  positives[x < 0] <- 0
  negatives <- x
  negatives[x > 0] <- 0

  if (!group_bar_chart || n_ts == 1) {
    # Bars are 1/frq wide, bar_gap is in % of alloted width, half of which needs to go to each side
    offset <- theme$bar_gap / (frq * 200)

    x_pos_left <- rep(ts_time + offset, each = n_ts)
    x_pos_right <- rep(ts_time + 1 / frq - offset, each = n_ts)

    h_pos <- rbind(rectbase = 0, apply(t(positives), 2L, cumsum))
    rect(x_pos_left,
      h_pos[1:n_ts, ],
      x_pos_right,
      h_pos[(1:n_ts) + 1, ],
      border = theme$bar_border,
      lwd = theme$bar_border_lwd,
      col = theme$bar_fill_color[1:n_ts]
    )

    h_neg <- rbind(rectbase = 0, apply(t(negatives), 2L, cumsum))
    rect(rep(ts_time + offset, each = n_ts),
      h_neg[1:n_ts, ],
      rep(ts_time + 1 / frq - offset, each = n_ts),
      h_neg[(1:n_ts) + 1, ],
      border = theme$bar_border,
      lwd = theme$bar_border_lwd,
      col = theme$bar_fill_color[1:n_ts]
    )
  } else {
    inter_group_offset_value <- theme$bar_group_gap / (frq * 200)

    x_pos_raw <- t(c(ts_time) + rep(1, length(ts_time)) %*% t(seq(0, 1 / frq, 1 / (n_ts * frq))))

    inter_group_offset <- t(rep(inter_group_offset_value, length(ts_time)) %*% t(c(1, rep(0, n_ts - 1), -1)))
    x_pos_group <- x_pos_raw + inter_group_offset

    x_pos_final <- apply(x_pos_group, 2, function(x) {
      seq(x[1], x[n_ts + 1], length.out = n_ts + 1)
    })

    x_left <- apply(x_pos_final, 2, function(x) {
      grp <- x[n_ts + 1] - x[1]
      mar <- grp * theme$bar_gap * theme$use_bar_gap_in_groups / 100
      bar <- (grp - (n_ts - 1) * mar) / n_ts
      seq(x[1], x[n_ts + 1] - bar, by = bar + mar)
    })

    x_right <- apply(x_pos_final, 2, function(x) {
      grp <- x[n_ts + 1] - x[1]
      mar <- grp * theme$bar_gap * theme$use_bar_gap_in_groups / 100
      bar <- (grp - (n_ts - 1) * mar) / n_ts
      seq(x[1] + bar, x[n_ts + 1], by = bar + mar)
    })

    rect(
      x_left,
      t(negatives),
      x_right,
      t(positives),
      border = theme$bar_border,
      lwd = theme$bar_border_lwd,
      col = theme$bar_fill_color[1:n_ts]
    )
  }
}
