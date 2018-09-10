# TODO: Order ci by value, for the pretties

#' @importFrom graphics par plot legend
#' @importFrom grDevices dev.size
#' @importFrom stats na.omit
add_legend <- function(tsln,
                      tsrn = NULL,
                      ci_names,
                      left_as_bar = FALSE,
                      left_as_band = FALSE,
                      theme = init_tsplot_theme()){
  # Grab the number of legends
  ll <- length(tsln)
  lr <- length(tsrn)
  lb <- length(c(tsln,tsrn))
  
  # Calculate how far below the top edge of the plot to place the legend
  plt <- par("plt")
  
  # theme$legend_margin_top is specified in % of device height so we need to scale it to % of plot height here
  inset_y <- 1 + theme$legend_margin_top/(100*(plt[4] - plt[3]))

  # Make vectors that "wrap around"
  theme$line_colors <- rep(theme$line_colors, ceiling(lb/length(theme$line_colors)))
  theme$bar_fill_colors <- rep(theme$bar_fill_color, ceiling(ll/length(theme$bar_fill_color)))
  theme$lty <- rep(theme$lty, ceiling(lb/length(theme$lty)))
  theme$lwd <- rep(theme$lwd, ceiling(lb/length(theme$lwd)))
  theme$point_symbol <- rep(theme$point_symbol, ceiling(lb/length(theme$point_symbol)))
  
  # Helper to insert ci legends at the correct positions
  splice_ci_names <- function(ts_names) {
    unlist(lapply(ts_names, function(x) {
      c(x, ci_names[[x]])
    }))
  }
  
  # Insert ci legends (if any)
  legend_l <- splice_ci_names(tsln)
  n_tot_l <- length(legend_l)
  
  # Initialize legend pch, col, lty and lwd with the theme parameters
  # where there are ts and ci band params where there are those
  is_ci_l <- !(legend_l %in% tsln)
  pch_l <- `if`(left_as_bar || left_as_band,
                rep(15, n_tot_l), 
                ifelse(theme$show_points, theme$point_symbol, NA)) # TODO: in case of T, F, T, do we want 1, NA, 3 or 1, NA, 2??
  pch_l[is_ci_l] <- 15
  col_l <- rep(NA, n_tot_l)
  col_l[!is_ci_l] <- theme$line_colors[1:ll]
  lty_l <- rep(0, n_tot_l)
  lty_l[!is_ci_l] <- theme$lty[1:ll]
  lwd_l <- rep(0, n_tot_l)
  lwd_l[!is_ci_l] <- theme$lwd[1:ll]
  
  # Set the legend colors of the ci bands 
  ci_color_indices_l <- cumsum(!is_ci_l)[is_ci_l]
  ci_legend_colors_l <- c()
  left_ci_colors <- theme$ci_colors[1:ll]
  for(i in unique(ci_color_indices_l)) {
    ci_legend_colors_l <- c(
      ci_legend_colors_l,
      rev(getCiLegendColors(left_ci_colors[i], sum(ci_color_indices_l == i), theme$ci_alpha))[order(ci_names[[tsln[i]]])]
    )
  }
  
  col_l[is_ci_l] <- ci_legend_colors_l
  
  # If left as bar, do not draw lines in the legend
  if(left_as_bar || left_as_band) {
    if(left_as_bar) {
      col_l[!is_ci_l] <- theme$bar_fill_color[1:ll]
    } else {
      col_l[!is_ci_l] <- theme$band_fill_color[1:ll]
    }
    lty_l[!is_ci_l] <- 0
    # Add sum line legend if necessary
    if(theme$sum_as_line && !is.null(theme$sum_legend)) {
      legend_l <- c(legend_l, theme$sum_legend)
      lty_l <- c(lty_l, theme$sum_line_lty)
      lwd_l <- c(lwd_l, theme$sum_line_lwd)
      col_l <- c(col_l, theme$sum_line_color)
      pch_l <- c(pch_l, NA)
    }
  }
  
  
  # initialize right legend params
  
  legend_r <- splice_ci_names(tsrn)
  n_tot_r <- length(legend_r)
  is_ci_r <- !(legend_r %in% tsrn)
  pch_r <- ifelse(theme$show_points, 
                  `if`(left_as_bar || left_as_band, theme$point_symbol[1:lr], theme$point_symbol[(ll+1):lb]),
                  NA)
  pch_r[is_ci_r] <- 15
  col_r <- rep(NA, n_tot_r)
  col_r[!is_ci_r] <- theme$line_colors[`if`(left_as_bar || left_as_band, 1:lr, (ll+1):lb)]
  
  ci_color_indices_r <- cumsum(!is_ci_r)[is_ci_r]
  ci_legend_colors_r <- c()
  right_ci_colors <- theme$ci_colors[`if`(left_as_bar || left_as_band, 1:lr, (ll+1):lb)]
  for(i in unique(ci_color_indices_r)) {
    ci_legend_colors_r <- c(
      ci_legend_colors_r, 
      rev(getCiLegendColors(right_ci_colors[i], sum(ci_color_indices_r == i), theme$ci_alpha))[order(ci_names[[tsrn[i]]])]
    )
  }
  
  col_r[is_ci_r] <- ci_legend_colors_r #namedColor2Hex(theme$ci_colors[ifelse(left_as_bar || left_as_band, 1:lr, (ll+1):lb)], theme$ci_alpha)[cumsum(!is_ci_r)[is_ci_r]]
  lty_r <- rep(0, n_tot_r)
  lty_r[!is_ci_r] <- theme$lty[`if`(left_as_bar || left_as_band, 1:lr, (ll+1):lb)]
  lwd_r <- rep(0, n_tot_r)
  lwd_r[!is_ci_r] <- theme$lwd[`if`(left_as_bar || left_as_band, 1:lr, (ll+1):lb)]
  
  
  # Merge left and right legends if desired
  if(theme$legend_all_left) {
    legend_l <- c(legend_l, legend_r)
    col_l <- c(col_l, col_r)
    lty_l <- c(lty_l, lty_r)
    lwd_l <- c(lwd_l, lwd_r)
    pch_l <- c(pch_l, pch_r)
  }
  
  
  # Pop quiz: Why are the legends placed relative to the top? Because then their anchor is at the top
  # and they grow downwards instead of up into the plotting area.

  # Draw the legends
  legend("topleft", 
         legend = legend_l,
         ncol = theme$legend_col,
         bty = "n",
         xpd = NA,
         cex = theme$legend_font_size,
         inset = c(0, inset_y),
         col = col_l,
         lty = lty_l,
         lwd = lwd_l,
         pch = pch_l,
         pt.cex = `if`(left_as_bar || left_as_band, theme$legend_box_size, 1),
         x.intersp = theme$legend_intersp_x,
         y.intersp = theme$legend_intersp_y,
         seg.len = theme$legend_seg.len)
  
  # Repeat the above steps (minus sum line) for the right series (if any)
  if(!is.null(tsrn) && !theme$legend_all_left) {
    legend("topright", 
           legend = legend_r,
           ncol = theme$legend_col,
           bty = "n",
           xpd = NA,
           cex = theme$legend_font_size,
           inset = c(0, inset_y),
           col = col_r,
           lty = lty_r,
           lwd = lwd_r,
           pch = pch_r,
           pt.cex = 1,
           x.intersp = theme$legend_intersp_x,
           y.intersp = theme$legend_intersp_y,
           seg.len = theme$legend_seg.len)
  }
}
