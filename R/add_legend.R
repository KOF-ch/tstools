# TODO: Order ci by value, for the pretties

#' @importFrom graphics par plot legend
#' @importFrom grDevices dev.size
#' @importFrom stats na.omit
add_legend <- function(tsln,
                      tsrn = NULL,
                      ci_names,
                      left_as_bar = F,
                      theme = init_tsplot_theme()){
  ll <- length(tsln)
  
  lr <- length(tsrn)
  lb <- length(c(tsln,tsrn))
  
  
  plot_size_in_in <- dev.size()*(par("plt")[c(2, 4)] - par("plt")[c(1, 3)])
  inset_y <- 1 + theme$legend_margin_top/plot_size_in_in[2]
  
  # Make vectors that "wrap around"
  theme$line_colors <- rep(theme$line_colors, ceiling(lb/length(theme$line_colors)))
  theme$bar_fill_colors <- rep(theme$bar_fill_color, ceiling(ll/length(theme$bar_fill_color)))
  theme$lty <- rep(theme$lty, ceiling(lb/length(theme$lty)))
  theme$lwd <- rep(theme$lwd, ceiling(lb/length(theme$lwd)))
  
  splice_ci_names <- function(ts_names) {
    unlist(lapply(ts_names, function(x) {
      c(x, ci_names[[x]])
    }))
  }
  
  legend_l <- splice_ci_names(tsln)
  n_tot_l <- length(legend_l)
  is_ci_l <- !(legend_l %in% tsln)
  pch_l <- rep(NA, n_tot_l)
  pch_l[is_ci_l] <- 15
  col_l <- rep(NA, n_tot_l)
  col_l[!is_ci_l] <- theme$line_colors[1:ll]
  
  ci_color_indices_l <- cumsum(!is_ci_l)[is_ci_l]
  ci_legend_colors_l <- c()
  left_ci_colors <- theme$ci_colors[1:ll]
  for(i in unique(ci_color_indices_l)) {
    ci_legend_colors_l <- c(
      ci_legend_colors_l,
      rev(getCiLegendColors(left_ci_colors[i], sum(ci_color_indices_l == i), theme$ci_alpha))[order(ci_names[[tsln[i]]])]
    )
  }
  
  col_l[is_ci_l] <- ci_legend_colors_l #namedColor2Hex(theme$ci_colors[1:ll], theme$ci_alpha)[]  # Get color at index 1 for all CI belonging to the 
                                                                      # 1st series etc.
  lty_l <- rep(0, n_tot_l)
  lty_l[!is_ci_l] <- theme$lty[1:ll]
  lwd_l <- rep(0, n_tot_l)
  lwd_l[!is_ci_l] <- theme$lwd[1:ll]
  
  legend_r <- splice_ci_names(tsrn)
  n_tot_r <- length(legend_r)
  is_ci_r <- !(legend_r %in% tsrn)
  pch_r <- rep(NA, n_tot_r)
  pch_r[is_ci_r] <- 15
  col_r <- rep(NA, n_tot_r)
  col_r[!is_ci_r] <- theme$line_colors[ifelse(left_as_bar, 1:lr, (ll+1):lb)]
  
  ci_color_indices_r <- cumsum(!is_ci_r)[is_ci_r]
  ci_legend_colors_r <- c()
  right_ci_colors <- theme$ci_colors[ifelse(left_as_bar, 1:lr, (ll+1):lb)]
  for(i in unique(ci_color_indices_r)) {
    ci_legend_colors_r <- c(
      ci_legend_colors_r, 
      rev(getCiLegendColors(right_ci_colors[i], sum(ci_color_indices_r == i), theme$ci_alpha))[order(ci_names[[tsrn[i]]])]
    )
  }
  
  col_r[is_ci_l] <- ci_legend_colors_r #namedColor2Hex(theme$ci_colors[ifelse(left_as_bar, 1:lr, (ll+1):lb)], theme$ci_alpha)[cumsum(!is_ci_r)[is_ci_r]]
  lty_r <- rep(0, n_tot_r)
  lty_r[!is_ci_r] <- theme$lty[ifelse(left_as_bar, 1:lr, (ll+1):lb)]
  lwd_r <- rep(0, n_tot_r)
  lwd_r[!is_ci_r] <- theme$lwd[ifelse(left_as_bar, 1:lr, (ll+1):lb)]
  
  # Pop quiz: Why are the legends placed relative to the top? Because then their anchor is at the top
  # and they grow downwards instead of up into the plotting area.
  
  if(is.null(tsrn)){
    if(!left_as_bar){
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
             pt.cex = 2,
             x.intersp = theme$legend_intersp_x,
             y.intersp = theme$legend_intersp_y)    
    } else {
      legend("topleft", 
             legend = tsln,
             ncol = theme$legend_col,
             bty = "n",
             xpd = NA,
             cex = theme$legend_font_size,
             inset = c(0, inset_y),
             fill = na.omit(theme$bar_fill_color[1:ll]),
             x.intersp = theme$legend_intersp_x,
             y.intersp = theme$legend_intersp_y)  
    }
    
  } else {
    if(!left_as_bar){
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
             pt.cex = 2,
             x.intersp = theme$legend_intersp_x,
             y.intersp = theme$legend_intersp_y)
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
             pt.cex = 2,
             x.intersp = theme$legend_intersp_x,
             y.intersp = theme$legend_intersp_y)
    } else {
      legend("topleft",
             legend = tsln,
             ncol = theme$legend_col,
             bty = "n",
             border = NA,
             xpd = NA,
             cex = theme$legend_font_size,
             inset = c(0, inset_y),
             fill = theme$bar_fill_colors[1:ll],
             x.intersp = theme$legend_intersp_x,
             y.intersp = theme$legend_intersp_y)
      legend("topright",
             legend = legend_r,
             bty = "n",
             border = NA,
             xpd = NA,
             cex = theme$legend_font_size,
             inset = c(0, inset_y),
             ncol = theme$legend_col,
             col = col_r,
             lty = lty_r,
             lwd = lwd_r,
             pch = pch_r,
             pt.cex = 2)
    }
    
  }
  
  
  if(theme$sum_as_line && !is.null(theme$sum_legend)) {
    legend("topleft",
           legend = c(rep(NA, ceiling(ll/theme$legend_col)), theme$sum_legend),
           bty = "n",
           border = NA,
           xpd = NA,
           cex = theme$legend_font_size,
           inset = c(0, inset_y),
           col = theme$sum_line_color,
           lty = c(rep(0, ceiling(ll/theme$legend_col)), theme$sum_line_lty),
           lwd = theme$sum_line_lwd,
           seg.len = 0.9)
  }
  
}
