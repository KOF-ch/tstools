#' @importFrom graphics par plot legend
#' @importFrom grDevices dev.size
#' @importFrom stats na.omit
add_legend <- function(tsln,
                      tsrn = NULL,
                      left_as_bar = F,
                      theme = init_tsplot_theme()){
  ll <- length(tsln)
  
  plot_size_in_in <- dev.size()*(par("plt")[c(2, 4)] - par("plt")[c(1, 3)])
  
  inset_y <- 1 + theme$legend_margin_top/plot_size_in_in[2]
  
  # Pop quiz: Why are the legends placed relative to the top? Because then their anchor is at the top
  # and they grow downwards instead of up into the plotting area.
  
  if(is.null(tsrn)){
    if(!left_as_bar){
      legend("topleft", 
             legend = tsln,
             ncol = theme$legend_col,
             bty = "n",
             xpd = NA,
             cex = theme$legend_font_size,
             inset = c(0, inset_y),
             col = na.omit(theme$line_colors[1:ll]),
             lty = na.omit(theme$lty[1:ll]),
             lwd = na.omit(theme$lwd[1:ll]),
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
    lr <- length(tsrn)
    lb <- length(c(tsln,tsrn))
    
    # Make vectors that "wrap around"
    line_colors <- rep(theme$line_colors, ceiling(lb/length(theme$line_colors)))
    bar_fill_colors <- rep(theme$bar_fill_color, ceiling(ll/length(theme$bar_fill_color)))
    lty <- rep(theme$lty, ceiling(lb/length(theme$lty)))
    lwd <- rep(theme$lwd, ceiling(lb/length(theme$lwd)))
    
    if(!left_as_bar){
      legend("topleft", 
             legend = tsln,
             ncol = theme$legend_col,
             bty = "n",
             xpd = NA,
             cex = theme$legend_font_size,
             inset = c(0, inset_y),
             col = na.omit(line_colors[1:ll]),
             lty = na.omit(lty[1:ll]),
             lwd = na.omit(lwd[1:ll]),
             x.intersp = theme$legend_intersp_x,
             y.intersp = theme$legend_intersp_y)
      legend("topright", 
             legend = tsrn,
             ncol = theme$legend_col,
             bty = "n",
             xpd = NA,
             cex = theme$legend_font_size,
             inset = c(0, inset_y),
             col = na.omit(line_colors[(ll+1):lb]),
             lty = na.omit(lty[(ll+1):lb]),
             lwd = na.omit(lwd[(ll+1):lb]),
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
             fill = bar_fill_colors[1:ll],
             x.intersp = theme$legend_intersp_x,
             y.intersp = theme$legend_intersp_y)
      legend("topright",
             legend = tsrn,
             bty = "n",
             border = NA,
             xpd = NA,
             cex = theme$legend_font_size,
             inset = c(0, inset_y),
             ncol = theme$legend_col,
             lty = lty[1:lr],
             lwd = lwd[1:lr],
             col = line_colors[1:lr])
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
