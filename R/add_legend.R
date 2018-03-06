#' @importFrom graphics par plot legend
#' @importFrom stats na.omit
add_legend <- function(tsln,
                      tsrn = NULL,
                      left_as_bar = F,
                      theme = init_tsplot_theme()){
  ll <- length(tsln)
  
  # Pop quiz: Why are the legends placed relative to the top? Because then their anchor is at the top
  # and they grow downwards instead of up into the plotting area.
  
  if(is.null(tsrn)){
    if(!left_as_bar){
      legend("topleft", 
             legend = tsln,
             ncol = theme$legend_col,
             bty = "n",
             xpd = NA,
             inset = c(0, 1.15),
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
             inset = c(0, 1.15),
             fill = na.omit(theme$bar_fill_color[1:ll]),
             x.intersp = theme$legend_intersp_x,
             y.intersp = theme$legend_intersp_y)  
    }
    
  } else {
    ll <- length(tsln)
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
             inset = c(0, 1.15),
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
             inset = c(0, 1.15),
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
             inset = c(0, 1.15),
             fill = bar_fill_colors[1:ll],
             x.intersp = theme$legend_intersp_x,
             y.intersp = theme$legend_intersp_y)
      legend("topright",
             legend = tsrn,
             bty = "n",
             border = NA,
             xpd = NA,
             inset = c(0, 1.15),
             ncol = theme$legend_col,
             lty = lty[1:lr],
             lwd = lwd[1:lr],
             col = line_colors[(ll+1):lb])
    }
    
  }

  
}
