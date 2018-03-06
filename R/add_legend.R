#' @importFrom graphics par plot legend
#' @importFrom stats na.omit
add_legend <- function(tsln,
                      tsrn = NULL,
                      left_as_bar = F,
                      theme = init_tsplot_theme()){
  par(fig = c(0, 1, 0, 1),
      oma = c(0.2, 3, 2, 1),
      mar = c(0, 0, 0, 0),
      new = TRUE)
  plot(0, 0, type="n", bty="n", xaxt="n", yaxt="n")
  
  ll <- length(tsln)
  
  if(is.null(tsrn)){
    if(!left_as_bar){
      legend("bottomleft", 
             legend = tsln,
             ncol = theme$legend_col,
             #horiz = TRUE, 
             bty = "n",
             col = na.omit(theme$line_colors[1:ll]),
             lty = na.omit(theme$lty[1:ll]),
             lwd = na.omit(theme$lwd[1:ll]),
             x.intersp = theme$legend_intersp_x,
             y.intersp = theme$legend_intersp_y)    
    } else {
      legend("bottomleft", 
             legend = tsln,
             ncol = theme$legend_col,
             #horiz = TRUE, 
             bty = "n",
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
      legend("bottomleft", 
             legend = tsln,
             ncol = theme$legend_col,
             bty = "n",
             col = na.omit(line_colors[1:ll]),
             lty = na.omit(lty[1:ll]),
             lwd = na.omit(lwd[1:ll]),
             x.intersp = theme$legend_intersp_x,
             y.intersp = theme$legend_intersp_y)
      legend("bottomright", 
             legend = tsrn,
             ncol = theme$legend_col,
             bty = "n",
             col = na.omit(line_colors[(ll+1):lb]),
             lty = na.omit(lty[(ll+1):lb]),
             lwd = na.omit(lwd[(ll+1):lb]),
             x.intersp = theme$legend_intersp_x,
             y.intersp = theme$legend_intersp_y)
    } else {
      legend("bottomleft",
             legend = tsln,
             ncol = theme$legend_col,
             bty = "n",
             border = NA,
             fill = bar_fill_colors[1:ll],
             x.intersp = theme$legend_intersp_x,
             y.intersp = theme$legend_intersp_y)
      legend("bottomright",
             legend = tsrn,
             bty = "n",
             border = NA,
             ncol = theme$legend_col,
             lty = lty[1:lr],
             lwd = lwd[1:lr],
             col = line_colors[1:lr])
    }
    
  }
  
  
}
