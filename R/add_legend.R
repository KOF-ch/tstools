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
    lr <- length(c(tsln,tsrn))
    if(!left_as_bar){
      legend("bottomleft", 
             legend = c(tsln,tsrn),
             ncol = theme$legend_col,
             bty = "n",
             col = na.omit(theme$line_colors[1:lr]),
             lty = na.omit(theme$lty[1:lr]),
             lwd = na.omit(theme$lwd[1:lr]),
             x.intersp = theme$legend_intersp_x,
             y.intersp = theme$legend_intersp_y)   
    } else {
      legend("bottomleft", 
             legend = c(tsln,tsrn),
             ncol = theme$legend_col,
             bty = "n",
             border = rep(NA,length(c(tsln,tsrn))),
             fill = theme$bar_fill_color[1:length(tsln)][1:length(c(tsln,tsrn))],
             col = theme$legend_col,
             lty = na.omit(theme$lty[1:length(tsrn)]),
             lwd = na.omit(theme$lwd[1:length(tsrn)]),
             x.intersp = theme$legend_intersp_x,
             y.intersp = theme$legend_intersp_y) 
    }
    
  }
  
  
}
