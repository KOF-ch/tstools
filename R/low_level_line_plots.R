draw_ts_lines <- function(x, theme = NULL,
                        ...){
  UseMethod("draw_ts_lines")
} 


draw_ts_lines.ts <- function(x, theme = NULL,
                           ...){
  
}


draw_ts_lines.list <- function(x, theme = NULL){
  nts <- length(x)
  op <- rep(theme$show_points, ceiling(nts/length(theme$show_points)))
  ops <- rep(theme$point_symbol, ceiling(nts/length(theme$point_symbol)))
  
  for (i in 1:nts) {
    xx <- as.numeric(time(x[[i]]))
    yy <- x[[i]]
    frq <- frequency(x[[i]])
    
    if(theme$line_to_middle){
      xx <- xx+(1/frq)/2
    }
    
    if(theme$NA_continue_line[i]) {
      yy_na <- is.na(yy)
      xx <- xx[!yy_na]
      yy <- yy[!yy_na]
    }
    
    lines(xx,yy,
          col = theme$line_colors[i],
          lwd = theme$lwd[i],
          lty = theme$lty[i],
          type = ifelse(theme$show_points[i], "o", "l"),
          pch = theme$point_symbol[i]
    )
  }
  
}

#' @importFrom graphics lines
draw_sum_as_line <- function(x, theme = NULL){
  xx <- as.numeric(time(x))
  yy <- x
  frq <- frequency(x)
  if(theme$line_to_middle) xx <- xx+(1/frq)/2
  lines(xx,yy,
        col = theme$sum_line_color,
        lwd = theme$sum_line_lwd,
        lty = theme$sum_line_lty
  )
}


