draw_ts_lines <- function(x, theme = NULL,
                        ...){
  UseMethod("draw_ts_lines")
} 


draw_ts_lines.ts <- function(x, theme = NULL,
                           ...){
  
}


draw_ts_lines.list <- function(x, theme = NULL){
  for (i in 1:length(x)){
    xx <- as.numeric(time(x[[i]]))
    yy <- x[[i]]
    frq <- frequency(x[[i]])
    
    if(theme$line_to_middle){
      xx <- xx+(1/frq)/2
    }
    
    lines(xx,yy,
          col = theme$line_colors[[i]],
          lwd = ifelse(length(theme$lwd) > 1,
                       theme$lwd[i],
                       theme$lwd),
          lty = ifelse(length(theme$lty) > 1,
                       theme$lty[i],
                       theme$lty)
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


