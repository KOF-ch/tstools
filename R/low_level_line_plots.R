#'@importFrom stats ts.union
draw_ts_lines <- function(x, theme = NULL, bandplot = FALSE){
  nts <- length(x)
  op <- rep(theme$show_points, ceiling(nts/length(theme$show_points)))
  ops <- rep(theme$point_symbol, ceiling(nts/length(theme$point_symbol)))
  
  # "harmonize" all ts, range wise
  if(bandplot) {
    x_mat <- do.call(ts.union, x)
    x_mat[is.na(x_mat)] <- 0
    x <- as.list(x_mat)
  }
  
  band_low <- rep(0, length(x[[1]]))
  
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
    
    if(!bandplot) {
      lines(xx,yy,
            col = theme$line_colors[i],
            lwd = theme$lwd[i],
            lty = theme$lty[i],
            type = ifelse(theme$show_points[i], "o", "l"),
            pch = theme$point_symbol[i]
      )
    } else {
      band_high <- band_low + yy
      polygon(c(xx, rev(xx)), c(band_low, rev(band_high)), border = NA, col = theme$band_fill_color[i])
      band_low <- band_high
    }
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


