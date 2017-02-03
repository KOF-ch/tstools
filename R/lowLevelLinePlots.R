#' @export
drawTsLines <- function(x, theme = NULL,
                        ...){
  UseMethod("drawTsLines")
} 

#' @rdname lowLevelLinePlots
#' @export
drawTsLines.ts <- function(x, theme = NULL,
                           ...){
  
}

#' @rdname lowLevelLinePlots
#' @export
drawTsLines.list <- function(x, theme = NULL){
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

#' @rdname lowLevelLinePlots
#' @export
drawTsLines.mts <- function(x, theme = NULL,
                            ...){
  
}




