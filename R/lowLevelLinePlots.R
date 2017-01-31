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
    lines(x[[i]],
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




