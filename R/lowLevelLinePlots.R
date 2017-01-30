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
drawTsLines.list <- function(x, theme = NULL,
                            ...){
  
}

#' @rdname lowLevelLinePlots
#' @export
drawTsLines.mts <- function(x, theme = NULL,
                           ...){
  
}




