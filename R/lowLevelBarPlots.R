#' @export
drawTsBars <- function(x, theme = NULL,
                       ...){
  UseMethod("drawTsBars")
} 

#' @rdname lowLevelBarplots
#' @export
drawTsBars.ts <- function(x, theme = NULL,
                            ...){
  
}

#' @rdname lowLevelBarplots
#' @export
drawTsBars.list <- function(x, theme = NULL,
                            ...){
  
}

#' @rdname lowLevelBarplots
#' @export
drawTsBars.mts <- function(x, theme = NULL,
                           ...){
  
}

.drawStackedBars <- function(pos_part = NULL,
                             neg_part = NULL){
  # it's mts for sure cause this function 
  # is only called by the mts method. 
  # split in pos / neg part
  
  
  
}





