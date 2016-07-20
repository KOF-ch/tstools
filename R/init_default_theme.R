#' Default Configuration List
#' 
#' Create a list containing with the default theme information. 
#' In case you want to define a custom definition the output of this function is 
#' a nice starting point. 
#' @export
initDefaultTheme <- function(date_range = NULL){
  kof_theme <- list()
  kof_theme$xlim <- date_range
  kof_theme$ygrid <- seq(-60, 60, 30)
  kof_theme$xlab <- NA
  kof_theme$ylab <- NA
  kof_theme$xaxs <- 'i'
  kof_theme$yaxs <- 'i'  
  kof_theme$xaxt <- 'n'  
  kof_theme$yaxt <- 'n'  
  kof_theme$lwd <- 1.5
  kof_theme$lty <- 1
  kof_theme$title_adj <- 0
  kof_theme$title_line <- 1.5
  kof_theme$subtitle_line <- .3
  kof_theme$title_cex.main <- 1
  kof_theme$subtitle_cex.main <- 1
  kof_theme$grid_color <- "#00000022"
  kof_theme$line_colors <- c(ETH7 = "#a8322d",
                             ETH5 = "#91056a",
                             ETH8 = "#007a92",
                             ETH8_60 = "#66b0c2",
                             ETH5_60 = "#cc67a7",
                             ETH7_50 = "#e19794")
  kof_theme$highlight_window_color <- "#91056a22"
  theme <- kof_theme
  theme
}
