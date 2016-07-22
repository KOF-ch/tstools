#' Default Configuration List
#' 
#' Create a list containing with the default theme information. 
#' In case you want to define a custom definition the output of this function is 
#' a nice starting point. 
#' @export
initDefaultTheme <- function(date_range = NULL){
  theme <- list()
  theme$xlim <- date_range
  theme$ygrid <- seq(-60, 60, 30)
  theme$xlab <- NA
  theme$ylab <- NA
  theme$lty <- 1
  # axis
  theme$xaxs <- 'i'
  theme$yaxs <- 'i'  
  theme$xaxt <- 'n'  
  theme$yaxt <- 'n'  
  theme$lwd <- 1.5
  theme$tcl_1 <- -0.5
  theme$tcl_2 <- -0.5
  theme$padj_1 <- .25 # how far are the labels away from ticks
  theme$padj_2 <- .25
  # titles
  theme$title_adj <- 0
  theme$title_line <- 1.5
  theme$subtitle_line <- .3
  theme$title_cex.main <- 1
  theme$subtitle_cex.main <- 1
  theme$lwd_ticks_1 <- 1.5
  theme$lwd_ticks_2 <- 1
  # ygrid
  theme$ygrid_lwd <- 1
  theme$ygrid_lty <- 1
  theme$ygrid_color <- "#00000022"
  # colors
  theme$line_colors <- c(ETH7 = "#a8322d",
                         ETH5 = "#91056a",
                         ETH8 = "#007a92",
                         ETH8_60 = "#66b0c2",
                         ETH5_60 = "#cc67a7",
                         ETH7_50 = "#e19794")
  theme$highlight_window_color <- "#91056a22"
  # margin
  # bottom,left,top,right
  theme$par <- c(5, 4, 4, 2) + 0.1 # R default
  #theme$height <- NULL
  #theme$width <- NULL
  theme$pointsize <- 10
  # legend
  theme$lgnd_offset <- 14
  theme$lgnd_cex_label <- .8
  theme$lgnd_xpd <- TRUE
  theme
}


initPrintTheme <- function(date_range = NULL){
  theme <- list()
  theme$xlim <- date_range
  theme$ygrid <- seq(-60, 60, 30)
  theme$xlab <- NA
  theme$ylab <- NA
  theme$lty <- 1
  # axis
  theme$xaxs <- 'i'
  theme$yaxs <- 'i'  
  theme$xaxt <- 'n'  
  theme$yaxt <- 'n'  
  theme$lwd <- 2
  theme$tcl_1 <- -0.5
  theme$tcl_2 <- -1
  theme$padj_1 <- .25 # how far are the labels away from ticks
  theme$padj_2 <- .25
  theme$cex.axis_1 <- .8
  theme$cex.axis_2 <- .8
  theme$axis_las_1 <- 1
  theme$axis_las_2 <- 2
  theme$yaxis_tick <- FALSE
  theme$yaxis_labels <- TRUE
  # ygrid
  theme$ygrid_lwd <- 1
  theme$ygrid_lty <- 1
  theme$ygrid_color <- "#00000022"
  # titles
  theme$title_adj <- 0
  theme$title_line <- 1.5
  theme$subtitle_line <- .3
  theme$title_cex.main <- 1
  theme$subtitle_cex.main <- 1
  theme$lwd_ticks_1 <- 1
  theme$lwd_ticks_2 <- 1
  # line colors
  theme$line_colors <- c(ETH7 = "#a8322d",
                             ETH5 = "#91056a",
                             ETH8 = "#007a92",
                             ETH8_60 = "#66b0c2",
                             ETH5_60 = "#cc67a7",
                             ETH7_50 = "#e19794")
  theme$highlight_window_color <- "#91056a22"
  # margin
  # bottom,left,top,right
  theme$par <- c(5.3,3.6,3,.45)
  theme$height <- 2
  theme$width <- 3.34
  theme$pointsize <- 7.5
  # legend
  theme$lgnd_offset <- 14
  theme$lgnd_cex_label <- .8
  theme$lgnd_xpd <- TRUE
  theme
}


