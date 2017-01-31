#' Default Configuration List
#' 
#' Create a list containing with the default theme information. 
#' In case you want to define a custom definition the output of this function is 
#' a nice starting point. This function returns a list containing defaults for
#' the following plot parameters: 
#' \describe{
#' \item{xlim}{range of the date column}
#' \item{ygrid}{vector of horizontal grid lines}
#' \item{xlab}{X-axis label}
#' \item{ylab}{Y-axis label}
#' \item{lty}{vector of line types}
#' \item{xaxs}{should the x-axis start a plot coordinate 0? possible values "r", "i", "e", "s", "d", see ?par}
#' \item{yaxs}{should the y-axis start a plot coordinate 0? possible values "r", "i", "e", "s", "d"}
#' \item{xaxt}{axis type, "n" = no axis}
#' \item{yaxt}{axis type, "n" = no axis}
#' \item{tcl_1}{tick mark length for quarterly ticks}
#' \item{tcl_2}{tick mark length for yearly ticks}
#' \item{padj_1}{how far are the labels away from quarterly ticks}
#' \item{padj_2}{how far are the labels away from yearly ticks}

#' \item{title_adj}{}
#' \item{title_line}{}
#' \item{subtitle_line}{}
#' \item{title_cex.main}{}
#' \item{subtitle_cex.main}{}
#' \item{lwd_ticks_1}{tick mark width for quarterly ticks}
#' \item{lwd_ticks_2}{tick mark width for yearly ticks}

#' \item{ygrid_lwd}{line width of the horizontal grid}
#' \item{ygrid_lty}{line type of the horizontal grid}
#' \item{ygrid_color}{color of the horizontal grid}

#' \item{line_colors}{Default colors of the KOF Swiss Economic Institute.}
#' \item{highlight_window_color}{Background color a highlight window.}
#' \item{par}{bottom,left,top,right margins}
#' \item{$height}{}
#' \item{$width}{}
#' \item{pointsize}{}
#' \item{lgnd_cex_label}{}
#' \item{lgnd_bty}{}
#' \item{lgnd_vertical_spacing}{}
#' \item{lgnd_inset}{}
#' \item{lgnd_xpd}{}
#' \item{fillUpPeriod}{}
#' }
#' @param date_range character defaults to NULL
#' @export
initDefaultLineTheme <- function(){
  theme <- list()
  theme$print_x <- T
  theme$print_y <- T
  theme$print_y_grid <- T
  theme$ygrid_steps <- 5
  theme$label_quarterly = T
  theme$fillYearWithNA <- TRUE
  theme$box <- F
  theme$ygrid <- seq(-60, 60, 30)
  theme$xlab <- NA
  theme$ylab <- NA
  theme$lty <- 1
  theme$use_ygrid <- T
  # axis
  theme$xaxs <- 'r'
  theme$yaxs <- 'i'  
  theme$xaxt <- 'n'  
  theme$yaxt <- 'n'  
  theme$lwd <- 1.5
  theme$tcl_1 <- -0.5
  theme$tcl_2 <- -.75
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
  theme$line_colors <- c(ETH8 = "#007a92",
                         ETH7 = "#a8322d",
                         ETH5_60 = "#cc67a7",
                         ETH5 = "#91056a",
                         ETH8_60 = "#66b0c2",
                         ETH7_50 = "#e19794")
  theme$highlight_window_color <- "#91056a22"
  # margin
  # bottom,left,top,right
  theme$par <- c(5, 4, 4, 2) + 0.1 # R default
  #theme$height <- NULL
  #theme$width <- NULL
  theme$pointsize <- 10
  # legend
  # legend
  theme$lgnd_cex_label <- .8
  theme$lgnd_bty <- "n"
  theme$lgnd_vertical_spacing <- 1.6
  theme$lgnd_inset <- c(0,0)
  theme$lgnd_xpd <- TRUE
  theme$fillUpPeriod <- TRUE
  theme
}

initDefaultBarTheme <- function(){
  theme <- list()
  theme$print_x <- T
  theme$print_y <- T
  theme$print_y_grid <- T
  theme$ygrid_steps <- 5
  theme$label_quarterly = TRUE
  theme$fillYearWithNA <- TRUE
  theme$box <- F
  theme$ygrid <- seq(-60, 60, 30)
  theme$xlab <- NA
  theme$ylab <- NA
  theme$lty <- 1
  theme$use_ygrid <- T
  # axis
  theme$xaxs <- 'i'
  theme$yaxs <- 'i'  
  theme$xaxt <- 'n'  
  theme$yaxt <- 'n'  
  theme$lwd <- 1.5
  theme$tcl_1 <- -0.5
  theme$tcl_2 <- -.75
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
  theme$line_colors <- c(ETH8 = "#007a92",
                         ETH7 = "#a8322d",
                         ETH5_60 = "#cc67a7",
                         ETH5 = "#91056a",
                         ETH8_60 = "#66b0c2",
                         ETH7_50 = "#e19794")
  theme$highlight_window_color <- "#91056a22"
  # margin
  # bottom,left,top,right
  theme$par <- c(5, 4, 4, 2) + 0.1 # R default
  #theme$height <- NULL
  #theme$width <- NULL
  theme$pointsize <- 10
  # legend
  # legend
  theme$lgnd_cex_label <- .8
  theme$lgnd_bty <- "n"
  theme$lgnd_vertical_spacing <- 1.6
  theme$lgnd_inset <- c(0,0)
  theme$lgnd_xpd <- TRUE
  theme
}









#' @export
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
  theme$line_colors <- c(ETH8 = "#007a92",
                         ETH7 = "#a8322d",
                         ETH5_60 = "#cc67a7",
                         ETH5 = "#91056a",
                         ETH8_60 = "#66b0c2",
                         ETH7_50 = "#e19794")
  theme$highlight_window_color <- "#91056a22"
  # margin
  # bottom,left,top,right
  theme$par <- c(5.3,3.6,3,.45)
  theme$height <- 2
  theme$width <- 3.34
  theme$pointsize <- 7.5
  # legend
  theme$lgnd_vertical_spacing <- 1.6
  theme$lgnd_cex_label <- .8
  theme$lgnd_bty <- "n"
  theme$lgnd_inset <- c(0,0)
  theme$lgnd_xpd <- TRUE
  theme$fillUpPeriod <- TRUE
  theme
}

#' @export
initPrint2YTheme <- function(date_range = NULL){
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
  theme$line_colors <- c(ETH8 = "#007a92",
                         ETH7 = "#a8322d",
                         ETH5_60 = "#cc67a7",
                         ETH5 = "#91056a",
                         ETH8_60 = "#66b0c2",
                         ETH7_50 = "#e19794")
  theme$highlight_window_color <- "#91056a22"
  # margin
  # bottom,left,top,right
  theme$par <- c(5,4,4,5)+.1
  theme$height <- 2
  theme$width <- 3.34
  theme$pointsize <- 7.5
  # legend
  theme$lgnd_cex_label <- .8
  theme$lgnd_bty <- "n"
  theme$lgnd_vertical_spacing <- 1.6
  theme$lgnd_inset <- c(0,0)
  # allows legends outside plot area
  theme$lgnd_xpd <- TRUE 
  theme$fillUpPeriod <- TRUE
  theme
}

#' @export
initLowLevelBarTheme <- function(){
  theme <- list()
  theme$border <- "black"
  theme$fill_color <- "blue"
  theme
}


#' @export
initDefaultTheme <- function(){
  theme <- list()
  theme$yaxs <- "r"
  theme$xaxs <- "r"
  theme$fillYearWithNA <- TRUE
  theme$year_labels_mid <- TRUE
  theme$yearly_ticks <- TRUE
  theme$quarterly_ticks <- TRUE
  theme$lwd_ticks_y <- 1.5
  theme$lwd_ticks_q <- 1
  theme$tcl_q <- -0.5
  theme$tcl_y <- -.75
  theme$bar_border_color <- "black"
  theme$y_grid_color <- "#CCCCCC"
  theme$y_grid_lwd <- 1
  theme$y_grid_lty <- 1
  theme$y_zero_higlight <- F
  theme$y_zero_higlight_color <- "#FF0000"
  theme
}



