#'@export
initDefaultTheme <- function(){
  theme <- list()
  theme$fillYearWithNAs <- TRUE
  theme$line_colors <- c(ETH6 = "#6f6f6e",
                         ETH6_60 = "#a9a9a8",
                         ETH4 = "#72791c",
                         ETH4_60 = "#a9af66",
                         ETH7 = "#a8322d",
                         ETH7_50 = "#e19794")
  theme$line_to_middle <- T
  theme$lwd <- c(2,3,1,4,2,4)
  theme$lty <- 1
  theme$xaxs <- "i"
  theme$yaxs <- "i"
  theme$bar_border <- "#000000"
  theme$bar_fill_color <- c(ETH8 = "#007A92",
                            ETH8_60 = "#66b0c2",
                            ETH8_30 = "#b3d7e0",
                            ETH8_20 = "#cce5eb",
                            ETH5 = "#91056a",
                            ETH5_60 = "#cc67a7",
                            ETH5_30 = "#e6b3d3")
  theme$sum_line_color <- "#1e1e1e"
  theme$highlight_window <- F
  theme$highlight_window_start <- NULL
  theme$highlight_window_end <- NULL
  theme$highlight_color <- "#e9e9e9"
  theme$use_box <- F
  theme$y_las <- 2 
  # X AXIS ###############
  theme$yearly_ticks <- T
  theme$quarterly_ticks <- T
  theme$monthly_ticks <- F
  theme$tcl_quarterly_tick_tcl <- -.5
  theme$tcl_yearly_tick <- -.75
  theme$lwd_yearly_ticks <- 1.5
  theme$lwd_quarterly_ticks <- 1
  theme$label_pos <- "mid"
  # Y AXIS
  theme$show_left_y_axis <- T
  theme$show_right_y_axis <- T
  theme$show_y_grids <- T
  theme$y_grid_color <- "#CCCCCC"
  theme
}

