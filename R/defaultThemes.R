#'@export
initDefaultTheme <- function(){
  theme <- list()
  theme$fillYearWithNAs <- TRUE
  theme$line_colors <- c(ETH8 = "#007a92",
                         ETH7 = "#a8322d",
                         ETH5 = "#91056a",
                         ETH5_60 = "#cc67a7",
                         ETH8_60 = "#66b0c2",
                         ETH7_50 = "#e19794")
  theme$lwd <- c(2,3,4,2,3,4)
  theme$lty <- 1
  theme$xaxs <- "i"
  theme$yaxs <- "i"
  theme$yearly_ticks <- T
  theme$quarterly_ticks <- T
  theme$year_labels_mid <- T
  theme$bar_border <- "black"
  theme$bar_pos_fill_color <- c(ETH8 = "#007A92",
                            ETH7 = "#A8322D",
                            ETH5 = "#91056A",
                            ETH5_60 = "#CC67A7",
                            ETH8_60 = "#66B0C2",
                            ETH7_50 = "#E19794")
  theme$bar_neg_fill_color <- c(ETH8 = "#007a92",
                                ETH7 = "#a8322d",
                                ETH5 = "#91056a",
                                ETH5_60 = "#cc67a7",
                                ETH8_60 = "#66b0c2",
                                ETH7_50 = "#e19794")
  theme$highlight_window <- F
  theme$highlight_window_start <- NULL
  theme$highlight_window_end <- NULL
  theme$highlight_color <- "#e9e9e9"
  theme
}
