# Define some ETH colors, put that in dataset later on 
ETH8 <- "#007a92" # KOF blau
ETH5 <- "#91056a"
ETH7 <- "#a8322d"
ETH8_60 <- "#66b0c2"
ETH5_60 <- "#cc67a7"
ETH7_50 <- "#e19794"

eth_survey_palette <- c("#a8322d", "#91056a", "#007a92",
                        "#66b0c2","#cc67a7","#e19794")
names(eth_survey_palette) <- c("ETH7","ETH5","ETH8",
                               "ETH8_60","ETH5_60","ETH7_60")

# generate some example time series 
# monthly 2010
ts1 <- ts(rnorm(60,sd = 5)+1:60,start=c(2010,1),freq=12)
# quarterly
ts2 <- ts(rnorm(20,sd = 5)-1:20,start=c(2010,1),freq=4)
tslist <- list(ts1 = ts1,ts2 = ts2)

# color check
plot(ts1,col=eth_survey_palette[1],lwd= 3)
lines(ts2,col=eth_survey_palette[3],lwd= 3)



# some cheap methods to handle time series 
# and a list of time series... 
tsplot <- function(series,...) UseMethod("p")

tsplot.ts <- function(series,...){
  li <- list(...)
  if(!all(unlist(lapply(li,is.ts))))
     stop("all optional objects need to be time series.")
  li
  
  
  
}

tsplot.list <- function(series,...){
  sel <- unlist(list(...))
  if(!all(unlist(lapply(series,is.ts))))
    stop("all elements of the list need to be objects of class ts.")
  # select the entire series if there is no particular selection
  if(!is.null(sel)){
    series[sel]  
  } else {
    series
  }
}


p(tslist,"ts2")


# build a ggplot2 alternative.... 












# base R experiments --- 
# makeImage <- function() {
#   
#   lwd_ts <- 4; lwd_trend <- 8
#   cex_label <- 1.75; cex_title <- 2
#   
#   pdf('ts_charts/ts_chart.pdf', height = 7, width = a4_asp * 7) # 7 is default, but any other number would do probably
#   
#   par(mar = c(6, 3, 5, 0.5), mgp = c(1.5, 0.2, 0))
#   
#   plot(ext_turnover_zero, col = 'darkorange', lwd = lwd_ts, xlab = NA, ylab = NA, 
#        xaxs = 'i', yaxs = 'i', xaxt = 'n', yaxt = 'n', ylim = c(-60, 60))
#   title(main = 'Larger title font size', adj = 0, line = 3.75, cex.main = cex_title)
#   mtext('Larger non-italic font', adj = 0, line = 1.5, cex = cex_title)
#   
#   lines(ext_trend_zero, lwd = lwd_trend)
#   legend(2010, -67, legend = c('Turnover', 'Trend'), box.col = NA, 
#          lty = c(1, 1), lwd = c(lwd_ts, lwd_trend), 
#          cex = cex_label, col = c('darkorange', 'black'), xpd = TRUE)
#   
#   axis(1, at = ext_qtr, labels = ext_label, tcl = 0.5, cex.axis = cex_label, padj = 0.25)
#   # axis(1, at = x_grid, lwd.ticks = 2, labels = FALSE) # thick tick marks
#   
#   axis(2, at = y_grid, lwd.ticks = 1, tcl = 0.5, las = 1, cex.axis = cex_label)
#   
#   for (y in 2010:2015) abline(v = y)
#   for (hl in y_grid) abline(h = hl)
#   
#   
#   dev.off()
# }
