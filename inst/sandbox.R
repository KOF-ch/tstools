# Todo: 
# finish axes and ticks of tsplot
# finish tsplot2y
# create pdf storage... 
# look at aspect ratio... 
# check title stuff... 


# random series
ts1 <- ts(rnorm(70,sd = 5),start=c(2010,1),freq=12)
# quarterly
ts2 <- ts(rnorm(40,40,sd = 5),start=c(2010,1),freq=4)
tslist <- list(ts1 = ts1,ts2 = ts2)
ts3 <- ts(rnorm(100,60,sd = 2),start=c(1990,1),freq=4)

library(tstools)


# plot from a list
debug(tsplot)
tsplot(ts1,ts2)


tsplot.list





# select single series from a list 

debug(tsplot.list)
tsplot(tslist)

# plot from multple
tsplot(ts1,ts2,ts3)

# use arguments title
tsplot(ts1,ts2,plot.title = "Testplot",
       plot.subtitle = "some subtitle")

# use dynamic grids depending on values
tsplot(ts1,ts2,plot.title = "Testplot",
       plot.subtitle = "some subtitle",
       ygrid_dynamic = T)

# specify y-axis F
tsplot(ts1,ts2,plot.title = "Testplot",
       plot.subtitle = "some subtitle",
       ygrid_dynamic = F,yaxis_factor = 20)


# adjust theme
# read out the default them, which is essentially a list:
my_theme_1 <- tsplot(ts1,ts2,plot.title = "Testplot",
                   plot.subtitle = "some subtitle",
                   ygrid_dynamic = F,
                   yaxis_factor = 20,
                   theme_out = T)

my_theme_1$ygrid <- seq(-10,70,10)

tsplot(ts1,ts2,plot.title = "Testplot",
       plot.subtitle = "some subtitle",
       ygrid_dynamic = F,yaxis_factor = 40,
       theme = my_theme_1)


abline(h=0,col="red",lwd=3)




tf1 <- function(series){
#  nm <- as.character(substitute(series))
  nm <- deparse(substitute(series))
  nm
}


tf1(ts1)


tf <- function(...){
  li <- list(...)
  li_one <- lapply(li,"[",1)
  li_two <- lapply(li,"[",-1)
  li_two
}
  
tf(a = c(2,3,5,6) , b = c(2,1,4,423) )










#########################################################
######################## DEPRECATED SANDBOX STUFF #######
#########################################################

# Define some ETH colors, put that in dataset later on 
ETH8 <- "#007a92" # KOF blau
ETH5 <- "#91056a"
ETH7 <- "#a8322d"
ETH8_60 <- "#66b0c2"
ETH5_60 <- "#cc67a7"
ETH7_50 <- "#e19794"

# define some kof defaults:
plot(ts1,xlab = NA, ylab = NA,xaxs = 'i', yaxs = 'i', xaxt = 'n', yaxt = 'n')

kof_theme <- list()
kof_theme$ygrid <- seq(-60, 60, 30)
kof_theme$xlab <- NA
kof_theme$ylab <- NA
kof_theme$xaxs <- 'i'
kof_theme$yaxs <- 'i'  
kof_theme$xaxt <- 'n'  
kof_theme$yaxt <- 'n'  
kof_theme$title_adj <- 0
kof_theme$title_line <- 3.75
kof_theme$grid_color <- "#00000022"
kof_theme$line_colors <- c(ETH7 = "#a8322d",
                           ETH5 = "#91056a",
                           ETH8 = "#007a92",
                           ETH8_60 = "#66b0c2",
                           ETH5_60 = "#cc67a7",
                           ETH7_50 = "#e19794")

kof_theme$line_colors[["ETH7"]]



eth_survey_palette <- c(ETH7 = "#a8322d",
                        ETH5 = "#91056a",
                        ETH8 = "#007a92",
                        ETH8_60 = "#66b0c2",
                        ETH5_60 = "#cc67a7",
                        ETH7_50 = "#e19794")

# generate some example time series 
# monthly 2010
ts1 <- ts(rnorm(70,sd = 5),start=c(2010,1),freq=12)
# quarterly
ts2 <- ts(rnorm(20,40,sd = 5)-1:20,start=c(2010,1),freq=4)
tslist <- list(ts1 = ts1,ts2 = ts2)

# color check
plot(ts1,col=eth_survey_palette[1],lwd= 3)
lines(ts2,col=eth_survey_palette[3],lwd= 3)


# the deep end note is mad useful w.r.t ... arguments
# http://www.burns-stat.com/the-three-dots-construct-in-r/

source("R/tsplot.R")


tsplot(ts1,ts2,ygrid_dynamic = T)








tplot <- function(ts){
  
  ts_time <- time(ts1)
  first_year <- min(floor(ts_time))
  last_year <- max(ceiling(ts_time))
  x_grid <- first_year:last_year
  y_grid <- seq(-60, 60, 30)
  a4_asp <- (210 / 2) / (275 / 4)  
  
  
  ext_qtr <- ts_time[abs(ts_time * 4 - floor(ts_time * 4)) < 0.001]
  ext_label <- ifelse(ext_qtr - floor(ext_qtr) == 0.5, as.character(floor(ext_qtr)), NA)
  
  plot(ts1, col = 'darkorange', lwd = 1, xlab = NA, ylab = NA, 
       xaxs = 'i', yaxs = 'i', xaxt = 'n', yaxt = 'n', ylim = c(-60, 60))
  title(main = 'Larger title font size', adj = 0, line = 3.75, cex.main = cex_title)
  mtext('Larger non-italic font', adj = 0, line = 1.5, cex = cex_title)
  
  for (hl in y_grid) abline(h = hl,col="#00000022")
  
  
}


tplot(ts1)


tslist




ext_qtr <- ext_time[abs(ext_time * 4 - floor(ext_time * 4)) < 0.001]
ext_label <- ifelse(ext_qtr - floor(ext_qtr) == 0.5, as.character(floor(ext_qtr)), NA)

x_grid <- first_year:last_year
y_grid <- seq(-60, 60, 30)

a4_asp <- (210 / 2) / (275 / 4)  # width -> 210 cm; height -> 297 - 22 mm (based on A4)

makeImage <- function() {
  
  lwd_ts <- 4; lwd_trend <- 8
  cex_label <- 1.75; cex_title <- 2
  
  pdf('ts_charts/ts_chart.pdf', height = 7, width = a4_asp * 7) # 7 is default, but any other number would do probably
  
  par(mar = c(6, 3, 5, 0.5), mgp = c(1.5, 0.2, 0))
  
  plot(ext_turnover_zero, col = 'darkorange', lwd = lwd_ts, xlab = NA, ylab = NA, 
       xaxs = 'i', yaxs = 'i', xaxt = 'n', yaxt = 'n', ylim = c(-60, 60))
  title(main = 'Larger title font size', adj = 0, line = 3.75, cex.main = 1)
  mtext('Larger non-italic font', adj = 0, line = .3, cex = 1)
  
  lines(ext_trend_zero, lwd = lwd_trend)
  legend(2010, -67, legend = c('Turnover', 'Trend'), box.col = NA, 
         lty = c(1, 1), lwd = c(lwd_ts, lwd_trend), 
         cex = cex_label, col = c('darkorange', 'black'), xpd = TRUE)
  
  axis(1, at = ext_qtr, labels = ext_label, tcl = 0.5, cex.axis = cex_label, padj = 0.25)
  # axis(1, at = x_grid, lwd.ticks = 2, labels = FALSE) # thick tick marks
  
  axis(2, at = y_grid, lwd.ticks = 1, tcl = 0.5, las = 1, cex.axis = cex_label)
  
  for (y in 2010:2015) abline(v = y)
  for (hl in y_grid) abline(h = hl)
  
  
  dev.off()
}


# build a ggplot2 alternative.... 
# ggfortify seems like promising package
# we can either use it our take parts of the code: 
# https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_ts.html












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
