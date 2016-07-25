
# get more realistic series for the 2 axis case
ts4 <- ts(rnorm(100,sd = 8)+1:100,
          start=c(1990,1),
          frequency = 4)
ts5 <- (ts4/lag(ts4, k=-4))-1

ts4
ts6 <- lag(ts4)/ts4

debug(tsplot2y)
tsplot2y(ts4,ts5)


# I think we need a check here to check 
# whether time series are numeric... throw an 
# exception if not!!
library(openxlsx)
baro <- read.xlsx("~/Downloads/kof_data_export_1469353651.xlsx")
baro <- na.omit(baro)
baro$kofbarometer <- as.numeric(baro$kofbarometer)
baro$kofbarometer_ref <- as.numeric(baro$kofbarometer_ref)


b_ts <- ts(baro$kofbarometer,start=c(1991,1),
              frequency = 12)
r_ts <- ts(baro$kofbarometer_ref,start=c(1991,1),
              frequency = 12)

library(devtools)
load_all("../tstools/")

library(tstools)

p_t <- initPrint2YTheme()
p_t$lwd <- 3
p_t$lty <- c(1,1,3)
p_t$lgnd_inset <- c(0,0)


undebug(tsplot2y)
undebug(tsplot)

# next... check colors, lwd and lty!!!
# got colors, lwd and lty now...
# get highlight box going now... 

w_b_ts <- window(b_ts,start=c(2005))
w_r_ts <- window(r_ts,start=c(2005))

tsplot(w_b_ts,theme = p_t,
       ygrid_factor = 4,highlight_window = c(2010,2015))



# 0.7304398 oct 2011
# 83.12832
# next up add pdf functionality 
# to tsplot2y !!!!!
# also add legend !!
tsplot2y(w_b_ts,
         w_r_ts,
         r_manual_y_range = c(-6,6),
         highlight_window = c(2013,2015),
         theme_2y = p_t,
         plot.title = "KOF Barometer",
         plot.subtitle = "Swiss GDP Growth",
         lgnd = c("Baro","GDP"),write_pdf = F)


dev.off()

plot(ts5)


tsplot(r_ts,manual_value_range = c(-6,6))



undebug(tsplot2y)
debug(tsplot.list)
tsplot2y(ts1,ts2)

x1 <- 1:5
x2 <- 1:10
y1 <- rnorm(5)
y2 <- rnorm(10,20)
par(mar=c(5,4,4,5)+.1)
plot(x1,y1,type="l",col="red")

plot(x2, y2,type="b",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
mtext("y2",side=4,line=3)
legend("topleft",col=c("red","blue"),lty=1,legend=c("y1","y2"))




ts1 <- ts(rnorm(100),start=c(1985,1),freq=12)
ts2 <- ts(rnorm(100,30),start=c(1990,1),freq=12)

par(mar=c(5,4,4,5)+.1)
plot(ts1)
par(new=TRUE)
plot(ts2,type="b",col="blue",yaxt="n",xlab="",ylab="")
axis(4)



# get min max dates from series
# tl <- list(x,y)
# min_date <- min(unlist(lapply(tl,function(x) min(time(x)))))
# max_date <- max(unlist(lapply(tl,function(x) max(time(x)))))
# 
# 
# # plot x time series
# plot(x, xlab = theme$xlab, ylab = theme$ylab, xaxt = theme$xaxt,
#      col = theme$line_colors[1],lwd = theme$lwd,
#      xlim = c(min_date,max_date),
#      ...)
# par(new=TRUE)
# plot(y,xlab = theme$xlab, ylab = theme$ylab, xaxt = theme$xaxt,
#      yaxt = theme$yaxt,
#      col = theme$line_colors[2], lwd = theme$lwd,
#      ...)
# axis(4)

plot(rnorm(100))
par(new=T,xpd=T)
plot(rnorm(10))
legend(1,-3,legend = c("a","b"))

