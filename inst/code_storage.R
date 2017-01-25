# Flow: 
# put two plots above each other... 
# otherwise axes can't have different scales, i.e. common 
# horizontal gridlines.. 

# idea... 

# 1. get background done, including x-axis, y-axis (left)
# 2. plot line chart that relates to left axis


plot(NULL,
     xlim = c(2010,2015),
     ylim = c(50,130),
     axes = F
)
box()

par(new=T)

tsplot(KOF$kofbarometer)

par(new=T)

plot(NULL,
     xlim = c(2010,2015),
     ylim = range(KOF$reference),
     axes = F,
     yaxs = "i",
     yaxs = "i",
     xlab = "",
     ylab = ""
)

blankPlot <- function(s){
  plot(NULL,
       xlim = c(2010,2015),
       ylim = range(s),
       axes = F,
       yaxs = "i",
       yaxs = "i",
       xlab = "",
       ylab = ""
  )
}

blankPlot(KOF$kofbarometer)
box()
lines(KOF$kofbarometer,col="blue")
axis(2,xaxs="i")
axis(1,yaxs="i")
par(new=T)
blankPlot(KOF$reference)
par(new=T)
barplot(t(KOF$reference))
lines(KOF$reference,col="red",lty=2)
axis(4)


plot(KOF$kofbarometer,
     xaxs="i",
     yaxs="i")

plot(time(KOF$reference),KOF$reference,type="h",xlim = range(time(KOF$reference)),
     ylim = c(-10,10),at="pretty")
par(new=T)
tsLinePlot(KOF$reference,manual_value_range = c(-10,10))
axis(1,xlim = c(1991,2016),at = 1991:2016)


lines(KOF$reference,col="blue")
par(new=T)
# nice par new hält nur einmal !! crucial





tt <- initDefaultBarTheme()
tt$box <- F
bpos <- tsBarPlot(short,manual_value_range = c(-10,10),theme = tt)
axis(1,at = bpos$bar_pos,labels = time(short))
lines(x = bpos$bar_pos,short,col="blue",lwd=3)
par(new=T)
oo <- tsLinePlot(short*20,manual_value_range = c(-90,70))



b_pos <- drawTsBars(short, manual_value_range = c(-10,10),no_plot = T)
y_ticks <- addYAxis(b_pos,y_grd_steps = 4,manual_value_range = c(-10,10))
addYGrids(y_ticks,theme = tt)
par(new=T)
b_pos <- drawTsBars(short, manual_value_range = c(-10,10))
addXAxis(b_pos)




short <- window(KOF$reference,start=c(2008,1),end=c(2012,4))
oo <- barplot(t(short),ylim = c(-10,10))
bb <- 
  x


axis(2,ylim = c(-10,10))
axis(side=1, labels = t_sq, at = bb$bar_pos)

abline(h=0,col="blue",lwd=1)
abline(h=2.7619020,col="blue",lwd=1)
abline(v = .7,col="blue",lwd=1)
abline(v = 24.7,col="blue",lwd=1)
abline(v = 2008,col="blue",lwd=1)
abline(v = 2012.25,col="blue",lwd=1)

par(new=T)

lines(x = bb$bar_pos, y = short,col="blue",lwd=2,lty=9)

# riddle solved: bars need to have same number of observations
# because axes are different... then sequence works, you can use the 
# damn bar_pos for the lines, too it's difficult the other way around... 
# isn't it?
axis(1)

barplot(t(short))


# how about
# 1) draw a line
lt$yaxs <- "i"
par(new = T)
lp <- tsLinePlot(short,theme = lt,
                 manual_value_range = c(-10,10),
                 manual_date_range = c(2008,2012.25))
plot(short,ylim = c(-10,10),xlim=c(2008,2012.25))
axis(2)

# 2) compute bar positions from 
bp <- seq(from = .7, to = 1.2 * length(short), by = 1.2)
par(new=T)
tsBarPlot(short,manual_value_range = c(-10,10))
barplot(t(short),xlim = range(bp),ylim = c(-10,10),offset = 0)
axis(1,at = bp,labels = time(short))


axis(side=1, labels = t_sq, at = oo)
axis(1,time(short),at=oo)
abline(v = 61.9,col="blue")
abline(h = 1.3250919,col="blue")
par(new=T)
plot(short[1],xlim = range(bb$bar_pos),ylim=c(-10,10))
lt <- initDefaultLineTheme()
lt$line_colors <- "blue"
tsLinePlot(short,theme = lt,
           manual_value_range = c(-10,10),
           manual_date_range = c(2008,2012.25))

lines(short)

tsLinePlot(short,theme = lt,
           manual_value_range = c(-10,10),
           date_range = c(2008,2012.25))

length()

box()


# 4) stacked ts bar chart


short <- window(KOF$reference,start=c(2008,1),end=c(2012,4))

bb <- tsBarPlot(short)
axis(1,bb)
axis(2,range(short))
box()
par(new = T)
tt <- initDefaultLineTheme()
tt$line_colors[1] <- "blue"
tsLinePlot(short,theme = tt,manual_value_range = range(short))

start(short)
end(short)

t_sq <- seq(from = as.Date(sprintf("%s-%s-01",
                                   start(short)[1],
                                   start(short)[2]),
                           format = "%Y-%m-%d"),
            by = paste(12/frequency(short),
                       "months",
                       sep=" "),
            length.out = length(short)
)

# Plot positive bars
c_barplot1 <- barplot(c_transposed_vect1, ylim=c_value_range, axes=F, col=theme_1$line_colors)
# Add negative bars
c_barplot2 <- barplot(c_transposed_vect2, ylim=c_value_range, axes=F, col=theme_1$line_colors, add=T)
# Add y-axis on left side
axis(side=2, ylim=c_value_range)
# Add x-axis of time series; for every month a tick
time_seq <- seq(from=as.Date(paste(start(c_vect)[1], start(c_vect)[2],1,sep="."), format="%Y.%m.%d"), by=paste(12/frequency(c_vect), "months", sep=" "), length.out=dim(c_vect)[1])
axis(side=1, labels = time_seq, at=c_barplot1)

# Add box around the plot
box() 

# Add the column sum as line plot to the barplot
if(show_sums_as_line==T) {
  
  c_vect_col_sums <- colSums(t(c_vect))
  lines(x=c_barplot1, y=c_vect_col_sums)
  
}




tsContributionChart <- function(tsl,
                                manual_value_range = NULL,
                                manual_date_range = NULL,
                                theme = NULL){
  if(is.null(theme)) theme <- initDefaultBarTheme()
  # get x and y range of the bar chart thing... but don't
  # plot it because grids need to be plotted first.
  xy_range <- tsBarPlot(tsl,
                        manual_value_range = manual_value_range,
                        manual_date_range = manual_date_range,
                        theme = theme,no_plot=T)
  
  plot(NULL,
       xlim = xy_range$date_range,
       ylim = xy_range$value_range,
       axes = F,
       xlab = "",
       ylab = ""
  )
  
  if(theme$print_x) addXAxis(xy_range)
  if(theme$print_y) addYAxis(xy_range,
                             y_grd_steps = theme$ygrid_steps,
                             manual_value_range = xy_range$value_range)
  # don't forget ygrids..
  if(theme$box) box()
  
  par(new=T)
  xy_range <- tsBarPlot(tsl,
                        manual_value_range = manual_value_range,
                        manual_date_range = manual_date_range,
                        theme = theme, no_plot=F)
  
  
  xy_range
}

debug(tsContributionChart)
debug(tsBarPlot)
suppressWarnings(tsContributionChart(short))
abline(v=300,col="blue")
plot(rnorm(10))

# 3) simple Bar chart
debug(tsBarPlot)
tsBarPlot(KOF$kofbarometer)
bb <- tsBarPlot(short)
lines(short,col="blue")
axis(1,bb)
axis
addXAxis(bb)
par(new = T)
xx <- tsLinePlot(KOF$reference,manual_value_range = range(short))
abline(h=0,col="blue",lwd=3)
addXAxis(xx)
tp <- addYAxis(xx,y_grd_steps = 4,manual_value_range = NULL)
addYGrids(tp,tt)
box()
par(new = T)
yy <- tsLinePlot(KOF$reference)
addYAxis(yy,right=T,y_grd_steps = 4,manual_value_range = NULL)



debug(tsplot2y)
tsplot2y(KOF$kofbarometer,KOF$reference)
undebug(tsplot.list)
tsplot(KOF$reference,manual_value_range = c(-20,10))











