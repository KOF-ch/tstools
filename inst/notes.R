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


lines(KOF$reference,col="blue")
par(new=T)
# nice par new hält nur einmal !! crucial

tsLinePlot <- function(tsl,
                manual_value_range = NULL,
                manual_date_range = NULL,
                theme = NULL,
                blank = F){
  # move the ts to list to the higher level later on:
  if(!is.list(tsl)) tsl <- as.list(tsl)
  
  if(is.null(theme)) theme <- initDefaultLineTheme()
  # determine xlim, ylim
  d <- list()
  d$date_range <- .getDateRange(tsl,manual_date_range)
  d$value_range <- .getValueRange(tsl,manual_value_range)
  # empty plot with xlim and ylim
  plot(NULL,
       xlim = d$date_range,
       ylim = d$value_range,
       axes = F,
       xlab = "",
       ylab = ""
  )
  # add lines
  for (i in 1:length(tsl)){
    lines(tsl[[i]],
          col = theme$line_colors[[i]],
          lwd = ifelse(length(theme$lwd) > 1,
                       theme$lwd[i],
                       theme$lwd),
          lty = ifelse(length(theme$lty) > 1,
                       theme$lty[i],
                       theme$lty)
    )
  }
  
  
  d
}

tsBarPlot <- function()



addYAxis <- function(d,
                     right = F,
                     y_grd_steps,
                     manual_value_range){
  stps <- abs(d$value_range[1]-
                d$value_range[2])/y_grd_steps
  
  if(is.null(manual_value_range)){
    tick_positions <- seq(d$value_range[1],
                          d$value_range[2],
                          by = ceiling(stps))    
  } else {
    tick_positions <- seq(manual_value_range[1],
                          manual_value_range[2],
                          by = ceiling(stps))    
  }
  
  if(right){
    axis(4, at = tick_positions)
  } else{
    axis(2, at = tick_positions)
  }
  tick_positions
}

addXAxis <- function(d, s){
  axis(1, at = seq(floor(d$date_range[1]),
                  ceiling(d$date_range[2]),
                  by = s))
}

addYGrids <- function(tick_positions,theme){
  for (hl in tick_positions){
    abline(h = hl,
           col = theme$ygrid_color)
  } 
}

# 1) Multiple lines in one plot, same y-axis
tsplot(KOF$reference,manual_value_range = c(-20,10))
tsplot(KOF$kofbarometer)

# 2) Multiple lines in one plot, l/r y-axis
tsplot2y(KOF$kofbarometer,KOF$reference)




xx <- tsLinePlot(KOF$kofbarometer)
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









tsp2y <- function(l_tsl,r_tsl,
                  l_manual_value_range = NULL,
                  r_manual_value_range = NULL,
                  manual_date_range = NULL,
                  theme = NULL){
  # 
  
  
}

.getValueRange(KOF$reference)
.getDateRange()
