tsplot2y <- function(x,y,theme = NULL,plot.title = NULL,plot.subtitle = NULL,
                     theme_out = F,...){
  # set margin a little different
  # to free space for another y-axis
  par(mar=c(5,4,4,5)+.1)
  
  if(is.null(theme)){
    kof_theme <- list()
    kof_theme$ygrid <- seq(-60, 60, 30)
    kof_theme$xlab <- NA
    kof_theme$ylab <- NA
    kof_theme$xaxs <- 'i'
    kof_theme$yaxs <- 'i'  
    kof_theme$xaxt <- 'n'  
    kof_theme$yaxt <- 'n'  
    kof_theme$lwd <- 1.5
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
    theme <- kof_theme
  }
 
  # left theme will be KOF theme... 
  # right theme needs to be defined here

  
  
  tsplot(x,theme = theme)
  par(new=TRUE)
  tsplot(y,theme = theme,print_x_axis = F)

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
}


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
