tsplot(KOF["kofbarometer"],tsr = KOF["reference"])

tsb1 <- ts(runif(30,-30,20),start=c(2010,1),frequency = 4)
tsb2 <- ts(runif(30,0,50),start=c(2010,1),frequency = 4)
tsb3 <- ts(runif(30,0,50),start=c(2010,1),frequency = 4)
tsb4 <- ts(runif(30,0,50),start=c(2010,1),frequency = 4)
ll <- list(t1 = tsb1, t2 = tsb2, t3 = tsb3)
llr <- list(t4 = tsb4)

tsplot(ll,tsr = llr,left_as_bar = T,
       plot_title = "Ich bin ein Titel",
       plot_subtitle = "Untertitel",
       manual_value_ticks_l = seq(-40,140,by=20),
       auto_legend = T)

tsplot(KOF)





tsplot(KOF["kofbarometer"],
       tsr = KOF["reference"])
tsplot(KOF,auto_legend = T)
tsplot(ll,auto_legend = T)
tsplot(ll,tsr = llr,auto_legend = T)
tsplot(ll,auto_legend = T,
       left_as_bar = T,
       manual_value_ticks_l = seq(-60,120,by=20))
tsplot(ll,tsr = llr,
       plot_title = "random stuff",
       plot_subtitle = "testing tstools",
       plot_subtitle_r = "right tstools (%)",
       left_as_bar = T, 
       manual_value_ticks_l = seq(-60,100,by=20),
       manual_value_ticks_r = seq(-20,140,by=20)
)



undebug(addLegend)
addLegend("kofbarometer")


a <- c("a","b","d")
b <- c("z","y")

ttl <- length(a) + length(b)

(ttl-(length(b)-1)):ttl














if(auto_legend){
  par(fig=c(0, 1, 0, 1),
      oma=c(0.5, 1, 2, 1),
      mar=c(0, 0, 0, 0),
      new=TRUE)
  plot(0, 0, type="n", bty="n", xaxt="n", yaxt="n")
  
  if(!left_as_bar){
    legend("bottomleft", 
           legend = cnames,
           horiz = TRUE, 
           bty = "n",
           col = theme$line_colors,
           lty = theme$lty,
           lwd = theme$lwd)  
  } else{
    legend("bottomleft", 
           legend = cnames,
           horiz = TRUE, 
           bty = "n",
           fill = theme$bar_fill_color)
    if(!is.null(tsr)) {
      legend("bottomleft", 
             legend = names(tsr),
             horiz = TRUE, 
             bty = "n",
             col = theme$line_colors,
             lty = theme$lty,
             lwd = theme$lwd)  
    }
    
    
  }
  
  
