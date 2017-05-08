tsplot(KOF["kofbarometer"],tsr = KOF["reference"],auto_legend = T)

tsb1 <- ts(runif(30,-30,20),start=c(2010,1),frequency = 4)
tsb2 <- ts(runif(30,0,50),start=c(2010,1),frequency = 4)
tsb3 <- ts(runif(30,0,50),start=c(2010,1),frequency = 4)
tsb4 <- ts(runif(30,0,50),start=c(2010,1),frequency = 4)
ll <- list(t1 = tsb1, t2 = tsb2, t3 = tsb3)
llr <- list(t4 = tsb4)

tsplot(ll,tsr = llr,left_as_bar = T,
       manual_value_ticks_l = seq(-40,100,by=20),
       auto_legend = T)


addLegend <- function(tsln,tsrn = NULL, left_as_bar = F,
                      theme = initDefaultTheme()){
  par(fig = c(0, 1, 0, 1),
      oma = c(0.5, 3, 2, 1),
      mar = c(0, 0, 0, 0),
      new = TRUE)
  plot(0, 0, type="n", bty="n", xaxt="n", yaxt="n")
  
  ll <- length(tsln)
  
  if(is.null(tsrn)){
    if(!left_as_bar){
      legend("bottomleft", 
             legend = tsln,
             ncol = 2,
             #horiz = TRUE, 
             bty = "n",
             col = na.omit(theme$line_colors[1:ll]),
             lty = na.omit(theme$lty[1:ll]),
             lwd = na.omit(theme$lwd[1:ll]))    
    } else {
      legend("bottomleft", 
             legend = tsln,
             ncol = 2,
             #horiz = TRUE, 
             bty = "n",
             fill = na.omit(theme$bar_fill_color[1:ll]))  
    }
    
  } else {
    lr <- length(c(tsln,tsrn))
    if(!left_as_bar){
      legend("bottomleft", 
             legend = c(tsln,tsrn),
             ncol = 3,
             bty = "n",
             col = na.omit(theme$line_colors[1:lr]),
             lty = na.omit(theme$lty[1:lr]),
             lwd = na.omit(theme$lwd[1:lr]))   
    } else {
      legend("bottomleft", 
             legend = c(tsln,tsrn),
             ncol = theme$legend_col,
             bty = "n",
             border = rep(NA,length(c(tsln,tsrn))),
             fill = theme$bar_fill_color[1:length(tsln)][1:length(c(tsln,tsrn))],
             col = c(rep(NA,length(tsln)),
                     theme$line_colors[(length(tsln)+1):length(c(tsln,tsrn))]),
             lty = na.omit(theme$lty[1:length(tsrn)]),
             lwd = na.omit(theme$lwd[1:length(tsrn)])) 
    }
    
  }
  
  
}


tsplot(KOF["kofbarometer"],tsr = KOF["reference"],auto_legend = T)
undebug(tsplot)
tsplot(KOF,auto_legend = T)
tsplot(ll,auto_legend = T)
tsplot(ll,tsr = llr,auto_legend = T)
tsplot(ll,auto_legend = T,left_as_bar = T,manual_value_ticks_l = seq(-60,100,by=20))
tsplot(ll,tsr = llr,
       auto_legend = T,
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
  
  
