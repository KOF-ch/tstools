
### WHAT IF a series doesn't start in the first period of the
## year

tslist <- list()
tslist$ts1 <- ts(runif(18,10,50),start=c(2010,4),
                 frequency = 4)
tslist$ts2 <- ts(runif(20,-10,20),start=c(2010,1),
                 frequency = 4)

tslist$ts3 <- ts(runif(20,-40,20),start=c(2010,1),
                 frequency = 4)

tslist$ts4 <- ts(runif(15,-40,20),start=c(2010,1),
                 frequency = 4)
tslist$ts5 <- ts(runif(40,-40,20),start=c(2010,1),
                 frequency = 12)



reflist <- list()
reflist$rs1 <- ts(runif(20,-3,3),start=c(2010,1),
              frequency = 4)

reflist$rs2 <- ts(runif(20,-5,5),start=c(2010,1),
                  frequency = 4)

debug(tsplot)
tsplot(tslist[1:2],tsr=range(reflist$rs1))




tsplot(tslist[1:2],tsr = reflist,
       manual_value_ticks_l = seq(-20,70,by=10),
       manual_value_ticks_r = seq(-8,10,by=2),
       left_as_bar = F,
       theme = tt)
box()

undebug(tsplot)
tsplot(tslist[1],tsr=reflist)

tsplot(tslist[[1]],tslist[[2]],tslist[[3]],tsr=reflist,left_as_bar = T)
tsplot(tslist[[1]],tslist[[2]],tslist[[3]],tsr=reflist)
tsplot(tslist[[1]],tslist[[2]],tslist[[3]],tsr=reflist,manual_date_ticks = 1990:2010)

tsplot(tslist[1:3],tsr=reflist,left_as_bar = T)


tt <- initDefaultTheme()
tt$highlight_window <- T
tt$label_pos <- "mid"
tt$line_to_middle <- T
tsplot(tslist[2:4],left_as_bar = T,theme = tt)
tsplot(tslist[2:4],left_as_bar = F,theme = tt)

exportTsList(tslist[1:3],date_format = "%Y-0%q",xlsx=T)


tslist$ts6 <- ts(runif(230,-10,10),start=c(1951,1),frequency = 4)
exportTsList(tslist[c(1:3,6)],date_format = "%Y-0%q",xlsx=T)















xx <- getGlobalXInfo(tslist[1:3],NULL,F)

tsplot(tslist[2:3],tsr = tslist[3:4],left_as_bar = T,fill_up_start = T)
tsplot(tslist[2:4],tsr = tslist$ts3,left_as_bar = T)

tsplot(tslist[1:3],tsr = reflist,left_as_bar = T)


tsplot(tslist[2:4],left_as_bar = F)

tt <- initDefaultTheme()
tt$highlight_window <- T
tsplot(tslist[2:4],left_as_bar = T,theme = tt)




debug(tsplot)
tsplot(tslist[2],left_as_bar = F)

undebug(tsplot)
tsplot(tslist$ts1)
tsplot(tslist,manual_date_ticks = 2010:2014)
tsplot(tslist)


undebug(drawTsBars)


tsplot(tslist[1:3],tsr = reflist,
       manual_date_ticks = 2010:2014,left_as_bar = T,theme=tt)

plot(rnorm(100))
rect(c(0,20,60),0,c(10,40,80),1,col=tt$bar_fill_color[2:4])


rect(20,0,40,1,col=tt$bar_fill_color[2])
rect(60,0,80,1,col=tt$bar_fill_color[3])





tsplot(tslist[1:3],tsr = NULL,
       manual_date_ticks = 2010:2014,
       left_as_bar = T,
       theme=tt)




tsplot(tslist$ts1,left_as_bar = F,theme = tt)



undebug(drawTsBars)
tsplot(tslist[1:3],manual_date_ticks = 2010:2015,left_as_bar = T)


# this seems to work... 
rect(ts_time[1],0,ts_time[1]+1/frq,h_pos[1L:NR_POS,1])

rect(2010,0,ts_time+1/frq)



tm <- do.call("cbind",tslist[1:3])

debug()
barplot(t(tm))
class(tm)

as.list(tm)

plot(rnorm)
abline(v=.3)
rect(0,c(-2,-1),0.1,c(-1,0),col=c("blue","green"))


rect(c(0,0.4),-2,c(0.2,0.5),0,col=c("blue","green"))


tf(tm,tm)



undebug(barplot)
barplot(t(do.call("cbind",tslist)))


hy <- t(tm)

apply(hy,2L,cumsum)


xyrect <- function(x1, y1, x2, y2, horizontal = TRUE, 
                   ...) {
  if (horizontal) 
    rect(x1, y1, x2, y2, ...)
  else rect(y1, x1, y2, x2, ...)
}

# else -0.01 * height)
height <- rbind(rectbase, apply(height, 2L, cumsum))

NR <- nrow(height)
NC <- ncol(height)


xyrect(height[1L:NR, i] + offset[i], w.l[i], 
       height[-1, i] + offset[i], w.r[i], horizontal = horiz, 
       angle = angle, density = density, col = col, 
       border = border)
