tslist <- list()
tslist$ts1 <- ts(runif(20,10,50),start=c(2010,1),
                 frequency = 4)
tslist$ts2 <- ts(runif(20,-10,20),start=c(2010,1),
                 frequency = 4)

tslist$ts3 <- ts(runif(20,-40,20),start=c(2010,1),
                 frequency = 4)

tslist$ts4 <- ts(runif(15,-40,20),start=c(2010,1),
                 frequency = 4)

reflist <- list()
reflist$rs1 <- ts(runif(20,-3,3),start=c(2010,1),
              frequency = 4)

reflist$rs2 <- ts(runif(20,-5,5),start=c(2010,1),
                  frequency = 4)

tslist[1:3]



undebug(tsplot)
tsplot(tslist$ts1,manual_date_ticks = 2010:2014,left_as_bar = T,theme=tt)

tsplot(tslist[1:3],tsr = reflist,manual_date_ticks = 2010:2014,left_as_bar = T,theme=tt)



tt <- initDefaultTheme()
tt$highlight_window <- T
tsplot(tslist$ts1,manual_date_ticks = 2010:2014,theme = tt)


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
