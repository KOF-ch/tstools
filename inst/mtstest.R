tslist <- list()
tslist$ts1 <- ts(runif(20,10,50),start=c(2010,1),
                 frequency = 4)
tslist$ts2 <- ts(runif(20,-10,20),start=c(2010,1),
                 frequency = 4)

tslist$ts3 <- ts(runif(20,-40,20),start=c(2010,1),
                 frequency = 4)



tsplot(tslist,manual_date_ticks = 2010:2014)

tm <- do.call("cbind",tslist)
class(tm)

as.list(tm)



tf(tm,tm)



undebug(barplot)
barplot(t(tm))


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


xyrect(height[1L:NR, i] + offset[i], w.l[i], 
       height[-1, i] + offset[i], w.r[i], horizontal = horiz, 
       angle = angle, density = density, col = col, 
       border = border)
