a <- barplot(t(short),
        width=rep(10,length(short)), # width is split in half... 
        space=0,
        yaxs="i",
        xlim = c(0,535),
        xaxs="i",
        ylim=c(-4,10))

axis(1,at=seq(from=0,to=10*52,by=10))
abline(v=0,col="blue")
abline(v=10,col="blue")
par(new=T)
plot(short,yaxs="i",axes=F,xaxs="i",
     ylim=c(-4,10),type="b",col="blue",lwd=2,xlim=c(0,535))
axis(4,ylim=c(50,130))
lines(x=a-5,y=short,type="b",col="blue",lwd=2)

# well this doesn't help much.. need to start from scratch drawing rectangles...
short <- window(KOF$reference,start=c(2008,1),end=c(2012,4))
short_b <- window(KOF$kofbarometer,start=c(2008,1),end=c(2012,4))


library(tempdisagg)
short_q <- ta(short,to = "quarterly",conversion = "average")
short_b_q <- ta(short_b,to = "quarterly",conversion = "average")

plot(short_b,axes=F,type="b",xlim=c(2008,2013))
axis(1,xlim=c(2008,2013))
# should do pos and neg sep
library(scales)
rect(time(short_b_q),0,time(short_b_q)+1/4,short_b_q,col = alpha("blue",.5))

rect(time(short_b),0,time(short_b)+1/12,short_b)



getTimeInfo(short_b)

debug(kofplot)
kofplot(short_b)
getTimeInfo(list(short_b))























range(unlist(short_b))
undebug(kofplot)
kofplot(short_b)
debug(getTimeInfo)
getTimeInfo(short_b)

# http://stats.stackexchange.com/questions/14942/can-i-pass-an-at-parameter-for-the-x-axis-locations-of-bars-to-an-r-barplot

time(short_b)







a <- barplot(t(short_q),
             width=rep(10,length(short_q)), # width is split in half... 
             space=0,
             yaxs="i",
             xlim = c(0,165),
             xaxs="i",
             ylim=c(-4,10))

axis(1,at=seq(from=0,to=10*52,by=10))
abline(v=0,col="blue")
abline(v=10,col="blue")
par(new=T)
plot(short,yaxs="i",axes=F,xaxs="i",
     ylim=c(-4,10),type="b",col="blue",lwd=2,xlim=c(0,535))
axis(4,ylim=c(50,130))
lines(x=a-5,y=short,type="b",col="blue",lwd=2)



x    <- sort(sample(1:100, 10, replace=FALSE)) # x-coordinates
y    <- log(x)                                 # y-coordinates
yD   <- c(0, 2*diff(y))                        # twice the change between steps
barW <- 1                                      # width of bars

plot(x, y, ylim=c(0, log(100)), pch=16)
rect(xleft=x-barW, ybottom=0, xright=x+barW, ytop=yD, col=gray(0.5))










x <- seq(from = 11, to = 132, by=11)
for (i in x){
  abline(v=i)
}




axis(1,at = a)
abline(v=6)
abline(v=132)
abline(v=11)


12x + 11y = 1
x = 10y

# zwischenraumgrösse
(1/131)
# x
(1/131)*10

c(x1, x2, y1, y2)

mx <- max(short)



a <- barplot(t(short),
             width=rep((1/131)*10,length(short)),
             space=(1/131),
             yaxs="i",
             xaxs="i",
             ylim=c(-4,10),
             xlim=range(a))

par(new=T)

# they don't have the same xlim !!!! 
plot(short,yaxs="i",xaxs="i",ylim=c(-4,10),type="b",col="blue",lwd=2)



a <- barplot(t(short),
             width=rep((1/131)*10,length(short)),
             space=(1/131),
             yaxs="i",
             xaxs="i",
             ylim=c(-4,10),
             xlim=)

lines(x=a,y=short,col="blue",lwd=3)

x = c(1:81)
bp <- barplot(x)
axis(side=1,at=bp[1+c(0,20,40,60,80)],labels=c(20,40,60,80,100))


b <- barplot(x)
axis(side=1,at=b[c(20,40,60,80)],labels=seq(20,80,by=20))


par(new = T)

# they don't have the same xlim !!!! 
plot(short,yaxs="i",xaxs="i",ylim=c(-4,10),type="b",col="blue",lwd=2)

