pw <- window(presidents,start=c(1965))
barplot(t(pw),ylim = c(0,80))
par(new=T,usr = par("usr"))
plot(pw,ylim = c(0,80),col="blue",lwd=3, yaxs="i",xaxs="i")



plot(short, ylim = c(-10,10),
     yaxs="i",col="blue",lwd=3)
par(new=T)
xx <- barplot(t(short),ylim = c(-10,10))
abline(h=0,col="blue",lwd=1)
abline(h=2.7619020,col="blue",lwd=1)
abline(v = .7,col="blue",lwd=1)
abline(v = 13.9,col="blue",lwd=1)
abline(v = 2008,col="blue",lwd=1)
abline(v = 2012.25,col="blue",lwd=1)



require(grDevices) # for colours
par(mfrow=c(1,1))
r <- barplot(tN, col = rainbow(20),main='short y axis',ann=FALSE,axes=FALSE)
usr <- par("usr")
par(usr=c(usr[1:2], 0, 20))
axis(2,at=seq(0,20,5))

par("usr")

par(usr = c(usr[1:2],0,80))
