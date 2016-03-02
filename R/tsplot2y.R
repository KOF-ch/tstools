tsplot2y <- function(x,y,...){
  
}




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




ts1 <- ts(rnorm(100),start=c(1990,1),freq=12)
ts2 <- ts(rnorm(100,30),start=c(1990,1),freq=12)

par(mar=c(5,4,4,5)+.1)
plot(ts1)
par(new=TRUE)
plot(ts2,type="b",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
