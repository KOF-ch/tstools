tsplot(KOF)

tsb1 <- ts(runif(30,-30,20),start=c(2010,1),frequency = 4)
tsb2 <- ts(runif(30,0,50),start=c(2010,1),frequency = 4)
tsb3 <- ts(runif(30,0,50),start=c(2010,1),frequency = 4)
tsb4 <- ts(runif(30,0,50),start=c(2010,1),frequency = 4)
ll <- list(t1 = tsb1, t2 = tsb2, t3 = tsb3)
llr <- list(t4 = tsb4)

tsplot(ll,tsr = llr,left_as_bar = T,
       manual_value_ticks_l = seq(-40,100,by=20))
