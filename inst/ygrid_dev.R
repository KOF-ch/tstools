# get order of magnitude
getMagnitude <- function(r){
  d <- diff(r)
  e <- round(log(d,10))
  m <- 10^e
  m
}


getTicks <- function(m1,m2=NULL,
                     prefer_max_grids = F,
                     potential_no_of_grids = c(4,5,6,7,8)){
  out <- list()
  allowed_space_1 <- potential_no_of_grids[(m1 %% potential_no_of_grids == 0)]
  allowed_space_2 <- integer()
  if(!is.null(m2)){
    allowed_space_2 <- potential_no_of_grids[(m2 %% potential_no_of_grids == 0)]
  }
  
  a <- c(allowed_space_1,allowed_space_2)
  d <- a[duplicated(a)]
  if(length(d) == 0) {
    if(prefer_max_grids){
      no_of_tcks <- max(allowed_space_1)  
    } else {
      no_of_tcks <- min(allowed_space_1)  
    }
  } else {
    if(prefer_max_grids){
      no_of_tcks <- max(d)  
    } else {
      no_of_tcks <- min(d)  
    }
    
  }
  
  out <- list()
  out$no_of_tcks <- no_of_tcks
  out$l_space <- m1/no_of_tcks
  out$r_space <- m2/no_of_tcks
  out
}

getTicks(o1,o2,prefer_max_grids = T)


o1 <- getMagnitude(c(50,1200))
o2 <- getMagnitude(c(0,100))

allowed_space_1 <- c(4:8)[(o1 %% 4:8 == 0)]
allowed_space_2 <- c(4:8)[(o2 %% 4:8 == 0)]


a <- c(allowed_space_1,allowed_space_2)
min(a[duplicated(a)])

min(a[duplicated(c(1,2,3,4))])




# http://stackoverflow.com/questions/361681/algorithm-for-nice-grid-line-intervals-on-a-graph
# set max number of ticks... 
d <- diff(range(c(50,1200)))
e <- round(log(d,10))
m <- 10^e

vpd <- m/5

ts <- d/5
mag <- trunc(log(ts,10))
magPow <- 10^mag

magMsd <- trunc(ts/magPow + 0.5)

if (magMsd > 5.0){
  magMsd = 10.0
} else if(magMsd > 2.0){
  magMsd = 5.0
} else if(magMsd > 1.0){
  magMsd = 2.0
}
  
magMsd*magPow


most_tick <- 10
min_space <- diff(range(tl$ts2))/ most_tick
mag <- log(min_space,base = 10)
resid <- min_space/mag
if(resid > 5){
  tick <- 10*mag
}

tslist


r <- range(tslist$ts2) 
a <- trunc((abs(r)*1.025)*sign(r))


tf <- diff(a) %/% 4:10 == diff(a) / 4:10
ft <- 4:10
ft[tf]


r2 <- range(tslist$ts4) 
a2 <- trunc((abs(r2)*1.025)*sign(r2))

tf2 <- diff(a2) %/% 4:10 == diff(a2) / 4:10
ft2 <- 4:10
ft2[tf2]


((input / x) + 1) * x;

((53 %/% 10) + 1) * 10



tl <- list()
tl$ts1 <- ts(runif(40,-3,3),start=c(1994,1),frequency=4)
tl$ts2 <- ts(runif(40,-30,50),start=c(1994,1),frequency=4)


r <- range(tl$ts1)*10
a <- trunc((abs(r)*1.025)*sign(r))

nchar(diff(a))

r2 <- range(tl$ts2)
a2 <- trunc((abs(r2)*1.025)*sign(r2))
diff(a2)

nchar(diff(a2))
