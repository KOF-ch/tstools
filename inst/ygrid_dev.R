
findGapSize <- function(r,tick_count){
  d <- diff(r)
  raw_tick_size <- d / (tick_count-1)
  m <- ceiling(log(raw_tick_size,10)-1);
  pow10m <- 10^m
  ceil_tick_size <- ceiling(raw_tick_size / pow10m) * pow10m;
  ceil_tick_size
}

findTicks <- function(r,tick_count){
  # potential tick count needs to sorted otherwise, 
  # automatic selection of 
  gaps <- findGapSize(r=r,sort(tick_count))
  lb <- (r[1] %/% gaps) * gaps
  d <- ceiling(diff(r))
  tms <- (d %/% gaps) + 1
  ub <- lb + (tms * gaps)
  seqs <- list()
  bys <- list()
  for(i in seq_along(gaps)){
    seqs[[i]] <- seq(lb[i],ub[i],gaps[i])
  }
  
  # prefer max number of ticks
  # that can be  devided by 10
  # second best: by 5
  # otherwise
  by10 <- which(gaps %% 10 == 0)
  by5 <- which(gaps %% 5 == 0)
  if(any(by10)){
    return(seqs[[max(by10)]])
  } else if(any(by5)){
    return(seqs[[max(by5)]])
  } else{
    w <- which.max((lb-r[1]) + (r[2]-ub))
    seqs[[w]]
  }
  
}



debug(findTicks)
findTicks(c(30,100),c(9))

kb <- range(KOF$kofbarometer)
ref <- range(KOF$reference)

f <- function(a=findGapSize){
  a(c(20,100),5)
}




# if any is by element is 10,5, or twenty prefer that? 
max(which(c(3,4) %% 10 == 0))











findBounds <- function(r_tsl, r_tsr = NULL,
                       potential_tick_count = c(5,6,8,10)){
  # find tick gaps at a different number of ticks (5-10)
  # this a vector, also lower/upper bound are vectors
  # denoting lower/upper bound 
  # suggestions for potential ticks
  l_ticks <- findTickSize(r_tsl,potential_tick_count)  
  kb[1] %/% l_ticks
  
  
  n_lower_b <- ((r_tsl[1] %/% l_ticks) * l_ticks) 
  n_upper_b <- ((r_tsl[2] %/% l_ticks)+1) * l_ticks
  
  # create difference to raw bounds 
  # and finds number of ticks with 
  # smallest total (top and bottom) difference 
  out <- list()
  wm <- which.min((r_tsl[1]-n_lower_b) +
                    (n_upper_b-r_tsl[2]))
  out$ticks <- seq(n_lower_b[wm],n_upper_b[wm],
                   potential_tick_count[wm])
  out$n_lower_b <- n_lower_b
  out$n_upper_b <- n_upper_b
  out$potential_tick_count <- potential_tick_count
  out
}

debug(findBounds)
xx <- findBounds(kb)

seq(xx$n_lower_b[4],xx$n_upper_b[4],
    by = xx$potential_tick_count[4])


rts <- findTickSize(ref,c(5,6,8,10))
findBounds(kb)

lb <- ((kb[1] %/% bts) * bts) 
ub <- ((kb[2] %/% bts)+1) * bts

lb1 <- ((ref[1] %/% rts) * rts) 
ub1 <- ((ref[2] %/% rts)+1) * rts

# use axis that wastes least space.., i.e., sum of difference is minimal on both ends... 
which.min((kb[1]-lb) + (ub-kb[2]))






r <- diff(t2)
tickCount <- 8
unroundedTickSize <- r/(tickCount-1);
x <- ceiling(log(unroundedTickSize,10)-1);
pow10x <- 10^x
roundedTickRange = ceiling(unroundedTickSize / pow10x) * pow10x;
roundedTickRange

bestTick(t2,8)







#http://stackoverflow.com/questions/326679/choosing-an-attractive-linear-scale-for-a-graphs-y-axis
# Find ylim 
t1 <- c(0,100)
t2 <- c(4,87)
t3 <- c(232,1234)
t4 <- c(-5,4)
d1 <- diff(t1)
d2 <- diff(t2)
d3 <- diff(t3)
d4 <- diff(t4)
m1 <- getMagnitude(t1)
m2 <- getMagnitude(t2)
m3 <- getMagnitude(t3)
m4 <- getMagnitude(t4)



  bestTick <- function(r,m){
    # get difference
    r <- diff(r)
    # minimum spacing of the ticks
    mini <- r / m
    # magnitude to determine the ballpark of the scale
    mag <- 10^(floor(log(mini,10)))
    # how often does mag fit into the minimum spacing??
    residual <- mini / mag
    if (residual > 5){
      tick <-  10 * mag
    } else if(residual >2){
      tick <- 5 * mag
    } else if(residual > 1){
      tick <- 2 * mag
    } else {
      tick <- mag
    }
    tick
    
  }  
  
  getMagnitude <- function(r){
    d <- diff(r)
    e <- round(log(d,10))
    m <- 10^e
    m
  }  

undebug(bestTick)
bestTick(c(198,435),50)

198 %/% 50
220 %/% 200
1*200

ma <- getMagnitude(c(193,434))
getTicks(ma)

bestTick(c(-.31,1.2),7)
bestTick(c(0,1100),6)


bestTick(c(-30,107),7)


# get order of magnitude
getMagnitude <- function(r){
  d <- diff(r)
  e <- round(log(d,10))
  m <- 10^e
  m
}

getTicks <- function(m1,r){
  m1/r
}

r1 <- c(0,74)
r2 <- c(0,183)
o1 <- getMagnitude(r1)
o2 <- getMagnitude(r2)

ceiling(o1 / diff(r1))

getTicks(o1,diff(r1))
getTicks(o2,diff(r2))

getTicks <- function(m1,m2=NULL,
                     prefer_max_grids = T,
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
  class(out) <- c("ticks",class(out))
  out
}


o1/diff(c(0,100))
o2/diff(c(0,500))

tcks <- getTicks(o1,o2)

#


diff(c(0,200))/o2

undebug(getTicks)
to <- getTicks(o1,o2)
xx <- range(c(50,1250))
round(xx[1]/to$l_space)
ceiling(xx[2]/to$l_space)*to$l_space

yy <- c(0,200)
round(yy[1]/to$r_space)
ceiling(yy[2]/to$l_space)*to$r_space


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
