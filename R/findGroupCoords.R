findGroupCoords <- function(x,theme,i){
  ts_time <- time(x)
  COLS <- ncol(x)
  FRQ <- frequency(x)
  T_SPACE <- 1/FRQ
  
  T_MARGIN_SPACE <- theme$total_bar_margin_pct*T_SPACE 
  MARGIN_SPACE <- T_MARGIN_SPACE/2
  T_BAR_SPACE <- (1-theme$total_bar_margin_pct)*T_SPACE
  BAR_SPACE <- T_BAR_SPACE/COLS
  # XL vector
  coords <- list()
  coords$xl <- cumsum(c(0,rep(BAR_SPACE, COLS-1))) +
    MARGIN_SPACE +
    ts_time[i]
  
  # YB vector
  coords$yb <- rep(0,COLS)
  
  # XR vector
  coords$xr <- coords$xl + BAR_SPACE
  
  # YT vector
  coords$yt <- apply(x,1,function(x) x)[,i]
  coords
}



# n_series <- ncol(x)
# t_space <- 1/frq
# 
# 
# 
# t_bar_space <- (1-theme$total_bar_margin_pct)*t_space 
# t_margin_space <- theme$total_bar_margin_pct*t_space 
# bar_space <- t_bar_space/n_series
# margin_space <- t_margin_space/2
# 
# # draw the positive part
# #h_pos <- rbind(rectbar = 0,apply(t(neg_0),2L,cumsum))
# h_pos <- rbind(rectbase = 0,apply(t(neg_0),2L,cumsum))
# NR_POS <- nrow(h_pos[-1,])
# NC_POS <- ncol(h_pos)
# #(ts_time[i]+cumsum(rep(bar_space,n_series)))
# 
# h_pos_gr <- rbind(rectbase = 0,apply(t(neg_0),2L,function(x) x))
# rect(
#   ts_time[i]+margin_space,
#   0,
#   ts_time[i]+bar_space,
#   h_pos_gr[-1,i][1],
#   col = theme$bar_fill_color[1],
#   border = theme$bar_border
# )
