draw_ts_bars <- function(x, group_bar_chart = FALSE, theme = NULL,
                       ...){
  UseMethod("draw_ts_bars")
} 


#' @importFrom graphics rect
draw_ts_bars.ts <- function(x, group_bar_chart = FALSE,
                          theme = NULL){
  ts_time <- time(x)
  frq <- frequency(x)
  neg_0 <- x
  pos_0 <- x
  neg_0[x < 0] <- 0
  pos_0[!x < 0] <- 0
  
  
  rect(ts_time,0,
       ts_time+1/frq,
       neg_0,
       border = theme$bar_border,
       lwd = theme$bar_border_lwd,
       col = theme$bar_fill_color[1])
  
  rect(ts_time,pos_0,
            ts_time+1/frq,
            0,
            border = theme$bar_border,
            lwd = theme$bar_border_lwd,
            col = theme$bar_fill_color[1])
  
  
}


#' @importFrom graphics rect
draw_ts_bars.list <- function(x,
                            group_bar_chart = FALSE,
                            theme = NULL){
  if(length(x) == 1){
    draw_ts_bars(x[[1]],
               group_bar_chart =  group_bar_chart,
               theme = theme)
  } else{
    m <- do.call("cbind",x)
    draw_ts_bars(m,
               group_bar_chart =  group_bar_chart,
               theme = theme)  
  }
  
}


#' @importFrom graphics rect
draw_ts_bars.mts <- function(x,
                           group_bar_chart =  FALSE,
                           theme = NULL){
  if(is.null(group_bar_chart)) group_bar_chart <- FALSE

  x[is.na(x)] <- 0
  
  ts_time <- time(x)
  frq <- frequency(x)
  neg_0 <- x
  pos_0 <- x
  neg_0[x < 0] <- 0
  pos_0[!x < 0] <- 0

  
    # draw the positive part
  #h_pos <- rbind(rectbar = 0,apply(t(neg_0),2L,cumsum))
  h_pos <- rbind(rectbase = 0,apply(t(neg_0),2L,cumsum))
  NR_POS <- nrow(h_pos[-1,])
  NC_POS <- ncol(h_pos)
  #(ts_time[i]+cumsum(rep(bar_space,n_series)))

  for (i in 1L:NC_POS) {
    if(group_bar_chart){
      coords <- find_group_coords(x,theme,i)      
      rect(coords$xl, coords$yb,
           coords$xr, coords$yt,
           col = theme$bar_fill_color,
           lwd = theme$bar_border_lwd,
           border = theme$bar_border
           )
    } else{
      rect(ts_time[i],
           h_pos[1L:NR_POS,i],
           ts_time[i]+1/frq,
           h_pos[-1,i],
           col = theme$bar_fill_color[1:NR_POS],
           lwd = theme$bar_border_lwd,
           border = theme$bar_border
      )    
    }
  
  }
  
  # draw the negative part
  h_neg <- rbind(rectbase = 0,apply(t(pos_0),2L,cumsum))
  for (i in 1L:NC_POS) {
    if(group_bar_chart){
      
    } else{
      rect(ts_time[i],
           h_neg[1L:NR_POS,i],
           ts_time[i]+1/frq,
           h_neg[-1,i],
           col = theme$bar_fill_color[1:NR_POS],
           lwd = theme$bar_border_lwd,
           border = theme$bar_border
      )  
    }
      
   
    
  }

}

