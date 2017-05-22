#' @export
drawTsBars <- function(x, theme = NULL,
                       ...){
  UseMethod("drawTsBars")
} 

#' @export
drawTsBars.ts <- function(x, theme = NULL){
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
       col = theme$bar_fill_color[1])
  
  rect(ts_time,pos_0,
            ts_time+1/frq,
            0,
            border = theme$bar_border,
            col = theme$bar_fill_color[1])
  
  
}


#' @export
drawTsBars.list <- function(x,
                            theme = NULL){
  if(length(x) == 1){
    drawTsBars(x[[1]],theme=theme)
  } else{
    m <- do.call("cbind",x)
    drawTsBars(m,theme = theme)  
  }
  
}


#' @export
drawTsBars.mts <- function(x, theme = NULL){
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
  
  for (i in 1L:NC_POS) {
  rect(ts_time[i],
       h_pos[1L:NR_POS,i],
       ts_time[i]+1/frq,
       h_pos[-1,i],
       col = theme$bar_fill_color[1:NR_POS],
       border = theme$bar_border
       )
  }
  
  # draw the negative part
  h_neg <- rbind(rectbase = 0,apply(t(pos_0),2L,cumsum))
  for (i in 1L:NC_POS) {
    rect(ts_time[i],
         h_neg[1L:NR_POS,i],
         ts_time[i]+1/frq,
         h_neg[-1,i],
         col = theme$bar_fill_color[1:NR_POS],
         border = theme$bar_border
    )
  }

}

