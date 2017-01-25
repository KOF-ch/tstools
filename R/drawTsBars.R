#' @export
drawTsBars <- function(tsl,
         show_sums_as_line = F,
         manual_value_range = NULL,
         manual_date_range = NULL,
         theme = NULL,
         no_plot = F){
  
  if(is.null(theme)){
    theme <- initDefaultBarTheme()
  }
  
  if(inherits(tsl,"ts")){
    tsl <- as.list(tsl)
  }
  
  if(!all(unlist(lapply(tsl,is.ts))))
    stop("all elements of the list need to be objects of class ts.")
  
  # time vector
  d <- list()
  ts_time <- unique(unlist(lapply(tsl,time)))
  d$ts_time <- round(ts_time,digits = 5)
  
  
  date_range <- range(ts_time)
  # matrix works well with pos / neg checks.
  tsmat <- do.call("cbind",tsl)
  
  par(mar = theme$mar)
  
  # check whether all values are positive... because this would
  # be much simpler with the stacking.
  if(!sum(tsmat < 0, na.rm = T) > 0){
    value_range <- c(floor(ifelse(is.null(dim(tsmat)),
                                  min(tsmat, na.rm = T),
                                  min(rowSums(tsmat,na.rm = T)
                                  ))),
                     ceiling(ifelse(is.null(dim(tsmat)),
                                    max(tsmat, na.rm = T),
                                    max(rowSums(tsmat, na.rm = T)
                                    ))))
    
    if(!is.null(manual_value_range)) value_range <- 
        manual_value_range
    
    pos_part <- barplot(t(tsmat),
                        ylim = value_range,
                        axes = F,
                        xlim = c(0,length(ts_time)*1.25),
                        col = theme$line_colors,
                        plot = !no_plot)
  } else {
    neg_0 <- tsmat
    pos_0 <- tsmat
    neg_0[tsmat < 0] <- 0
    pos_0[!tsmat < 0] <- 0
    
    
    
    value_range <- c(floor(ifelse(is.null(dim(tsmat)),
                                  min(tsmat, na.rm = T),
                                  min(rowSums(tsmat,na.rm = T)
                                  ))),
                     ceiling(ifelse(is.null(dim(tsmat)),
                                    max(tsmat, na.rm = T),
                                    max(rowSums(tsmat, na.rm = T)
                                    ))))
    
    if(!is.null(manual_value_range)) value_range <- 
      manual_value_range
    
    
    pos_part <- barplot(t(neg_0),
                        ylim = value_range,
                        axes = F,
                        xlim = c(0,length(ts_time)*1.25),
                        col = theme$line_colors,
                        plot = !no_plot)
    neg_part <- barplot(t(pos_0),
                        ylim = value_range,
                        axes = F,
                        xlim = c(0,length(ts_time)*1.25),
                        col = theme$line_colors,
                        plot = !no_plot,add=T)
    
    if(show_sums_as_line==T) {
      
      c_vect_col_sums <- colSums(t(c_vect))
      lines(x=c_barplot1, y=c_vect_col_sums)
      
    }
  }
  
  
  d$value_range <- value_range
  d$date_range <- date_range
  d$bar_pos <- pos_part
  d
}
