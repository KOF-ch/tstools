#' @export
kofplot <- function(tsl,
                    tsr = NULL,
                    manual_date_range = NULL,
                    l_manual_value_range = NULL,
                    global_theme = NULL,
                    tsl_theme = NULL,
                    tsr_theme = NULL){
  
  if(is.null(global_theme)) global_theme <- initDefaultTheme()
  
  if(global_theme$fillYearWithNA){
    tsl <- lapply(tsl,fillUpYearWithNAs)
    if(!is.null(tsr)) tsr <- lapply(tsr,fillUpYearWithNAs)
  }
  
  
  all_series <- c(tsl,tsr)
  global_info <- getTimeInfo(all_series)
  tsl_info <- getTimeInfo(tsl)
  if(!is.null(tsr)) tsr_info <- getTimeInfo(tsr)
  
  if(!is.null(l_manual_value_range)){
    tsl_info$y_range <- l_manual_value_range
  }
  
  
  # put left
  plot(NULL,
       xlim = global_info$x_range,
       ylim = tsl_info$y_range,
       axes = F,
       xlab = "",
       ylab = "",
       yaxs = tsl_theme$yaxs,
       xaxs = tsl_theme$xaxs
  )
  
  # add x-axis  #####################
  if(global_theme$year_labels_mid){
    if(global_theme$yearly_ticks){
      axis(1,
           at = global_info$x_range[1]:global_info$x_range[2],
           xlim = global_info$x_range,
           lwd.ticks = global_theme$lwd_ticks_y,
           tcl = global_theme$tcl_y,
           labels = FALSE)
    }
    
    q_tick_pos <- seq(global_info$x_range[1],
                      global_info$x_range[2],by=.25)
    
    blanks <- rep(NA,length(q_tick_pos))
    q_tick_tf <- q_tick_pos - floor(q_tick_pos) == .5
    blanks[q_tick_tf] <- trunc(q_tick_pos[q_tick_tf])
    q_tick_lab <- blanks

    if(global_theme$quarterly_ticks){
      axis(1,at = q_tick_pos,
           xlim = global_info$x_range,
           lwd.ticks = global_theme$lwd_ticks_q,
           labels = q_tick_lab)
    }
  } else{
    if(global_theme$yearly_ticks){
      axis(1,
           at = global_info$x_range[1]:global_info$x_range[2],
           xlim = global_info$x_range,
           lwd.ticks = global_theme$lwd_ticks_y)
    }
    
    if(global_theme$quarterly_ticks){
      axis(1,at = seq(global_info$x_range[1],
                      global_info$x_range[2],by=.25),
           xlim = global_info$x_range,
           lwd.ticks = global_theme$lwd_ticks_q,
           labels = FALSE)
    }
  }
  
  # add y-axis LEFT #####################
  
  
  
  
  # add y-axis right
  
  
  
  
}

#' @export
getTimeInfo <- function(tsl){
  info <- list()
  info$x_range <- range(sapply(tsl,time),na.rm = T)
  info$y_range <- range(unlist(tsl),na.rm = T)
  info
}
