#' @export
addYAxis <- function(d,
                     right = F,
                     y_grd_steps,
                     manual_value_range){
  stps <- abs(d$value_range[1]-
                d$value_range[2])/y_grd_steps
  
  if(is.null(manual_value_range)){
    tick_positions <- seq(d$value_range[1],
                          d$value_range[2],
                          by = ceiling(stps))    
  } else {
    tick_positions <- seq(manual_value_range[1],
                          manual_value_range[2],
                          by = ceiling(stps))    
  }
  
  if(right){
    axis(4, at = tick_positions)
  } else{
    axis(2, at = tick_positions)
  }
  tick_positions
}

#' @export
addXAxis <- function(d, isBar = F, theme){
  # BAR PLOT ###########
  if(isBar){
    # set labels, we need blanks, because we do not want to have a label
    # at every x-tick
    blanks <- rep(NA,length(d$ts_time))
    
    # the base barplot function has its own understanding of x-axis
    if(theme$label_quarterly){
      #tf <- abs(d$ts_time*4 - floor(d$ts_time *4)) < 0.001
    } else{
      # replace blanks where we want labels
      # in this case only at begin of full years
      tf <- abs(d$ts_time-trunc(d$ts_time)) < 0.001
      blanks[tf] <- trunc(d$ts_time[tf])
      bar_pos_labels <- blanks
      axis(1, at = d$bar_pos, labels = bar_pos_labels, yaxs = theme$yaxs,
          tcl = theme$tcl_1, cex.axis = theme$cex.axis_1, padj = theme$padj_1)
      # thick tick marks
      axis(1, at = d$bar_pos[tf],labels = FALSE,
           tcl = theme$tcl_2,
           lwd.ticks = theme$lwd_ticks_1)
    }
  # LINE PLOT ###########  
  } else {
    ts_time <- d$ts_time
    ext_qtr <- ts_time[abs(ts_time * 4 - floor(ts_time * 4)) < 0.001]
    ext_label <- ifelse(ext_qtr - floor(ext_qtr) == 0.5,
                        as.character(floor(ext_qtr)), NA)
    if(theme$label_quarterly){
      # x-axis
      axis(1, at = ext_qtr, labels = ext_label,
           tcl = theme$tcl_1, cex.axis = theme$cex.axis_1, padj = theme$padj_1)
      axis(1, at = d$date_range[1]:d$date_range[2],
           tcl = theme$tcl_2,
           lwd.ticks = theme$lwd_ticks_1, labels = FALSE) # thick tick marks
    } else {
      axis(1, at = ts_time,labels = ts_time,
           tcl = theme$tcl_1, cex.axis = theme$cex.axis_1, padj = theme$padj_1)
    }
  }
  
}

#' @export
addYGrids <- function(tick_positions,theme){
  for (hl in tick_positions){
    abline(h = hl,
           col = theme$ygrid_color)
  } 
}



