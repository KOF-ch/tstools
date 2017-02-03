#' @export 
print.SQL <- function(x,...){
  cat(gsub("\n[ \t]+","\n",x))
}


getGlobalXInfo <- function(tsl,tsr,fill_up_start){
  global_x <- list()
  
  if(!is.null(tsr)){
    all_ts <- c(tsl,tsr)  
  } else{
    all_ts <- tsl
  }
  
  all_ts_ext <- lapply(all_ts,fillUpYearWithNAs,fill_up_start = fill_up_start)
  global_x$x_range <- range(unlist(lapply(all_ts_ext,time)))
  
  global_x$min_year <- trunc(global_x$x_range[1])
  global_x$max_year <- trunc(global_x$x_range[2])
  
  # Yearly tick positions
  global_x$yearly_tick_pos <- global_x$min_year:global_x$max_year
  global_x$quarterly_tick_pos <- seq(from = global_x$min_year,
                                     to = global_x$max_year,
                                     by = .25)
  global_x$monthly_tick_pos <- seq(from = global_x$min_year,
                                   to = global_x$max_year,
                                   by = 1/12)
  
  # labels
  global_x$year_labels_start <- global_x$min_year:global_x$max_year
  global_x$year_labels_middle_q <- ifelse(global_x$quarterly_tick_pos -
                                          floor(global_x$quarterly_tick_pos) == 0.5,
                                        as.character(floor(global_x$quarterly_tick_pos)),
                                        NA)
  global_x$year_labels_middle_m <- ifelse(global_x$monthly_tick_pos -
                                          floor(global_x$monthly_tick_pos) == 0.5,
                                        as.character(floor(global_x$monthly_tick_pos)),
                                        NA)
                                        
  global_x
}

# o <- diff(r)*theme$y_offset_pct
# d$x_range <- r + c(-o,o)
#' determine ticks and grid position
#' theme contains which grids should be drawn etc.
.getDateInfo <- function(tsr,tsl,theme,
                         manual_date_ticks){
  d <- list()
  if(!is.null(manual_date_ticks)){
    d$x_ticks <- manual_date_ticks
    d$x_range <- range(manual_date_ticks)
    return(d)
  } else{
    # NO MANUAL X-AXIS given
    all_series <- c(tsl,tsr)
    d$x_range <- range(time(unlist(all_series)))
    if(theme$yearly_ticks){
      
    }
    if(theme$quarterly_ticks){
      if(theme$year_labels_mid){
        
      }
    }
  }
}



#' Make sure right axis object is of appropriate class.
.sanitizeTsr <- function(tsr){
  if(is.null(tsr)){
    return(tsr)
  } else if(inherits(tsr,"mts")){
    as.list(tsr)
  } else if(inherits(tsr,"ts")){
    list(tsr)
  } else if(inherits(tsr,"list")){
    tsr
  } else {
    stop("Time series object to be plotted on the right axis, 
         has to be either of class ts, mts or list.")
  }
}

.getXAxisInfo <- function(tsl,tsr,theme){
  unlist(c(tsr,tsl))
}







