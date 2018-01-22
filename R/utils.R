#' Compute Decimal Time from a ts Period Vector
#' 
#' Standard ts object use a vector of length two to store a period. E.g. 2010,1 means first quarter of 2010, 
#' if the series was quarterly and first month if the series was monthly etc. 
#' 
#' @param v integer vector denoting a period in time
#' @param f frequency
#' @export
computeDecimalTime <- function(v,f){
  multi <- 1/f
  (v[2]-1)*multi + v[1]
}


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
# determine ticks and grid position
# theme contains which grids should be drawn etc.
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



# Make sure right axis object is of appropriate class.
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


addYGrids <- function(tick_positions,theme){
  for (hl in tick_positions){
    abline(h = hl,
           col = theme$y_grid_color)
  } 
}

#'@export
findGapSize <- function(r,tick_count){
  d <- diff(r)
  raw_tick_size <- d / (tick_count-1)
  m <- ceiling(log(raw_tick_size,10)-1);
  pow10m <- 10^m
  ceil_tick_size <- ceiling(raw_tick_size / pow10m) * pow10m;
  ceil_tick_size
}

#'@export
findTicks <- function(r,tick_count){
  # potential tick count needs to sorted otherwise, 
  # automatic selection of 
  gaps <- findGapSize(r=r,sort(tick_count))
  lb <- (r[1] %/% gaps) * gaps
  d <- ceiling(diff(r))
  tms <- (d %/% gaps) + 1
  ub <- lb + (tms * gaps)  
  # correct algorithm when values are below upper bound
  ub_large_enough <- ub >= r[2]
  tms[!ub_large_enough] <- tms[!ub_large_enough] + 1
  ub <- lb + (tms * gaps)
  # overwrite everything else if there is only one tick
  # cause this is rather a forced command because tick_count
  # was determined in a previous call!
  if(length(tick_count) == 1){
    ub <- lb + ((tick_count-1) * gaps)
  }
  seqs <- list()
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

formatNumericDate <- function(date, freq, date_format = NULL) {
  year <- floor(date)
  if(freq[1] == 4) {
    if(is.null(date_format)) {
      quarter <- 4*(date - year) + 1
      return(sprintf("%d Q%d", year, quarter))
    } else {
      month <- floor(12*(date - year)) + 1
    }
  } else {
    month <- floor(12*(date - year)) + 1
    if(is.null(date_format)) {
      return(sprintf("%d %d", year, month))
    }
  }
  
  format(as.Date(sprintf("%d-%d-01", year, month)), date_format)
}

