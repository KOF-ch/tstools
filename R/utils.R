#' Compute Decimal Time from a ts Period Vector
#' 
#' Standard ts object use a vector of length two to store a period. E.g. 2010,1 means first quarter of 2010, 
#' if the series was quarterly and first month if the series was monthly etc. 
#' 
#' @param v integer vector denoting a period in time
#' @param f frequency
#' @export
compute_decimal_time <- function(v,f){
  multi <- 1/f
  (v[2]-1)*multi + v[1]
}


# function is called by tsplot, do not need to export
# it but let's write a minimal comment on what it does.
getGlobalXInfo <- function(tsl,tsr,fill_up_start){
  global_x <- list()
  
  if(!is.null(tsr)){
    all_ts <- c(tsl,tsr)  
  } else{
    all_ts <- tsl
  }
  
  all_ts_ext <- lapply(all_ts,fill_year_with_nas,fill_up_start = fill_up_start)
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




# Make sure right axis object is of appropriate class.
sanitizeTsr <- function(tsr){
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

#' @importFrom graphics abline
addYGrids <- function(tick_positions,theme){
  for (hl in tick_positions[2:length(tick_positions)]){
    abline(h = hl,
           col = theme$y_grid_color)
  } 
}


findGapSize <- function(r,tick_count){
  d <- diff(r)
  raw_tick_size <- d / (tick_count-1)
  m <- ceiling(log(raw_tick_size,10)-1);
  pow10m <- 10^m
  ceil_tick_size <- ceiling(raw_tick_size / pow10m) * pow10m;
  ceil_tick_size
}


findTicks <- function(r, tick_count, preferred_gap_sizes, preserve_sign = FALSE){
  # potential tick count needs to sorted otherwise, 
  # automatic selection of
  gaps <- findGapSize(r=r,sort(tick_count))
  lb <- (r[1] %/% gaps) * gaps
  
  ub <- lb + ((tick_count - 1) * gaps)  
  
  if(length(tick_count) == 1) {
    ub <- lb + ((tick_count - 1)*gaps)
  }
  
  # nudge the generated range around a bit to ensure the series are more or less "centered"
  # i.e. there are no empty ticks
  lb_too_low <- r[1] > lb + gaps & (!preserve_sign | sign(lb) == sign(lb + gaps/2))
  lb[lb_too_low] <- lb[lb_too_low] + gaps[lb_too_low]/2
  ub[lb_too_low] <- ub[lb_too_low] + gaps[lb_too_low]/2
  
  ub_too_high <- r[2] < ub - gaps & (!preserve_sign | sign(ub) == sign(ub - gaps/2))
  lb[ub_too_high] <- lb[ub_too_high] - gaps[ub_too_high]/2
  ub[ub_too_high] <- ub[ub_too_high] - gaps[ub_too_high]/2

  seqs <- list()
  for(i in seq_along(gaps)) {
    seqs[[i]] <- seq(lb[i],ub[i],gaps[i])
  }
  
  # Try to select a reasonably pretty gap size
  preferred_gap_sizes <- sort(preferred_gap_sizes, decreasing = TRUE)
  for(gs in preferred_gap_sizes) {
    by_gs <- which(gaps %% gs == 0)
    
    # If one or more ranges with the desired gap size exist
    # return the one with the least number of ticks
    if(any(by_gs)) {
      return(seqs[[min(by_gs)]])
    }
  }
  
  # No pretty gaps found
  w <- which.max((lb-r[1]) + (r[2]-ub))
  seqs[[w]]
}

formatNumericDate <- function(date, freq, date_format = NULL) {
  year <- floor(date + 1/24)
  if(freq[1] == 4) {
    if(is.null(date_format)) {
      quarter <- 4*(date - year) + 1
      return(sprintf("%d Q%d", year, quarter))
    } else {
      month <- floor(12*(date - year)) + 1
    }
  } else {
    month <- floor(12*(date - year + 1/24)) + 1  # Why "+ 1/24"? Because floating point arithmetic. 12*0.0833333 may not be 12.
    if(is.null(date_format)) {
      return(sprintf("%d-%02d", year, month))
    }
  }
  
  format(as.Date(sprintf("%d-%d-01", year, month)), date_format)
}

