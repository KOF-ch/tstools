#' Compute Decimal Time from a ts Period Vector
#' 
#' Standard ts object use a vector of length two to store a period. E.g. 2010,1 means first quarter of 2010, 
#' if the series was quarterly and first month if the series was monthly etc. 
#' 
#' @param v integer vector denoting a point in time
#' @param f frequency
#' @export
compute_decimal_time <- function(v,f){
  multi <- 1/f
  (v[2]-1)*multi + v[1]
}

#' Compute the Period Vector representation of a Decimal Time value
#' 
#' The period value will be rounded down to the nearest integer.
#' This function is not vectorized so only a single value can be
#' converted at a time.
#' 
#' @param dtime numeric decimal time value denoting a point in time
#' @param frq integer frequency
get_date_vector <- function(dtime, frq){
  y <- floor(dtime)
  p <- dtime - y
  c(y, floor(frq * p + 1 + 1/(4*frq)))
}

# function is called by tsplot, do not need to export
# it but let's write a minimal comment on what it does.
getGlobalXInfo <- function(tsl, tsr, fill_up, fill_up_start, dt, manual_ticks){
  global_x <- list()
  
  if(!is.null(tsr)){
    all_ts <- c(tsl,tsr)  
  } else{
    all_ts <- tsl
  }
  
  if(is.null(manual_ticks)) {
    if(fill_up) {
      all_ts <- lapply(all_ts,fill_year_with_nas,fill_up_start = fill_up_start)
    }
    
    global_x$x_range <- range(unlist(lapply(all_ts,time)))
    
    # Set the lower bound to correspond with a quarterly tick, for pretties
    global_x$x_range[1] <- trunc(global_x$x_range[1]*4)/4
    global_x$x_range[2] <- trunc(global_x$x_range[2]*4 + 0.76)/4
    
    # Yearly tick positions
    global_x$yearly_tick_pos <- seq(floor(global_x$x_range[1]), global_x$x_range[2] + dt, dt)
    
    # labels
    global_x$year_labels_start <- seq(global_x$x_range[1], global_x$x_range[2] + dt, dt)
  } else {
    global_x$x_range <- range(manual_ticks)
    global_x$yearly_tick_pos <- manual_ticks
    global_x$year_labels_start <- manual_ticks
  }
  
  global_x$min_year <- trunc(global_x$x_range[1])
  global_x$max_year <- trunc(global_x$x_range[2])+1
  
  if(dt == 1) {
    global_x$quarterly_tick_pos <- seq(from = global_x$min_year,
                                       to = global_x$max_year,
                                       by = .25)
    # global_x$monthly_tick_pos <- seq(from = global_x$min_year,
    #                                  to = global_x$max_year,
    #                                  by = 1/12)
    global_x$year_labels_middle_q <- ifelse(global_x$quarterly_tick_pos -
                                            floor(global_x$quarterly_tick_pos) == 0.5,
                                          as.character(floor(global_x$quarterly_tick_pos)),
                                          NA)
    # global_x$year_labels_middle_m <- ifelse(global_x$monthly_tick_pos -
    #                                       floor(global_x$monthly_tick_pos) == 0.5,
    #                                     as.character(floor(global_x$monthly_tick_pos)),
    #                                      NA)
  } else {
    global_x$quarterly_tick_pos <- NA
    global_x$year_labels_middle_q <- NA
  }
                                          
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

#' @importFrom graphics lines
addYGrids <- function(tick_positions, xlim, theme){
  for (hl in tick_positions[2:length(tick_positions)]){
    lines( x = xlim,
           y = c(hl, hl),
           col = theme$y_grid_color,
           lwd = theme$lwd_y_ticks,
           xpd = TRUE)
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


findTicks <- function(r, tick_count, preferred_gap_sizes, round_ticks = FALSE, preserve_sign = FALSE){
  # potential tick count needs to sorted otherwise, 
  # automatic selection of
  gap_count <- tick_count-1
  gaps <- findGapSize(r=r,sort(tick_count))
  lb <- (r[1] %/% gaps) * gaps
  
  ub <- lb + (gap_count * gaps)  
  
  # nudge the generated range around a bit to ensure the series are more or less "centered"
  # i.e. there are no empty ticks
  lb_too_low <- r[1] > lb + gaps & (!preserve_sign | (sign(ub) == sign(ub + gaps/2) & sign(lb) == sign(lb + gaps/2)))
  lb[lb_too_low] <- lb[lb_too_low] + gaps[lb_too_low]/2
  ub[lb_too_low] <- ub[lb_too_low] + gaps[lb_too_low]/2
  
  ub_too_high <- r[2] < ub - gaps & (!preserve_sign | (sign(ub) == sign(ub - gaps/2) & sign(lb) == sign(lb - gaps/2)))
  lb[ub_too_high] <- lb[ub_too_high] - gaps[ub_too_high]/2
  ub[ub_too_high] <- ub[ub_too_high] - gaps[ub_too_high]/2
  
  seqs <- list()
  for(i in seq_along(gaps)) {
    seqs[[i]] <- seq(lb[i],ub[i],gaps[i])
  }
  
  # First take any best fitting range
  w <- which.max((lb-r[1]) + (r[2]-ub))
  out <- seqs[[w]]
  
  # Try to select a reasonably pretty gap size
  preferred_gap_sizes <- sort(preferred_gap_sizes, decreasing = TRUE)
  for(gs in preferred_gap_sizes) {
    by_gs <- which(gaps %% gs == 0)
    
    # If one or more ranges with the desired gap size exist
    # return the one with the least number of ticks
    if(any(by_gs)) {
      out <- seqs[[min(by_gs)]]
      break
    }
  }
  
  if(round_ticks) {
    out <- floor(out)
  }
  
  out
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

alpha2Hex <- function(alpha) {
  if(is.character(alpha)) {
    return(alpha)
  }
  
  if(floor(alpha) == alpha) {
    alpha <- as.hexmode(alpha)  
  } else {
    alpha <- as.hexmode(floor(256*alpha))
  }
}

#' @importFrom grDevices colors col2rgb rgb
namedColor2Hex <- function(color, alpha = NULL) {
  if(is.numeric(alpha)) {
    alpha <- alpha2Hex(alpha)
  }

  known_colors <- color %in% colors()
  
  color[known_colors] <- rgb(t(col2rgb(color[known_colors])), maxColorValue = 255)
  
  no_alpha <- nchar(color) < 9
  
  color[no_alpha] <- paste0(color[no_alpha], alpha)
  
  color
}


#' Helper to calculate ci colors for legends
#' 
#' @param color The color of the ci band
#' @param n The number if ci bands
#' @param alpha The alpha/transparency of the ci band
#' 
#' @details 
#' Color may be specified as either a named color or a hex value
#' Transparency may be specified as a hex value, number 0-255 or number 0-1
#' 
#' @return A vector of non-transparent colors that result from
#' oberlaying color over pure white 1:n times
#' 
#' @importFrom grDevices col2rgb rgb
getCiLegendColors <- function(color, n = 1, alpha = NULL) {
  colorRGBA <- col2rgb(color, alpha = TRUE)
  colorRGB <- colorRGBA[1:3,1]
  
  alpha <- ifelse(is.null(alpha), colorRGBA["alpha", 1], as.numeric(paste0("0x", alpha2Hex(alpha))))/256
  
  out <- c()
  
  ca <- colorRGB
  cb <- col2rgb("white")
  
  for(i in seq(n)) {
    cb <- alpha*ca + (1 - alpha)*cb
    out[i] <- rgb(t(round(cb)), maxColorValue = 255) #paste(as.hexmode(floor(cb)), collapse = "")
  }
  
  out
}
