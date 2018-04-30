#' Generate a list of random time series
#' 
#' Useful for development or generating easily reproducible examples
#'
#' @param n The number of ts objects to generate
#' @param lengths The lengths of the time series
#' @param starts The start points of the time series in single number notation (e.g. 1990.5)
#' @param frequencies The frequencies of the time series
#' @param ranges_min The minimum values of the time series (if normally_distributed == FALSE)
#' @param ranges_max The maximum values of the time series (if normally_distributed == FALSE)
#' @param shifts The shifts of time series values per series
#' @param ts_names The names of the ts objects in the resulting list
#' @param seed The random seed to be used
#' @param random_NAs Whether or not to introcude NA values at random positions in the ts
#' @param random_NA_proportions The fraction of values to be replaced with NAs if random_NAs is TRUE for the series
#' @param normally_distributed Use normal distribution instead of uniform
#' @param normal_means The means to use (if normally_distributed == TRUE)
#' @param normal_sds The sds to use (if normally_distributed == TRUE)
#' @param frequency_shifts Introduce frequency shifts (from 4 to 12) in the ts
#' @param frequency_shift_after After what fraction of the ts to shift frequencies
#'
#' @details 
#' Except for n, all parameters accept either a single value or a vector of values. If a single value is
#' supplied, that value is used for all time series being generated. If a vector is supplied, its values
#' will be used for the corresponding series (e.g. starts[1] is used for the first series, starts[2] for
#' the second and so on). Vectors are recycled if n is larger than their length.
#'
#' @return A list of ts objects
#' @export
#'
#' @examples
generate_random_ts <- function(n = 1,
                               lengths = 36,
                               starts = 1988,
                               frequencies = 12,
                               ranges_min = -1,
                               ranges_max = 1,
                               shifts = 0,
                               ts_names = sprintf("ts%d", 1:n),
                               seed = 30042018,
                               random_NAs = FALSE,
                               random_NA_proportions = 0.1,
                               normally_distributed = FALSE,
                               normal_means = 0,
                               normal_sds = 1,
                               frequency_shifts = FALSE,
                               frequency_shift_after = 0.5) {
  
  if(any(frequency_shifts & frequencies != 12)) {
    # may also determine locaton of error for bettar feedback
    stop("Frequency shift only supported if frequency is 12!")
  }
  
  set.seed(seed)
  
  out <- list()
  
  recycle <- function(values, index) {
    index <- index - 1
    values[(index %% length(values)) + 1]
  }
  
  for(i in 1:n) {
    n_x <- recycle(lengths, i)
    
    if(recycle(normally_distributed, i)) {
      x <- rnorm(n_x, recycle(normal_means, i), recycle(normal_sds, i)) + recycle(shifts, i)
    } else {
      x <- runif(n_x, recycle(ranges_min, i), recycle(ranges_max, i)) + recycle(shifts, i)
    }
    
    if(recycle(random_NAs, i)) {
      n_na <- ceiling(recycle(random_NA_proportions, i)*n_x)
      pos_na <- sample(1:n_x, n_na)
      x[pos_na] <- NA
    }
    
    if(recycle(frequency_shifts, i)) {
      breakpoint <- ceiling(recycle(frequency_shift_after, i)*n_x)
      
      if(breakpoint > 0) {
        before_shift <- x[1:breakpoint]
        after_shift <- x[(breakpoint+1):n_x]
        
        before_shift[!c(TRUE, FALSE, FALSE)] <- NA
        
        x <- c(before_shift, after_shift)
      }
    }
    
    s <- ts(x, start = recycle(starts, i), frequency = recycle(frequencies, i))
    
    nm <- recycle(ts_names, i)
    
    out[[nm]] <- s
  }
  
  out
}
