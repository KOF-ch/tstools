#' Set Periods to NA
#' 
#' This function is typically used to discard information in non-quarter month. 
#' I.e., data is only kept in January, April, July and December and otherwise set
#' to NA. In combination with \code{\link{m_to_q}} this function is useful to 
#' turn monthly series into quarterly series by letting the quarter month values
#' represent the entire quarter. This can be useful when data was interpolated
#' because of mixing data of different frequencies and needs to be converted 
#' back to a regular, quarterly time series. 
#' 
#' @param series ts object
#' @param keep_month integer vector denoting the months that not be set to NA. 
#' Defaults to c(1,4,7,10)
#' 
#' @examples 
#' tsq <- ts(1:20,start=c(1990,1),frequency = 4)
#' aa <- tsqm(tsq)
#' m_to_q(set_month_to_NA(aa))
#' 
#' @importFrom zoo index
#' @export
#' @importFrom zoo index
set_month_to_NA <- function(series, keep_month = c(1,4,7,10)){
  # get the index of the keep periods
  # in order to replace the non-keep month with NA
  idx <- index(series) - floor(index(series))
  series[!idx %in% idx[keep_month]] <- NA
  series
}
