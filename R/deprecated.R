#' Deprecated function(s) in tstools
#' 
#' These functions are provided for compatibility with older version of
#' the tstools package.  They may eventually be completely
#' removed.
#' @rdname tstools-deprecated
#' @name tstools-deprecated
#' @param ... Parameters to be passed to the modern version of the function
#' @docType package
#' @export  computeDecimalTime
#' @aliases computeDecimalTime
#' @section Details:
#' \tabular{rl}{
#'   \code{computeDecimalTime} \tab now a synonym for \code{\link{compute_decimal_time}}\cr
#' }
#
computeDecimalTime <- function(...) {
  .Deprecated("compute_decimal_time", package="tstools")
  compute_decimal_time(...)
}

# concatTs <- function(...) {
#   .Deprecated("concat_ts", package="tstools")
#   concat_ts(...)
# }
# 
# fillupYearWitnNAs <- function(...) {
#   .Deprecated("fill_year_with_nas", package="tstools")
#   fill_year_with_nas(...)
# }
# 
# importTimeSeries <- function(...) {
#   .Deprecated("read_ts", package="tstools")
#   read_ts(...)
# }
# 
# initDefaultTheme <- function(...) {
#   .Deprecated("default_plot_theme", package="tstools")
#   default_plot_theme(...)
# }
# 
# overlapSortedLists <- function(...) {
#   .Deprecated("overlap_sorted_lists", package="tstools")
#   overlap_sorted_lists(...)
# }
# 
# overlapTslByName <- function(...) {
#   .Deprecated("oberlap_ts_list_by_name", package="tstools")
#   oberlap_ts_list_by_name(...)
# }
# 
# resolveOverlap <- function(...) {
#   .Deprecated("resolve_overlap", package="tstools")
#   resolve_overlap(...)
# }
# 
# stripLeadingNAsFromTs <- function(...) {
#   .Deprecated("strip_leading_nas", package="tstools")
#   strip_leading_nas(...)
# }
# 
# writeTimeSeries <- function(...) {
#   .Deprecated("write_ts", package="tstools")
#   write_ts(...)
# }

NULL