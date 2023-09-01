#' Deprecated function(s) in tstools
#'
#' These functions are provided for compatibility with older version of
#' the tstools package.  They may eventually be completely
#' removed.
#' @rdname tstools-deprecated
#' @name tstools-deprecated
#' @param ... Parameters to be passed to the modern version of the function
#' @export  computeDecimalTime concatTs fillupYearWitnNAs importTimeSeries initDefaultTheme overlapSortedLists overlapTslByName resolveOverlap stripLeadingNAsFromTs stripTrailingNAsFromTs writeTimeSeries
#' @aliases computeDecimalTime concatTs fillupYearWitnNAs importTimeSeries initDefaultTheme overlapSortedLists overlapTslByName resolveOverlap stripLeadingNAsFromTs stripTrailingNAsFromTs writeTimeSeries
#' @section Details:
#' \tabular{rl}{
#'   \code{computeDecimalTime} \tab now a synonym for \code{\link{compute_decimal_time}}\cr
#'   \code{concatTs} \tab now a synonym for \code{\link{concat_ts}}\cr
#'   \code{fillupYearWitnNAs} \tab now a synonym for \code{\link{fill_year_with_nas}}\cr
#'   \code{importTimeSeries} \tab now a synonym for \code{\link{read_ts}}\cr
#'   \code{init_tsplot_theme} \tab now a synonym for \code{\link{init_tsplot_theme}}\cr
#'   \code{overlapSortedLists} \tab now a synonym for \code{\link{overlap_sorted_ts_lists}}\cr
#'   \code{overlapTslByName} \tab now a synonym for \code{\link{overlap_ts_lists_by_name}}\cr
#'   \code{resolveOverlap} \tab now a synonym for \code{\link{resolve_ts_overlap}}\cr
#'   \code{stripLeadingNAsFromTs} \tab now a synonym for \code{\link{strip_ts_of_leading_nas}}\cr
#'   \code{stripTrailingNAsFromTs} \tab now a synonym for \code{\link{strip_ts_of_trailing_nas}}\cr
#'   \code{writeTimeSeries} \tab now a synonym for \code{\link{write_ts}}\cr
#' }
#'
NULL
computeDecimalTime <- function(...) {
  .Deprecated("compute_decimal_time", package = "tstools")
  compute_decimal_time(...)
}
concatTs <- function(...) {
  .Deprecated("concat_ts", package = "tstools")
  concat_ts(...)
}
fillupYearWitnNAs <- function(...) {
  .Deprecated("fill_year_with_nas", package = "tstools")
  fill_year_with_nas(...)
}
importTimeSeries <- function(...) {
  .Deprecated("read_ts", package = "tstools")
  read_ts(...)
}
initDefaultTheme <- function(...) {
  .Deprecated("init_tsplot_theme", package = "tstools")
  init_tsplot_theme(...)
}
overlapSortedLists <- function(...) {
  .Deprecated("overlap_sorted_ts_lists", package = "tstools")
  overlap_sorted_ts_lists(...)
}
overlapTslByName <- function(...) {
  .Deprecated("oberlap_ts_list_by_name", package = "tstools")
  overlap_ts_lists_by_name(...)
}
resolveOverlap <- function(...) {
  .Deprecated("resolve_ts_overlap", package = "tstools")
  resolve_ts_overlap(...)
}
stripLeadingNAsFromTs <- function(...) {
  .Deprecated("strip_ts_of_leading_nas", package = "tstools")
  strip_ts_of_leading_nas(...)
}
stripTrailingNAsFromTs <- function(...) {
  .Deprecated("strip_ts_of_leading_nas", package = "tstools")
  strip_ts_of_trailing_nas(...)
}
writeTimeSeries <- function(...) {
  .Deprecated("write_ts", package = "tstools")
  write_ts(...)
}
