#' Concat Time Series list wise
#' 
#' Concat overlapping time series list wise. List needs
#' to be of same length. Takes names of list B.
#' 
#' @param listA list of time series
#' @param listB list of time series
#' @export
overlap_sorted_ts_lists <- function(listA,listB){
  stopifnot(length(listA) == length(listB))
  concat_list <- lapply(seq_along(listA),function(x){
    out <- tryCatch({resolveOverlap(listA[[x]],listB[[x]])},
                    error = function(e){
                      attr(listB[[x]],"concat") <- FALSE
                      listB[[x]]
                    } )
    out
  })
  names(concat_list) <- names(listB)
  concat_list
}
