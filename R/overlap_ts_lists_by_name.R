#' Resolve Overlap Listwise, helpful with SA
#'
#'
#' @param listA list of time series often of lower frequency
#' @param listB list of time series often of higher frequency
#' @param chunkA character chunk representing frequencies, defaults to _f4.
#' @param chunkB character chunk representing frequences, defaults to _f12.
#' @export
overlap_ts_lists_by_name <- function(listA, listB,
                                     chunkA = "_f4",
                                     chunkB = "_f12") {
  nma <- names(listA)
  ccl <- lapply(nma, function(x) {
    nmb <- gsub(x, chunkA, chunkB)
    out <- tryCatch(
      {
        resolveOverlap(
          listA[[x]],
          listB[[nmb]]
        )
      },
      error = function(e) {
        attr(listB[[nmb]], "concat") <- FALSE
        listB[[nmb]]
      }
    )
    out
  })
  ccl
}
