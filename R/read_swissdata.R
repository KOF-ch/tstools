#' Read swissdata style csv to tslist
#' 
#' @param path Path to the CSV file
#' @param key_columns The swissdata dimensions to appear in the ts key
#' 
#' @details 
#' The order of dimensions in key_columns determines their order in the key
#' The resulting ts_key will be of the form <swissdata-set-name>.<instance of key_columns[1]>...
#' 
#' @export
read_swissdata <- function(path, key_columns) {
  dataset <- gsub("(.+wd\\/)(.+)(\\/.+)","\\2",path)
  raw <- data.table::fread(path)
  
  raw[,series := do.call(paste,
                         c(dataset,.SD,sep=".")),
                         .SDcols = key_columns]
  a <- long_to_ts(raw[, .(series, date, value)])
}

