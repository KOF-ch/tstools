#' importTimeSeries
#'
#' Import time series data from a file.
#' 
#' @param file Path to the file to be read
#' @param format Which file format is the data stored in? If no format is supplied, importTimeSeries will attempt to guess
#' from the file extension.
#' @return A named list of ts objects
#' @export
importTimeSeries <- function(file, format=c("csv", "xlsx", "json")) {
  if(length(format) == 1) {
    format <- match.arg(format)  
  } else {
    # Try to guess format from extension
    format <- regmatches(file, regexec(".*?[.](.*)$", file))[[1]][2]
    if(!(format %in% c("csv", "xlsx", "json"))) {
      stop("Could not detect file format. Please supply format parameter!\nValid file formats are csv, xlsx and json.")
    }
  }
  
  switch(format, 
         "csv" = importTimeSeries.csv(file),
         "xlsx" = importTimeSeries.xlsx(file),
         "json" = importTimeSeries.json(file)
  )
}

# Could export these, but no real need.
importTimeSeries.csv <- function(file) {
  csv <- read.csv(file, sep=";", stringsAsFactors=FALSE)
  
  if(length(csv) == 3 && all(names(csv) == c("date", "value", "series"))) {
    long_to_ts(csv)
  } else {
    wide_to_ts(csv)
  }
}

#' @importFrom openxlsx read.xlsx
importTimeSeries.xlsx <- function(file) {
  xlsx <- read.xlsx(file)
  
  if(length(xlsx) == 3 && all(names(xlsx) == c("date", "value", "series"))) {
    long_to_ts(xlsx)
  } else {
    wide_to_ts(xlsx)
  }
}

#' @importFrom jsonlite fromJSON
importTimeSeries.json <- function(file) {
  data <- fromJSON(readLines(file))
  
  lapply(data, json_to_ts)
}