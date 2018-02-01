#' Import time series data from a file.
#'
#' If importing from a zip file, the archive should contain a single file with the extension .csv, .xlsx or .json.
#' 
#' @param file Path to the file to be read
#' @param format Which file format is the data stored in? If no format is supplied, read_ts will attempt to guess
#' from the file extension.
#' @param sep character seperator for csv files. defaults to ','.
#' @return A named list of ts objects
#' 
#' @importFrom data.table fread
#' @importFrom utils unzip
#' @export
read_ts <- function(file,
                             format = c("csv", "xlsx",
                                        "json", "zip"),
                             sep = ",") {
  if(length(format) == 1) {
    format <- match.arg(format)  
  } else {
    # Try to guess format from extension
    format <- regmatches(file, regexec(".*?[.](.*)$", file))[[1]][2]
    if(!(format %in% c("csv", "xlsx", "json", "zip"))) {
      stop("Could not detect file format. Please supply format parameter!\nValid file formats are csv, xlsx, json and zip.")
    }
  }
  
  if(format == "zip") {
    contents <- unzip(file, list=TRUE)
    
    if(nrow(contents) > 1) {
      warning("Found more than 1 file in zip archive, proceeding with the first one...")
    }
    
    zipped_file <- contents$Name[1]
    message(sprintf("Found file %s in zip archive, proceeding...", zipped_file))
  
    zipped_format <- regmatches(zipped_file, regexec(".*?[.](.*)$", zipped_file))[[1]][2]
    if(!(zipped_format %in% c("csv", "xlsx", "json"))) {
      stop("Zipped file is not a csv-, xlsx- or json-file!")
    }
    
    file <- unzip(file, zipped_file, exdir = tempdir())
    format <- zipped_format
  }
  
  switch(format, 
         "csv" = read_ts.csv(file),
         "xlsx" = read_ts.xlsx(file),
         "json" = read_ts.json(file)
  )
}

# Could export these, but no real need.
read_ts.csv <- function(file, sep = ",") {
  csv <- fread(file, sep = sep, stringsAsFactors = FALSE, colClasses = "numeric")
  
  if(length(csv) == 3 && length(setdiff(names(csv), c("date", "value", "series"))) == 0) {
    long_to_ts(csv)
  } else {
    wide_to_ts(csv)
  }
}


read_ts.xlsx <- function(file) {
  xlsx_available <- requireNamespace("openxlsx")
  
  if(!xlsx_available) return(warning("openxlsx not available. Install openxlsx or export to csv."))    
  
  xlsx <- openxlsx::read.xlsx(file)
  
  if(length(xlsx) == 3 && length(setdiff(names(xlsx), c("date", "value", "series"))) == 0) {
    long_to_ts(xlsx)
  } else {
    wide_to_ts(xlsx)
  }
}

#' @importFrom jsonlite fromJSON
read_ts.json <- function(file) {
  data <- fromJSON(readLines(file))
  
  lapply(data, json_to_ts)
}
