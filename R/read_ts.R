#' Import time series data from a file.
#'
#' If importing from a zip file, the archive should contain a single file with the extension .csv, .xlsx or .json.
#'
#' @param file Path to the file to be read
#' @param format Which file format is the data stored in? If no format is supplied, read_ts will attempt to guess
#' from the file extension.
#' @param sep character seperator for csv files. defaults to ','.
#' @param skip numeric See data.table's fread.
#' @param column_names character vector denoting column names, defaults to c("date","value","series).
#' @param keep_last_freq_only in case there is a frequency change in a time series,
#' should only the part of the series be returned that has the same frequency as
#' the last observation. This is useful when data start out crappy and then stabilize
#' after a while. Defaults to FALSE. Hence only the last part of the series is returned.
#' @param force_xts If set to true, the time series will be returned as xts objects regargless of
#' regularity. Setting this to TRUE means keep_last_freq_only is ignored.
#' @return A named list of ts objects
#'
#' @importFrom data.table fread
#' @importFrom utils unzip
#' @export
read_ts <- function(file,
                    format = c(
                      "csv", "xlsx",
                      "json", "zip"
                    ),
                    sep = ",",
                    skip = 0,
                    column_names = c("date", "value", "series"),
                    keep_last_freq_only = FALSE,
                    force_xts = FALSE) {
  if (length(format) == 1) {
    format <- match.arg(format)
  } else {
    # Try to guess format from extension
    format <- regmatches(file, regexec(".*?[.](.*)$", file))[[1]][2]
    if (!(format %in% c("csv", "xlsx", "json", "zip"))) {
      stop("Could not detect file format. Please supply format parameter!\nValid file formats are csv, xlsx, json and zip.")
    }
  }

  if (format == "zip") {
    contents <- unzip(file, list = TRUE)

    if (nrow(contents) > 1) {
      warning("Found more than 1 file in zip archive, proceeding with the first one...")
    }

    zipped_file <- contents$Name[1]
    message(sprintf("Found file %s in zip archive, proceeding...", zipped_file))

    zipped_format <- regmatches(zipped_file, regexec(".*?[.](.*)$", zipped_file))[[1]][2]
    if (!(zipped_format %in% c("csv", "xlsx", "json"))) {
      stop("Zipped file is not a csv-, xlsx- or json-file!")
    }

    file <- unzip(file, zipped_file, exdir = tempdir())
    format <- zipped_format
  }

  switch(format,
    "csv" = read_ts.csv(file, sep, skip = skip, column_names = column_names, keep_last_freq_only = keep_last_freq_only, force_xts = force_xts),
    "xlsx" = read_ts.xlsx(file, column_names = column_names, keep_last_freq_only = keep_last_freq_only, force_xts = force_xts),
    "json" = read_ts.json(file)
  )
}

# Could export these, but no real need.
read_ts.csv <- function(file, sep = ",", skip, column_names = c("date", "value", "series"),
                        keep_last_freq_only = FALSE,
                        force_xts = FALSE) {
  csv <- fread(file, sep = sep, stringsAsFactors = FALSE, colClasses = "numeric", skip = skip)

  if (length(csv) == 3 && length(setdiff(names(csv), column_names)) == 0) {
    long_to_ts(csv, keep_last_freq_only = keep_last_freq_only, force_xts = force_xts)
  } else {
    wide_to_ts(csv, keep_last_freq_only = keep_last_freq_only, force_xts = force_xts)
  }
}


read_ts.xlsx <- function(file, column_names = c("date", "value", "series"),
                         keep_last_freq_only = FALSE,
                         force_xts = FALSE) {
  xlsx_available <- requireNamespace("openxlsx")

  if (!xlsx_available) {
    return(warning("openxlsx not available. Install openxlsx or export to csv."))
  }

  xlsx <- data.table::as.data.table(openxlsx::read.xlsx(file))

  if (length(xlsx) == 3 && length(setdiff(names(xlsx), column_names)) == 0) {
    long_to_ts(xlsx, keep_last_freq_only = keep_last_freq_only, force_xts = force_xts)
  } else {
    wide_to_ts(xlsx, keep_last_freq_only = keep_last_freq_only, force_xts = force_xts)
  }
}

#' @importFrom jsonlite fromJSON
read_ts.json <- function(file) {
  data <- fromJSON(readLines(file))

  lapply(data, json_to_ts)
}
