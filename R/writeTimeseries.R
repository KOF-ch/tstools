#' writeTimeSeries
#' 
#' Export a list of time series to a file.
#' @param tl list of time series
#' @param fname character file name. If set to NULL a standard file name chunk + Sys.Date is used.
#' @param date_format character denotes the date format. Defaults to NULL. If set to null the default is used: Jan 2010.
#' @param timestamp_file If TRUE, the current date will be appended to the file name
#'
#' @param wide optional for csv and xlsx. If TRUE, the data is written in a wide format. Defaults to FALSE.
#' @param json_pretty optional for json. If TRUE the JSON is outputted in a more human readable format.
#' This results in larger file sizes. Defualts to FALSE.
#'
#' @importFrom jsonlite toJSON
#' @import data.table
#' @export
writeTimeSeries <- function(tl,
                            fname = "timeseriesdb_export",
                            format = c("csv", "xlsx", "json", "rdata"),
                            date_format = NULL, timestamp_file = TRUE,
                            round_digits = NULL,
                            ...)
{
  args <- list(...)
  
  # Match format
  format <- match.arg(format);

  # Timestamp filename
  if(timestamp_file) {
    fname <- paste0(fname,"_",gsub("-","_",Sys.Date()))
  }
  
  wide <- ifelse(!is.null(args$wide), args$wide, FALSE)

  # check for format compatability
  if(format %in% c("csv", "xlsx") && wide) {
    ts_lengths <- sapply(tl, length)
    if(!all(diff(ts_lengths)==0)) {
      warning("list contains time series of different lengths. Export to wide .csv or xlsx is not recommended.")
    }
  }
  
  # Export data
  if(format == "rdata") {
     # Dump list into an empty environment
     env <- new.env();
     
     varname <- ifelse(!is.null(args$rdata_varname), args$rdata_varname, fname)
     
     env[[varname]] <- tl;
     
     write_name <- paste0(fname, ".RData")
     
     # Save said environment
     save(list=ls(env), file=write_name, envir=env)
  } else {
    nTs <- length(tl)
    
    if(format != "json") {
      if(!wide) {
        # convert the list into a pretty, long data.frame
        tl_lengths <- data.table(length = sapply(tl, length))
        
        index <- seq(nrow(tl_lengths))
        
        tsdf <- tl_lengths[, .(internal_index = seq(length)), by = index]
        
        tl_names <- names(tl)
        
        tsdf[, `:=`(
                      freq = frequency(tl[[index]]),
                      series = tl_names[index],
                      value = as.numeric(tl[[index]][internal_index]),
                      date_numeric = as.numeric(time(tl[[index]]))), 
             by = index]
        
        tsdf[, date := formatNumericDate(date_numeric, freq, date_format), by = freq]
        
        tsdf[ , `:=`(index = NULL, date_numeric = NULL, freq = NULL, internal_index = NULL)]
        setcolorder(tsdf, c("date", "value", "series"))

      } else {
        tsmat <- do.call("cbind", tl)
        dates <- time(tsmat[,1])
        freq <- frequency(tl[[1]])
        
        tsdf <- as.data.table(tsmat)
        tsdf[, date := formatNumericDate(dates, freq, date_format)]
        
        setcolorder(tsdf, c(nTs+1, seq(nTs)))
      }
    }
    
    if(format == "json") {
      json_pretty <- ifelse(!is.null(args$json_pretty), args$json_pretty, FALSE) # TODO: getArgs helper?
    
      # Output an object of arrays of objects { "key": [{"date": time1, "value": value1}, ...], ...}
      jsondf <- lapply(tl, function(x) {
        t <- time(x)
        f <- frequency(x)
        
        t <- formatNumericDate(t, f, date_format)
        
        data.frame(date = t, value = x, row.names = NULL)
      })
      json <- toJSON(jsondf, pretty=json_pretty, digits=16)
      
      write_name <- paste(fname, "json", sep=".")
      
      # Write json as a "single element CSV" for speed
      fwrite(list(json),
                         file = write_name,
             quote = FALSE, col.names = FALSE)
      
    } else {
      if(wide) {
        # Check for frequency consistency
        frq <- unique(sapply(tl,frequency))
        if(length(frq) != 1) {
          stop("All time series need to have the same frequency for proper wide export.")
        }
      }
      
      
      xlsx_available <- requireNamespace("openxlsx", quietly = TRUE)
      if(!xlsx_available) {
        format <- "csv"
        warning("package openxlsx not available, writing .csv")
      }
      
      if(format == "xlsx"){
        if(nTs > 1000) {
          stop("XSLX format can not handle more than 1000 time series")
        } else if(nrow(tsdf) > 1e6) {
          stop("XLSX format can not handle more than 1'000'000 rows")
        }
        
        write_name <- paste0(fname, ".xlsx")
        openxlsx::write.xlsx(tsdf,
                             paste0(fname,".xlsx"))
        
      } else{
        write_name <- paste0(fname, ".csv")
        
        fwrite(tsdf, write_name) 
      }
    }
  }
  
  should_zip <- ifelse(!is.null(args$zip), args$zip, FALSE)
  
  if(should_zip) {
    zip_name <- paste0(fname, ".zip")
    unlink(zip_name)
    zip(zip_name, write_name)
  }
}
