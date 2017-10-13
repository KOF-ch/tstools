#' TODO: Dox
#' @param tl list of time series
#' @param fname character file name. If set to NULL a standard file name chunk + Sys.Date is used.
#' @param wide logical. Should the resulting data.frame be cast to wide format? Defaults to TRUE
#' @param xlsx logical. Should data be exported to .xlsx? Defaults to FALSE.
#' @param LC_TIME_LOCALE character time locale that differs from the standard locale. e.g. en_US.UTF-8. Defaults to NULL and uses the standard locale then. 
#' @param date_format character denotes the date format. Defaults to NULL. If set to null the default is used: Jan 2010. In combination with LC\_TIME\_Locale various international date formats can be produced. 
#' @importFrom reshape2 dcast
#' @importFrom xts as.xts
#' @importFrom openxlsx write.xlsx
#' @importFrom zoo as.yearmon
#' @export
writeTimeSeries <- function(tl,
                            fname = "timeseriesdb_export",
                            format = c("csv", "xlsx", "json", "rdata"),
                            date_format = NULL,
                            ...)
{
  args = list(...)
  
  # Match format
  format = match.arg(format);

  # Timestamp filename
  fname <- paste0(fname,"_",gsub("-","_",Sys.Date()))
  
  # Standardize to xts (readTimeSeries may return ts, zoo, xts)
  tl <- lapply(tl, as.xts)
  
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
    wide = ifelse(!is.null(args$wide), args$wide, FALSE)
    
    if(!wide && format != "json") {
      # Make the list into a pretty, long data.frame
      out_list <- lapply(names(tl),function(x){
        t <- time(tl[[x]])
        if(!is.null(date_format)) {
          t <- format(t, date_format)
        }
        dframe <- data.frame(time = t,
                             value = tl[[x]],row.names = NULL)
        dframe$series <- x
        dframe
      })
      tsdf <- do.call("rbind",out_list)
    } else {
      tsdf <- as.data.frame(tl)
      tsdf$time <- time(tl[[1]])
      if(!is.null(date_format)) {
        tsdf$time <- format(tsdf$time, date_format)
      }
      tsdf <- tsdf[, c(nTs+1, seq(1, nTs))]
    }
    
    if(format == "json") {
      json_apiformat <- ifelse(!is.null(args$json_apiformat), args$json_apiformat, FALSE)
      json_pretty <- ifelse(!is.null(args$json_pretty), args$json_pretty, FALSE) # TODO: getArgs helper?
      
      if(json_apiformat) {
        # Meh, spin our own serializer
        newline <- ifelse(json_pretty, "\n", "")
        json <- paste0("[", newline, "[\"", paste(names(tsdf), collapse="\",\""), "\"],", newline)
        for(i in seq(1, nrow(tsdf))) {
          json <- paste0(json, "[\"", tsdf[i, 1], "\",", paste(tsdf[i, seq(2, nTs+1)], collapse=","), "]", ifelse(i == nrow(tsdf), "", ","), newline)
        }
        json <- paste0(json, "]")
      } else {
        # Output an object of arrays of objects { "key": [{"time": time1, "value": value1}, ...], ...}
        jsondf <- lapply(tl, function(x) {
          t <- time(x)
          if(!is.null(date_format)) {
            t <- format(t, date_format)
          }
          data.frame(time=as.character(t), value=x, row.names=NULL)
        })
        json <- toJSON(jsondf, pretty=json_pretty, digits=16)
      }
      
      write_name <- paste(fname, "json", sep=".")
      
      write(json, write_name)
      
    } else {
      if(wide) {
        # Check for frequency consistency
        frq <- unique(sapply(tl,frequency))
        if(length(frq) != 1) {
          stop("All time series need to have the same frequency for proper wide export.")
        }
      }
      
      if(format == "xlsx"){
        if(nTs > 1000) {
          stop("XSLX format can not handle more than 1000 time series")
        } else if(nrow(tsdf) > 1e6) {
          stop("XLSX format can not handle more than 1'000'000 rows")
        }
        
        write_name <- paste0(fname, ".xlsx")
        
        write.xlsx(tsdf, paste0(fname,".xlsx"))
      } else{
        write_name <- paste0(fname, ".csv")
        
        write.table(tsdf, paste0(fname,".csv"), row.names = F, quote = F, sep=";", dec=".")  
      }
    }
  }
  
  should_zip <- ifelse(!is.null(args$zip), args$zip, FALSE)
  
  if(should_zip) {
    zip(paste0(fname, ".zip"), write_name)
  }
}