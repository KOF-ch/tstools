# toJSON method, toCSV method, toXLSX method

#' EXPERIMENTAL: Write a List of Time Series to .csv, .xlsx, or .json
#' 
#' The writeTslist generic is usually used wrapped 
#' within \code{\link{exportTslist}}. It writes a list
#' of multiple time series objects to disk according
#' to the class of the list of time series. You may want
#' to set this class manually and call the writeTsList directly, but using the wrapper for writing is the way to go for most people. 
#' 
#' @param tsl list of time series
#' @param fn character filename, defaults to NULL which sets the file name automatically. 
#' @param date_format specify an output date format using \code{\link{format}}'s syntax. 
#' @param ... additional optional parameters which may be method specific. the 'wide' parameter is specific to .xlsx and .csv exports.
#' @rdname writeTslist
#' @export
writeTslist <- function(tsl, fn = NULL, date_format = NULL, ...) UseMethod("writeTslist")



#' @importFrom jsonlite toJSON
#' @importFrom utils zip
#' @rdname writeTslist
#' @export
writeTslist.toJSON <- function(tsl,
                               fn = NULL,
                               date_format = NULL, ...){
  ll <- lapply(tsl,function(x){
    xx <- xts::as.xts(x)
    
    if(frequency(x) == 4){
      dv <- as.yearqtr(time(xx))  
    } else if(frequency(x) == 12){
      dv <- as.yearqtr(time(xx))  
    } else{
      dv <- as.character(time(xx))
    }
    
    if(!is.null(date_format)){
     dv <- format(dv,date_format) 
    }
    
    d <- data.frame(date = dv,
                    value = x,
                    stringsAsFactors = F)
    d
    })
  
  write(toJSON(ll,
               dataframe = "values" ,
               pretty=T),
        paste(fn,"json",sep="."))
  if(zip) zip(paste(fn,"zip",sep = "."),
              paste(fn,"json",sep="."))
}








