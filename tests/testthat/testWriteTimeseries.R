context("write_ts")

n <- 100
freq <- 12
t <- yearmon(seq(2011, 2011+(n-1)/freq, 1/freq))
xts <- list(
  ts1 = xts::xts(runif(n), order.by=t),
  ts2 = xts::xts(runif(n), order.by=t)
)

ts <- list(
  ts1 = as.ts(xts$ts1, start=start(xts$ts1), end=end(xts$ts1)),
  ts2 = as.ts(xts$ts2, start=start(xts$ts2), end=end(xts$ts2))
)

zoo <- list(
  ts1 = as.zoo(xts$ts1),
  ts2 = as.zoo(xts$ts2)
)

fn <- file.path(tempdir(), "test_ts")
fn_read <- paste0(fn, "_",gsub("-","_",Sys.Date()))

csv_wide_name <- paste0(fn, "_wide", "_", gsub("-","_",Sys.Date()), ".csv")
csv_wide_transposed_name <- paste0(fn, "_wide_transposed_", gsub("-", "_", Sys.Date()), ".csv")
csv_long_name <- paste0(fn, "_long", "_", gsub("-","_",Sys.Date()), ".csv")
xlsx_wide_name <- paste0(fn, "_wide", "_", gsub("-","_",Sys.Date()), ".xlsx")
xlsx_wide_transposed_name <- paste0(fn, "_wide_transposed_", gsub("-", "_", Sys.Date()), ".xlsx")
xlsx_long_name <- paste0(fn, "_long", "_", gsub("-","_",Sys.Date()), ".xlsx")
rdata_read_name <- paste0(fn, "_", gsub("-","_",Sys.Date()), ".RData")
json_read_name <- paste0(fn, "_", gsub("-","_",Sys.Date()), ".json")
zip_read_name <- paste0(fn, "_", gsub("-","_",Sys.Date()), ".zip")

test_suite <- function(tslist) {
    test_that("CSV wide export works", {
      
      write_ts(tslist, format="csv", fname = paste0(fn, "_wide"),
                      date_format="%Y-%m", timestamp_to_fn = T, wide=TRUE)
      xts_read <- read.csv(csv_wide_name, sep=",", stringsAsFactors=FALSE)
      
      expect_that(dim(xts_read), equals(c(n, 3)))
      expect_that(names(xts_read), equals(c("date", "ts1", "ts2")))
      
      t_read <- as.yearmon(xts_read$date)
      xts_read$date <- NULL
      xts_read <- lapply(xts_read, function(x) {
        xts::xts(x, order.by = t_read)
      })
      
      expect_that(xts_read, equals(xts, tolerance=1e-4))
    })
    
    test_that("CSV transposed export works", {
      write_ts(tslist, format="csv",
                      fname=paste0(fn, "_wide_transposed"),
                      date_format="%Y-%m", timestamp_to_fn = T,
                      wide=TRUE, transpose=TRUE)
      
      xts_read <- read.csv(csv_wide_transposed_name, sep=",", stringsAsFactors = FALSE, header = FALSE)
      
      expect_that(dim(xts_read), equals(c(3, n+1)))
      
      t_read <- as.yearmon(as.character(xts_read[1,2:(n+1)]))
      xts_read_xts <- list()
      
      for(i in 2:3) {
        xts_read_xts[[i-1]] <- xts::xts(as.numeric(xts_read[i,2:(n+1)]), order.by = t_read)
      }
      
      names(xts_read_xts) <- xts_read[2:3, 1]
      
      expect_that(xts_read_xts, equals(xts, tolerance = 1e-4))
    })
    
    test_that("CSV long export works", {
      write_ts(tslist, format="csv",
                      fname=paste0(fn, "_long"),
               date_format="%Y-%m", timestamp_to_fn = T)
      xts_read <- read.csv(csv_long_name, sep=",",
                           stringsAsFactors = FALSE)
      
      expect_that(dim(xts_read), equals(c(2*n, 3)))
      expect_that(setdiff(names(xts_read), c("date", "value", "series")), equals(character(0)))
    
      xts_read <- reshape2::dcast(xts_read, date ~ series)
      
      t_read <- as.yearmon(xts_read$date)
      xts_read$date <- NULL
      xts_read <- lapply(xts_read, function(x) {
        xts::xts(x, order.by=t_read)
      })
    
      expect_that(xts_read, equals(xts, tolerance=1e-4))
    })
    
    test_that("XLSX wide export works", {
      
      write_ts(tslist, format = "xlsx",
                      fname = paste0(fn, "_wide"),
                      date_format = "%Y-%m", timestamp_to_fn = T,
                      wide = TRUE)
      xts_read <- openxlsx::read.xlsx(xlsx_wide_name)
      
      expect_that(dim(xts_read), equals(c(n, 3)))
      expect_that(names(xts_read), equals(c("date", "ts1", "ts2")))
      
      t_read <- as.yearmon(xts_read$date)
      xts_read$date <- NULL
      xts_read <- lapply(xts_read, function(x) {
        xts::xts(x, order.by=t_read)
      })
      
      expect_that(xts_read, equals(xts, tolerance=1e-4))
    })
    
    test_that("XLSX transposed export works", {
      write_ts(tslist, format="xlsx",
                      fname=paste0(fn, "_wide_transposed"),
                      date_format="%Y-%m", timestamp_to_fn = T,
                      wide=TRUE, transpose=TRUE)
      
      xts_read <- openxlsx::read.xlsx(xlsx_wide_transposed_name)
      
      expect_that(dim(xts_read), equals(c(2, n+1)))
      
      t_read <- as.yearmon(as.character(names(xts_read)[2:(n+1)]))
      xts_read_xts <- list()
      
      for(i in 1:2) {
        xts_read_xts[[i]] <- xts::xts(as.numeric(xts_read[i,2:(n+1)]), order.by = t_read)
      }
      
      names(xts_read_xts) <- xts_read[1:2, 1]
      
      expect_that(xts_read_xts, equals(xts, tolerance = 1e-4))
    })
    
    test_that("XLSX long export works", {
      
      write_ts(tslist, format="xlsx", fname=paste0(fn, "_long"), date_format="%Y-%m", timestamp_to_fn = T)
      xts_read <- openxlsx::read.xlsx(xlsx_long_name)
      
      expect_that(dim(xts_read), equals(c(2*n, 3)))
      expect_that(setdiff(names(xts_read), c("date", "value", "series")), equals(character(0)))
      
      xts_read <- reshape2::dcast(xts_read ,date ~ series)
      
      t_read <- as.yearmon(xts_read$date)
      xts_read$date <- NULL
      xts_read <- lapply(xts_read, function(x) {
        xts(x, order.by=t_read)
      })
      
      expect_that(xts_read, equals(xts, tolerance=1e-4))
    })
    
    test_that("RData export works", {
      rdata_read_name <- paste0(fn_read, ".RData")
      
      write_ts(tslist, format="rdata", fname=fn,
               timestamp_to_fn = T)
      load(rdata_read_name)
      
      tsl <- get("tslist")
      
      tsl <- lapply(tsl, function(x) {
        y <- xts::xts(x, yearmon(time(x)))
        unname(y)
      })
      
      expect_that(xts, equals(tsl))
    })
    
    test_that("Named RData export works", {
      varname <- "test_ts_read"
      
      write_ts(tslist, format="rdata", fname = fn,
               timestamp_to_fn = T,
               rdata_varname = varname)
      load(rdata_read_name)
      
      tsl <- get(varname)
      
      tsl <- lapply(tsl, function(x) {
        y <- xts::xts(x, yearmon(time(x)))
        unname(y)
      })
      
      expect_that(xts, equals(tsl))
    })
    
    test_that("JSON export works", {
      
      write_ts(tslist, fname=fn, format="json", date_format="%Y-%m", timestamp_to_fn = T,)
      fid <- file(json_read_name)
      xts_read <- jsonlite::fromJSON(readLines(fid))
      close(fid)
      
      expect_equal(length(xts_read), 2)
      expect_equal(dim(xts_read[[1]]), c(100, 2))
      expect_equal(names(xts_read), c("ts1", "ts2"))
      
      xts_read <- lapply(xts_read, function(x) {
        xts(x$value, order.by=as.yearmon(x$date))
      })
      
      expect_equal(xts, xts_read)
    })
    
    test_that("Pretty JSON export works", {
      json_read_name <- paste0(fn_read, ".json")
      
      write_ts(tslist, fname=fn, format="json", date_format="%Y-%m", timestamp_to_fn = T, json_pretty=TRUE)
      fid <- file(json_read_name)
      xts_read <- jsonlite::fromJSON(readLines(fid))
      close(fid)
      
      expect_equal(length(xts_read), 2)
      expect_equal(dim(xts_read[[1]]), c(100, 2))
      expect_equal(names(xts_read), c("ts1", "ts2"))
      
      xts_read <- lapply(xts_read, function(x) {
        xts(x$value, order.by=as.yearmon(x$date))
      })
      
      expect_equal(xts, xts_read)
    })
    
    test_that("Zipping works", {
      
      write_ts(tslist, fname=fn, format="json", date_format="%Y-%m", timestamp_to_fn = T, json_pretty=TRUE, zip=TRUE)
      
      expect_true(file.exists(zip_read_name))
      
      expect_true(file.exists(json_read_name))
      
      fid <- file(json_read_name)
      xts_read <- jsonlite::fromJSON(readLines(fid))
      close(fid)
    
      expect_equal(length(xts_read), 2)
      expect_equal(names(xts_read), c("ts1", "ts2"))
      
      xts_read <- lapply(xts_read, function(x) {
        xts(x$value, order.by=as.yearmon(x$date))
      })
      
      expect_equal(xts, xts_read)
    })
    
    test_that("imports work", {
      expect_equal(ts, read_ts(csv_wide_name))
      expect_equal(ts, read_ts(csv_wide_transposed_name))
      expect_equal(ts, read_ts(csv_long_name))
      expect_equal(ts, read_ts(xlsx_wide_name))
      expect_equal(ts, read_ts(xlsx_wide_transposed_name))
      expect_equal(ts, read_ts(xlsx_long_name))
      expect_equal(ts, read_ts(json_read_name))
      expect_equal(ts, read_ts(zip_read_name))
      
      faulty_zip <- "fz.zip"
      message(csv_long_name)
      zip(faulty_zip, c(csv_long_name, csv_wide_name))
      expect_warning(read_ts(faulty_zip), "Found more than 1 file")
      unlink(faulty_zip)
    
      temp <- tempfile()
      write("test", temp)
      zip(faulty_zip, temp)
      expect_error(read_ts(faulty_zip), "Zipped file is not")
      unlink(faulty_zip)
      
      expect_error(read_ts(read_ts(json_read_name, format="jpeg")), "should be one of")
      expect_error(read_ts("randomfile.txt"), "Could not detect")
    })
    
    test_that("differing lengths work", {
      n1 <- 10
      freq <- 12
      t1 <- yearmon(seq(2011, 2011+(n1-1)/freq, 1/freq))
      n2 <- 13
      t2 <- yearmon(seq(2011, 2011+(n2-1)/freq, 1/freq))
      faulty_xts <- list(
        ts1 = xts::xts(runif(n1), order.by=t1),
        ts2 = xts::xts(runif(n2), order.by=t2)
      )
      faulty_ts <- list(
        ts1 = as.ts(faulty_xts$ts1,
                    start = start(faulty_xts$ts1),
                    end = end(faulty_xts$ts1)),
        ts2 = as.ts(faulty_xts$ts2,
                    start = start(faulty_xts$ts2),
                    end = end(faulty_xts$ts2))
      )
      
      expect_warning(write_ts(faulty_xts,
                                     format = "csv",
                                     wide = T,
                                     fname = "faulty",
                                     timestamp_file = F),
                     "list contains")
      read_faulty_ts <- read_ts("faulty.csv")
      expect_equal(read_faulty_ts, faulty_ts, tolerance = 1e-3)
    })
}

########################################
# Run the actual tests
########################################

test_suite(xts)
test_suite(ts)
test_suite(zoo)

########################################
# LOOK AT ME, I'M THE PART THAT MATTERS!
########################################


unlink(csv_long_name)
unlink(csv_wide_name)
unlink(xlsx_long_name)
unlink(xlsx_wide_name)
unlink(rdata_read_name)
unlink(json_read_name)
unlink(zip_read_name)
