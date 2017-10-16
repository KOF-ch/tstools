context("writeTimeseries")

n <- 100
freq <- 12
t <- yearmon(seq(2011, 2011+(n-1)/freq, 1/freq))
xts <- list(
  #ts1 = ts(data=runif(100), start=2011.5, frequency=12),
  #ts2 = ts(data=runif(100), start=2011.5, frequency=12)
  ts1 = xts(runif(n), order.by=t),
  ts2 = xts(runif(n), order.by=t)
)

fn <- "test_ts"
fn_read <- paste0(fn, "_",gsub("-","_",Sys.Date()))

test_that("CSV wide export works", {
  csv_wide_name <- paste0(fn_read, ".csv")
  
  writeTimeSeries(xts, format="csv", fname=fn, date_format="%Y-%m", wide=TRUE)
  xts_read <- read.csv(csv_wide_name, sep=";", stringsAsFactors=FALSE)
  
  unlink(csv_wide_name)
  
  expect_that(dim(xts_read), equals(c(n, 3)))
  expect_that(names(xts_read), equals(c("time", "ts1", "ts2")))
  
  t_read <- as.yearmon(xts_read$time)
  xts_read$time <- NULL
  xts_read <- lapply(xts_read, function(x) {
    xts(x, order.by=t_read)
  })
  
  expect_that(xts_read, equals(xts, tolerance=1e-4))
})

test_that("CSV long export works", {
  csv_long_name <- paste0(fn_read, ".csv")
  
  writeTimeSeries(xts, format="csv", fname=fn, date_format="%Y-%m")
  xts_read <- read.csv(csv_long_name, sep=";", stringsAsFactors=FALSE)
  
  unlink(csv_long_name)
  
  expect_that(dim(xts_read), equals(c(2*n, 3)))
  expect_that(names(xts_read), equals(c("time", "value", "series")))

  xts_read <- reshape2::dcast(xts_read ,time ~ series)
  
  t_read <- as.yearmon(xts_read$time)
  xts_read$time <- NULL
  xts_read <- lapply(xts_read, function(x) {
    xts(x, order.by=t_read)
  })

  expect_that(xts_read, equals(xts, tolerance=1e-4))
})

test_that("XLSX wide export works", {
  xlsx_wide_name <- paste0(fn_read, ".xlsx")
  
  writeTimeSeries(xts, format="xlsx", fname=fn, date_format="%Y-%m", wide=TRUE)
  xts_read <- openxlsx::read.xlsx(xlsx_wide_name)
  
  unlink(xlsx_wide_name)
  
  expect_that(dim(xts_read), equals(c(n, 3)))
  expect_that(names(xts_read), equals(c("time", "ts1", "ts2")))
  
  t_read <- as.yearmon(xts_read$time)
  xts_read$time <- NULL
  xts_read <- lapply(xts_read, function(x) {
    xts(x, order.by=t_read)
  })
  
  expect_that(xts_read, equals(xts, tolerance=1e-4))
})

test_that("XLSX long export works", {
  xlsx_long_name <- paste0(fn_read, ".xlsx")
  
  writeTimeSeries(xts, format="xlsx", fname=fn, date_format="%Y-%m")
  xts_read <- openxlsx::read.xlsx(xlsx_long_name)
  
  unlink(xlsx_long_name)
  
  expect_that(dim(xts_read), equals(c(2*n, 3)))
  expect_that(names(xts_read), equals(c("time", "value", "series")))
  
  xts_read <- reshape2::dcast(xts_read ,time ~ series)
  
  t_read <- as.yearmon(xts_read$time)
  xts_read$time <- NULL
  xts_read <- lapply(xts_read, function(x) {
    xts(x, order.by=t_read)
  })
  
  expect_that(xts_read, equals(xts, tolerance=1e-4))
})

test_that("RData export works", {
  rdata_read_name <- paste0(fn_read, ".RData")
  
  writeTimeSeries(xts, format="rdata", fname=fn)
  load(rdata_read_name)
  
  expect_that(xts, equals(get(fn_read)))
})

test_that("Named RData export works", {
  rdata_read_name <- paste0(fn_read, ".RData")
  
  varname <- "test_ts_read"
  
  writeTimeSeries(xts, format="rdata", fname=fn, rdata_varname=varname)
  load(rdata_read_name)
  
  expect_that(xts, equals(get(varname)))
})

test_that("JSON export works", {
  json_read_name <- paste0(fn_read, ".json")
  
  writeTimeSeries(xts, fname=fn, format="json", date_format="%Y-%m")
  fid <- file(json_read_name)
  xts_read <- jsonlite::fromJSON(readLines(fid))
  close(fid)
  
  unlink(json_read_name)
  
  expect_equal(length(xts_read), 2)
  expect_equal(dim(xts_read[[1]]), c(100, 2))
  expect_equal(names(xts_read), c("ts1", "ts2"))
  
  xts_read <- lapply(xts_read, function(x) {
    xts(x$value, order.by=as.yearmon(x$time))
  })
  
  expect_equal(xts, xts_read)
})

test_that("Pretty JSON export works", {
  json_read_name <- paste0(fn_read, ".json")
  
  writeTimeSeries(xts, fname=fn, format="json", date_format="%Y-%m", json_pretty=TRUE)
  fid <- file(json_read_name)
  xts_read <- jsonlite::fromJSON(readLines(fid))
  close(fid)
  
  unlink(json_read_name)
  
  expect_equal(length(xts_read), 2)
  expect_equal(dim(xts_read[[1]]), c(100, 2))
  expect_equal(names(xts_read), c("ts1", "ts2"))
  
  xts_read <- lapply(xts_read, function(x) {
    xts(x$value, order.by=as.yearmon(x$time))
  })
  
  expect_equal(xts, xts_read)
})

test_that("API JSON export works", {
  json_read_name <- paste0(fn_read, ".json")
  
  writeTimeSeries(xts, fname=fn, format="json", date_format="%Y-%m", json_apiformat=TRUE)
  fid <- file(json_read_name)
  xts_read <- jsonlite::fromJSON(readLines(fid))
  close(fid)
  
  unlink(json_read_name)
  
  dim_read <- dim(xts_read)
  
  expect_equal(dim_read, c(n+1, 3))
  expect_equal(xts_read[1,], c("time", "ts1", "ts2"))
  
  names_read <- xts_read[1,]
  xts_read <- xts_read[seq(2, dim_read[1]),]
  
  t_read <- as.yearmon(xts_read[, 1])
  
  xts_read <- lapply(seq(2, dim_read[2]), function(x) {
    xts(as.numeric(xts_read[, x]), order.by=t_read)
  })
  
  names(xts_read) <- names_read[2:length(names_read)]
  
  expect_equal(xts, xts_read)
})

test_that("Pretty API JSON export works", {
  json_read_name <- paste0(fn_read, ".json")
  
  writeTimeSeries(xts, fname=fn, format="json", date_format="%Y-%m", json_apiformat=TRUE, json_pretty=TRUE)
  fid <- file(json_read_name)
  xts_read <- jsonlite::fromJSON(readLines(fid))
  close(fid)
  
  unlink(json_read_name)
  
  dim_read <- dim(xts_read)
  
  expect_equal(dim_read, c(n+1, 3))
  expect_equal(xts_read[1,], c("time", "ts1", "ts2"))
  
  names_read <- xts_read[1,]
  xts_read <- xts_read[seq(2, dim_read[1]),]
  
  t_read <- as.yearmon(xts_read[, 1])
  
  xts_read <- lapply(seq(2, dim_read[2]), function(x) {
    xts(as.numeric(xts_read[, x]), order.by=t_read)
  })
  
  names(xts_read) <- names_read[2:length(names_read)]
  
  expect_equal(xts, xts_read)
})

test_that("Zipping works", {
  json_read_name <- paste0(fn_read, ".json")
  zip_read_name <- paste0(fn_read, ".zip")
  
  writeTimeSeries(xts, fname=fn, format="json", date_format="%Y-%m", json_apiformat=TRUE, json_pretty=TRUE, zip=TRUE)
  
  expect_true(file.exists(zip_read_name))
  
  unlink(json_read_name)
  unzip(zip_read_name)
  
  expect_true(file.exists(json_read_name))
  
  fid <- file(json_read_name)
  xts_read <- jsonlite::fromJSON(readLines(fid))
  close(fid)
  
  unlink(json_read_name)
  unlink(zip_read_name)
  
  dim_read <- dim(xts_read)
  
  expect_equal(dim_read, c(n+1, 3))
  expect_equal(xts_read[1,], c("time", "ts1", "ts2"))
  
  names_read <- xts_read[1,]
  xts_read <- xts_read[seq(2, dim_read[1]),]
  
  t_read <- as.yearmon(xts_read[, 1])
  
  xts_read <- lapply(seq(2, dim_read[2]), function(x) {
    xts(as.numeric(xts_read[, x]), order.by=t_read)
  })
  
  names(xts_read) <- names_read[2:length(names_read)]
  
  expect_equal(xts, xts_read)
})
