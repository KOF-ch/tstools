read_swissdata <- function(path, key_columns) {
  path <- "~/repositories/github/swissdata/wd/ch.bfs.dhu/ch.bfs.dhu.csv"
  dataset <- gsub("(.+wd\\/)(.+)(\\/.+)","\\2",path)
  raw <- data.table::fread(path)
  key_columns <- c("structure","trans","price.adj","seas")
  
  raw[,ts_key := do.call(paste,
                         c(dataset,.SD,sep=".")),
                         .SDcols = key_columns]
}




