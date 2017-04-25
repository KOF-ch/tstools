library(microbenchmark)
library(timeseriesdb)
library(kofbts)


load("~/Desktop/keys_for_meta.RData")

# http://stackoverflow.com/questions/12206600/how-to-speed-up-insertion-performance-in-postgresql
# https://www.postgresql.org/docs/current/static/populate.html




md_unl <- new.env()
SURVEY <- "INU"
RUN <- 2
microbenchmark({
  for(i in seq_along(pg_keys[,"ts_key"][1:100000])){
    key_chunks <- unlist(strsplit(pg_keys$ts_key[i],"\\."))
    md_list <- list()
  #  md_list$alias <- createAlias(pg_keys[i,"ts_key"])
    md_list$survey <- SURVEY
    md_list$run <- RUN
    md_list$level <- key_chunks[4]
    md_list$instance <- key_chunks[5]
    md_list$legacy_key <- pg_keys$lm_ts_key[i]
    md_list$seasonal <- "original"
    md_list$item <- substr(pg_keys$ts_key[i],
                           regexpr("(\\.[^\\.]*$)",pg_keys$ts_key[i])+1,
                           nchar(pg_keys$ts_key[i]))
    md_list$variable <- gsub("(.+)(q_ql.+|q_cb.+|response)\\.(.+)","\\2",
                             pg_keys$ts_key[i])
    
    timeseriesdb::addMetaInformation(pg_keys$ts_key[i],
                       md_list,
                       meta_env = md_unl,overwrite_objects = T)
  }
},times = 1)


# con <- kofbts::kofDbConnect()
# microbenchmark({
#   storeMetaChunkWise(md_unl,con)},times=1)
# 
# library(timeseriesdb)
sandbox <- createConObj(dbname = "sandbox",passwd = "",dbhost = "localhost")

microbenchmark({
  storeMetaChunkWise(md_unl,sandbox)},times=1)

microbenchmark({
updateMetaInformation(md_unl,sandbox,keys = ls(envir=md_unl)[1:1000])},times=1)

# no UPDATE 10K: 23s
# no UPDATE 100K: 218s

# / 

postgresqlW


a <- capture.output(write.csv2(m,file = ""))

sql <- sprintf('CREATE TABLE 
                md_updates(ts_key varchar, meta_data hstore);
                COPY md_updates FROM STDIN;')

dbSendQuery(sandbox, "copy md_updates from stdin")

df <- data.frame(ts_key = c("a1","a2"),
                 meta_data = c("b1=>1,b2=>2","d2=>1"))
postgresqlCopyInDataframe(sandbox, df)



con <- kofbts::kofDbConnect()
readTimeSeries(fame_keys[1],con,schema = "lgcy_timeseries")
readTimeSeries(fame_keys[25], con, schema="lgcy_timeseries")

unique(fame_keys)

load("~/Desktop/fame_keys.rdata")



