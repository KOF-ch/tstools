# packages
library(timeseriesdb)
library(zoo)

con <- kofbts::kofDbConnect()



# Beispiel für EFV Datenlieferung

q_efv_set <- loadTsSet(con,user_name = "datenservice",set_name = "ds_efv_quarterly",
                       schema = "lgcy_timeseries")



efv_q <- readTimeSeries(q_efv_set$keys,con,schema = "lgcy_timeseries")



debug(exportTsList)
exportTsList(efv_q,fname = "exportTsList.xlsx",date_format = "%Y-0%q",xlsx = T)

min_date <- min(tt$time)
max_date <- max(tt$time)

unique(tt$series)

oo <- do.call("cbind",efv_q)
head(data.frame(date = format(time(oo),"%Y"),oo))

