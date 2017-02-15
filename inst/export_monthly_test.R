exportTsList(tslist[1:3])

mlist <- list()
mlist$ts1 <- ts(runif(30,30,89),start=c(1998,1),frequency=12)
mlist$ts2 <- ts(runif(50,10,39),start=c(1998,1),frequency=12)
mlist$ts3 <- ts(runif(70,-4,20),start=c(1998,1),frequency=12)

exportTsList(mlist)

exportTsList(mlist,xlsx = T)


kofdeployment/export_data/data_service_alpiq.R
for (k in 1:2)
  
{
  
  for (i in 1:length(countries)) {
    exportTsList(assign(paste0("tslist_", countries[i], "_", structure[k]), tslist2[[i]][[k]]),
                 
                 fname = paste0("~/rdata/datenservice/output/alpiq/", countries[i], "_", structure[k]), xlsx = T,
                 auto_date = F, date_format = date[k])
    
  }
}
