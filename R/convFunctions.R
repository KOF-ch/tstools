
pct <- function(x){
  
  result <- ((x/lag(x,-1))-1)*100
  colnames(result) <- colnames(x)
  return(result)
  
}



annpct <- function(x){
  
  result <- ((x/lag(x,-1))^frequency(x)-1)*100
  colnames(result) <- colnames(x)
  return(result)
  
}




yoypct <- function(x){
  
  result <- ((x/lag(x,-frequency(x)))-1)*100
  colnames(result) <- colnames(x)
  return(result)
  
}



grc <- function(x){
  # Growth Contributions
  
  res <- (x-lag(x,-1))/lag(predict(td(ta(ts(rowSums(x), start = time(x)[1], frequency = frequency(x)), "average", "annual") ~ 1,
                                      "average", frequency(x), "uniform")),-frequency(x))*100
  colnames(res) <- colnames(x)
  return(res)
}



anngrc <- function(x){
  # Annualized Growth Contributions
  
  res <- ((1+(x-lag(x,-1))/lag(predict(td(ta(ts(rowSums(x), start = time(x)[1], frequency = frequency(x)), "average", "annual") ~ 1,
                                          "average", frequency(x), "uniform")),-frequency(x)))^frequency(x)-1)*100
  colnames(res) <- colnames(x)
  return(res)
}



yoygrc <- function(x){
  # Year-over-year Growth Contributions
  
  res <- (x-lag(x,-frequency(x)))/lag(predict(td(ta(ts(rowSums(x), start = time(x)[1], frequency = frequency(x)), "average", "annual") ~ 1,
                                                 "average", frequency(x), "uniform")),-frequency(x))*100
  colnames(res) <- colnames(x)
  return(res)
}





tsidx <- function(x, byear, bperiod = NULL){
  # Get Index
  
  if(is.null(bperiod)){
    
    base_value <- window(ta(x, conversion = "average", to = "annual"),
                         start = byear,
                         end = byear)
    
    
  } else {
    
    base_value <- window(x, 
                         start = byear + (bperiod-1)/frequency(x), 
                         end = byear + (bperiod-1)/frequency(x))
  }
  
  base_value_ext <- ts(data = base_value,
                       start = time(x)[1],
                       end = time(x)[length(time(x))],
                       frequency = frequency(x))
  
  res <- x/base_value_ext*100
  
  colnames(res) <- colnames(x)
  
  return(res)
  
}



searchdb <- function(x, schema){
  
  sql <- paste0("SELECT ts_key FROM ",schema,".timeseries_main WHERE ts_key ~ '",x,"'")
  
  
  result <- dbGetQuery(con, sql)
  
  #Displaying Output
  
  # head(result)
  keys <- result$ts_key
  # tslist <- paste0(readTimeSeries(keys[1:20], con, schema='",schema,"'))
  # There are 4 schemes in the testdb: timeseries, ext_timeseries, lgcy_timeseries, pblc_timeseries
  
  return(keys)
  
}



cts <- function(ts1,ts2){
  # Concatenate two time series
  
  if(!isTRUE(all.equal(frequency(ts1),frequency(ts2)))){
    
    warning("Time series do not have the same frequency")
    
  } else if(!isTRUE(all.equal(time(ts1)[length(ts1)] + 1/frequency(ts1),time(ts2)[1]))){
    
    warning("Time series do not fit together")
    
  } else {
    
    ts(c(ts1,ts2),
       start = time(ts1)[1],
       frequency = frequency(ts1))
  }
}


ts2csv <- function(x, file){
  
  source("lib/setMount.R")
  
  # Create regular series
  if(frequency(x) == 1){
    output <- zoo(x = x, order.by = time(x))
  } else if(frequency(x) == 4){
    output <- zoo(x = x, order.by = as.yearqtr(time(x)))
  } else if(frequency(x) == 12){
    output <- zoo(x = x, order.by = as.yearmon(time(x)))
  } else {
    output <- zoo(x = x, order.by = as.Date(time(x)))
  }
  
  # Write file and make sure it's chmod 770 afterwards
  if(file.exists(setMount(file))) 
    file.remove(setMount(file))
  write.zoo(output,
            col.names = T,
            file = setMount(file),
            sep=";")
  system(sprintf("chmod 770 %s",setMount(file)))
  
}





chainedGrc <- function(rlist,nlist){
  # Compute growth contributions by weighting real growth rates by average nominal gdp of the previous year
  
  # Compute real growth rates
  rlist_pct_ls <- lapply(rlist, function(x) pct(x))
  
  # Create matrix from list
  rlist_pct <- do.call(cbind,rlist_pct_ls)
  
  # Compute annual averages and assign them to each quarter
  nlist_lvl <- lapply(nlist, function(x) predict(td(ta(x, conversion = "average", to = "annual") ~ 1, 
                                                    conversion = "average",
                                                    to = "quarterly",
                                                    method = "uniform")))
  
  # Lag by one year such that each real growth rate is weighted by the previous year's average nominal weight
  nlist_lvl_lag <- lag(nlist_lvl, -frequency(nlist_lvl))
  
  # Calculate nominal weights
  nlist_weights_ls <- lapply(nlist_lvl_lag, function(x) x/Reduce("+",nlist))
  
  # Create matrix from list
  nlist_weights <- do.call(cbind,nlist_weights_ls)
  
  # Weight real growth rates by their respective nominal weights
  tsresult <- rlist_pct*nlist_weights
  
  # Assign names of rlist to result
  colnames(tsresult) <- names(rlist) 
  
  return(tsresult)
  
}




kplot <- function(x, ylab = NULL, title = NULL){
  # Line Chart
  
  autoplot.zoo(x, facets = NULL,
               main = title) + 
    xlab(NULL) +
    ylab(ylab) +
    theme_bw() + theme(legend.position = "bottom",
                       legend.title = element_blank(),
                       legend.key = element_blank())
  
}


kbar_stacked <- function(x, ylab = NULL, title = NULL){
  # Stacked Bar Chart
  
  dat <- fortify.zoo(x, melt = TRUE)
  
  dat1 <- subset(dat,Value >= 0)
  dat2 <- subset(dat,Value < 0)
  
  suppressWarnings(ggplot() + 
                     geom_bar(data = dat1, aes(x=Index, y=Value, fill=Series),stat = "identity", position = "stack") +
                     geom_bar(data = dat2, aes(x=Index, y=Value, fill=Series),stat = "identity", position = "stack") +
                     xlab(NULL) +
                     ylab(ylab) +
                     ggtitle(title) +
                     theme_bw() + 
                     theme(legend.position = "bottom",
                           legend.title = element_blank(),
                           legend.key = element_blank()))
}



kbar_grouped <- function(x, ylab = NULL, title = NULL){
  # Grouped Bar Chart
  
  ggplot(aes(x = Index, y = Value, fill = Series), data = fortify.zoo(x, melt = TRUE)) + 
    xlab(NULL) +
    ylab(ylab) +
    ggtitle(title) +
    geom_bar(stat="identity", position = position_dodge()) +
    theme_bw() + 
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.key = element_blank())
}




karea <- function(x, ylab = NULL, title = NULL){
  # Stacked Area Chart
  
  ggplot(aes(x = Index, y = Value, fill = Series), data = fortify.zoo(x, melt = TRUE)) + 
    xlab(NULL) +
    ylab(ylab) +
    ggtitle(title) +
    geom_area() +
    theme_bw() + 
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.key = element_blank())
}
