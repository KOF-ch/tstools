
fcastExternal <- function(country, variables, src){
  
  # Run this loop over list of available external forecast sources
  fc_list <- lapply(variables, function(x){
    
    # Run this loop over list of gdp components to predict
    src_list <- lapply(src, function(y){
      
      # Get historical data from Eurostat, convert to annual rates in billions
      ts.q <- tslist[[paste(country,"ds.historical", x, sep=".")]]/1000*4
      
      # Quarterly time series of historical values, e.g. Eurostat or OECD
      ts.a <- tslist[[paste(country,"ds", y, x, sep=".")]]
      
      # Quarterlize the growth rates, yields growth rates
      ts.fc <- try(predict(td(pct(ts.a) ~ 1, to = "quarterly", conversion = "sum", method = "denton-cholette")), silent = T)
      
      # Use historical data and append external forecasts
      ts.fc2 <- try(ts(data = c(ts.q, cumprod(c(ts.q[length(ts.q)], 1 + window(ts.fc, start = time(ts.q)[length(ts.q)] + 0.25) / 100))[-1]),
                       start = time(ts.q)[1],
                       frequency = frequency(ts.q)), silent = T)
      
    })
    
    # Use only valid time series
    src_list <- src_list[which(sapply(src_list, is.ts))]
    
    # Average over the remaining time series
    mod.avg <- Reduce('+', src_list)/length(src_list)
    
    # Should the forecasting horizon lie beyond the horizon of the series, add the missing quarters using a random walk
    if(!isTRUE(all.equal(horizon, time(mod.avg)[length(mod.avg)]))){
      
      mod.avg.fc <- predict(rwf(pct(mod.avg)), h = 4)$mean
      
      mod.avg <- ts(data = c(mod.avg, cumprod(c(mod.avg[length(mod.avg)],
                                                1 + window(mod.avg.fc, start = time(mod.avg)[length(mod.avg)] + 0.25) / 100))[-1]),
                    start = time(mod.avg)[1],
                    frequency = frequency(mod.avg))
    }
    
  })
  
  # Assign names
  names(fc_list) <- paste(country,"kof",vja_current,variables,sep=".")
  
  return(fc_list)
  
}




fcastAgg <- function(nlist,rlist,baseYear){
  
  # Documentation: http://www.imf.org/external/pubs/ft/qna/pdf/qnachapter8.pdf
  
  # Calculate quarterly volume indices from the previous year
  
  rlist_pct_ls <- lapply(rlist, function(x){
    
    x/predict(td(lag(ta(x, conversion = "average", to = "annual"),-1) ~ 1, to = "quarterly", conversion = "average", method = "uniform"))
    
  })
  
  # Create matrix from list
  rlist_pct <- do.call(cbind,rlist_pct_ls)
  
  # Compute annual averages of the nominal GDP and assign them to each quarter
  nlist_lvl <- lapply(nlist, function(x) predict(td(ta(x, conversion = "average", to = "annual") ~ 1, conversion = "average", to = "quarterly", method = "uniform")))
  
  # Lag by one year such that each real growth rate is weighted by the previous year's average nominal weight
  nlist_lvl_lag <- lapply(nlist_lvl, function(x) lag(x, -frequency(x)))
  
  # Calculate nominal weights
  nlist_weights_ls <- lapply(nlist_lvl_lag, function(x) x/Reduce("+",nlist_lvl_lag))
  
  # Create matrix from list
  nlist_weights <- do.call(cbind, nlist_weights_ls)
  
  # Weight real growth rates by their respective nominal weights
  grw <- na.omit(ts(data = rowSums(rlist_pct*nlist_weights),
                  start = time(rlist_pct*nlist_weights)[1],
                  frequency = frequency(rlist_pct*nlist_weights)))
  
  
  baseVal <- 100
  
  
  # Reconstruct index
  for(i in seq(from = 1, to = length(grw), by = frequency(grw))){
    
    res <- coredata(baseVal) * window(grw,
                                      start = time(grw)[i],
                                      end = time(grw)[i+frequency(grw)-1])
    
    baseVal <- ta(res, to = "annual", conversion = "average")
    
    if(i == 1){
      
      result <- res
      
    } else {
      
      result <- cts(result,res)
      
    }
  }
  
  # Rebasing to reference year
  nlvl <- ta(Reduce("+",nlist_lvl), conversion = "average", to = "annual")
  rlvl <- ta(result, conversion = "average", to = "annual")
  result_lvl <- result*coredata(window(nlvl,baseYear,baseYear))/coredata(window(rlvl,baseYear,baseYear))
  
  return(result_lvl)
  
}







