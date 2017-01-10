#' Prediction of uniform temporal disaggregation of time series
#' 
#' Common function to calculate different growth contributions.
#' 
#' @param x multivariate time series object
#' @examples 
#' mat <- matrix(c(7+rnorm(48),50+rnorm(48),70+rnorm(48),100+rnorm(48)),ncol=4)
#' mts_object <- ts(mat, frequency=12, start=c(2004,1))
#' colnames(mts_object) <- c("IT","DE","CH","FR")
#' ts_pred_uniform(mts_object)
#' @importFrom tempdisagg ta td
#' @importFrom stats lag predict
#' @author Florian Eckert and Heiner Mikosch, KOF, ETH Zurich 
#' @export
ts_pred_uniform <- function(x){
  
  # Sum over all columns of the multivariate time series
  ts_sums <- ts(rowSums(x), 
                start = time(x)[1], 
                frequency = frequency(x))
  # Temporal aggregation of time series - annual average
  ts_aa <- ta(ts_sums, 
              conversion = "average", 
              to = "annual") 
  # Prediction of uniform temporal disaggregation of time series
  ts_pred <- predict(td(ts_aa ~ 1, 
                        conversion = "average", 
                        to = frequency(x), 
                        method = "uniform"))
  ts_pred
}
#' Growth contribution
#' 
#' Uses function ts_pred_uniform to predict the uniform temporal disaggregation of time series.
#' 
#' @param x multivariate time series object
#' @importFrom stats lag predict
#' @examples 
#' mat <- matrix(c(7+rnorm(48),50+rnorm(48),70+rnorm(48),100+rnorm(48)),ncol=4)
#' mts_object <- ts(mat, frequency=12, start=c(2004,1))
#' colnames(mts_object) <- c("IT","DE","CH","FR")
#' grc(mts_object)
#' @importFrom stats lag predict
#' @author Florian Eckert and Heiner Mikosch, KOF, ETH Zurich 
#' @export
grc <- function(x){
  
  # Prediction of uniform temporal disaggregation of time series
  ts_pred <- ts_pred_uniform(x)
  # Growth contribution
  res <- (x-lag(x,-1))/lag(ts_pred,-frequency(x))*100
  colnames(res) <- colnames(x)
  res
  
}
#' Annualized Growth Contributions
#' 
#' Uses function ts_pred_uniform to predict the uniform temporal disaggregation of time series.
#' 
#' @param x multivariate time series object
#' @examples 
#' mat <- matrix(c(7+rnorm(48),50+rnorm(48),70+rnorm(48),100+rnorm(48)),ncol=4)
#' mts_object <- ts(mat, frequency=12, start=c(2004,1))
#' colnames(mts_object) <- c("IT","DE","CH","FR")
#' anngrc(mts_object)
#' @importFrom stats lag predict
#' @author Florian Eckert and Heiner Mikosch, KOF, ETH Zurich 
#' @export
anngrc <- function(x){
  
  # Prediction of uniform temporal disaggregation of time series
  ts_pred <- ts_pred_uniform(x)
  # Annualized Growth Contributions
  res <- ((1+(x-lag(x,-1))/lag(ts_pred,-frequency(x)))^frequency(x)-1)*100
  colnames(res) <- colnames(x)
  res
  
}
#' # Year-over-year Growth Contributions
#' 
#' Uses function ts_pred_uniform to predict the uniform temporal disaggregation of time series.
#' 
#' @param x multivariate time series object
#' @importFrom stats lag predict
#' @examples 
#' mat <- matrix(c(7+rnorm(48),50+rnorm(48),70+rnorm(48),100+rnorm(48)),ncol=4)
#' mts_object <- ts(mat, frequency=12, start=c(2004,1))
#' colnames(mts_object) <- c("IT","DE","CH","FR")
#' yoygrc(mts_object)
#' @author Florian Eckert and Heiner Mikosch, KOF, ETH Zurich 
#' @importFrom stats lag predict
#' @export
yoygrc <- function(x){
  
  # Prediction of uniform temporal disaggregation of time series
  ts_pred <- ts_pred_uniform(x)
  # Year-over-year Growth Contributions
  res <- (x-lag(x,-frequency(x)))/lag(ts_pred,-frequency(x))*100
  colnames(res) <- colnames(x)
  res
  
}
#' Weight real growth rates by their respective nominal weights
#'
#' Computes growth contributions by weighting real growth rates by average nominal gdp of the previous year
#'
#' @param rlist list of time series object 
#' @param nlist list of time series object
#' @importFrom stats lag predict
#' @examples
#' mat_r <- matrix(c(7+rnorm(12),50+rnorm(12),70+rnorm(12),100+rnorm(12)),ncol=4)
#' r_ts <- ts(mat_r, frequency=4, start=c(2004,1))
#' colnames(r_ts) <- c("IT","DE","CH","FR")
#' rlist<-as.list(r_ts)
#' mat_n <- matrix(c(7+rnorm(12),50+rnorm(12),70+rnorm(12),100+rnorm(12)),ncol=4)
#' n_ts <- ts(mat_n, frequency=4, start=c(2004,1))
#' colnames(n_ts) <- c("IT","DE","CH","FR")
#' nlist<-as.list(n_ts)
#' chainedGrc(rlist,nlist)
#' @author Florian Eckert and Heiner Mikosch, KOF, ETH Zurich 
#' @export
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
  
  tsresult
  
}
