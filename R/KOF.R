#' KOF Barometer - Swiss Business Cycle Indicator
#'
#' A list of time series containing two time series the KOF Barometer and the growth of Swiss GDP over time. KOF Barometer is a monthly business cycle indicator computed by the KOF Swiss Economic Institute. The GDP growth rate is used as a reference series to the Barometer.
#'
#' @format A list of two time series of class ts
#' \describe{
#'   \item{kofbarometer}{KOF Barometer Indicator}'
#'   \item{reference}{Reference series to KOF Barometer, change in Swiss GDP compared to previous month}
#'   \item{baro_point_fc}{Auto Arima point forecast of the KOF Barometer}
#'   \item{baro_lo_80}{Auto Arima 80 percent CI lower bound of the KOF Barometer forecast}
#'   \item{baro_hi_80}{Auto Arima 80 percent CI upper bound of the KOF Barometer forecast}
#'   \item{baro_lo_95}{Auto Arima 95 percent CI lower bound of the KOF Barometer forecast}
#'   \item{baro_hi_95}{Auto Arima 95 percent CI upper bound of the KOF Barometer forecast}
#'   ...
#' }
#' @source \url{https://kof.ethz.ch/en/forecasts-and-indicators/indicators/kof-economic-barometer.html}
#'
"KOF"
