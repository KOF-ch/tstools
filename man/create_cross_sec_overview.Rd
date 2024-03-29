% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_cross_sec_overview.R
\name{create_cross_sec_overview}
\alias{create_cross_sec_overview}
\title{Create an Overview data.table of (last) observations}
\usage{
create_cross_sec_overview(list_of_rows, col_labels, tsl, selected_period)
}
\arguments{
\item{list_of_rows}{list of time series names}

\item{col_labels}{character list of column labels}

\item{tsl}{list of time series object to select from}

\item{selected_period}{numeric date as in defining ts objects.}
}
\description{
Create a data.table that shows the i-th obsersvation of
several time series.
}
\examples{
tsl <- generate_random_ts(10, lengths = 20)
list_of_rows <- list(
  "group 1" = c("ts1", "ts2", "ts3", "ts4"),
  "group 2" = c("ts5", "ts6", "ts7", "ts10")
)
# These are no real +,=,- values just random data.
create_cross_sec_overview(
  list_of_rows,
  c("+", "=", "-", "random"),
  tsl, c(1988, 12)
)
}
