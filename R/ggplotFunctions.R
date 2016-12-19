#' Different charts using ggplot
#' 
#' Line chart
#' 
#' Plots a line chart for each column in the two dimensional multivariate time series.
#' 
#' @param x object of class multivariate time series
#' @param ylab title for the y-axis, NULL by default
#' @param title title in the plot, NULL by default
#' @examples 
#' ts_object <- ts(matrix(rnorm(48),nrow=24),start=c(2008,1), frequency=12)
#' kplot(ts_object)
#' @author Florian Eckert and Heiner Mikosch, KOF, ETH Zurich 
#' @import ggplot2 
#' @importFrom zoo autoplot.zoo
#' @export
kplot <- function(x, ylab = NULL, title = NULL){
  
  autoplot.zoo(x, facets = NULL,
               main = title) + 
    xlab(NULL) +
    ylab(ylab) +
    theme_bw() + theme(legend.position = "bottom",
                       legend.title = element_blank(),
                       legend.key = element_blank())
  
}
#' Stacked bar chart
#' 
#' Plots a stacked bar chart for each column in the multivariate time series. 
#' 
#' @param x object of class multivariate time series
#' @param ylab title for the y-axis, NULL by default
#' @param title title in the plot, NULL by default
#' @examples 
#' ts_object <- ts(matrix(rnorm(48),nrow=24),start=c(2008,1), frequency=12)
#' kbar_stacked(ts_object)
#' @author Florian Eckert and Heiner Mikosch, KOF, ETH Zurich 
#' @import ggplot2
#' @importFrom zoo fortify.zoo
#' @export
kbar_stacked <- function(x, ylab = NULL, title = NULL){
  
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
#' Grouped bar chart
#'
#' Groups the bar charts for each column in the multivariate time series.
#'
#' @param x object of class multivariate time series
#' @param ylab title for the y-axis, NULL by default
#' @param title title in the plot, NULL by default 
#' @examples 
#' ts_object <- ts(matrix(rnorm(48),nrow=24),start=c(2008,1), frequency=12)
#' kbar_grouped(ts_object)
#' @author Florian Eckert and Heiner Mikosch, KOF, ETH Zurich 
#' @import ggplot2
#' @export
kbar_grouped <- function(x, ylab = NULL, title = NULL){
  
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
#' Stacked area chart
#'
#' Stacks the area chart for each column in the multivariate time series
#'
#' @param x object of class multivariate time series
#' @param ylab title for the y-axis, NULL by default
#' @param title title in the plot, NULL by default
#' @examples 
#' ts_object <- ts(matrix(rnorm(48),nrow=24),start=c(2008,1), frequency=12)
#' karea(ts_object) 
#' @author Florian Eckert and Heiner Mikosch, KOF, ETH Zurich
#' @import ggplot2 
#' @export
karea <- function(x, ylab = NULL, title = NULL){
  
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
