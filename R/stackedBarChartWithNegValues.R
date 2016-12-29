#' 29122016
#' Stacked bar charts with positive and negative values and supplementary line plots
#'
#' @param vect matrix
#' @param vect_ts object of class time series
#' @examples
#' library(tstools)
#' vect <- cbind(50+rnorm(36), rnorm(36)+c(-2,2), -20+rnorm(36), 100+rnorm(36))
#' vect_ts <- ts(vect, frequency=12, start=c(2004,1))
#' stackedBarChartsWithNegValues(vect_ts)
#' addLinePlot(vect_ts[,2])
#' @export
stackedBarChartsWithNegValues <- function(c_vect, show_sums_as_line=T, theme=NULL) {
  
  # If time series object contains positive and negative values,
  # split into positive and negative part in the plot.
  # The bars with negative values will be drawn below the x-axis.
  
  if(sum(c_vect < 0) > 0){
    
    c_vect1 <- c_vect
    c_vect2 <- c_vect
    # Split into parts with positive respectively negative values
    c_vect1[c_vect < 0] <- 0
    c_vect2[c_vect > 0] <- 0
    # Vectors are transposed
    c_transposed_vect1 <- t(c_vect1)
    c_transposed_vect2 <- t(c_vect2)
    # Find the range of the stacked bar charts 
    c_value_range <- c(floor(min(colSums(c_transposed_vect2))), ceiling(max(colSums(c_transposed_vect1))))
    
    # Initialise default theme with predefined colors for KOF
    if(is.null(theme)){
      theme_1 <- initDefaultTheme()
    }
    
    # Plot positive bars
    c_barplot1 <- barplot(c_transposed_vect1, ylim=c_value_range, axes=F, col=theme_1$line_colors)
    # Add negative bars
    c_barplot2 <- barplot(c_transposed_vect2, ylim=c_value_range, axes=F, col=theme_1$line_colors, add=T)
    # Add y-axis on left side
    axis(side=2, ylim=c_value_range)
    # Add x-axis of time series; for every month a tick
    time_seq <- seq(from=as.Date(paste(start(c_vect)[1], start(c_vect)[2],1,sep="."), format="%Y.%m.%d"), by=paste(12/frequency(c_vect), "months", sep=" "), length.out=dim(c_vect)[1])
    axis(side=1, labels = time_seq, at=c_barplot1)
    
    # Add box around the plot
    box() 

    # Add the column sum as line plot to the barplot
    if(show_sums_as_line==T) {
      
      c_vect_col_sums <- colSums(t(c_vect))
      lines(x=c_barplot1, y=c_vect_col_sums)
      
    }
    
  } 
  
}
#' Add a line plot to the stacked bar chart
addLinePlot <- function(c_vect){
 
  par(new=T)
  c_value_range <- range(c_vect)
  plot(c_vect, ylim=c_value_range, axes=F)
  # Add supplementary y-axis on right side
  axis(side=4, ylim=c_value_range) 
  # Add x-axis at y=0 (with respect to y-axis on right side)
  abline(h=0)
  
}
