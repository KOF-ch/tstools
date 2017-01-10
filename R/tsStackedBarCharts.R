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







#' 29122016
#' Stacked bar charts with positive and negative values and supplementary line plots
#'
#' @param vect matrix
#' @param vect_ts object of class time series
#' @examples
#' vect <- cbind(50+rnorm(36), rnorm(36)+c(-2,2), -20+rnorm(36), 100+rnorm(36))
#' vect_ts <- ts(vect, frequency=12, start=c(2004,1))
#' stackedBarChartsWithNegValues(vect_ts)
#' addLinePlot(vect_ts[,2])
#' @export

data(KOF)

tsmat <- do.call("cbind",KOF)
sum(tsmat < 0) > 0 
value_range
neg <- tsmat < 0




xx <- initDefaultTheme()

KOF$reference
tsStackedBarChart(KOF)
debug(tsStackedBarChart)

tli <- list()
tli$ts1 <- ts(rnorm(30,-1,10),start=c(2000,1),frequency = 4)
tli$ts2 <- ts(rnorm(30,10,40),start=c(2000,1),frequency = 4)


tm <- tsStackedBarChart(tli)

#'@author Caroline Siegenthaler, Matthias Bannert
#'@export
tsStackedBarChart <- function(li, show_sums_as_line = T,
                              theme = NULL,
                              print_x_axis = T,
                              quarter_ticks = T,
                              print_y_axis = T,
                              print_y_right = F,
                              ygrid = T,
                              ygrid_factor = 5,
                              yaxis_factor = 20,
                              manual_value_range = NULL,
                              manual_date_range = NULL){
    if(is.null(theme)){
    theme <- initDefaultTheme()
  }
  
  if(!all(unlist(lapply(li,is.ts))))
    stop("all elements of the list need to be objects of class ts.")
  
  if(theme$fillUpPeriod){
    series <- lapply(li,fillUpYearWithNAs)
  }
  
  # matrix works well with pos / neg checks.
  tsmat <- do.call("cbind",li)
  # bottom, left, top, right margins defined via theme.
  par(mar = theme$mar)
  
  ts_time <- unique(unlist(lapply(series,time)))
  # floating problems when comparing stuff, set it to 
  # 5 digits ... 
  ts_time <- round(ts_time,digits = 5)
  date_range <- range(ts_time)
  
  if(!is.null(manual_date_range)){
    theme$xlim = manual_date_range
    ts_time <- seq(from = manual_date_range[1],
                   to = manual_date_range[2],by = .25)
    date_range <- manual_date_range
  }
  

  if(sum(tsmat < 0) > 0){
    neg_0 <- tsmat
    pos_0 <- tsmat
    neg_0[tsmat < 0] <- 0
    pos_0[!tsmat < 0] <- 0
    value_range <- c(floor(min(rowSums(pos_0))),
                     ceiling(max(rowSums(neg_0))))
    value_range <- round(((value_range/10)+c(-1,1))*10)
    
    if(!is.null(manual_value_range)) value_range <-
      manual_value_range

    barplot(t(neg_0),
            ylim = value_range,
            axes = F,
            col = theme$line_colors)
    barplot(t(pos_0),
            ylim = value_range,
            axes = F,
            col = theme$line_colors,
            add=T)
    
    if(print_y_axis) .doYAxisWithHorizontalGrids(theme,
                                                 value_range,
                                                 ygrid_factor = ygrid_factor)
    
    if(print_x_axis){
      # axis and ticks defintion
      if(quarter_ticks){
        ext_qtr <- ts_time[abs(ts_time * 4 - floor(ts_time * 4)) < 0.001]
        ext_label <- ifelse(ext_qtr - floor(ext_qtr) == 0.5, as.character(floor(ext_qtr)), NA)
        # x-axis
        axis(1, at = ext_qtr, labels = ext_label,
             tcl = theme$tcl_1, cex.axis = theme$cex.axis_1, padj = theme$padj_1)
        axis(1, at = date_range[1]:date_range[2],
             tcl = theme$tcl_2,
             lwd.ticks = theme$lwd_ticks_1, labels = FALSE) # thick tick marks
        
      } else{
        axis(1, at = min_date_value:max_date_value,
             tcl = theme$tcl_1,
             cex.axis = theme$cex.axis_1,
             padj = theme$padj_1)
      }
    }
    
  }
}
  
  # 
  # # If time series object contains positive and negative values,
  # # split into positive and negative part in the plot.
  # # The bars with negative values will be drawn below the x-axis.
  # 
  # if(sum(c_vect < 0) > 0){
  #   
  #   c_vect1 <- c_vect
  #   c_vect2 <- c_vect
  #   # Split into parts with positive respectively negative values
  #   c_vect1[c_vect < 0] <- 0
  #   c_vect2[c_vect > 0] <- 0
  #   # Vectors are transposed
  #   c_transposed_vect1 <- t(c_vect1)
  #   c_transposed_vect2 <- t(c_vect2)
  #   # Find the range of the stacked bar charts 
  #   c_value_range <- c(floor(min(colSums(c_transposed_vect2))), ceiling(max(colSums(c_transposed_vect1))))
  #   
  #   # Initialise default theme with predefined colors for KOF
  #   if(is.null(theme)){
  #     theme_1 <- initDefaultTheme()
  #   }
  #   
  #   # Plot positive bars
  #   c_barplot1 <- barplot(c_transposed_vect1, ylim=c_value_range, axes=F, col=theme_1$line_colors)
  #   # Add negative bars
  #   c_barplot2 <- barplot(c_transposed_vect2, ylim=c_value_range, axes=F, col=theme_1$line_colors, add=T)
  #   # Add y-axis on left side
  #   axis(side=2, ylim=c_value_range)
  #   # Add x-axis of time series; for every month a tick
  #   time_seq <- seq(from=as.Date(paste(start(c_vect)[1], start(c_vect)[2],1,sep="."), format="%Y.%m.%d"), by=paste(12/frequency(c_vect), "months", sep=" "), length.out=dim(c_vect)[1])
  #   axis(side=1, labels = time_seq, at=c_barplot1)
  #   
  #   # Add box around the plot
  #   box() 
  #   
  #   # Add the column sum as line plot to the barplot
  #   if(show_sums_as_lineT) {
  #     
  #     c_vect_col_sums <- colSums(t(c_vect))
  #     lines(x=c_barplot1, y=c_vect_col_sums)
  #     
  #   }
  #   
  # } 
  

