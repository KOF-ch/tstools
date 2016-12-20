# 20122016
# Stacked bar charts with positive and negative values and supplementary lines
#
# Example of a stacked bar chart, with positive and negative values
vect<-cbind(50+rnorm(36), rnorm(36), -20+rnorm(36), 100+rnorm(36))
# Make it a time series object
vect.ts<-ts(vect,frequency=12,start=c(2004,1))
# Draw stacked bar chart
stackedBarChartsWithNegValues(vect.ts)
# Add line plot
addLinePlot(vect.ts[,2])

stackedBarChartsWithNegValues <- function(c.vect, posAndNegValues=T, showSumsAsLine=T) {
  
  # If the time series object contains positive and negative values that should be
  # split into positive and negative part in the plot
  if(posAndNegValues==T){
    
    # The bars with negative values should be drawn below the x-axis
    c.vect1<-c.vect
    c.vect2<-c.vect
    # Split into bar charts with positive respectively negative values
    c.vect1[c.vect<0]<-0
    c.vect2[c.vect>0]<-0
    # Vectors are transposed (refer to time axis)
    c.transposed.vect1<-t(c.vect1)
    c.transposed.vect2<-t(c.vect2)
    # Find the range of the stacked bar chart 
    c.max<-round(max(colSums(c.transposed.vect1)))
    c.min<-round(min(colSums(c.transposed.vect2)))
    
    # Colors need adjustment for ETHZ
    # Plot positive bars
    c.barplot1<-barplot(c.transposed.vect1, ylim=c(c.min, c.max), axes=F, col=c("red","green","blue","violet"))
    # Add negative bars
    c.barplot2<-barplot(c.transposed.vect2, ylim=c(c.min, c.max), axes=F, col=c("red","green","blue","violet"), add=T)
    # Add y-axis on left side
    axis(side=2, ylim=c(c.min, c.max))
    # Add x-axis of time series; for every month a tick
    time.seq<-seq(from=as.Date("2004/1/1"), by="1 months", length.out=36)
    axis(side=1, labels = time.seq, at=c.barplot1)
    
    # Add box around the plot and title
    box() 
    title("Stacked bar chart with negative values")
    
    # Add the column sum as line to the stacked bar chart, if wished
    if(showSumsAsLine==T) {
      
      c.vectColSums<-colSums(t(c.vect))
      # Add the line to the barplot
      lines(x=c.barplot1, y=c.vectColSums)
      
    }
    
  } 
  
}


addLinePlot <- function(c.vect){
 
  par(new=T)
  # Add a line to the stacked bar chart
  c.min<-min(c.vect)
  c.max<-max(c.vect)
  plot(c.vect, ylim=c(c.min,c.max), axes=F)
  # Add supplementary y-axis on right side
  axis(side=4, ylim=c(c.min,c.max)) 
  # Add x-axis at y=0 (y-axis on right side)
  abline(h=0)
  
}
