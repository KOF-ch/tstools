# 19122016
# Example of a stacked bar chart, with negative and positive values
c.vect<-cbind(50+rnorm(36), rnorm(36), -20+rnorm(36), 100+rnorm(36))
# Make it a time series object
c.vect<-ts(c.vect,frequency=12,start=c(2004,1))

# The bars with negative values are drawn below the x-axis
# http://stackoverflow.com/questions/27948446/about-barplot-for-data-with-negative-values
c.vect1<-c.vect
c.vect2<-c.vect
# Split into bar charts with positive respectively negative values
c.vect1[c.vect<0]<-0
c.vect2[c.vect>0]<-0
# Vectors are transposed
c.transposed.vect1<-t(c.vect1)
c.transposed.vect2<-t(c.vect2)
# Find the range of the stacked bar chart 
c.max<-round(max(colSums(c.transposed.vect1)))
c.min<-round(min(colSums(c.transposed.vect2)))

# Plot positive bars
barplot(c.transposed.vect1, ylim=c(c.min, c.max), axes=F, col=c("red","green","blue","violet"))
# Add negative bars
barplot(c.transposed.vect2, ylim=c(c.min, c.max), axes=F, col=c("red","green","blue","violet"), add=T)
# Add y-axis on left side
axis(side=2, ylim=c(c.min, c.max))

par(new=T)
# Add a line to the stacked bar chart
plot(c.vect[,2], ylim=c(-3,3), axes=F)
# Add supplementary y-axis on right side of the plot
axis(side=4, ylim=c(-3,3))

# Construct x-axis
time.seq<-seq(from=as.Date("2004/1/1"), by="1 months", length.out=36)
# For every month a tick is drawn
axis(side=1, labels = time.seq, at=seq(from=2004, by=1/12, length.out=36)) 
# Add box around the plot
box() 

title("Stacked bar chart with negative values")