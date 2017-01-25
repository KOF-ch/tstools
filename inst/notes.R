
# short data for better overview
short <- window(KOF$reference,start=c(2008,1),end=c(2012,4))

# bar plot skeleton, no axis etc... just like the 
# tsLinePlot. will be plotted by a wrapper!
# e.g. tsStackedBarPlot, tsplot2y /w right_as_bar option.
# 1) Multiple lines in one plot, same y-axis
ll <- initDefaultLineTheme()
# label quarterly puts the year label in the middle of the year
ll$label_quarterly <- F

tsplot(short,manual_value_range = c(-20,10),theme = ll)
tsplot(short,manual_value_range = c(-20,10))

tsplot(KOF$reference,manual_value_range = c(-20,10),theme = ll)
# might want to reduce x-axis label font-size using theme... 
tsplot(KOF$kofbarometer)


# 2) Multiple lines in one plot, l/r y-axis
# sweet this works as planned... !
tsplot2y(KOF$kofbarometer,KOF$reference)

# 3) simple ts bar chart
tt <- initDefaultBarTheme()
tt$label_quarterly <- T
tsBarPlot(short)
tsBarPlot(short,theme = tt)

# 4) stacked bar chart 
tsli <- list(ts1 = ts(runif(20,-5,10),start=c(2010,1),frequency = 4),
             ts2= ts(runif(20,-7,12),start=c(2010,1),frequency = 4))

# with sum as line when there are more than one series as input
tsBarPlot(tsli,manual_value_range = c(-30,30))

# 5) add additional lines to bar chart (use right y-axis)


# add highlight window again! 

# clean up options

# prepare small presentations

# write pdf options

# talk about the issue tracker

# non-goals
