
# short data for better overview
short <- window(KOF$reference,start=c(2008,1),end=c(2012,4))

# bar plot skeleton, no axis etc... just like the 
# tsLinePlot. will be plotted by a wrapper!
# e.g. tsStackedBarPlot, tsplot2y /w right_as_bar option.
# 1) Multiple lines in one plot, same y-axis
# label quarterly puts the year label in the middle of the year
tsplot(short,manual_value_range = c(-10,10))

# might want to reduce x-axis label font-size using theme... 
tsplot(KOF$kofbarometer)

# 2) Multiple lines in one plot, l/r y-axis
# sweet this works as planned... !
tsplot2y(KOF$kofbarometer,KOF$reference)

# 3) simple ts bar chart
tsBarPlot(short)
tt <- initDefaultBarTheme()
tt$label_quarterly <- F
tsBarPlot(short,theme = tt)

# 4) stacked bar chart 
tsli <- list(ts1 = ts(runif(20,-5,10),start=c(2010,1),frequency = 4),
             ts2= ts(runif(20,-7,12),start=c(2010,1),frequency = 4))

# with sum as line when there are more than one series as input
tsBarPlot(tsli,manual_value_range = c(-30,30))


################# Ok til here, par bosition is middle... ###########


#### START EXPERIMENTING WITH BARs... 















# 5) add additional lines to bar chart (use right y-axis)
short <- window(KOF$reference,start=c(2008,1),end=c(2012,4))
short_b <- window(KOF$kofbarometer,start=c(2008,1),end=c(2012,4))

# Quartals balken mitte sollte mitte des Quartals sein, 
# also mitte 2. monat

# something goes wrong here... fix the right y-axis part in draw_ts_bars...
# might be double plot new... 
aa <- tsBarPlot(short,short_b,manual_value_range = c(-20,20),r_manual_value_range = c(50,130),quiet=F)


# add highlight window again! 

# year label in the middle of the year and first tick

# clean up options

# prepare small presentations

# write documentation

# write vignette

# write pdf options

# talk about the issue tracker

# non-goals

# show you can add stuff using addLines and co... 

# show KOF indicator dashboard... 

# release end-of-the-week



