
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
tt$label_quarterly <- F
tsBarPlot(short)
tsBarPlot(short,theme = tt)





# 4) stacked bar chart with sum as line


# 5) add additional lines to bar chart (use right y-axis)
