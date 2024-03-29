% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/themes.R
\name{init_tsplot_theme}
\alias{init_tsplot_theme}
\alias{init_tsplot_print_theme}
\title{Initiate Default Theme}
\usage{
init_tsplot_theme(
  auto_bottom_margin = FALSE,
  band_fill_color = c(ETH_Petrol = colors$ETH_Petrol$`100`, ETH_Petrol_60 =
    colors$ETH_Petrol$`60`, ETH_Petrol_40 = colors$ETH_Petrol$`40`, ETH_Petrol_20 =
    colors$ETH_Petrol$`20`, ETH_Purple = colors$ETH_Purple$`100`, ETH_Purple_60 =
    colors$ETH_Purple$`60`, ETH_Purple_40 = colors$ETH_Purple$`40`),
  bar_border = "#000000",
  bar_border_lwd = 1,
  bar_fill_color = c(ETH_Petrol = colors$ETH_Petrol$`100`, ETH_Petrol_60 =
    colors$ETH_Petrol$`60`, ETH_Petrol_40 = colors$ETH_Petrol$`40`, ETH_Petrol_20 =
    colors$ETH_Petrol$`20`, ETH_Purple = colors$ETH_Purple$`100`, ETH_Purple_60 =
    colors$ETH_Purple$`60`, ETH_Purple_40 = colors$ETH_Purple$`40`),
  bar_gap = 15,
  bar_group_gap = 30,
  ci_alpha = "44",
  ci_colors = line_colors,
  ci_legend_label = "\%ci_value\%\% ci for \%series\%",
  default_bottom_margin = 15,
  fill_up_start = FALSE,
  fill_year_with_nas = TRUE,
  highlight_color = colors$ETH_Grey$`20`,
  highlight_window = FALSE,
  highlight_window_end = NA,
  highlight_window_freq = 4,
  highlight_window_start = NA,
  highlight_y_values = NA,
  highlight_y_lwd = 2,
  highlight_y_color = "#000000",
  label_pos = "mid",
  legend_all_left = FALSE,
  legend_box_size = 2,
  legend_col = 1,
  legend_font_size = 1,
  legend_intersp_x = 1,
  legend_intersp_y = 1,
  legend_margin_bottom = 5,
  legend_margin_top = 12,
  legend_seg.len = 2,
  line_colors = c(ETH_Green_60 = colors$ETH_Green$`60`, ETH_Green_100 =
    colors$ETH_Green$`100`, ETH_Petrol_20 = colors$ETH_Petrol$`20`, ETH_Purple_60 =
    colors$ETH_Purple$`60`, ETH_Petrol_60 = colors$ETH_Petrol$`60`, ETH_Purple_100 =
    colors$ETH_Purple$`100`, ETH_Petrol_100 = colors$ETH_Petrol$`100`),
  line_to_middle = TRUE,
  lty = 1,
  lwd = c(2, 3, 1, 4, 2, 4),
  lwd_box = 1.5,
  lwd_quarterly_ticks = 1,
  lwd_x_axis = 1.5,
  lwd_y_axis = 1.5,
  lwd_y_ticks = 1.5,
  lwd_yearly_ticks = 1.5,
  margins = c(NA, 7, 12, 7),
  NA_continue_line = FALSE,
  output_wide = FALSE,
  point_symbol = 1:18,
  pointsize = 12,
  preferred_y_gap_sizes = c(25, 20, 15, 10, 5, 2.5, 1, 0.5),
  quarterly_ticks = TRUE,
  range_must_not_cross_zero = TRUE,
  show_left_y_axis = TRUE,
  show_points = FALSE,
  show_right_y_axis = TRUE,
  show_x_axis = TRUE,
  show_y_grids = TRUE,
  subtitle_adj = 0,
  subtitle_adj_r = 0.9,
  subtitle_cex = 1,
  subtitle_margin = 2,
  subtitle_outer = FALSE,
  subtitle_transform = "toupper",
  sum_as_line = FALSE,
  sum_legend = "sum",
  sum_line_color = c(ETH_Petrol_100 = colors$ETH_Petrol$`100`),
  sum_line_lty = 1,
  sum_line_lwd = 3,
  tcl_quarterly_ticks = -0.4,
  tcl_y_ticks = -0.75,
  tcl_yearly_ticks = -0.75,
  title_adj = 0,
  title_cex.main = 1,
  title_margin = 5,
  title_outer = FALSE,
  title_transform = NA,
  total_bar_margin_pct = 0.2,
  use_bar_gap_in_groups = FALSE,
  use_box = FALSE,
  x_tick_dt = 1,
  xaxs = "i",
  y_grid_color = colors$ETH_Grey$`40`,
  y_grid_count = c(5, 6, 8, 10),
  y_grid_count_strict = FALSE,
  y_las = 2,
  y_range_min_size = NULL,
  y_tick_force_integers = FALSE,
  y_tick_margin = 0.15,
  yaxs = "i",
  yearly_ticks = TRUE
)

init_tsplot_print_theme(
  output_wide = FALSE,
  margins = c(NA, 10/if (output_wide) 1 + 1/3 else 1, 10, 7/if (output_wide) 1 + 1/3 else
    1),
  lwd = scale_theme_param_for_print(c(2, 3, 1, 4, 2, 4), if (output_wide) c(10 + 2/3, 6)
    else c(8, 6)),
  sum_line_lwd = scale_theme_param_for_print(3, if (output_wide) c(10 + 2/3, 6) else c(8,
    6)),
  lwd_box = scale_theme_param_for_print(1.5, if (output_wide) c(10 + 2/3, 6) else c(8,
    6)),
  lwd_x_axis = scale_theme_param_for_print(1.5, if (output_wide) c(10 + 2/3, 6) else c(8,
    6)),
  lwd_yearly_ticks = scale_theme_param_for_print(1.5, if (output_wide) c(10 + 2/3, 6)
    else c(8, 6)),
  lwd_quarterly_ticks = scale_theme_param_for_print(1, if (output_wide) c(10 + 2/3, 6)
    else c(8, 6)),
  lwd_y_axis = scale_theme_param_for_print(1.5, if (output_wide) c(10 + 2/3, 6) else c(8,
    6)),
  lwd_y_ticks = scale_theme_param_for_print(1.5, if (output_wide) c(10 + 2/3, 6) else
    c(8, 6)),
  legend_intersp_y = scale_theme_param_for_print(1, if (output_wide) c(10 + 2/3, 6) else
    c(8, 6)),
  legend_box_size = scale_theme_param_for_print(2, if (output_wide) c(10 + 2/3, 6) else
    c(8, 6)),
  legend_margin_top = 8,
  legend_margin_bottom = 3,
  legend_seg.len = scale_theme_param_for_print(2, if (output_wide) c(10 + 2/3, 6) else
    c(8, 6)),
  pointsize = scale_theme_param_for_print(12, if (output_wide) c(10 + 2/3, 6) else c(8,
    6)),
  ...
)
}
\arguments{
\item{auto_bottom_margin}{logical Should the bottom margin be automatically calculated? This will be overridden if margins[1] is not NA. Default FALSE}

\item{band_fill_color}{character vector of hex colors for the bands if left_as_band == TRUE.}

\item{bar_border}{character hex colors for the border around bars in bar charts.}

\item{bar_border_lwd}{numeric The line width of the borders of bars in barplots. Default 1}

\item{bar_fill_color}{character vector of hex colors for the bars if left_as_bar == TRUE}

\item{bar_gap}{numeric The width of the gap between bars, in \% of space alloted to the bar.}

\item{bar_group_gap}{numeric The width of the gap between groups of bars if group_bar_chart is TRUE.}

\item{ci_alpha}{Numeric 0-255, numeric 0-1 or hey 00-FF, transparency of the confidence interval bands}

\item{ci_colors}{Named colors or hex values Colors of the confidence interval bands}

\item{ci_legend_label}{character A formatting template for how the ci bands should be labelled. May contain the
placeholders. '\%ci_value\%' will be replaced with the ci label. '\%series\%' (will be replaced with the series name)
exactly once. Defaults to '\%ci_value\% ci for \%series\%'}

\item{default_bottom_margin}{numeric The bottom margin to use when margins[1] is NA but neither auto_legend nor auto_bottom_margin are true. Default 3}

\item{fill_up_start}{logical shoule the start of the year also be filled? Has no effect if fill_year_with_nas == FALSE. Default FALSE}

\item{fill_year_with_nas}{logical should year be filled up with missing in order to plot the entire year on the axis. Defaults to TRUE,}

\item{highlight_color}{character hex color code of highlight background, defaults to "#e9e9e9".}

\item{highlight_window}{logical should a particular time span be highlighted by different background color. Defaults to FALSE.}

\item{highlight_window_end}{integer vector highlight window start position, defaults to NA.,}

\item{highlight_window_freq}{integer frequency of the higlight window defintion, defaults to 4.}

\item{highlight_window_start}{integer vector highlight window start position, defaults to NA.}

\item{highlight_y_values}{numeric Vector of y values to highlight with a bold line}

\item{highlight_y_lwd}{integer Line width of the lines to highlight y values}

\item{highlight_y_color}{character Color of the lines to highlight y values}

\item{label_pos}{character, denotes where the x-axis label is at. defaults to "mid", alternative value: "start".}

\item{legend_all_left}{logical Should all legend entries be drawn on the left side of the plot? Default FALSE}

\item{legend_box_size}{numeric The size of the squares denoting bar colors in the legend. Default 2}

\item{legend_col}{integer number of columns for the legend, defaults to 3.}

\item{legend_font_size}{numeric passed on to the \code{cex} parameter of \code{\link{legend}}, defaults to 1}

\item{legend_intersp_x}{numeric same as base \code{\link{legend}} parameter, defaults to 1}

\item{legend_intersp_y}{numeric same as base \code{\link{legend}} parameter, defaults to 1}

\item{legend_margin_bottom}{numeric Distance between bottom of legend and bottom of graphic in \% of device height, default 5}

\item{legend_margin_top}{numeric Distance between bottom of plot and top of legends \% of device height, defaults to 12}

\item{legend_seg.len}{numeric Length of the line segments in the legend. Default 2}

\item{line_colors}{character vector of hex colors for 6 lines.}

\item{line_to_middle}{logical try to put a line into the middle of the plot. defaults to TRUE.}

\item{lty}{integer vector line type defaults to 1.}

\item{lwd}{integer vector line width, defaults to c(2,3,1,4,2,4).}

\item{lwd_box}{numeric Line width of the box around the plot. Default 1.5}

\item{lwd_quarterly_ticks}{numeric, width of yearly ticks, defaults to 1.}

\item{lwd_x_axis}{numeric The line width of the x axis. Default 1.5}

\item{lwd_y_axis}{numeric The line width of the y axis. Default 1.5}

\item{lwd_y_ticks}{numeric Line width of the y ticks. Default 1.5}

\item{lwd_yearly_ticks}{numeric, width of yearly ticks, defaults to 1.5.}

\item{margins}{integer vector defaults to c(NA, 4, 3, 3) + 0.1. Set margins[1] to NA to automatically determine the bottom margin such that the legend fits (if either auto_legend or auto_bottom_margin are TRUE)}

\item{NA_continue_line}{boolean If true, NA values in time series are ignored and a contonuous line is drawn. Multiple values to turn this behavior on/off for indivitual series are supported. Default FALSE}

\item{output_wide}{logical Should the output file be in a wide format (16:9) or (4:3)? Only if output_format is not "plot". Default FALSE}

\item{point_symbol}{integer or character The symbol to use for marking data points. Multiple values can be supplied to set the symbol for each individual series See \code{pch} in \code{?par}. Default 1:18}

\item{pointsize}{Numeric Point size of text, in 1/72 of an inch}

\item{preferred_y_gap_sizes}{numeric c(25, 20, 15, 10, 5, 2.5, 1, 0.5),}

\item{quarterly_ticks}{logical, should quarterly ticks be shown. Defaults to TRUE.}

\item{range_must_not_cross_zero}{logical automatic range finders are forced to do not find ranges below zero. Defaults to TRUE.}

\item{show_left_y_axis}{logical: should left y axis be shown, defaults to TRUE.}

\item{show_points}{boolean Whether to draw the symbol specified by point_symbol at the data points. Multiple values can be supplied to enable/disable showing points for each individual series Default FALSE}

\item{show_right_y_axis}{logical: should left y axis be shown, defaults to TRUE.}

\item{show_x_axis}{locigal: should x axis be shown, defaults to TRUE}

\item{show_y_grids}{logical should y_grids by shown at all, defaults to TRUE.}

\item{subtitle_adj}{numeric same as base \code{\link{plot}} parameter, defaults to 0.}

\item{subtitle_adj_r}{numeric same as base \code{\link{plot}} parameter, defaults to .9}

\item{subtitle_cex}{numeric same as base \code{\link{plot}} parameter, defaults to 1.}

\item{subtitle_margin}{numeric How far above the plot the title is placed in \% of the device height. Defaults to 2.}

\item{subtitle_outer}{logical same as base \code{\link{plot}} parameter, defaults to TRUE}

\item{subtitle_transform}{function to transform the subtitle, defaults to "toupper",}

\item{sum_as_line}{logical should the sum of stacked time series be displayed as a line on top of stacked bar charts. Defaults to FALSE,}

\item{sum_legend}{character Label for the sum line, defaults to "sum". Set to NULL to not label the line at all.}

\item{sum_line_color}{character hex color of of sum_as_line, defaults "#91056a".}

\item{sum_line_lty}{integer line type of sum_as_line, defaults to 1.}

\item{sum_line_lwd}{integer line width of sum_as_line, defaults to 3.}

\item{tcl_quarterly_ticks}{numeric, length of quarterly ticks. See tcl_yearly_ticks, defaults to -0.4}

\item{tcl_y_ticks}{numeric Length of y ticks, see \code{tcl_yearly_ticks}. Default -0.75}

\item{tcl_yearly_ticks}{numeric, length of yearly ticks. Analogous to \code{cex} for \code{\link{axis}}. defaults to -0.75.}

\item{title_adj}{numeric, same as base \code{\link{plot}} parameter, defaults to 0.}

\item{title_cex.main}{numeric, same as base \code{\link{plot}} parameter defaults to 1}

\item{title_margin}{numeric How far above the plot the title is placed in \% of the device height. Default 8}

\item{title_outer}{logical, currently undocumented. Defaults to TRUE.}

\item{title_transform}{function to transform the title, defaults to NA.}

\item{total_bar_margin_pct}{numeric defintion as in base plot, defaults to "i", defaults to .2,}

\item{use_bar_gap_in_groups}{logical Should there be gaps of size bar_gap between the bars in a group if group_bar_chart = TRUE? Default FALSE}

\item{use_box}{logical use a box around the plot.}

\item{x_tick_dt}{numeric The distance between ticks on the x axis in years. The first tick will always be at the start of the plotted time series. Defaults to 1.}

\item{xaxs}{character axis defintion as in base plot, defaults to "i".}

\item{y_grid_color}{character hex color of grids. Defaults to gray "#CCCCCC".}

\item{y_grid_count}{integer vector preferred y grid counts c(5,6,8,10).}

\item{y_grid_count_strict}{logical should we strictly stick to preferred y grid count? Defaults to FALSE.}

\item{y_las}{integer, same as base \code{\link{plot}} parameter defaults to 2.}

\item{y_range_min_size}{= NULL  ,}

\item{y_tick_force_integers}{logical Should y ticks be forced (rounded down) to whole numbers? Default FALSE}

\item{y_tick_margin}{numeric, minimal percentage of horizontal grid that needs to be clean, i.e., without lines or bars. Defaults to 0.15 (15 percent).}

\item{yaxs}{character axis defintion as in base plot, defaults to "i".}

\item{yearly_ticks}{logical, should yearly ticks be shown. Defaults to TRUE.}

\item{...}{All the other arguments to \code{init_tsplot_thene}}
}
\description{
The \code{\link{tsplot}} methods provide a theme argument which is used to pass on a plethora of useful defaults. These defaults are essentially stored in a list. Sometimes the user may want to tweak some of these defaults while keeping most of them.
Hence the init_tsplot_theme function create a fresh list object containing default values for lot of different layout parameters etc. By replacing single elements of the list and passing the entire list to the plot function, single aspects can be tweaked while keeping most defaults. Init defaultTheme does not need any parameters.

This function provides sensible defaults for margins, font size, line width etc. scaled to
the dimensions of the output file.
}
\details{
Themes are essentially list that contain \code{\link{par}} parameters. Below all items are listed, some of them with comments.
The per-line parameters (\code{line_colors, lwd, lty, show_points, point_symbol}) are recycled if more time series than elements on the corresponding
theme vectors are supplied. e.g. if four time series are plotted but only two line_colors are supplied, the first and third series have the first color,
while the second and fourth series have the second color.
The list contains the following elements:
}
\examples{
\dontrun{
# create a list
data(KOF)
tt <- init_tsplot_theme()
# adjust a single element
tt$highlight_window <- TRUE
# pass the list to tsplot
tsplot(KOF$kofbarometer, theme = tt)
# for more theme examples check the vignette
vignette("tstools")
}

}
\author{
Matthias Bannert
}
