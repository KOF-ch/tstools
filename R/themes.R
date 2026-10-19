colors <- yaml::read_yaml(system.file("colors.yaml", package = "tstools"))

#' Initiate Default Theme
#'
#' The \code{\link{tsplot}} methods provide a theme argument which is used to pass on a plethora of useful defaults. These defaults are essentially stored in a list. Sometimes the user may want to tweak some of these defaults while keeping most of them.
#' Hence the init_tsplot_theme function create a fresh list object containing default values for lot of different layout parameters etc. By replacing single elements of the list and passing the entire list to the plot function, single aspects can be tweaked while keeping most defaults. Init defaultTheme does not need any parameters.
#'
#' @details
#' Themes are essentially list that contain \code{\link{par}} parameters. Below all items are listed, some of them with comments.
#' The per-line parameters (\code{line_colors, lwd, lty, show_points, point_symbol}) are recycled if more time series than elements on the corresponding
#' theme vectors are supplied. e.g. if four time series are plotted but only two line_colors are supplied, the first and third series have the first color,
#' while the second and fourth series have the second color.
#' The list contains the following elements:
#' @param auto_bottom_margin logical Should the bottom margin be automatically calculated? This will be overridden if margins[1] is not NA. Default FALSE
#' @param band_fill_color character vector of hex colors for the bands if left_as_band == TRUE.
#' @param bar_border character hex colors for the border around bars in bar charts.
#' @param bar_border_lwd numeric The line width of the borders of bars in barplots. Default 1
#' @param bar_fill_color character vector of hex colors for the bars if left_as_bar == TRUE
#' @param bar_gap numeric The width of the gap between bars, in \% of space alloted to the bar.
#' @param bar_group_gap numeric The width of the gap between groups of bars if group_bar_chart is TRUE.
#' @param ci_alpha Numeric 0-255, numeric 0-1 or hey 00-FF, transparency of the confidence interval bands
#' @param ci_colors Named colors or hex values Colors of the confidence interval bands
#' @param ci_legend_label character A formatting template for how the ci bands should be labelled. May contain the
#' placeholders. '\%ci_value\%' will be replaced with the ci label. '\%series\%' (will be replaced with the series name)
#' exactly once. Defaults to '\%ci_value\% ci for \%series\%'
#' @param default_bottom_margin numeric The bottom margin to use when margins[1] is NA but neither auto_legend nor auto_bottom_margin are true. Default 3
#' @param fill_up_start logical shoule the start of the year also be filled? Has no effect if fill_year_with_nas == FALSE. Default FALSE
#' @param fill_year_with_nas logical should year be filled up with missing in order to plot the entire year on the axis. Defaults to TRUE,
#' @param highlight_color character hex color code of highlight background, defaults to "#e9e9e9".
#' @param highlight_window logical should a particular time span be highlighted by different background color. Defaults to FALSE.
#' @param highlight_window_end integer vector highlight window start position, defaults to NA.,
#' @param highlight_window_freq integer frequency of the higlight window defintion, defaults to 4.
#' @param highlight_window_start integer vector highlight window start position, defaults to NA.
#' @param highlight_y_values numeric Vector of y values to highlight with a bold line
#' @param highlight_y_lwd integer Line width of the lines to highlight y values
#' @param highlight_y_color character Color of the lines to highlight y values
#' @param label_pos character, denotes where the x-axis label is at. defaults to "mid", alternative value: "start".
#' @param legend_all_left logical Should all legend entries be drawn on the left side of the plot? Default FALSE
#' @param legend_box_size numeric The size of the squares denoting bar colors in the legend. Default 2
#' @param legend_col integer number of columns for the legend, defaults to 3.
#' @param legend_font_size numeric passed on to the \code{cex} parameter of \code{\link{legend}}, defaults to 1
#' @param legend_intersp_x numeric same as base \code{\link{legend}} parameter, defaults to 1
#' @param legend_intersp_y numeric same as base \code{\link{legend}} parameter, defaults to 1
#' @param legend_margin_bottom numeric Distance between bottom of legend and bottom of graphic in \% of device height, default 5
#' @param legend_margin_top numeric Distance between bottom of plot and top of legends \% of device height, defaults to 12
#' @param legend_seg.len numeric Length of the line segments in the legend. Default 2
#' @param line_colors character vector of hex colors for 6 lines.
#' @param line_to_middle logical try to put a line into the middle of the plot. defaults to TRUE.
#' @param lty integer vector line type defaults to 1.
#' @param lwd integer vector line width, defaults to c(2,3,1,4,2,4).
#' @param lwd_box numeric Line width of the box around the plot. Default 1.5
#' @param lwd_quarterly_ticks numeric, width of yearly ticks, defaults to 1.
#' @param lwd_x_axis numeric The line width of the x axis. Default 1.5
#' @param lwd_y_axis numeric The line width of the y axis. Default 1.5
#' @param lwd_y_ticks numeric Line width of the y ticks. Default 1.5
#' @param lwd_yearly_ticks numeric, width of yearly ticks, defaults to 1.5.
#' @param margins integer vector defaults to c(NA, 4, 3, 3) + 0.1. Set margins[1] to NA to automatically determine the bottom margin such that the legend fits (if either auto_legend or auto_bottom_margin are TRUE)
#' @param NA_continue_line boolean If true, NA values in time series are ignored and a contonuous line is drawn. Multiple values to turn this behavior on/off for indivitual series are supported. Default FALSE
#' @param output_wide logical Should the output file be in a wide format (16:9) or (4:3)? Only if output_format is not "plot". Default FALSE
#' @param point_symbol integer or character The symbol to use for marking data points. Multiple values can be supplied to set the symbol for each individual series See \code{pch} in \code{?par}. Default 1:18
#' @param pointsize Numeric Point size of text, in 1/72 of an inch
#' @param preferred_y_gap_sizes numeric c(25, 20, 15, 10, 5, 2.5, 1, 0.5),
#' @param quarterly_ticks logical, should quarterly ticks be shown. Defaults to TRUE.
#' @param range_must_not_cross_zero logical automatic range finders are forced to do not find ranges below zero. Defaults to TRUE.
#' @param show_left_y_axis logical: should left y axis be shown, defaults to TRUE.
#' @param show_points boolean Whether to draw the symbol specified by point_symbol at the data points. Multiple values can be supplied to enable/disable showing points for each individual series Default FALSE
#' @param show_right_y_axis logical: should left y axis be shown, defaults to TRUE.
#' @param show_x_axis locigal: should x axis be shown, defaults to TRUE
#' @param show_y_grids logical should y_grids by shown at all, defaults to TRUE.
#' @param subtitle_adj numeric same as base \code{\link{plot}} parameter, defaults to 0.
#' @param subtitle_adj_r numeric same as base \code{\link{plot}} parameter, defaults to .9
#' @param subtitle_cex numeric same as base \code{\link{plot}} parameter, defaults to 1.
#' @param subtitle_margin numeric How far above the plot the title is placed in \% of the device height. Defaults to 2.
#' @param subtitle_outer logical same as base \code{\link{plot}} parameter, defaults to TRUE
#' @param subtitle_transform function to transform the subtitle, defaults to "toupper",
#' @param sum_as_line logical should the sum of stacked time series be displayed as a line on top of stacked bar charts. Defaults to FALSE,
#' @param sum_legend character Label for the sum line, defaults to "sum". Set to NULL to not label the line at all.
#' @param sum_line_color character hex color of of sum_as_line, defaults "#91056a".
#' @param sum_line_lty integer line type of sum_as_line, defaults to 1.
#' @param sum_line_lwd integer line width of sum_as_line, defaults to 3.
#' @param tcl_quarterly_ticks numeric, length of quarterly ticks. See tcl_yearly_ticks, defaults to -0.4
#' @param tcl_y_ticks numeric Length of y ticks, see \code{tcl_yearly_ticks}. Default -0.75
#' @param tcl_yearly_ticks numeric, length of yearly ticks. Analogous to \code{cex} for \code{\link{axis}}. defaults to -0.75.
#' @param title_adj numeric, same as base \code{\link{plot}} parameter, defaults to 0.
#' @param title_cex.main numeric, same as base \code{\link{plot}} parameter defaults to 1
#' @param title_margin numeric How far above the plot the title is placed in \% of the device height. Default 8
#' @param title_outer logical, currently undocumented. Defaults to TRUE.
#' @param title_transform function to transform the title, defaults to NA.
#' @param total_bar_margin_pct numeric defintion as in base plot, defaults to "i", defaults to .2,
#' @param use_bar_gap_in_groups logical Should there be gaps of size bar_gap between the bars in a group if group_bar_chart = TRUE? Default FALSE
#' @param use_box logical use a box around the plot.
#' @param x_tick_dt numeric The distance between ticks on the x axis in years. The first tick will always be at the start of the plotted time series. Defaults to 1.
#' @param xaxs character axis defintion as in base plot, defaults to "i".
#' @param y_grid_color character hex color of grids. Defaults to gray "#CCCCCC".
#' @param y_grid_count integer vector preferred y grid counts c(5,6,8,10).
#' @param y_grid_count_strict logical should we strictly stick to preferred y grid count? Defaults to FALSE.
#' @param y_las integer, same as base \code{\link{plot}} parameter defaults to 2.
#' @param y_range_min_size = NULL  ,
#' @param y_tick_force_integers logical Should y ticks be forced (rounded down) to whole numbers? Default FALSE
#' @param y_tick_margin numeric, minimal percentage of horizontal grid that needs to be clean, i.e., without lines or bars. Defaults to 0.15 (15 percent).
#' @param yaxs character axis defintion as in base plot, defaults to "i".
#' @param yearly_ticks logical, should yearly ticks be shown. Defaults to TRUE.
#' @examples
#' \dontrun{
#' # create a list
#' data(KOF)
#' tt <- init_tsplot_theme()
#' # adjust a single element
#' tt$highlight_window <- TRUE
#' # pass the list to tsplot
#' tsplot(KOF$kofbarometer, theme = tt)
#' # for more theme examples check the vignette
#' vignette("tstools")
#' }
#'
#' @author Matthias Bannert
#' @export
init_tsplot_theme <- function(
  auto_bottom_margin = FALSE,
  band_fill_color = c(
    ETH_Petrol = colors$ETH_Petrol$`100`,
    ETH_Petrol_60 = colors$ETH_Petrol$`60`,
    ETH_Petrol_40 = colors$ETH_Petrol$`40`,
    ETH_Petrol_20 = colors$ETH_Petrol$`20`,
    ETH_Purple = colors$ETH_Purple$`100`,
    ETH_Purple_60 = colors$ETH_Purple$`60`,
    ETH_Purple_40 = colors$ETH_Purple$`40`
  ),
  bar_border = "#000000",
  bar_border_lwd = 1,
  bar_fill_color = c(
    ETH_Petrol = colors$ETH_Petrol$`100`,
    ETH_Petrol_60 = colors$ETH_Petrol$`60`,
    ETH_Petrol_40 = colors$ETH_Petrol$`40`,
    ETH_Petrol_20 = colors$ETH_Petrol$`20`,
    ETH_Purple = colors$ETH_Purple$`100`,
    ETH_Purple_60 = colors$ETH_Purple$`60`,
    ETH_Purple_40 = colors$ETH_Purple$`40`
  ),
  bar_gap = 15,
  bar_group_gap = 30,
  ci_alpha = "44",
  ci_colors = line_colors,
  ci_legend_label = "%ci_value%% ci for %series%",
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
  line_colors = c(
    ETH_Green_60 = colors$ETH_Green$`60`,
    ETH_Green_100 = colors$ETH_Green$`100`,
    ETH_Petrol_20 = colors$ETH_Petrol$`20`,
    ETH_Purple_60 = colors$ETH_Purple$`60`,
    ETH_Petrol_60 = colors$ETH_Petrol$`60`,
    ETH_Purple_100 = colors$ETH_Purple$`100`,
    ETH_Petrol_100 = colors$ETH_Petrol$`100`
  ),
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
  subtitle_adj_r = .9,
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
  total_bar_margin_pct = .2,
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
) {
  as.list(environment())[names(formals())]
}

scale_theme_param_for_print <- function(value, dims) {
  constant <- 6
  (dims[2] / constant) * value
}

#' Initialize a tsplot theme with parameters scaled to device size
#'
#' This function provides sensible defaults for margins, font size, line width etc. scaled to
#' the dimensions of the output file.
#'
#' @param ... All the other arguments to \code{init_tsplot_thene}
#'
#' @rdname init_tsplot_theme
#'
#' @export
init_tsplot_print_theme <- function(
  output_wide = FALSE,
  margins = c(NA, 10 / `if`(output_wide, 1 + 1 / 3, 1), 10, 7 / `if`(output_wide, 1 + 1 / 3, 1)),
  lwd = scale_theme_param_for_print(c(2, 3, 1, 4, 2, 4), `if`(output_wide, c(10 + 2 / 3, 6), c(8, 6))),
  sum_line_lwd = scale_theme_param_for_print(3, `if`(output_wide, c(10 + 2 / 3, 6), c(8, 6))),
  lwd_box = scale_theme_param_for_print(1.5, `if`(output_wide, c(10 + 2 / 3, 6), c(8, 6))),
  lwd_x_axis = scale_theme_param_for_print(1.5, `if`(output_wide, c(10 + 2 / 3, 6), c(8, 6))),
  lwd_yearly_ticks = scale_theme_param_for_print(1.5, `if`(output_wide, c(10 + 2 / 3, 6), c(8, 6))),
  lwd_quarterly_ticks = scale_theme_param_for_print(1, `if`(output_wide, c(10 + 2 / 3, 6), c(8, 6))),
  lwd_y_axis = scale_theme_param_for_print(1.5, `if`(output_wide, c(10 + 2 / 3, 6), c(8, 6))),
  lwd_y_ticks = scale_theme_param_for_print(1.5, `if`(output_wide, c(10 + 2 / 3, 6), c(8, 6))),
  legend_intersp_y = scale_theme_param_for_print(1, `if`(output_wide, c(10 + 2 / 3, 6), c(8, 6))),
  legend_box_size = scale_theme_param_for_print(2, `if`(output_wide, c(10 + 2 / 3, 6), c(8, 6))),
  legend_margin_top = 8,
  legend_margin_bottom = 3,
  legend_seg.len = scale_theme_param_for_print(2, `if`(output_wide, c(10 + 2 / 3, 6), c(8, 6))),
  pointsize = scale_theme_param_for_print(12, `if`(output_wide, c(10 + 2 / 3, 6), c(8, 6))),
  ...
) {
  init_tsplot_theme(
    margins = margins,
    lwd = lwd,
    sum_line_lwd = sum_line_lwd,
    lwd_box = lwd_box,
    lwd_x_axis = lwd_x_axis,
    lwd_yearly_ticks = lwd_yearly_ticks,
    lwd_quarterly_ticks = lwd_quarterly_ticks,
    lwd_y_axis = lwd_y_axis,
    lwd_y_ticks = lwd_y_ticks,
    legend_intersp_y = legend_intersp_y,
    legend_box_size = legend_box_size,
    legend_margin_top = legend_margin_top,
    legend_margin_bottom = legend_margin_bottom,
    legend_seg.len = legend_seg.len,
    pointsize = pointsize,
    output_wide = output_wide,
    ...
  )
}

#' Initiate Default Theme
#'
#' The \code{\link{tsggplot}} methods provide a theme argument which is used to
#' pass on a plethora of useful defaults. These defaults are essentially
#' stored in a list. Sometimes the user may want to tweak some of these
#' defaults while keeping most of them. Hence the init_tsplot_theme function
#' creates a fresh list object containing default values for a lot of
#' different layout parameters etc. By replacing single elements of the list
#' and passing the entire list to the plot function, single aspects can be
#' tweaked while keeping most defaults. Init defaultTheme does not need any
#' parameters.
#'
#' @details
#' Themes are essentially lists that contain \code{\link{par}} parameters.
#' Below all items are listed, some of them with comments. The per-line
#' parameters (\code{line_colors, lwd, lty, show_points, point_symbol}) are
#' recycled if more time series than elements on the corresponding theme
#' vectors are supplied. e.g. if four time series are plotted but only two
#' line_colors are supplied, the first and third series have the first color,
#' while the second and fourth series have the second color. The list contains
#' the following elements:
#' @param auto_bottom_margin logical Should the bottom margin be automatically
#' calculated? This will be overridden if margins[1] is not NA. Default FALSE
#' @param axis.line.x Appearance of bottom x-axis line, defined using
#' [ggplot2::element_line()]. Use to control line color, width, etc.
#' Set to [ggplot2::element_blank()] to hide the y-axis.
#' @param axis.line.y Appearance of left y-axis line, defined using
#' [ggplot2::element_line()]. Use to control line color, width, etc.
#' Overrides axis.line.y.left and axis.line.y.right if set other than
#' [ggplot2::element_line()].
#' Set to [ggplot2::element_blank()] to hide the y-axis.
#' @param axis.line.y.left Appearance of left y-axis line, defined using
#' [ggplot2::element_line()]. Use to control line color, width, etc.
#' @param axis.line.y.right Appearance of right y-axis line, defined using
#' [ggplot2::element_line()]. Use to control line color, width, etc.
#' @param axis.minor.ticks.length Length of minor axis ticks, set relative to
#' `axis.ticks.length` using [ggplot2::rel()].
#' @param axis.minor.ticks.x.bottom Appearance of bottom x-axis minor ticks,
#' defined using [ggplot2::element_line()]. Use to style or enable minor
#' ticks.
#' @param axis.text An `element_text` object applied to all axis text
#' elements (x and y), unless overridden. Set to `element_blank()` to
#' remove all axis text.
#' @param axis.text.x An `element_text` object specifying styling for
#' x-axis text labels (e.g., `hjust`, `size`, `angle`).
#' @param axis.text.x.pos A string indicating the horizontal alignment
#' of x-axis text. Use `"mid"` to center-align (`hjust = 0`), or
#' `"start"` for alignment.
#' @param axis.text.y An `element_text` object specifying styling for the
#' y-axes text labels.
#' @param axis.text.y.left An `element_text` object specifying styling
#' for left y-axis text labels.
#' @param axis.text.y.right An `element_text` object specifying styling
#' for right y-axis text labels.
#' @param axis.ticks.length Length of major axis ticks. Set using
#' [ggplot2::unit()], e.g., `unit(10, "pt")`.
#' @param axis.ticks.x.bottom Appearance of bottom x-axis major ticks, defined
#' using [ggplot2::element_line()]. Use to control line color, width, etc.
#' @param axis.ticks.y Appearance of y-axis ticks, defined using
#' [ggplot2::element_line()]. Use to control line color, width, etc.
#' @param panel.grid.major.x ggplot2 element_blank for major x-axis grid lines.
#' @param panel.grid.major.y ggplot2 element_line for major y-axis grid lines.
#' @param panel.grid.minor ggplot2 element_blank for minor grid lines.
#' @param plot.title ggplot2 element_text for bold plot titles of size 20.
#' @param axis_x_tick_dt numeric The distance between ticks on the x axis in
#' years. The first tick will always be at the start of the plotted time
#' series. If neq 1 then quarterly ticks will not be shown. Defaults to 1.
#' @param axis_x_label_dt numeric The distance between labels on the x axis in
#' years. Defaults to 2.
#' @param axis_x_pad numeric how far past the last data point (and before the
#' first, symmetrically) the x-axis extends, in years. Defaults to NULL,
#' which adds no margin when fill_year_with_nas == TRUE (the axis already
#' ends with the filled-up year, as in tsplot) and otherwise scales the
#' margin to the series' own span -- proportionally small for short series
#' (so e.g. a few days of daily data isn't dwarfed by a fixed multi-month
#' margin), capped at one quarter once the series spans a quarter or more.
#' Set to a specific value (0 for none) to override.
#' @param axis_x_date_ticks character, only relevant for daily/weekly xts
#' series (which plot on a real Date x-axis). \code{"auto"} (the default)
#' lets ggplot2 pick sensible day/week/month/year tick spacing from the
#' series' own span -- necessary for short series, since the alternative is
#' always year-spaced regardless of span (broken for anything shorter than
#' a year, e.g. producing an \code{NA} tick). \code{"years"} keeps that
#' fixed year-based spacing (using axis_x_label_dt), matching the numeric
#' (ts/monthly/quarterly/annual) x-axis' own always-year-based convention.
#' @param band_fill_color character vector of hex colors for the bands if
#' left_as_band == TRUE.
#' @param bar_border_color character hex colors for the border around bars in
#' bar charts.
#' @param bar_border_linewidth numeric The line width of the borders of bars in
#' barplots. Default 1
#' @param bar_fill_color character vector of hex colors for the bars if
#' left_as_bar == TRUE
#' @param bar_gap numeric The width of the gap between bars, in \% of space
#' allotted to the bar.
#' @param ci_alpha Numeric 0-255, numeric 0-1 or hex 00-FF, transparency of
#' the confidence interval bands
#' @param ci_colors Named colors or hex values Colors of the confidence
#' interval bands
#' @param ci_legend_label character A formatting template for how the ci bands
#' should be labeled. May contain the placeholders. '\%ci_value\%' will be
#' replaced with the ci label. '\%series\%' (will be replaced with the series
#' name) exactly once. Defaults to '\%ci_value\% ci for \%series\%'
#' @param fill_up_start logical should the start of the year also be filled?
#' Has no effect if fill_year_with_nas == FALSE. Default FALSE
#' @param fill_year_with_nas logical should year be filled up with missing in
#' order to plot the entire year on the axis. Defaults to TRUE,
#' @param grids_x_count integer vector preferred x grid counts c(5,6,8,10).
#' @param grids_x_count_strict logical should we strictly stick to preferred x
#' grid count? Defaults to FALSE.
#' @param highlight_window logical should a particular time span be
#' highlighted by different background color. Defaults to FALSE.
#' @param highlight_window_alpha numeric opacity of the highlight window
#' between 0 (transparent) and 1 (opaque), defaults to 0.55.
#' @param highlight_window_color character hex color code of highlight
#' background, defaults to "#e9e9e9".
#' @param highlight_window_end for a \code{ts}/monthly/quarterly/annual xts
#' series, an integer \code{c(year, period)} vector (or list of them, paired
#' with \code{highlight_window_start}), interpreted using
#' \code{highlight_window_freq}. For a daily/weekly/hourly xts series (which
#' plot on a Date/datetime x-axis instead), a \code{Date}/\code{POSIXct}
#' value (or anything \code{as.Date()}/\code{as.POSIXct()} accepts, e.g. a
#' \code{"YYYY-MM-DD"} string) instead -- \code{highlight_window_freq} is
#' not used in this case. Defaults to NA, which highlights up to the end of
#' the plotted range.
#' @param highlight_window_freq integer frequency of the highlight window
#' definition, defaults to 4. Only used for \code{ts}/monthly/quarterly/
#' annual series -- see \code{highlight_window_start}/
#' \code{highlight_window_end}.
#' @param highlight_window_start same format as \code{highlight_window_end}
#' (see there), for the start of the window instead. Defaults to NA, which
#' highlights starting 2 years before the end of the plotted range.
#' @param legend_all_left logical When a \code{tsr} (right-axis) series is
#' plotted, should all legend entries be merged into a single legend instead
#' of two separately grouped ones (one for the left-axis series, one for the
#' right-axis series) below the plot? Defaults to FALSE. Ignored when there
#' is no \code{tsr}. Note this only
#' affects static (print/save) output; \code{tsggplotly()} always renders a
#' single merged legend regardless of this setting.
#' @param line_colors character vector of hex colors for 6 lines.
#' @param line_to_middle logical try to put a line into the middle of the
#' plot. defaults to TRUE.
#' @param linetype integer vector line type defaults to 1.
#' @param linewidth integer vector line width, defaults to c(2,3,1,4,2,4).
#' @param NA_continue_line boolean If true, NA values in time series are
#' ignored and a continuous line is drawn. Multiple values to turn this
#' behavior on/off for individual series are supported. Default FALSE
#' @param point_symbol integer or character The symbol to use for marking data
#' points. Multiple values can be supplied to set the symbol for each
#' individual series See \code{pch} in \code{?par}. Default 1:18
#' @param pointsize Numeric Point size of text, in 1/72 of an inch
#' @param preferred_y_gap_sizes numeric c(25, 20, 15, 10, 5, 2.5, 1, 0.5),
#' @param quarterly_ticks logical, should quarterly ticks be shown. Defaults
#' to TRUE.
#' @param range_must_not_cross_zero logical automatic range finders are forced
#' to do not find ranges below zero. Defaults to TRUE.
#' @param show_points boolean Whether to draw the symbol specified by
#' point_symbol at the data points. Multiple values can be supplied to
#' enable/disable showing points for each individual series Default FALSE
#' @param sum_as_line logical should the sum of stacked time series be
#' displayed as a line on top of stacked bar charts. Defaults to FALSE,
#' @param sum_legend character Label for the sum line, defaults to "sum". Set
#' to NULL to not label the line at all.
#' @param sum_line_color character hex color of of sum_as_line, defaults
#' "#91056a".
#' @param sum_line_linetype integer line type of sum_as_line, defaults to 1.
#' @param sum_line_linewidth integer line width of sum_as_line, defaults to 3.
#' @param yearly_ticks logical, should yearly ticks be shown. Defaults to TRUE.
#' @param y_range_min_size = NULL  ,
#' @param y_tick_force_integers logical Should y ticks be forced (rounded
#' down) to whole numbers? Default FALSE
#' @param y_tick_margin numeric, minimal percentage of horizontal grid that
#' needs to be clean, i.e., without lines or bars. Defaults to 0.15 (15
#' percent).
#' @param ... additional ggplot2 theme arguments
#'
#' @inheritParams ggplot2::theme
#'
#' @examples
#' \dontrun{
#' # create a list
#' data(KOF)
#' tt <- init_tsggplot_theme()
#' # adjust a single element
#' tt$highlight_window <- TRUE
#' # pass the list to tsplot
#' tsggplot(KOF$kofbarometer, theme = tt)
#' }
#'
#' @importFrom ggplot2 element_blank element_line element_text rel unit
#'
#' @seealso \href{https://ggplot2.tidyverse.org/reference/theme.html}{ggplot2 theme vignette}
#'
#' @author Merlin Scherer
#' @export
init_tsggplot_theme <- function(
  axis.line.x = element_line(
    color = colors$ETH_Grey$`40`,
    linewidth = 0.5
  ),
  axis.line.y = element_line(),
  axis.line.y.left = element_line(
    color = colors$ETH_Grey$`40`,
    linewidth = 0.5
  ),
  axis.line.y.right = element_line(
    color = colors$ETH_Grey$`40`,
    linewidth = 0.5
  ),
  axis.minor.ticks.length = rel(0.3),
  axis.minor.ticks.x.bottom = aes(),
  axis.text = element_text(),
  axis.text.x = element_text(size = 13),
  axis.text.x.pos = "mid",
  axis.text.y.left = element_text(size = 13),
  axis.text.y.right = element_text(size = 13),
  axis.ticks.length = unit(10, "pt"),
  axis.ticks.x.bottom = element_line(),
  axis.ticks.y = element_blank(),
  text = element_text(family = "sans"),
  linetype = 1,
  linewidth = c(1, 1.25, 0.75, 1.5, 1, 1.5),
  legend.justification = "center",
  legend.position = "bottom",
  legend.title = element_text(size = 13),
  panel.background = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(
    color = colors$ETH_Grey$`20`,
    linewidth = 0.5
  ),
  panel.grid.minor = element_blank(),
  plot.title = element_text(
    size = 20,
    face = "bold"
  ),
  plot.subtitle = element_text(size = 13),
  plot.caption = element_text(),
  plot.tag = element_text(size = 13),
  sum_as_line = FALSE,
  sum_legend = "sum",
  sum_line_color = c(ETH_Petrol_100 = colors$ETH_Petrol$`100`),
  sum_line_linetype = 1,
  sum_line_linewidth = 3,
  axis_x_label_dt = 1,
  axis_x_tick_dt = 1,
  axis_x_pad = NULL,
  axis_x_date_ticks = "auto",
  auto_bottom_margin = FALSE,
  band_fill_color = c(
    ETH_Petrol = colors$ETH_Petrol$`100`,
    ETH_Petrol_60 = colors$ETH_Petrol$`60`,
    ETH_Petrol_40 = colors$ETH_Petrol$`40`,
    ETH_Petrol_20 = colors$ETH_Petrol$`20`,
    ETH_Purple = colors$ETH_Purple$`100`,
    ETH_Purple_60 = colors$ETH_Purple$`60`,
    ETH_Purple_40 = colors$ETH_Purple$`40`
  ),
  bar_border_color = "#000000",
  bar_border_linewidth = 0.1,
  bar_fill_color = c(
    ETH_Petrol = colors$ETH_Petrol$`100`,
    ETH_Petrol_60 = colors$ETH_Petrol$`60`,
    ETH_Petrol_40 = colors$ETH_Petrol$`40`,
    ETH_Petrol_20 = colors$ETH_Petrol$`20`,
    ETH_Purple = colors$ETH_Purple$`100`,
    ETH_Purple_60 = colors$ETH_Purple$`60`,
    ETH_Purple_40 = colors$ETH_Purple$`40`
  ),
  bar_gap = 0.1, # TODO: does not work with tsggplot
  #    bar_group_gap = 30,
  ci_alpha = "44",
  ci_colors = line_colors,
  ci_legend_label = "%ci_value%% ci for %series%",
  fill_up_start = FALSE,
  fill_year_with_nas = TRUE,
  grids_x_count = c(5, 6, 8, 10),
  grids_x_count_strict = FALSE,
  highlight_window = FALSE,
  highlight_window_alpha = 0.55,
  highlight_window_color = colors$ETH_Grey$`20`,
  highlight_window_end = NA,
  highlight_window_freq = 4,
  highlight_window_start = NA,
  legend_all_left = FALSE,
  line_colors = c(
    ETH_Green_60 = colors$ETH_Green$`60`,
    ETH_Green_100 = colors$ETH_Green$`100`,
    ETH_Petrol_20 = colors$ETH_Petrol$`20`,
    ETH_Purple_60 = colors$ETH_Purple$`60`,
    ETH_Petrol_60 = colors$ETH_Petrol$`60`,
    ETH_Purple_100 = colors$ETH_Purple$`100`,
    ETH_Petrol_100 = colors$ETH_Petrol$`100`
  ),
  line_to_middle = TRUE,
  NA_continue_line = FALSE,
  point_symbol = 1:18,
  pointsize = 12,
  preferred_y_gap_sizes = c(25, 20, 15, 10, 5, 2.5, 1, 0.5),
  quarterly_ticks = TRUE,
  range_must_not_cross_zero = TRUE,
  show_points = FALSE,
  y_range_min_size = NULL,
  y_tick_force_integers = FALSE,
  y_tick_margin = 0.15,
  yearly_ticks = TRUE,
  ...
) {
  args <- as.list(environment())[setdiff(names(formals()), "...")]
  dots <- list(...)
  theme <- c(args, dots)
  structure(theme, class = "tsggplot_theme")
}
