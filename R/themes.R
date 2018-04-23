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
#'
#' @param margins integer vector defaults to c(NA, 4, 3, 3) + 0.1. Set margins[1] to NA to automatically determine the bottom margin such that the legend fits (if either auto_legend or auto_bottom_margin are TRUE)
#' @param auto_bottom_margin logical Should the bottom margin be automatically calculated? This will be overridden if margins[1] is not NA. Default FALSE 
#' @param default_bottom_margin numeric The bottom margin to use when margins[1] is NA but neither auto_legend nor auto_bottom_margin are true. Default 3
#' @param fillYearWithNAs logical should year be filled up with missing in order to plot the entire year on the axis. Defaults to TRUE,
#' @param line_colors character vector of hex colors for 6 lines. 
#' @param line_to_middle logical try to put a line into the middle of the plot. defaults to TRUE.
#' @param lwd integer vector line width, defaults to c(2,3,1,4,2,4).
#' @param lty integer vector line type defaults to 1. 
#' @param show_points boolean Whether to draw the symbol specified by point_symbol at the data points. Multiple values can be supplied to enable/disable showing points for each individual series Default FALSE
#' @param point_symbol integer or character The symbol to use for marking data points. Multiple values can be supplied to set the symbol for each individual series See \code{pch} in \code{?par}. Default 1:18
#' @param NA_continue_line boolean If true, NA values in time series are ignored and a contonuous line is drawn. Multiple values to turn this behavior on/off for indivitual series are supported. Default FALSE
#' @param xaxs character axis defintion as in base plot, defaults to "i".
#' @param yaxs character axis defintion as in base plot, defaults to "i".
#' @param bar_border character hex colors for the border around bars in bar charts. 
#' @param total_bar_margin_pct numeric defintion as in base plot, defaults to "i", defaults to .2,
#' @param bar_fill_color character vector of hex colors for 6 time series. 
#' @param sum_as_line logical should the sum of stacked time series be displayed as a line on top of stacked bar charts. 
#' defaults to FALSE,
#' @param sum_legend character Label for the sum line, defaults to "sum". Set to NULL to not label the line at all.
#' @param sum_line_lty integer line type of sum_as_line, defaults to 1.
#' @param sum_line_lwd integer line width of sum_as_line, defaults to 3.
#' @param sum_line_color character hex color of of sum_as_line, defaults "#91056a".
#' @param highlight_window logical should a particular time span be highlighted by different background color. Defaults
#' to FALSE.
#' @param highlight_window_freq integer frequency of the higlight window defintion, defaults to 4. 
#' @param highlight_window_start integer vector highlight window start position, defaults to NA.
#' @param highlight_window_end integer vector highlight window start position, defaults to NA.,
#' @param highlight_color character hex color code of highlight background, defaults to "#e9e9e9".
#' @param use_box logical use a box around the plot.
#' @param y_las integer, same as base \code{\link{plot}} parameter defaults to 2.
#' @param yearly_ticks logical, should yearly ticks be shown. Defaults to TRUE.
#' @param quarterly_ticks logical, should quarterly ticks be shown. Defaults to TRUE.
#' @param monthly_ticks logical, should monthly ticks be shown. Defaults to FALSE.
#' @param lwd_yearly_ticks numeric, width of yearly ticks, defaults to 1.5.
#' @param lwd_quarterly_ticks numeric, width of yearly ticks, defaults to 1.
#' @param tcl_yearly_ticks numeric, length of yearly ticks. Analogous to \code{cex} for \code{\link{axis}}. defaults to -0.75.
#' @param tcl_quarterly_ticks numeric, length of quarterly ticks. See tcl_yearly_ticks, defaults to -0.4
#' @param label_pos character, denotes where the x-axis label is at. defaults to "mid", alternative value: "start".
#' @param show_left_y_axis logical: should left y axis be shown, defaults to TRUE.
#' @param show_right_y_axis logical: should left y axis be shown, defaults to TRUE.
#' @param y_grid_count integer vector preferred y grid counts c(5,6,8,10).
#' @param show_y_grids logical should y_grids by shown at all, defaults to TRUE.
#' @param y_grid_color character hex color of grids. Defaults to gray "#CCCCCC". 
#' @param y_grid_count_strict logical should we strictly stick to preferred y grid count? Defaults to FALSE. 
#' @param y_tick_margin numeric, minimal percentage of horizontal grid that needs to be clean, i.e., 
#' without lines or bars. Defaults to 0.15 (15 percent).
#' @param preferred_y_gap_sizes numeric c(25, 20, 15, 10, 5, 2.5, 1, 0.5),
#' @param y_range_min_size = NULL  ,
#' @param legend_col integer number of columns for the legend, defaults to 3.
#' @param legend_margin_top numeric Distance between bottom of plot and top of legends in inches, defaults to 0.45
#' @param title_outer logical, currently undocumented. Defaults to TRUE. 
#' @param title_adj numeric, same as base \code{\link{plot}} parameter, defaults to 0.
#' @param title_line numeric same as base \code{\link{plot}} parameter, defaults to .8.
#' @param title_cex.main numeric, same as base \code{\link{plot}} parameter defaults to 1
#' @param title_transform function to transform the title, defaults to NA.
#' @param subtitle_adj numeric same as base \code{\link{plot}} parameter, defaults to 0.
#' @param subtitle_outer numeric same as base \code{\link{plot}} parameter, defaults to TRUE
#' @param subtitle_line numeric same as base \code{\link{plot}} parameter, defaults to -.6
#' @param subtitle_transform function to transform the subtitle, defaults to "toupper",
#' @param subtitle_adj_r numeric same as base \code{\link{plot}} parameter, defaults to .9
#' @param subtitle_cex numeric same as base \code{\link{plot}} parameter, defaults to 1
#' @param legend_intersp_x numeric same as base \code{\link{plot}} parameter, defaults to 1
#' @param legend_intersp_y numeric same as base \code{\link{plot}} parameter, defaults to 1 
#' @param legend_font_size numeric passed on to the \code{cex} parameter of \code{\link{legend}}, defaults to 1
#' @param range_must_not_cross_zero logical automatic range finders are forced to do not find ranges below zero. Defaults to TRUE.
#' @examples 
#' # create a list
#' data(KOF)
#' tt <- init_tsplot_theme()
#' # adjust a single element
#' tt$highlight_window <- TRUE
#' # pass the list to tsplot
#' tsplot(KOF$kofbarometer,theme = tt)
#' # for more theme examples check the vignette
#' vignette("tstools")
#' 
#' 
#' @author Matthias Bannert
#' @export
init_tsplot_theme <- function(
  margins = c(NA, 4, 3, 3) + 0.1,
  auto_bottom_margin = FALSE,
  default_bottom_margin = 3,
  fillYearWithNAs = TRUE,
  line_colors = c("ETH_8_100" = "#a9af66",
                         "ETH_4_100" = "#72791c",
                         "ETH_8_20" = "#cce5eb",
                         "ETH_5_60" = "#cc67a7",
                         "ETH_8_60" = "#66b0c2",
                         "ETH_5_100" = "#91056a",
                         "ETH_4_60" = "#007a92"),
  line_to_middle = TRUE,
  lwd = c(2,3,1,4,2,4),
  lty = 1,
  show_points = FALSE,
  point_symbol = 1:18,
  NA_continue_line = FALSE,
  xaxs = "i",
  yaxs = "i",
  bar_border = "#000000",
  total_bar_margin_pct = .2,
  bar_fill_color = c(ETH8 = "#007A92",
                            ETH8_60 = "#66b0c2",
                            ETH8_30 = "#b3d7e0",
                            ETH8_20 = "#cce5eb",
                            ETH5 = "#91056a",
                            ETH5_60 = "#cc67a7",
                            ETH5_30 = "#e6b3d3"),
  sum_as_line = FALSE,
  sum_legend = "sum",
  sum_line_lty = 1,
  sum_line_lwd = 3,
  sum_line_color = c("ETH_8_100" = "#007a92",
                         "ETH_4_100" = "#72791c",
                         "ETH_8_20" = "#cce5eb",
                         "ETH_5_60" = "#cc67a7",
                         "ETH_8_60" = "#66b0c2",
                         "ETH_5_100" = "#91056a",
                         "ETH_4_60" = "#a9af66"),
  highlight_window = FALSE,
  highlight_window_freq = 4,
  highlight_window_start = NA,
  highlight_window_end = NA,
  highlight_color = "#e9e9e9",
  use_box = FALSE,
  y_las = 2 ,
  yearly_ticks = TRUE,
  quarterly_ticks = TRUE,
  monthly_ticks = FALSE,
  lwd_yearly_ticks = 1.5,
  lwd_quarterly_ticks = 1,
  tcl_yearly_ticks = -0.75,
  tcl_quarterly_ticks = -0.4,
  label_pos = "mid",
  show_left_y_axis = TRUE,
  show_right_y_axis = TRUE,
  y_grid_count = c(5,6,8,10),
  show_y_grids = TRUE,
  y_grid_color = "#CCCCCC",
  y_grid_count_strict = FALSE,
  y_tick_margin = 0.15,
  preferred_y_gap_sizes = c(25, 20, 15, 10, 5, 2.5, 1, 0.5),
  y_range_min_size = NULL,
  legend_col = 1,
  legend_margin_top = 0.45,
  title_outer = FALSE,
  title_adj = 0,
  title_line = 1.8,
  title_cex.main = 1,
  title_transform = NA,
  subtitle_adj = 0,
  subtitle_outer = FALSE,
  subtitle_line = 0.5,
  subtitle_transform = "toupper",
  subtitle_adj_r = .9,
  subtitle_cex = 1,
  legend_intersp_x = 1,
  legend_intersp_y = 1,
  legend_font_size = 1,
  range_must_not_cross_zero = TRUE){
  e <- environment()
  li <- lapply(names(formals()),get,envir = e)
  names(li) <- names(formals())
  li
}
  

