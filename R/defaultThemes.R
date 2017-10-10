#' Initiate default theme
#' 
#' The \code{\link{tsplot}} methods provide a theme argument which is used to pass on a plethora of useful defaults. These defaults are essentially stored in a list. Sometimes the user may want to tweak some of these defaults while keeping most of them. 
#' Hence the initDefaultTheme function create a fresh list object containing default values for lot of different layout parameters etc. By replacing single elements of the list and passing the entire list to the plot function, single aspects can be tweaked while keeping most defaults. Init defaultTheme does not need any parameters. 
#' 
#' @details 
#' Themes are essentially list that contain \code{\link{par}} parameters. Below all items are listed, some of them with comments. I will try to write comments on all params soon. 
#' The list contains the following elements:
#' * margins               
#' * fillYearWithNAs logical: should a year be filled up with NAs if values do not go until the last period of the last year?       
#' * line_colors vector of RGB code strings defaults 
#' * line_to_middle        
#' * lwd integer vector of line width                   
#' * lty integer vector of line types, solid, dashed etc. see also \code{\link{par}}.                   
#' * xaxs                  
#' * yaxs                  
#' * bar_border RGB color code string of all bars borders
#' * total_bar_margin_pct share of space left for margins when grouping bar charts 
#' * bar_fill_color vector RGB color code strings denoting the fill of (stacked) bar chart elements
#' * sum_line_color        
#' * highlight_window logical should a highlight window be used for background?     
#' * highlight_window_start integer vector year,period
#' * highlight_window_end integer vector year,period
#' * highlight_color RGB string color, color of the highlight background window       
#' * use_box logical: should there be a box around the plot? defaults to FALSE.               
#' * y_las                 
#' * lwd_ticks_1           
#' * lwd_ticks_2           
#' * yearly_ticks logical: should there be yearly tick marks?         
#' * quarterly_ticks logical: should there be quarterly tick marks?       
#' * monthly_ticks logical: should there be monthly tick marks?         
#' * tcl_quarterly_tick_tcl
#' * tcl_yearly_tick       
#' * lwd_yearly_ticks      
#' * lwd_quarterly_ticks   
#' * label_pos             
#' * show_left_y_axis      
#' * show_right_y_axis     
#' * y_grid_count          
#' * show_y_grids          
#' * y_grid_color          
#' * legend_col            
#' * title_outer           
#' * title_adj             
#' * title_line            
#' * title_cex.main        
#' * subtitle_adj          
#' * subtitle_outer        
#' * subtitle_line         
#' * subtitle_cex.main     
#' * subtitle_transform    
#' * subtitle_adj_r        
#' 
#' @examples 
#' # create a list
#' data(KOF)
#' tt <- initDefaultTheme()
#' # adjust a single element
#' tt$highlight_window <- T
#' # pass the list to tsplot
#' tsplot(KOF$kofbarometer,theme = tt)
#' # for more theme examples check the vignette
#' vignette("tstools")
#' 
#' 
#' @md
#' @author Matthias Bannert
#' @export
initDefaultTheme <- function(){
  theme <- list()
  theme$margins <- c(5, 2.5, 3, 3) + 0.1
  theme$fillYearWithNAs <- TRUE
  theme$line_colors <- c("ETH_8_100" = "#007a92",
                         "ETH_4_100" = "#72791c",
                         "ETH_8_20" = "#cce5eb",
                         "ETH_5_60" = "#cc67a7",
                         "ETH_8_60" = "#66b0c2",
                         "ETH_5_100" = "#91056a",
                         "ETH_4_60" = "#a9af66")
  theme$line_to_middle <- T
  theme$lwd <- c(2,3,1,4,2,4)
  theme$lty <- 1
  theme$xaxs <- "i"
  theme$yaxs <- "i"
  theme$bar_border <- "#000000"
  theme$total_bar_margin_pct <- .2
  theme$bar_fill_color <- c(ETH8 = "#007A92",
                            ETH8_60 = "#66b0c2",
                            ETH8_30 = "#b3d7e0",
                            ETH8_20 = "#cce5eb",
                            ETH5 = "#91056a",
                            ETH5_60 = "#cc67a7",
                            ETH5_30 = "#e6b3d3")
  theme$sum_line_color <- "#1e1e1e"
  # Highlight window ############
  theme$highlight_window <- F
  theme$highlight_window_freq <- 4
  theme$highlight_window_start <- NA
  theme$highlight_window_end <- NA
  theme$highlight_color <- "#e9e9e9"
  theme$use_box <- F
  theme$y_las <- 2 
  # X AXIS ###############
  theme$lwd_ticks_1 <- 1.5
  theme$lwd_ticks_2 <- 1
  theme$yearly_ticks <- T
  theme$quarterly_ticks <- T
  theme$monthly_ticks <- F
  theme$tcl_quarterly_tick_tcl <- -.5
  theme$tcl_yearly_tick <- -.75
  theme$lwd_yearly_ticks <- 1.5
  theme$lwd_quarterly_ticks <- 1
  theme$label_pos <- "mid"
  # Y AXIS
  theme$show_left_y_axis <- T
  theme$show_right_y_axis <- T
  theme$y_grid_count <- c(5,6,8,10)
  theme$show_y_grids <- T
  theme$y_grid_color <- "#CCCCCC"
  # legend
  theme$legend_col <- 3
  # titles
  theme$title_outer <- T
  theme$title_adj <- 0
  theme$title_line <- .8
  theme$title_cex.main <- 1
  theme$title_transform <- NA
  theme$subtitle_adj <- 0
  theme$subtitle_outer <- T
  theme$subtitle_line <- -.6
  theme$subtitle_cex.main <- 1
  theme$subtitle_transform <- "toupper"
  theme$subtitle_adj_r <- .9
  theme
}

