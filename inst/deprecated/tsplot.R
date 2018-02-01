#' Basic Times Series Plot Method
#'
#' @param series object of class ts, or list of time series series.
#' @param ... objects of class ts
#' @param theme list holding extra style arguments to be passed to plot.
#' @param ygrid_dynamic logical, defaults to FALSE. Should ygrids be dynamic?
#' If not, the ygrid configuration of the theme is used. 
#' @param ygrid_factor numeric, defaults to 5. Factor by which the difference 
#' between the maximum and the minimum y value should be divided. This parameter
#' determines the number of horizontal grid lines when dynamic ygrid_dynamic is 
#' TRUE.
#' 
#' @export
tsplot <- function(series, ...,
                   theme = NULL,
                   plot.title = NULL,
                   plot.subtitle = NULL,
                   lgnd = NULL,
                   write_pdf = F,
                   crop_pdf = F,
                   cex_label = 0.65,
                   fname = NULL,
                   ygrid = T,
                   ygrid_factor = 4,
                   yaxis_factor,
                   theme_out,
                   print_x_axis,
                   print_y_axis = T,
                   print_y_right = F,
                   highlight_window = NULL,
                   manual_date_range = NULL,
                   manual_value_range = NULL,
                   auto_name = NULL) UseMethod("tsplot")


#' @rdname tsplot
#' @export
tsplot.ts <- function(series, ...,
                      theme = NULL,
                      plot.title = NULL,
                      plot.subtitle = NULL,
                      lgnd = NULL,
                      write_pdf = F,
                      crop_pdf = F,
                      cex_label = 0.65,
                      fname = NULL,
                      ygrid = T,
                      ygrid_factor = 4,
                      yaxis_factor = 20,
                      quarter_ticks = T,
                      theme_out = F,
                      print_x_axis = T,
                      print_y_axis = T,
                      print_y_right = F,
                      highlight_window = NULL,
                      manual_date_range = NULL,
                      manual_value_range = NULL,
                      auto_name = NULL){
  
  li <- list(...)
  
  # get the name of the first time series for 
  # file naming if no file name is specified.
  auto_nm <- deparse(substitute(series))
  
  # list of time series
  tl <- c(list(series),li)
  
  if(!all(unlist(lapply(li,is.ts)))) 
    stop("all list elements must be of class ts!")
  
  # basically pass it all on to the list method of tsplot
  tsplot(tl,theme = theme, 
         ygrid_factor = ygrid_factor,
         yaxis_factor = yaxis_factor,
         plot.title = plot.title,
         plot.subtitle = plot.subtitle,
         lgnd = lgnd,
         write_pdf = write_pdf,
         crop_pdf = crop_pdf,
         cex_label = cex_label,
         fname = fname,
         theme_out = theme_out,
         ygrid = ygrid,
         print_x_axis = print_x_axis,
         print_y_axis = print_y_axis,
         print_y_right = print_y_right,
         highlight_window = highlight_window,
         manual_date_range = manual_date_range,
         manual_value_range = manual_value_range,
         auto_name = auto_nm)  
  
}


#' @rdname tsplot
#' @export
tsplot.mts <- function(series,...,
                      theme = NULL,
                      plot.title = NULL,
                      plot.subtitle = NULL,
                      lgnd = NULL,
                      write_pdf = F,
                      crop_pdf = F,
                      cex_label = 0.65,
                      fname = NULL,
                      ygrid = T,
                      ygrid_factor = 4,
                      yaxis_factor = 20,
                      quarter_ticks = T,
                      theme_out = F,
                      print_x_axis = T,
                      print_y_axis = T,
                      print_y_right = F,
                      highlight_window = NULL,
                      manual_date_range = NULL,
                      manual_value_range = NULL,
                      auto_name = NULL){
  
  li <- list(...)
  
  # get the name of the first time series for 
  # file naming if no file name is specified.
  auto_nm <- deparse(substitute(series))
  
  # list of time series
  tl <- c(list(series),li)
  
  if(!all(unlist(lapply(li,is.ts)))) 
    stop("all list elements must be of class ts!")
  
  # basically pass it all on to the list method of tsplot
  tsplot(tl,theme = theme, 
         ygrid_factor = ygrid_factor,
         yaxis_factor = yaxis_factor,
         plot.title = plot.title,
         plot.subtitle = plot.subtitle,
         lgnd = lgnd,
         write_pdf = write_pdf,
         crop_pdf = crop_pdf,
         cex_label = cex_label,
         fname = fname,
         theme_out = theme_out,
         ygrid = ygrid,
         print_x_axis = print_x_axis,
         print_y_axis = print_y_axis,
         print_y_right = print_y_right,
         highlight_window = highlight_window,
         manual_date_range = manual_date_range,
         manual_value_range = manual_value_range,
         auto_name = auto_nm)  
  
}


#' @rdname tsplot
#' @export
tsplot.list <- function(series,
                        theme = NULL,
                        plot.title = NULL,
                        plot.subtitle = NULL,
                        manual_date_range = NULL,
                        manual_value_range = NULL,
                        highlight_window = NULL,
                        lgnd = NULL,
                        write_pdf = F,
                        crop_pdf = F,
                        fname = NULL,
                        cex_label = 0.65,
                        ygrid = T,
                        ygrid_factor = 5,
                        yaxis_factor = 20,
                        quarter_ticks = T,
                        theme_out = F,
                        print_x_axis = T,
                        print_y_axis = T,
                        print_y_right = F,
                        auto_name = NULL,
                        ...){
  # some sanity checks
  if(!all(unlist(lapply(series,is.ts))))
    stop("all elements of the list need to be objects of class ts.")
  
  if(is.null(theme)){
    theme <- initDefaultLineTheme()
  }
  
  
  # don't have default colors for more than 6 lines
  if(length(series) > 6) stop("This convenience plot function does not
                              support more than 6 series in one plot.
                              Don't use this theme / template in case
                              you need more, just use basic plotting
                              and build such a plot on your own.")
  
  # fill up year with NAs before the series are being passed 
  # on to other functions, but check whether freq is either 
  # quarterly or monthly, this makes quarterly ticks look 
  # better... 
  all_q_or_m <- all(sapply(series,function(x) frequency(x) %in% c(4,12)))
  if(all(theme$fillYearWithNA & all_q_or_m)){
    series <- lapply(series,fill_year_with_nas)
  }
  
  
  xy_range_left <- tsLinePlot(series,manual_date_range = manual_date_range,
                              manual_value_range = manual_value_range,
                              theme = theme)
  if(theme$print_x) addXAxis(xy_range_left, theme = theme)
  if(theme$print_y) {
    y_ticks <- addYAxis(xy_range_left,
                   y_grd_steps = theme$ygrid_steps,
                   manual_value_range = manual_value_range)
    if(theme$print_y_grid) addYGrids(y_ticks,theme = theme)
  }
  if(theme$box) box()
  }

