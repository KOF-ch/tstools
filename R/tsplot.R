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
                        highlight_window = NULL,
                        manual_date_range = NULL,
                        manual_value_range = NULL,
                        auto_name = NULL,
                        ...){
  # some sanity checks
  if(!all(unlist(lapply(series,is.ts))))
    stop("all elements of the list need to be objects of class ts.")
  
  if(is.null(theme)){
    theme <- initDefaultTheme()
  }
  
  # don't have default colors for more than 6 lines
  if(length(series) > 6) stop("This convenience plot function does not
                              support more than 6 series in one plot.
                              Don't use this theme / template in case
                              you need more, just use basic plotting
                              and build such a plot on your own.")
  if(theme$fillUpPeriod){
    series <- lapply(series,fillUpYearWithNAs)
  }
  
  # Determine time range (x-axis)
  # and value range (y-axis)# ############
  ts_time <- unique(unlist(lapply(series,time)))
  ts_time <- round(ts_time,digits = 5)
  date_range <- range(ts_time)
  # value range #######################
  value_range <- range(unlist(series),na.rm=T)
  value_range <- trunc(value_range)
  value_range <- c((floor(value_range[1]/10)-1)*10,
                   (ceiling(value_range[2]/10)+1)*10)
  
  # overwite automatically generated ranges #
  if(!is.null(manual_date_range)){
    date_range <- manual_date_range
    ts_time <- seq(from = date_range[1],
                   to = date_range[2],
                   by = .25)
  }
  
  if(!is.null(manual_value_range)){
    value_range <- manual_value_range 
  }
  
  if(write_pdf){
    if(is.null(fname)){
      fname <- auto_name
      pdf(paste0(fname,".pdf"),
          pointsize = theme$pointsize,
          height = theme$height,
          width = theme$width) # 7 is default, but any other number would do probably  
    } else {
      if(!is.character(fname)) stop("file name needs to be a character without file extension.")
      pdf(paste0(fname,".pdf"),
          pointsize = theme$pointsize,
          height = theme$height,
          width = theme$width) # 7 is default, but any other number would do probably  
    }  
  }
  
  # bottom,left,top,right
  par(mar = theme$mar)
  
  # Define First Plot Plot ###############
  plot(series[[1]],
       xlim = date_range,
       ylim = value_range,
       col = theme$line_colors[[1]],
       lwd = theme$lwd[1],
       lty = theme$lty[1],
       # a4_asp = (210 / 2) / (275 / 4),
       xlab = theme$xlab,
       ylab = theme$ylab,
       xaxs = theme$xaxs,
       yaxs = theme$yaxs,
       xaxt = "n",
       yaxt = "n",
       ...)
  
  # BACKGROUND (i.e. plot over these elements) START ################
  # print x-axis, always true in standalone plots
  if(print_x_axis) .addXAxis(quarter_ticks = quarter_ticks,
                             date_range = date_range,
                             theme = theme,
                             ts_time)
  
  # y-axis
  if(print_y_axis) .addYAxis(theme = theme,
                             ygrid = ygrid,
                             value_range = value_range,
                             ygrid_factor = ygrid_factor,
                             print_y_right = print_y_right)

  if(!is.null(highlight_window)){
    rect(highlight_window[1],
         value_range[1],
         highlight_window[2],
         value_range[2],
         col = theme$highlight_window_color,
         border = NA)
  } 
  
  # BACKGROUND (i.e. plot over these elements) END 
  
  # add multiple time series to the plot #####
  if(length(series) > 1){
    for (i in 2:length(series)){
      lines(series[[i]],
            col=theme$line_colors[[i]],
            lwd = ifelse(length(theme$lwd) > 1,
                         theme$lwd[i],
                         theme$lwd),
            lty = ifelse(length(theme$lty) > 1,
                         theme$lty[i],
                         theme$lty)
            )
    }
  }

  # LEGENDS, TITLE ####################
  # Add Title to plot 
  # title
  if(!is.null(plot.title)){
    title(main = plot.title, adj = theme$title_adj,
          line = theme$title_line,
          cex.main = theme$title_cex.main)  
  }
  
  # subtitle
  if(!is.null(plot.subtitle)){
    mtext(plot.subtitle, adj = theme$title_adj,
          line = theme$subtitle_line,
          cex = theme$subtitle_cex)  
  }
  
  if(theme_out) theme
  
  if(!is.null(lgnd)){
    legend("bottomleft",
           inset = theme$lgnd_inset,
           legend = lgnd,
           box.col = NA, 
           lty = theme$lty,
           lwd = theme$lwd,
           cex = theme$lgnd_cex_label,
           col = theme$line_colors,
           bty = theme$lgnd_bty, 
           xpd = theme$lgnd_xpd)
    
  }
  # EXPORT ####################################
  if(write_pdf) dev.off()
  if(crop_pdf & write_pdf) {
    if(Sys.which("pdfcrop") == "") cat("pdfcrop is not installed. To use this option, install pdfcrop if your on a 'Nix OS. If you're on Windows you're out of luck (anyway).") else {
      run_this <- sprintf("pdfcrop %s %s",paste0(fname,".pdf"),paste0(fname,".pdf"))
      system(run_this)
    }
  }
  ###################
}

