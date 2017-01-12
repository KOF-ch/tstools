#' @param ygrid_factor numeric to devide range by in order to determine y-grid. 
#' It's suggested to choose a factor that can be devided by 2, because it works
#' well with symmetric grids.
#' @author Caroline Siegenthaler, Matthias Bannert
#' @export
tsContributionChart <- function(li, show_sums_as_line = T,
                                theme = NULL,
                                sum_line_color = "#999999",
                                print_x_axis = T,
                                quarter_ticks = T,
                                print_y_axis = T,
                                print_y_right = F,
                                ygrid = T,
                                ygrid_factor = 4,
                                yaxis_factor = 20,
                                box = T,
                                symmetric_value_range = T,
                                manual_value_range = NULL,
                                manual_date_range = NULL){
  if(is.null(theme)){
    theme <- initDefaultTheme()
  }
  
  if(!all(unlist(lapply(li,is.ts))))
    stop("all elements of the list need to be objects of class ts.")
  
  if(theme$fillUpPeriod){
    li <- lapply(li,fillUpYearWithNAs)
  }
  
  if(inherits(li,"ts")){
    li <- as.list(li)
  }
  
  # matrix works well with pos / neg checks.
  tsmat <- do.call("cbind",li)
  # bottom, left, top, right margins defined via theme.
  par(mar = theme$mar)
  
  ts_time <- unique(unlist(lapply(li,time)))
  # floating problems when comparing stuff, set it to 
  # 5 digits ... 
  ts_time <- round(ts_time,digits = 5)
  date_range <- range(ts_time)
  
  if(!is.null(manual_date_range)){
    theme$xlim = manual_date_range
    ts_time <- seq(from = manual_date_range[1],
                   to = manual_date_range[2],by = .25)
    date_range <- manual_date_range
  }
  
  
  if(sum(tsmat < 0, na.rm = T) > 0){
    neg_0 <- tsmat
    pos_0 <- tsmat
    neg_0[tsmat < 0] <- 0
    pos_0[!tsmat < 0] <- 0
    value_range <- c(floor(ifelse(is.null(dim(pos_0)),
                                  min(pos_0, na.rm = T),
                                  min(rowSums(pos_0,
                                              na.rm = T)))
    ),
    ceiling(ifelse(is.null(dim(neg_0)),
                   max(neg_0, na.rm = T),
                   max(rowSums(neg_0, na.rm = T))
    )
    )
    )
    value_range <- round(((value_range/10)+c(-1,1))*10)
    
    if(!is.null(manual_value_range)) value_range <-
      manual_value_range
    
    if(symmetric_value_range){
      mx <- max(abs(value_range))
      mx <- ifelse(mx %% 2 == 0, mx, mx+1)
      value_range <- mx * c(-1,1)
    } 
    
    
    # plot the bars twice... otherwise grid is over the bars...
    pos_part <- barplot(t(neg_0),
                        ylim = value_range,
                        axes = F,
                        col = theme$line_colors)

    if(print_y_axis){
      .doYAxisWithHorizontalGrids(theme,
                                  value_range,
                                  ygrid_factor = ygrid_factor)
    }
    
    pos_part <- barplot(t(neg_0),
                        ylim = value_range,
                        axes = F,
                        col = theme$line_colors,add = T)

    neg_part <- barplot(t(pos_0),
                        ylim = value_range,
                        axes = F,
                        col = theme$line_colors,
                        add = T)

   if(print_x_axis) .addTsAxis(quarter_ticks = quarter_ticks,
                                ts_time = ts_time,
                                date_range = date_range,
                                at = pos_part,
                                theme = theme)
    }

  if(show_sums_as_line && !is.null(dim(tsmat))) {
    lines(x = pos_part,
          y = rowSums(tsmat),
          col = sum_line_color,
          lwd = theme$lwd,
          lty = theme$lty) 
  }
  if(box) box()
}
