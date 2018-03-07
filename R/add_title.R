add_title <- function(plot_title, plot_subtitle, plot_subtitle_r, theme) {
  if(!is.null(plot_title)){
    if(!any(is.na(theme$title_transform))){
      plot_title <- do.call(theme$title_transform,
                            list(plot_title))
    } 
    title(main = plot_title, adj = theme$title_adj,
          line = theme$title_line,
          outer = theme$title_outer,
          cex.main = theme$title_cex.main)    
  }
  
  if(!is.null(plot_subtitle)){
    if(!is.null(theme$subtitle_transform)){
      plot_subtitle <- do.call(theme$subtitle_transform,
                               list(plot_subtitle))
    } 
    mtext(plot_subtitle, adj = theme$title_adj,
          line = theme$subtitle_line,
          outer = theme$subtitle_outer,
          cex = theme$subtitle_cex)    
  }
  
  
  if(!is.null(plot_subtitle_r)){
    if(!is.null(theme$subtitle_transform)){
      plot_subtitle_r <- do.call(theme$subtitle_transform,
                                 list(plot_subtitle_r))
    } 
    mtext(plot_subtitle_r,
          adj = theme$subtitle_adj_r,
          line = theme$subtitle_line,
          outer = theme$subtitle_outer,
          cex = theme$subtitle_cex)    
  }
}