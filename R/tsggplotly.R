#' Estimate which x-axis tick labels would overlap and blank them out
#'
#' \code{tsggplot()}'s \code{guide_axis(check.overlap = TRUE)} (see #13) only
#' thins labels visually at grid draw time -- it never touches the ggplot
#' object's scale data, so \code{plotly::ggplotly()} (which reads that data,
#' not the rendered grob) always bakes every tick label into an explicit
#' \code{tickvals}/\code{ticktext} array, which Plotly then renders
#' unconditionally regardless of overlap (see #15). This replicates the same
#' greedy left-to-right overlap check ggplot2 itself uses, based on actual
#' text metrics for the assumed render width, rather than a fixed skip-every-
#' Nth heuristic.
#'
#' @param tickvals numeric tick positions, in the same units as \code{x_range}
#' @param ticktext character tick labels, one per \code{tickvals}
#' @param x_range numeric length-2 vector, the axis range \code{tickvals} sits in
#' @param width_in numeric, assumed plot width in inches used only to decide
#'   which labels are far enough apart to both show -- Plotly widgets are
#'   still fully resizable/zoomable afterwards
#' @param fontsize_pt numeric tick label font size in points
#' @param pad_in numeric minimum gap left between adjacent kept labels, in inches
#' @importFrom grDevices dev.cur dev.off dev.set pdf
#' @importFrom grid convertWidth grobWidth textGrob gpar
#' @noRd
thin_plotly_x_labels <- function(tickvals, ticktext, x_range, width_in = 7,
                                 fontsize_pt = 11, pad_in = 0.08) {
  if (length(tickvals) < 2) {
    return(ticktext)
  }

  ord <- order(tickvals)
  vals <- tickvals[ord]
  txt <- ticktext[ord]

  pos_in <- (vals - x_range[1]) / diff(x_range) * width_in

  cur_dev <- grDevices::dev.cur()
  grDevices::pdf(NULL)
  on.exit({
    grDevices::dev.off()
    if (cur_dev > 1) grDevices::dev.set(cur_dev)
  })

  label_w_in <- vapply(txt, function(l) {
    if (!nzchar(l)) {
      return(0)
    }
    grid::convertWidth(
      grid::grobWidth(grid::textGrob(l, gp = grid::gpar(fontsize = fontsize_pt))),
      "in",
      valueOnly = TRUE
    )
  }, numeric(1))

  keep <- logical(length(txt))
  last_right <- -Inf
  for (i in seq_along(txt)) {
    left <- pos_in[i] - label_w_in[i] / 2
    if (i == 1 || left >= last_right + pad_in) {
      keep[i] <- TRUE
      last_right <- pos_in[i] + label_w_in[i] / 2
    }
  }
  txt[!keep] <- ""

  out <- character(length(txt))
  out[ord] <- txt
  out
}

#' Reconstruct real dates/datetimes from tsggplot()'s numeric x-axis positions
#'
#' \code{tsggplot()} plots x as a plain number for every frequency: days-
#' since-epoch for daily/weekly, seconds-since-epoch for hourly (the same
#' numbers \code{scale_x_date()}/\code{scale_x_datetime()} use internally),
#' decimal-year (\code{year + fraction_of_year}) for everything else (see
#' \code{getNumericTimeIndex()} in utils.R). \code{plotly::ggplotly()} always
#' flattens this to a plain "linear" axis regardless -- even the daily/
#' weekly/hourly cases lose their Date/POSIXct typing on conversion -- so
#' Plotly's auto zoom/tick logic sees bare numbers like 2020.5 with no
#' notion of dates. This inverts whichever encoding back to a real Date/
#' POSIXct object (kept as such, not a character string, so a trace's x
#' still reads as continuous rather than discrete data -- see the
#' populate_categorical_axes() comment at this function's call site) so the
#' x-axis can be declared \code{type = "date"} instead, letting Plotly's own
#' date-aware tick formatter take over when zooming.
#'
#' Exact for the daily/weekly (days-since-epoch) and hourly (seconds-since-
#' epoch) cases. For the decimal-year case, this is approximate to within
#' about a day (ts's \code{k/frequency} fractions and this day-of-year
#' fraction don't perfectly agree) and includes whatever line_to_middle
#' half-period shift was already baked into the plotted position -- both
#' irrelevant for picking sensible zoomed tick labels, which is all this is
#' used for.
#'
#' @param x numeric, tsggplot()'s plotted x positions
#' @param dominant_freq character, \code{meta$global_x$dominant_freq} --
#'   which numeric encoding \code{x} is in (see above)
#' @noRd
tsggplotly_numeric_x_to_date <- function(x, dominant_freq) {
  if (isTRUE(dominant_freq %in% c("daily", "weekly"))) {
    return(as.Date(floor(x), origin = "1970-01-01"))
  }
  if (isTRUE(dominant_freq == "hourly")) {
    return(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"))
  }
  year <- floor(x)
  frac <- x - year
  days_in_year <- ifelse(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0), 366, 365)
  as.Date(sprintf("%d-01-01", year)) + round(frac * days_in_year)
}

#' Recursively remap R/ggplot2 font family aliases to real CSS font stacks
#'
#' \code{plotly::ggplotly()} sets an explicit \code{family} on many
#' individual elements (axis titles, tick labels, ...) inherited straight
#' from the static plot's theme -- \code{"sans"}/\code{"serif"}/\code{"mono"}
#' aren't valid CSS, so those would otherwise keep falling back to whatever
#' the browser's own default is, even after setting a real font stack on the
#' top-level \code{layout$font}, which only covers elements that don't
#' already have their own explicit \code{family}.
#'
#' @param x a (possibly deeply nested) list, e.g. \code{p$x$layout}
#' @param aliases named character vector mapping alias -> real CSS font stack
#' @noRd
fix_font_family_aliases <- function(x, aliases) {
  if (!is.list(x)) {
    return(x)
  }
  if (is.character(x$family) && length(x$family) == 1 && x$family %in% names(aliases)) {
    x$family <- aliases[[x$family]]
  }
  for (i in seq_along(x)) {
    x[[i]] <- fix_font_family_aliases(x[[i]], aliases)
  }
  x
}

#' Convert ggplot2 time series object to plotly object
#'
#' @param p ggplot2 figure, as returned by \code{\link{tsggplot}}. If it has
#'   a \code{tsr} (right-axis) series with the default split left/right
#'   legend (a \code{ggnewscale}-based split, which can't be converted to
#'   plotly directly), the merged-legend equivalent \code{tsggplot()} built
#'   alongside it is converted instead, then its traces are re-split into
#'   two plotly legends by series name, so the interactive plot still shows
#'   the same two left-/right-axis groups as the static one.
#' @param ... additional arguments passed on to \code{\link[plotly]{ggplotly}}
#' @param x_tick_mode character, how to avoid overlapping x-axis tick labels
#'   in the interactive plot (see #13, #15). \code{"thin"} (the default)
#'   estimates which yearly labels would overlap at the given/assumed width
#'   and blanks them out, the same way the static \code{tsggplot()} plot
#'   does, while still drawing every tick mark. \code{"auto"} instead hands
#'   tick placement entirely to Plotly (\code{tickmode = "auto"}) on a real
#'   date-typed x-axis, so Plotly recomputes "nice", properly date-formatted
#'   tick positions dynamically as the plot is resized or zoomed (e.g.
#'   month/day labels once zoomed in, instead of decimal years like
#'   2020.5), at the cost of no longer aligning ticks to exact year starts.
#' @param axis_titles logical, show the axis titles set through
#'   \code{labs} (\code{x}, \code{y}, \code{y_right}) in the interactive
#'   plot? Defaults to \code{FALSE}, so the axes stay unnamed and the series
#'   are identified by the legend and the hover text.
#' @details If the theme's \code{text} font family is a single font name
#'   with no CSS fallback (e.g. \code{"Comic Sans MS"} rather than
#'   \code{"Comic Sans MS, cursive"}), this warns that the viewer's browser
#'   will silently substitute its own default font if that one isn't
#'   installed, rather than erroring -- give a full font stack to avoid it.
#'
#' @importFrom ggplot2 calc_element
#' @importFrom utils modifyList
#'
#' @export
tsggplotly <- function(p, ..., x_tick_mode = c("thin", "auto"), axis_titles = FALSE) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("tsggplotly() needs the 'plotly' package: install.packages(\"plotly\")",
      call. = FALSE
    )
  }
  x_tick_mode <- match.arg(x_tick_mode)
  dots <- list(...)
  meta <- attr(p, "tsggplot_meta")

  # tsggplotly() can't convert the ggnewscale-based split legend directly
  # (the split-off geom silently loses its data in plotly::ggplotly()) --
  # convert the merged-legend equivalent tsggplot() already built for this
  # instead, then re-split its traces into two plotly legends below (#16).
  # split_legend/legend_groups are captured before the swap: on
  # merged_legend_fallback itself, split_legend is always FALSE (that's the
  # point of it), but its legend_groups (same tsl/tsr either way) are what
  # the re-split needs.
  split_legend <- isTRUE(meta$split_legend)
  legend_groups <- meta$legend_groups
  if (split_legend && !is.null(meta$merged_legend_fallback)) {
    p <- meta$merged_legend_fallback
    meta <- attr(p, "tsggplot_meta")
  }

  # ggplot2/R's generic font family aliases ("sans", "serif", "mono") are
  # graphics-device names, not valid CSS -- a browser doesn't recognize
  # "sans" and silently falls back to its own default (often a serif font),
  # rather than matching the static plot's actual sans-serif look.
  css_family_aliases <- c(
    sans = "Arial, Helvetica, sans-serif",
    serif = "Times New Roman, Times, serif",
    mono = "Courier New, Courier, monospace"
  )
  to_css_family <- function(x) {
    if (is.character(x) && x %in% names(css_family_aliases)) css_family_aliases[[x]] else x
  }

  # A single font name with no CSS fallback stack (e.g. "Comic Sans MS"
  # instead of "Comic Sans MS, cursive") isn't inherently wrong, but if it
  # isn't installed on the viewer's system, the browser silently falls back
  # to its own default rather than erroring -- the same failure mode as the
  # "sans"/"serif"/"mono" aliases above, just for a font the theme names
  # explicitly instead of one of R's graphics-device generics. These are
  # common enough to be installed almost everywhere that warning about them
  # would just be noise.
  web_safe_fonts <- c(
    "Arial", "Helvetica", "Helvetica Neue", "Verdana", "Georgia", "Tahoma",
    "Times New Roman", "Times", "Courier New", "Courier", "Trebuchet MS",
    "Impact", "Segoe UI", "Calibri"
  )
  collect_risky_families <- function(x, acc = character(0)) {
    if (!is.list(x)) {
      return(acc)
    }
    fam <- x$family
    if (is.character(fam) && length(fam) == 1 && nzchar(fam) &&
      !grepl(",", fam, fixed = TRUE) && !(fam %in% web_safe_fonts)) {
      acc <- union(acc, fam)
    }
    for (i in seq_along(x)) {
      acc <- collect_risky_families(x[[i]], acc)
    }
    acc
  }

  text_family <- to_css_family(p$theme$text$family)
  if (is.null(text_family) || !nzchar(text_family)) {
    text_family <- css_family_aliases[["sans"]]
  }

  # Resolve a fill colour from a theme element (e.g. panel.background),
  # falling back to fully transparent when the theme leaves it unset --
  # matching this function's previous hardcoded default for plots that
  # don't customize it, while following the theme for ones that do.
  resolve_fill <- function(fill) {
    if (is.null(fill) || is.na(fill) || !nzchar(fill)) "rgba(0,0,0,0)" else fill
  }
  plot_bg <- resolve_fill(ggplot2::calc_element("plot.background", p$theme)$fill)
  panel_bg <- resolve_fill(ggplot2::calc_element("panel.background", p$theme)$fill)

  # ggplotly() sets its own axis line style per axis, inconsistently (e.g.
  # the left y-axis came out black instead of the theme's actual grey, and
  # the overlaid yaxis2 gets no line at all since it starts out undefined)
  # -- read the theme directly instead for all three axes. Captured here,
  # before p gets reassigned to the converted plotly object below -- a
  # closure referencing p$theme directly would instead see that later,
  # theme-less value once actually called. complete_theme() resolves
  # inherited defaults (e.g. axis.text's actual grey30-ish colour, which
  # isn't set explicitly anywhere in the theme) the same way ggplot2 does at
  # render time -- p$theme alone only holds the *overridden* elements, so
  # anything relying on inheritance would otherwise fall back to ggplot2's
  # internal placeholder defaults instead of the real rendered value.
  static_theme <- ggplot2::complete_theme(p$theme)
  resolve_axis_line <- function(theme_key) {
    el <- ggplot2::calc_element(theme_key, static_theme)
    if (is.null(el) || inherits(el, "element_blank")) {
      return(list(showline = FALSE))
    }
    list(
      showline = TRUE,
      linecolor = el$colour,
      # ggplot2 linewidth -> plotly's pixel-based line width
      linewidth = (if (is.null(el$linewidth)) 0.5 else as.numeric(el$linewidth)) * 96 / 72.27
    )
  }

  # A single named value, wrapped in a list only when non-NULL -- avoids
  # ever embedding a bare NULL as a nested list value (see the yaxis2$title
  # comment below for why that matters).
  maybe_list <- function(name, value) {
    if (is.null(value)) list() else setNames(list(value), name)
  }

  # Plotly colour fields want a CSS colour string; col2rgb()/rgba() handles
  # named colours, 6- and 8-digit hex alike, rather than assuming a format.
  to_plotly_color <- function(colour) {
    if (is.null(colour) || is.na(colour)) {
      return(NULL)
    }
    rgb <- grDevices::col2rgb(colour, alpha = TRUE)
    sprintf("rgba(%d,%d,%d,%s)", rgb[1], rgb[2], rgb[3], round(rgb[4] / 255, 3))
  }

  # ggplotly() derives tick label font size/colour by measuring the actual
  # rendered grobs, which is unreliable across different axis/guide setups --
  # e.g. it measured the (identically-themed, size = 13) x- and y-axis tick
  # labels as 17px and 12px respectively for the exact same plot. Read the
  # theme directly instead, the same way resolve_axis_line() does.
  resolve_text_font <- function(theme_key) {
    el <- ggplot2::calc_element(theme_key, static_theme)
    if (is.null(el) || inherits(el, "element_blank")) {
      return(NULL)
    }
    list(
      family = to_css_family(el$family),
      # ggplot2 font size (pt) -> plotly's pixel-based font size
      size = as.numeric(el$size) * 96 / 72.27,
      color = to_plotly_color(el$colour)
    )
  }

  legend_position <- p$theme$legend.position

  # Collect each series' custom hover text ("value: ...") from the line/point
  # layer data before conversion. It's a plain data column, not an aes()
  # mapping (mapping it would make ggplot2 warn "Ignoring unknown
  # aesthetics" on every ordinary, non-plotly tsggplot() call), so ggplotly()
  # has no way to pick it up on its own -- it's injected into the matching
  # trace by name below instead.
  text_by_series <- list()
  for (l in p$layers) {
    d <- l$data
    if (is.data.frame(d) && !is.null(d$text) && !is.null(d$series)) {
      for (s in levels(d$series)) {
        text_by_series[[s]] <- as.character(d$text[as.character(d$series) == s])
      }
    }
  }

  # ggplotly() ignores labs(subtitle) altogether, added back below
  subtitle <- p$labels$subtitle

  # ggplotly() turns the highlight window into a filled scatter trace, which
  # Plotly always draws above bar traces, so it would wash the bars out.
  # Taken out here, redrawn below as layout shapes placed below the traces,
  # like in the static plot.
  highlight <- NULL
  is_hl_layer <- vapply(p$layers, function(l) isTRUE(attr(l$data, "tsggplot_highlight")), logical(1))
  if (any(is_hl_layer)) {
    hl_layer <- p$layers[[which(is_hl_layer)[1]]]
    highlight <- list(
      data = hl_layer$data,
      fill = hl_layer$aes_params$fill,
      alpha = hl_layer$aes_params$alpha
    )
    p$layers <- p$layers[!is_hl_layer]
  }

  p <- plotly::ggplotly(p, ...)
  p$x$layout <- fix_font_family_aliases(p$x$layout, css_family_aliases)
  p$x$data <- fix_font_family_aliases(p$x$data, css_family_aliases)

  qt <- meta$quarterly_tick_marks
  if (!is.null(qt)) {
    # tsggplot()'s "mid" x-axis label positioning draws its quarterly ticks
    # as an actual geom_segment() layer (a real ggplot2 workaround, since
    # ggplot2's native minor-tick support only covers yearly breaks) rather
    # than a genuine axis element -- ggplotly() has no way to tell that
    # apart from real plotted data, so it converts it into an ordinary,
    # fully visible trace (with hover, in the data, at trace level).
    # Drop it here, then redraw the marks as plain shapes for "thin" mode
    # only (below, next to the yearly tick shapes) -- "auto" mode hands tick
    # placement to Plotly, whose own ticks these fixed positions don't line
    # up with, which is what made them look like stray, mixed-direction
    # ticks there. Identify the trace by the same y-range tsggplot.R built
    # it at (y_min to y_min + tick_h) -- real data essentially never has
    # every single point confined to that one, narrow, specific interval.
    # Also require an empty trace name -- this layer is added with
    # inherit.aes = FALSE and no colour/fill mapping, so it never gets one,
    # unlike every real series (tsggplot() always names each one, even
    # list elements left unnamed by the user get an auto-generated
    # "series_N" name) -- an extra safeguard against the unlikely case of
    # real data coincidentally falling entirely within that narrow interval.
    y_lo <- qt$y_range[1] - 1e-6
    y_hi <- qt$y_range[2] + 1e-6
    is_tick_mark_trace <- function(d) {
      y <- d$y[!is.na(d$y)]
      !nzchar(d$name %||% "") && length(y) > 0 && all(y >= y_lo & y <= y_hi)
    }
    is_tm <- vapply(p$x$data, is_tick_mark_trace, logical(1))
    # keep its line style so the redrawn marks (thin mode) look the same
    qt_line <- if (any(is_tm)) p$x$data[[which(is_tm)[1]]]$line else NULL
    p$x$data <- p$x$data[!is_tm]
  }

  xa <- p$x$layout$xaxis
  if (identical(xa$tickmode, "array") && length(xa$tickvals) > 1) {
    if (x_tick_mode == "auto") {
      p$x$layout$xaxis$tickmode <- "auto"
      p$x$layout$xaxis$tickvals <- NULL
      p$x$layout$xaxis$ticktext <- NULL
      p$x$layout$xaxis$categoryorder <- NULL
      p$x$layout$xaxis$categoryarray <- NULL

      # Give Plotly real dates instead of tsggplot()'s plain numeric x (see
      # tsggplotly_numeric_x_to_date()), so its own zoom-aware tick
      # formatter can show month/day labels once zoomed in, rather than
      # decimal years like 2020.5 at every zoom level.
      dominant_freq <- meta$global_x$dominant_freq
      # format() rather than as.character(): POSIXct's as.character() drops
      # the time-of-day entirely for a vector where every element happens
      # to land exactly at midnight (an easy thing for a range's start/end
      # to do), silently reverting to a bare date and confusing Plotly's
      # date parser for what's still an hourly series.
      format_range <- function(d) if (inherits(d, "POSIXct")) format(d, "%Y-%m-%d %H:%M:%S") else as.character(d)
      if (!identical(xa$type, "date")) {
        p$x$layout$xaxis$type <- "date"
        if (!is.null(xa$range)) {
          p$x$layout$xaxis$range <- format_range(tsggplotly_numeric_x_to_date(xa$range, dominant_freq))
        }
        for (i in seq_along(p$x$data)) {
          trace_x <- p$x$data[[i]]$x
          if (is.numeric(trace_x)) {
            # Left as a real Date/POSIXct object, not a character string --
            # plotly_build()'s populate_categorical_axes() classifies any
            # character trace x as discrete data (is.discrete() treats
            # is.character() as true), silently re-populating
            # categoryorder/categoryarray with every one of its (possibly
            # hundreds of) unique values even though tickmode is "auto" and
            # tickvals/ticktext were just cleared above -- keeping the
            # actual Date/POSIXct class avoids that path entirely, and
            # Plotly's own JSON serialization already renders it correctly.
            p$x$data[[i]]$x <- tsggplotly_numeric_x_to_date(trace_x, dominant_freq)
          }
        }
      }
    } else {
      width_px <- if (is.null(dots$width)) 700 else dots$width
      fontsize_pt <- if (is.null(xa$tickfont$size)) 11 else xa$tickfont$size
      p$x$layout$xaxis$ticktext <- thin_plotly_x_labels(
        xa$tickvals, xa$ticktext, xa$range,
        width_in = width_px / 96,
        fontsize_pt = fontsize_pt
      )

      # tickvals/ticktext sit at mid_pts (label positions, centered within
      # each year -- see the Global X-Axis section of tsggplot.R) rather
      # than the true year-start positions, which only exist as the
      # static plot's separate minor-tick axis (guide_axis(minor.ticks =
      # TRUE)) -- something ggplotly() has no concept of and drops
      # entirely. Since this plot draws no gridlines/ticks of its own, and
      # thin_plotly_x_labels() blanks most labels for longer series, a mark
      # is drawn at each real year-start position (meta$global_x$
      # yearly_tick_pos) as a manual shape instead, matching what the
      # static plot's minor ticks show regardless of label thinning.
      tick_shape <- function(xv, height, line) {
        list(
          type = "line",
          xref = "x", yref = "y domain",
          x0 = xv, x1 = xv, y0 = 0, y1 = height,
          line = line
        )
      }
      in_range <- function(x) x[x >= xa$range[1] & x <= xa$range[2]]
      tick_shapes <- list()
      yearly_x <- meta$global_x$yearly_tick_pos
      if (!is.null(yearly_x)) {
        tick_shapes <- lapply(
          in_range(as.numeric(yearly_x)), tick_shape,
          height = 0.015,
          line = list(color = xa$tickcolor, width = xa$tickwidth)
        )
      }
      # The small quarterly marks between the yearly ones (dropped as a
      # trace above) -- redrawn as shapes too, in the style the static
      # plot's own layer had, so thin mode keeps its year-and-quarter
      # ticks without a stray, hoverable data trace behind them.
      if (!is.null(qt)) {
        quarterly_shapes <- lapply(
          in_range(as.numeric(qt$x)), tick_shape,
          height = qt$height_frac,
          line = if (is.null(qt_line)) list(color = xa$tickcolor, width = xa$tickwidth) else qt_line
        )
        tick_shapes <- c(tick_shapes, quarterly_shapes)
      }
      # plotly::layout()'s later merge (see layout_args below) only queues
      # changes into p$x$layoutAttrs, applied at build/print time via a
      # per-field merge that mishandles a plain (unnamed) list like this
      # one -- direct assignment here, like the ticktext mutation above,
      # actually sticks.
      if (length(tick_shapes) > 0) p$x$layout$shapes <- tick_shapes
    }
  }

  for (i in seq_along(p$x$data)) {
    nm <- p$x$data[[i]]$name
    txt <- text_by_series[[nm]]
    if (!is.null(txt) && length(txt) == length(p$x$data[[i]]$x)) {
      p$x$data[[i]]$text <- txt
      p$x$data[[i]]$hoverinfo <- "text"
    }
  }

  layout_args <- list(
    p = p,
    paper_bgcolor = plot_bg,
    plot_bgcolor = panel_bg,
    font = resolve_text_font("text"),
    title = maybe_list("font", resolve_text_font("plot.title")),
    hoverlabel = maybe_list("font", resolve_text_font("text")),
    # ggplotly() already derives orientation/x/y/xanchor for "bottom",
    # "top", "left" and "right" straight from theme$legend.position, so
    # those are left alone here; only "none" needs a manual assist, since
    # ggplotly() doesn't act on it (the legend and its trace entries stay
    # visible otherwise).
    showlegend = !identical(legend_position, "none"),
    legend = c(maybe_list("font", resolve_text_font("legend.text")), list(title = list(text = "")))
  )

  p <- do.call(plotly::layout, layout_args)

  if (!is.null(highlight) && nrow(highlight$data) > 0) {
    to_x <- function(v) {
      if (identical(p$x$layout$xaxis$type, "date")) {
        d <- if (is.numeric(v)) tsggplotly_numeric_x_to_date(v, meta$global_x$dominant_freq) else v
        if (inherits(d, "POSIXct")) format(d, "%Y-%m-%d %H:%M:%S") else as.character(d)
      } else {
        as.numeric(v)
      }
    }
    hl_rgb <- grDevices::col2rgb(highlight$fill)
    hl_fill <- sprintf(
      "rgba(%d,%d,%d,%s)", hl_rgb[1], hl_rgb[2], hl_rgb[3],
      if (is.null(highlight$alpha)) 1 else highlight$alpha
    )
    highlight_shapes <- lapply(seq_len(nrow(highlight$data)), function(i) {
      list(
        type = "rect", layer = "below",
        xref = "x", x0 = to_x(highlight$data$xmin[i]), x1 = to_x(highlight$data$xmax[i]),
        yref = "paper", y0 = 0, y1 = 1,
        fillcolor = hl_fill, line = list(width = 0)
      )
    })
    p$x$layout$shapes <- c(p$x$layout$shapes, highlight_shapes)
  }

  # ggplotly() only makes room in the top margin for a one-line title, and
  # Plotly centres the title in that margin, so a multi-line title (a "\n" in
  # it, turned into <br /> by Plotly later on) or a subtitle would run into
  # the plot. The bundled plotly.js has no native subtitle either, so it
  # becomes a further line of the title, sized/coloured from plot.subtitle.
  # The title is pinned to the top of the figure and the top margin sized to
  # the lines it actually has. Assigned directly (like the axis styles
  # below), since layout() only queues its changes.
  title_text <- p$x$layout$title$text
  if (!is.null(title_text)) {
    # ggplotly() wraps a bold title as "<b> title </b>", and that leading
    # space indents just the first line
    title_text <- sub("\\s+(</[bi]>)$", "\\1", sub("^(<[bi]>)\\s+", "\\1", title_text))
    p$x$layout$title$text <- title_text
  }
  has_title <- !is.null(title_text) && nzchar(trimws(title_text))
  sub_font <- resolve_text_font("plot.subtitle")
  # resolve_text_font() is NULL when the theme hides the subtitle
  has_subtitle <- !is.null(subtitle) && nzchar(subtitle) && !is.null(sub_font)
  if (has_title || has_subtitle) {
    line_height <- 1.2
    # ggplotly()'s own top margin already holds one title line
    gg_title_size <- if (has_title && !is.null(p$x$layout$title$font$size)) p$x$layout$title$font$size else 0
    title_size <- resolve_text_font("plot.title")$size
    if (is.null(title_size)) title_size <- if (gg_title_size > 0) gg_title_size else 16
    sub_size <- if (has_subtitle && !is.null(sub_font$size)) sub_font$size else 16

    count_lines <- function(x) 1 + lengths(regmatches(x, gregexpr("<br\\s*/?>|\n", x)))
    n_title_lines <- if (has_title) count_lines(title_text) else 0
    n_sub_lines <- if (has_subtitle) count_lines(subtitle) else 0
    block_height <- (n_title_lines * title_size + n_sub_lines * sub_size) * line_height

    if (has_subtitle) {
      sub_style <- c(
        if (!is.null(sub_font$size)) sprintf("font-size:%gpx", sub_font$size),
        if (!is.null(sub_font$color)) sprintf("color:%s", sub_font$color)
      )
      # plain text in the static plot, so not to be read as HTML
      sub_text <- gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", subtitle, fixed = TRUE), fixed = TRUE), fixed = TRUE)
      sub_html <- sprintf("<span style=\"%s\">%s</span>", paste(sub_style, collapse = ";"), sub_text)
      p$x$layout$title$text <- if (has_title) paste0(title_text, "<br>", sub_html) else sub_html
    }
    p$x$layout$title$y <- 1
    p$x$layout$title$yref <- "container"
    p$x$layout$title$yanchor <- "top"
    # Plotly's "top" anchor sits above the first line's cap height
    title_pad <- 0.85 * (if (has_title) title_size else sub_size)
    p$x$layout$title$pad <- list(t = title_pad)
    p$x$layout$margin$t <- p$x$layout$margin$t - gg_title_size + block_height + title_pad
  }

  # Axis line style (showline/linecolor/linewidth) is set by direct
  # assignment, not via the layout_args/plotly::layout() call above --
  # plotly::layout() only queues changes into p$x$layoutAttrs, and its
  # later merge doesn't reliably override values ggplotly() already set on
  # xaxis/yaxis during conversion (e.g. it left the left y-axis black
  # instead of the theme's actual colour).
  #
  # Native tick marks are suppressed in "thin" mode because it draws its own
  # instead (the tick_shapes block above, at the true year-start positions
  # ggplotly() has no concept of) -- leaving Plotly's native ticks on too
  # would just double them up. "auto" mode draws no substitute of its own
  # and hands tick placement (and, on a date axis, tick density/hierarchy)
  # entirely to Plotly already, so it gets Plotly's own default tick
  # appearance too, rather than one derived from the static plot's theme.
  # That means not just switching "ticks" on, but also actually removing
  # ggplotly()'s own ticklen/tickwidth/tickcolor (inherited from the static
  # plot's *major* tick length, which is zeroed out by the "mid" label-
  # positioning hack) rather than leaving them at that stale 0 -- an
  # explicit 0 renders an invisible (zero-length) tick regardless of
  # "ticks", unlike the field being genuinely absent, which is what lets
  # Plotly's own non-zero default (5px) take over.
  xaxis_ticks <- if (x_tick_mode == "auto") {
    list(ticks = "outside", ticklen = NULL, tickwidth = NULL, tickcolor = NULL)
  } else {
    list(ticks = "")
  }
  p$x$layout$xaxis <- modifyList(p$x$layout$xaxis, c(xaxis_ticks, resolve_axis_line("axis.line.x")))
  p$x$layout$yaxis <- modifyList(p$x$layout$yaxis, resolve_axis_line("axis.line.y.left"))

  # Tick label font: same reasoning as the axis line style above -- read the
  # theme directly rather than trust ggplotly()'s own (unreliable) derived
  # size/colour. Axis title font: usually blank (tsggplot only turns titles
  # on when a label is actually supplied), so resolve_text_font() returning
  # NULL here is expected and harmless.
  p$x$layout$xaxis$tickfont <- resolve_text_font("axis.text.x")
  p$x$layout$yaxis$tickfont <- resolve_text_font("axis.text.y.left")
  p$x$layout$xaxis$title <- modifyList(p$x$layout$xaxis$title, list(font = resolve_text_font("axis.title.x")))
  p$x$layout$yaxis$title <- modifyList(p$x$layout$yaxis$title, list(font = resolve_text_font("axis.title.y")))
  if (!axis_titles) {
    p$x$layout$xaxis$title <- list(text = "")
    p$x$layout$yaxis$title <- list(text = "")
  }

  # The right-axis series are already rescaled into the left axis's numeric
  # range (the same trick ggplot2's sec_axis() relies on for a static plot),
  # so the traces don't need to move to a second y-axis -- overlaying a
  # cosmetic yaxis2 with the *true* right-axis range/ticks over the same
  # panel area reproduces the same dual-axis look plotly-side.
  if (!is.null(meta) && !is.null(meta$right_y)) {
    # A NULL embedded as a *nested* list value (e.g. list(text = NULL, font =
    # NULL)) isn't the same as a NULL at this list's own top level -- the
    # latter is what modifyList() below treats as "leave unset", but the
    # former survives into the merged yaxis2 as a real list of NULLs, which
    # later crashes plotly_build()'s schema validation. Build it with only
    # the keys that actually have a value instead.
    right_title <- if (is.null(meta$y_right_label) || !axis_titles) {
      NULL
    } else {
      c(list(text = meta$y_right_label), maybe_list("font", resolve_text_font("axis.title.y.right")))
    }
    p$x$layout$yaxis2 <- modifyList(
      if (is.null(p$x$layout$yaxis2)) list() else p$x$layout$yaxis2,
      c(
        list(
          title = right_title,
          overlaying = "y",
          side = "right",
          range = meta$right_y$y_range,
          tickvals = meta$right_y$y_ticks,
          tickfont = resolve_text_font("axis.text.y.right"),
          automargin = TRUE,
          # Plotly draws a reference line at 0 on every axis by default; the
          # static plot doesn't, and it's not meaningful here since 0 on this
          # axis has no special significance beyond being part of the range.
          zeroline = FALSE
        ),
        resolve_axis_line("axis.line.y.right")
      )
    )
  }

  if (!is.null(meta) && !is.null(meta$right_y)) {
    # Plotly.js won't actually render an axis with no trace bound to it --
    # ticks, axis line and title all silently disappear -- even though it's
    # fully defined in the layout above. Add one invisible point on yaxis2
    # so the overlaid secondary axis actually shows up.
    #
    # Its x position is taken from an existing trace, not
    # p$x$layout$xaxis$range[1] -- in "auto" mode that range has already
    # been reformatted to a plain date/datetime *string* (see above), which
    # doesn't match the real traces' actual Date/POSIXct x any more. Mixing
    # a character x on one trace with Date x on the others made Plotly warn
    # "Can't display both discrete & non-discrete data on same axis" and
    # print that warning right into any document that renders this plot.
    # Borrowing a real trace's x sidesteps needing to know or replicate
    # whichever type/format the x-axis currently uses.
    p <- plotly::add_trace(p,
      x = p$x$data[[1]]$x[1], y = mean(meta$right_y$y_range),
      yaxis = "y2", type = "scatter", mode = "markers",
      marker = list(opacity = 0), showlegend = FALSE,
      hoverinfo = "skip", inherit = FALSE
    )
  }

  # rename legend items
  for (i in seq_along(p$x$data)) {
    name <- p$x$data[[i]]$name

    if (!is.null(name)) {
      # Remove leading/trailing parentheses
      name <- gsub("^\\(|\\)$", "", name)

      # Remove trailing ", 1" or ",\\s*\\d+"
      name <- sub(",\\s*\\d+$", "", name)

      p$x$data[[i]]$name <- name
    }
  }

  if (split_legend && !is.null(legend_groups)) {
    # Re-split the merged legend built above into two plotly legends (#16),
    # by series name, matching the static plot's separate left-/right-axis
    # guide-boxes. plotly.js supports more than one legend per figure via a
    # trace's "legend" field ("legend"/"legend2"/...) plus a matching
    # layout$legend2 -- unlike ggplot2, there's no native fill-vs-colour
    # guide split to piggyback on, so traces are assigned to a legend by
    # matching their name (just cleaned up above) against tsl/tsr (+ the
    # sum line). Has to run after that cleanup: ggplotly() names a grouped
    # geom's trace (e.g. a bar series) "(a,1)", not the plain series name.
    is_right <- vapply(p$x$data, function(d) !is.null(d$name) && d$name %in% legend_groups$right, logical(1))
    for (i in which(is_right)) p$x$data[[i]]$legend <- "legend2"

    # Both boxes keep the single legend's derived position/style (always
    # "bottom" -- the only position tsggplot() ever uses for a split
    # legend, see guide_legend(position = "bottom") in tsggplot.R) and grow
    # outward from the plot's horizontal center, mirroring the static
    # plot's legend.spacing.x gap between its two guide-boxes. The empty
    # title set via layout_args above is only resolved into p$x$layout$legend
    # at build time (plotly::layout() just queues it into p$x$layoutAttrs,
    # like the axis line styling elsewhere in this function) -- legend2
    # doesn't exist yet at that point to receive the same reconciliation,
    # so it's set directly here.
    base_legend <- modifyList(p$x$layout$legend, list(title = list(text = "")))
    p$x$layout$legend <- modifyList(base_legend, list(x = 0.48, xanchor = "right"))
    p$x$layout$legend2 <- modifyList(base_legend, list(x = 0.52, xanchor = "left"))
  }

  risky_families <- collect_risky_families(p$x$layout)
  if (length(risky_families) > 0) {
    noun <- if (length(risky_families) > 1) "font families" else "font family"
    warning(
      "tsggplotly(): the theme uses ", noun, " ",
      paste(sprintf('"%s"', risky_families), collapse = ", "),
      " with no CSS fallback. If not installed in the viewer's browser, Plotly will ",
      "silently render in its own default font instead of erroring. Consider a full font ",
      "stack instead, e.g. theme(text = element_text(family = \"",
      risky_families[1], ", Arial, sans-serif\")).",
      call. = FALSE
    )
  }

  p
}
