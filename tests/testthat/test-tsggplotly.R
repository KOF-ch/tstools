skip_if_not_installed("plotly")

test_that("tsggplotly", {
  tsl <- list(AirPassengers = AirPassengers)
  tsg <- list(JohnsonJohnson = JohnsonJohnson)
  labs <- list(
    x = "Engine displacement (litres)",
    y = "Highway miles per gallon",
    y_right = "Medication per kilogram",
    title = "Air Passengers",
    subtitle = "In thousands"
  )
  theme <- init_tsggplot_theme(
    plot.title = element_text(size = 30, face = "bold", color = "red"),
    plot.subtitle = element_text(size = 20),
    plot.caption = element_text(size = 25),
    plot.tag = element_text(size = 15),
    # tsggplotly() can't convert the split (ggnewscale-based) legend, so
    # charts headed there need the merged legend (see tsggplotly()'s guard).
    legend_all_left = TRUE
  )
  expect_no_warning(p <- tsggplot(tsl, tsr = tsg, labs = labs, theme = theme))

  fig <- tsggplotly(p, axis_titles = TRUE)
  expect_s3_class(fig, "plotly")

  built <- plotly::plotly_build(fig)
  traces <- setNames(built$x$data, sapply(built$x$data, function(d) if (is.null(d$name)) "" else d$name))

  # the custom "value: ..." hover text (not ggplotly's raw default) is
  # present, and matches the true (un-rescaled) series values -- for the
  # secondary-axis series in particular, the trace's y data is rescaled
  # into the left axis's numeric range, so the hover text has to be
  # sourced from the original values instead
  expect_equal(traces[["AirPassengers"]]$hoverinfo, "text")
  expect_equal(
    traces[["AirPassengers"]]$text,
    paste("value:", as.numeric(AirPassengers)),
    ignore_attr = TRUE
  )
  expect_equal(traces[["JohnsonJohnson"]]$hoverinfo, "text")
  expect_equal(
    traces[["JohnsonJohnson"]]$text,
    paste("value:", as.numeric(JohnsonJohnson)),
    ignore_attr = TRUE
  )

  # traces stay one point per observation (text varying per point must not
  # fragment the line into disconnected single-point segments)
  expect_equal(length(traces[["AirPassengers"]]$x), length(AirPassengers))
  expect_equal(length(traces[["JohnsonJohnson"]]$x), length(JohnsonJohnson))

  # a real, non-guessed secondary axis, built from the true right-axis
  # range/ticks attached to the plot as tsggplot_meta
  meta <- attr(p, "tsggplot_meta")
  expect_equal(built$x$layout$yaxis2$title$text, labs$y_right)
  expect_equal(built$x$layout$yaxis2$range, meta$right_y$y_range)
  expect_equal(built$x$layout$yaxis2$tickvals, meta$right_y$y_ticks)
  expect_equal(built$x$layout$yaxis2$overlaying, "y")

  # Plotly.js doesn't actually render an axis with no trace bound to it
  # (ticks/line/title silently disappear), even though it's fully defined
  # in the layout -- an invisible point on yaxis2 keeps it visible
  yaxis2_traces <- Filter(function(d) identical(d$yaxis, "y2"), built$x$data)
  expect_equal(length(yaxis2_traces), 1)
  expect_equal(yaxis2_traces[[1]]$marker$opacity, 0)

  # the interactive plot's font follows the theme passed to tsggplot()
  # (mapped to a real CSS font stack -- "sans" alone isn't valid CSS and
  # browsers silently fall back to a serif font for it)
  expect_equal(built$x$layout$font$family, "Arial, Helvetica, sans-serif")
  expect_equal(built$x$layout$legend$orientation, "h")
})

test_that("tsggplotly", {
  theme <- init_tsggplot_theme(
    axis.line.y = element_line(colour = NA),
    axis.line.x = element_line(colour = NA),
    axis.text.x = element_text(hjust = 0, size = 10),
    axis.text.y.left = element_text(size = 10),
    axis.ticks.x.bottom = element_blank(),
    axis.minor.ticks.x.bottom = aes(colour = NA),
    legend.justification = "left",
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 15),
    grids_x_show = TRUE,
    axis_x_label_dt = 1,
    text = element_text(family = "sans")
  )
  labs <- list(
    title = "Air Passengers",
    subtitle = "In thousands"
  )
  p <- tsggplot(list("Prognose Frühjahr" = diff(log(AirPassengers)) * 100),
    tsr = list("Niveau, rechte Skala" = AirPassengers),
    left_as_bar = TRUE,
    theme = theme,
    labs = labs
  )
  t <- list(
    family = "Courier New, monospace",
    size = 14,
    color = "blue"
  )

  fig <- tsggplotly(p)
  fig <- plotly::layout(fig, font = t)
  fig <- plotly::layout(
    fig,
    font = t,
    xaxis = list(titlefont = t, tickfont = t),
    yaxis = list(titlefont = t, tickfont = t),
    legend = list(font = t),
    title = list(font = t)
  )

  expect_s3_class(fig, "plotly")

  built <- plotly::plotly_build(fig)
  expect_equal(built$x$layout$font$family, t$family)
  expect_equal(built$x$layout$xaxis$titlefont$family, t$family)
  expect_equal(built$x$layout$yaxis$titlefont$family, t$family)
  expect_equal(built$x$layout$legend$font$family, t$family)

  # left_as_bar + tsr (secondary axis) still produces a real, non-guessed
  # yaxis2 and doesn't error out
  meta <- attr(p, "tsggplot_meta")
  expect_false(is.null(meta$right_y))
  expect_equal(built$x$layout$yaxis2$range, meta$right_y$y_range)
})

test_that("tsggplotly x_tick_mode (#15)", {
  # 40 yearly labels won't all fit at the assumed default width -- this is
  # the same overlap guide_axis(check.overlap = TRUE) hides visually for
  # the static plot, which ggplotly() can't see (it reads the scale's
  # break/label data, not the rendered grob)
  long_ts <- ts(runif(40 * 12), start = c(1950, 1), frequency = 12)
  p <- tsggplot(list(A = long_ts))

  expect_equal(eval(formals(tsggplotly)$x_tick_mode), c("thin", "auto"))

  fig_thin <- tsggplotly(p)
  xa_thin <- fig_thin$x$layout$xaxis
  expect_equal(xa_thin$tickmode, "array")
  # tick marks/gridlines stay at every year...
  expect_equal(length(xa_thin$tickvals), 40)
  # ...but most of the labels text got blanked out to avoid overlap
  expect_true(sum(xa_thin$ticktext == "") > 20)
  expect_true(any(xa_thin$ticktext != ""))

  fig_auto <- tsggplotly(p, x_tick_mode = "auto")
  xa_auto <- fig_auto$x$layout$xaxis
  expect_equal(xa_auto$tickmode, "auto")
  expect_null(xa_auto$tickvals)
  expect_null(xa_auto$ticktext)

  # "auto" mode gives Plotly a real date-typed x-axis (instead of tsggplot's
  # plain decimal-year numbers) so its own zoom/tick logic can format ticks
  # as dates -- "thin" mode is untouched, still plain numeric.
  expect_equal(xa_auto$type, "date")
  expect_equal(xa_thin$type, "linear")
  built_auto_data <- plotly::plotly_build(fig_auto)$x$data[[1]]
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", built_auto_data$x)))
  expect_equal(as.integer(format(as.Date(built_auto_data$x[1]), "%Y")), 1950)

  # a short series with few yearly labels shouldn't lose any of them
  short_ts <- ts(runif(5 * 12), start = c(2010, 1), frequency = 12)
  p_short <- tsggplot(list(A = short_ts))
  fig_short <- tsggplotly(p_short)
  xa_short <- fig_short$x$layout$xaxis
  expect_true(all(xa_short$ticktext != ""))

  # tickvals/ticktext sit at mid_pts (label positions, centered within each
  # year), not the true year-start positions -- so blanked-or-not, tick
  # marks are drawn separately as shapes at the real year starts
  # (meta$global_x$yearly_tick_pos), matching the static plot's minor
  # ticks. Built explicitly (plotly::layout()'s own merge mishandles a
  # plain list like "shapes", so tsggplotly() assigns it directly).
  meta <- attr(p, "tsggplot_meta")
  built_thin <- plotly::plotly_build(fig_thin)
  shapes <- built_thin$x$layout$shapes
  is_yearly <- vapply(shapes, function(s) isTRUE(all.equal(s$y1, 0.015)), logical(1))
  shape_x <- function(sel) sort(vapply(shapes[sel], function(s) s$x0, numeric(1)))
  expect_equal(shape_x(is_yearly), sort(as.numeric(meta$global_x$yearly_tick_pos)))

  # ...and the small quarterly marks between them, which the static plot
  # draws as a geom_segment() layer that ggplotly() would otherwise turn
  # into a stray data trace (see the dedicated test below) -- redrawn as
  # shapes here so thin mode keeps them.
  expect_equal(shape_x(!is_yearly), sort(as.numeric(meta$quarterly_tick_marks$x)))

  # "auto" mode hands ticks to Plotly entirely -- no manual shapes needed
  built_auto <- plotly::plotly_build(fig_auto)
  expect_equal(length(built_auto$x$layout$shapes), 0)

  # "auto" mode's tick *marks* need to actually be automatic too, not just
  # the labels -- ticks were unconditionally suppressed (ticks = "") further
  # down, which made sense for "thin" mode (which draws its own instead, the
  # shapes checked above) but silently left "auto" with moving/reformatting
  # labels and no visible tick marks at all. Length/width/colour are left to
  # Plotly's own defaults rather than a static-theme-derived value -- "auto"
  # already hands tick placement (and, on a date axis, tick density) to
  # Plotly, so its native tick appearance is the appropriate match too.
  expect_equal(built_auto$x$layout$xaxis$ticks, "outside")
  expect_equal(built_thin$x$layout$xaxis$ticks, "")

  # ticklen/tickwidth/tickcolor must actually be absent (not merely left at
  # ggplotly()'s own derived value of 0, inherited from the static plot's
  # zeroed-out *major* tick length) -- an explicit 0 renders an invisible,
  # zero-length tick regardless of "ticks", unlike the field being
  # genuinely unset, which is what lets Plotly's own non-zero default apply.
  expect_null(built_auto$x$layout$xaxis$ticklen)
  expect_null(built_auto$x$layout$xaxis$tickwidth)
  expect_null(built_auto$x$layout$xaxis$tickcolor)

  # plotly_build()'s populate_categorical_axes() treats any *character*
  # trace x as discrete data and silently re-populates categoryorder/
  # categoryarray with every unique value (regardless of tickmode/type)
  # unless ticktext/tickvals are already set -- both are deliberately NULL
  # in "auto" mode, so this only stays off if the trace x itself is a real
  # Date/POSIXct object rather than a formatted date string.
  expect_null(built_auto$x$layout$xaxis$categoryorder)
  expect_null(built_auto$x$layout$xaxis$categoryarray)
  expect_s3_class(built_auto$x$data[[1]]$x, "Date")
})

test_that("tsggplotly x_tick_mode = 'auto' with a tsr series doesn't warn about mixed discrete/non-discrete axes", {
  # The invisible dummy point added to force yaxis2 to render (see the
  # add_trace() comment) used to take its x position straight from
  # p$x$layout$xaxis$range[1] -- fine normally, but "auto" mode has already
  # reformatted that range into a plain date/datetime *string* by this
  # point, while every real trace's x is a Date/POSIXct object. Plotly
  # warns "Can't display both discrete & non-discrete data on same axis"
  # (and prints that warning straight into any Rmd chunk that renders the
  # plot) whenever a character x sits next to a Date one like that.
  long_ts <- ts(runif(30), start = c(2000, 1), frequency = 1)
  p <- tsggplot(list(A = long_ts), tsr = list(B = long_ts + 1), labs = list(y_right = "right"))

  expect_no_warning(fig_auto <- tsggplotly(p, x_tick_mode = "auto"))
  expect_no_warning(built <- plotly::plotly_build(fig_auto))

  dummy_trace <- Filter(function(d) identical(d$yaxis, "y2"), built$x$data)[[1]]
  expect_s3_class(dummy_trace$x, "Date")
})

test_that("tsggplotly x_tick_mode = 'auto' reconstructs exact dates for daily/weekly series", {
  idx <- seq(as.Date("2020-01-01"), by = "day", length.out = 10)
  daily_xts <- xts::xts(seq_along(idx), order.by = idx)
  p <- tsggplot(list(A = daily_xts))

  fig_auto <- tsggplotly(p, x_tick_mode = "auto")
  built <- plotly::plotly_build(fig_auto)
  expect_equal(built$x$layout$xaxis$type, "date")
  # days-since-epoch encoding inverts exactly, no line_to_middle drift
  expect_equal(as.Date(built$x$data[[1]]$x), idx, ignore_attr = TRUE)
})

test_that("tsggplotly follows the theme's background and legend position", {
  long_ts <- ts(runif(30), start = c(2000, 1), frequency = 1)

  # defaults: transparent backgrounds, legend visible
  p_default <- tsggplot(list(A = long_ts))
  built_default <- plotly::plotly_build(tsggplotly(p_default))
  expect_equal(built_default$x$layout$paper_bgcolor, "rgba(0,0,0,0)")
  expect_equal(built_default$x$layout$plot_bgcolor, "rgba(0,0,0,0)")
  expect_true(built_default$x$layout$showlegend)

  # custom plot/panel background colours carry over
  p_bg <- tsggplot(list(A = long_ts),
    theme = init_tsggplot_theme(
      plot.background = ggplot2::element_rect(fill = "grey90"),
      panel.background = ggplot2::element_rect(fill = "white")
    )
  )
  built_bg <- plotly::plotly_build(tsggplotly(p_bg))
  expect_equal(built_bg$x$layout$paper_bgcolor, "grey90")
  expect_equal(built_bg$x$layout$plot_bgcolor, "white")

  # legend.position = "none" actually hides the legend (ggplotly() alone
  # does not act on this)
  p_none <- tsggplot(list(A = long_ts), theme = init_tsggplot_theme(legend.position = "none"))
  built_none <- plotly::plotly_build(tsggplotly(p_none))
  expect_false(built_none$x$layout$showlegend)

  # legend.position = "right" is left to ggplotly()'s own derivation
  # instead of forcing a hardcoded bottom-right position
  p_right <- tsggplot(list(A = long_ts), theme = init_tsggplot_theme(legend.position = "right"))
  built_right <- plotly::plotly_build(tsggplotly(p_right))
  expect_null(built_right$x$layout$legend$orientation)
})

test_that("tsggplotly x_tick_mode = 'auto' reconstructs real datetimes for hourly series", {
  hourly_idx <- seq(as.POSIXct("2023-01-01", tz = "UTC"), by = "hour", length.out = 5 * 24)
  hourly_xts <- xts::xts(seq_along(hourly_idx), order.by = hourly_idx)
  p <- tsggplot(list(A = hourly_xts), theme = init_tsggplot_theme(fill_year_with_nas = FALSE))

  fig_auto <- tsggplotly(p, x_tick_mode = "auto")
  built <- plotly::plotly_build(fig_auto)
  expect_equal(built$x$layout$xaxis$type, "date")
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$", built$x$data[[1]]$x)))
  expect_equal(format(as.Date(built$x$data[[1]]$x[1])), "2023-01-01")
  # kept as a real POSIXct, not a formatted string (see the
  # populate_categorical_axes() test above for why that matters)
  expect_s3_class(built$x$data[[1]]$x, "POSIXct")

  # the series starts exactly at midnight -- as.character() on a POSIXct
  # silently drops the time-of-day for a vector where every element lands
  # on one, which a 2-element range (start, end) can easily do even when
  # the full trace (with plenty of non-midnight hours) doesn't
  expect_equal(built$x$layout$xaxis$range[1], "2023-01-01 00:00:00")

  # "thin" mode isn't affected -- doesn't error and still produces the
  # correct (already date-formatted) tick labels
  fig_thin <- tsggplotly(p)
  built_thin <- plotly::plotly_build(fig_thin)
  expect_false(anyNA(built_thin$x$layout$xaxis$ticktext))
})

test_that("tsggplotly maps R's generic font family aliases to real CSS font stacks", {
  long_ts <- ts(runif(30), start = c(2000, 1), frequency = 1)

  for (alias in c("sans", "serif", "mono")) {
    p <- tsggplot(list(A = long_ts), theme = init_tsggplot_theme(text = ggplot2::element_text(family = alias)))
    built <- plotly::plotly_build(tsggplotly(p))
    expect_match(built$x$layout$font$family, "^[A-Za-z ]+(, [A-Za-z ]+)*, (sans-serif|serif|monospace)$")
    expect_false(identical(built$x$layout$font$family, alias))
  }

  # an already-real font name passes through unchanged
  p_custom <- tsggplot(list(A = long_ts), theme = init_tsggplot_theme(text = ggplot2::element_text(family = "Georgia")))
  built_custom <- plotly::plotly_build(tsggplotly(p_custom))
  expect_equal(built_custom$x$layout$font$family, "Georgia")
})

test_that("tsggplotly warns about a theme font with no CSS fallback stack", {
  long_ts <- ts(runif(30), start = c(2000, 1), frequency = 1)

  # a common, essentially-always-installed font: no warning
  p_safe <- tsggplot(list(A = long_ts), theme = init_tsggplot_theme(text = ggplot2::element_text(family = "Georgia")))
  expect_no_warning(tsggplotly(p_safe))

  # one of the R/ggplot2 aliases this function already maps to a full CSS
  # stack: no warning either
  p_alias <- tsggplot(list(A = long_ts), theme = init_tsggplot_theme(text = ggplot2::element_text(family = "sans")))
  expect_no_warning(tsggplotly(p_alias))

  # a font the user already gave a fallback stack for: no warning
  p_stack <- tsggplot(
    list(A = long_ts),
    theme = init_tsggplot_theme(text = ggplot2::element_text(family = "Comic Sans MS, cursive"))
  )
  expect_no_warning(tsggplotly(p_stack))

  # an obscure/decorative font with no fallback: warn, since the viewer's
  # browser will otherwise just silently substitute its own default instead
  # of the theme's actual intended font if it isn't installed
  p_risky <- tsggplot(
    list(A = long_ts),
    theme = init_tsggplot_theme(text = ggplot2::element_text(family = "Comic Sans MS"))
  )
  expect_warning(tsggplotly(p_risky), "Comic Sans MS")
})

test_that("tsggplotly fixes font aliases on individual elements too, not just the global default", {
  # plotly::ggplotly() sets its own explicit "sans" family on axis titles/
  # tick labels (inherited from the static theme), which otherwise
  # overrides the global layout$font default we set -- a real CSS font
  # stack has to reach these individually too.
  long_ts <- ts(runif(30), start = c(2000, 1), frequency = 1)
  p <- tsggplot(list(A = long_ts), tsr = list(B = long_ts + 1), labs = list(y = "left", y_right = "right"))
  built <- plotly::plotly_build(tsggplotly(p, axis_titles = TRUE))

  not_sans <- function(family) !identical(family, "sans") && grepl("sans-serif", family)
  expect_true(not_sans(built$x$layout$yaxis$title$font$family))
  expect_true(not_sans(built$x$layout$yaxis$tickfont$family))
  expect_true(not_sans(built$x$layout$xaxis$tickfont$family))
})

test_that("tsggplotly gives left, right and secondary axes matching, theme-derived line styling", {
  long_ts <- ts(runif(30), start = c(2000, 1), frequency = 1)
  theme <- init_tsggplot_theme(
    axis.line.x = ggplot2::element_line(colour = "#123456", linewidth = 2),
    axis.line.y.left = ggplot2::element_line(colour = "#123456", linewidth = 2),
    axis.line.y.right = ggplot2::element_line(colour = "#123456", linewidth = 2)
  )
  p <- tsggplot(list(A = long_ts), tsr = list(B = long_ts + 1), labs = list(y_right = "right"), theme = theme)
  built <- plotly::plotly_build(tsggplotly(p))

  for (ax in list(built$x$layout$xaxis, built$x$layout$yaxis, built$x$layout$yaxis2)) {
    expect_true(ax$showline)
    expect_equal(ax$linecolor, "#123456")
  }
  # all three got the same linewidth conversion, not just whichever one
  # ggplotly() happened to compute correctly on its own
  expect_equal(built$x$layout$xaxis$linewidth, built$x$layout$yaxis$linewidth)
  expect_equal(built$x$layout$yaxis$linewidth, built$x$layout$yaxis2$linewidth)

  # element_blank() hides the line instead of leaving some stale default
  p_blank <- tsggplot(
    list(A = long_ts),
    theme = init_tsggplot_theme(axis.line.y.left = ggplot2::element_blank())
  )
  built_blank <- plotly::plotly_build(tsggplotly(p_blank))
  expect_false(built_blank$x$layout$yaxis$showline)
})

test_that("tsggplotly gives left, right and secondary axes matching, theme-derived tick label fonts", {
  # ggplotly() derives tick label font size/colour by measuring rendered
  # grobs, which is unreliable across axes -- confirmed by comparing against
  # a plain vanilla ggplot2 plot, where left/right came out identical: for
  # this exact same (evenly-themed, size = 10 both sides) plot, it measured
  # the x-axis tick labels as ~50% bigger than the y-axis ones. Read the
  # theme directly instead, like the axis line/colour case above.
  long_ts <- ts(runif(30), start = c(2000, 1), frequency = 1)
  theme <- init_tsggplot_theme(
    axis.text.x = ggplot2::element_text(size = 10),
    axis.text.y.left = ggplot2::element_text(size = 10),
    axis.text.y.right = ggplot2::element_text(size = 10)
  )
  p <- tsggplot(list(A = long_ts), tsr = list(B = long_ts + 1), labs = list(y_right = "right"), theme = theme)
  built <- plotly::plotly_build(tsggplotly(p, axis_titles = TRUE))

  sizes <- vapply(
    list(built$x$layout$xaxis, built$x$layout$yaxis, built$x$layout$yaxis2),
    function(ax) ax$tickfont$size,
    numeric(1)
  )
  expect_equal(sizes[1], sizes[2])
  expect_equal(sizes[2], sizes[3])

  colors <- vapply(
    list(built$x$layout$xaxis, built$x$layout$yaxis, built$x$layout$yaxis2),
    function(ax) ax$tickfont$color,
    character(1)
  )
  expect_equal(colors[1], colors[2])
  expect_equal(colors[2], colors[3])

  # axis.text colour ("grey30"-ish by ggplot2 default) isn't set anywhere in
  # tsggplot's own theme, so this only comes out right if the *inherited*
  # default is resolved (e.g. via ggplot2::complete_theme()), not just the
  # theme's own explicitly-set elements.
  expect_equal(colors[1], "rgba(77,77,77,1)")

  # right-axis title text still carries a font (not silently dropped when
  # building it defensively around the "no right label" case)
  expect_false(is.null(built$x$layout$yaxis2$title$font))

  # a tsr plot with no right-axis label doesn't crash plotly_build() (this
  # used to error: "attempt to set an attribute on NULL", from embedding a
  # bare NULL as a nested list(text = NULL, font = NULL) value)
  p_no_label <- tsggplot(list(A = long_ts), tsr = list(B = long_ts + 1), theme = theme)
  expect_no_error(plotly::plotly_build(tsggplotly(p_no_label)))
})

test_that("tsggplotly reflects custom data-line and gridline styling from the theme", {
  # these aren't something tsggplotly() touches itself (ggplotly() converts
  # the geoms/panel grid natively), but worth locking in given the axis
  # line case above shows theme styling silently not making it through is
  # a real failure mode, not just a hypothetical one
  long_ts <- ts(runif(30), start = c(2000, 1), frequency = 1)
  theme <- init_tsggplot_theme(
    line_colors = c("#FF00FF"),
    linewidth = 4,
    panel.grid.major.y = ggplot2::element_line(colour = "#00FF00", linewidth = 3)
  )
  p <- tsggplot(list(A = long_ts), theme = theme)
  built <- plotly::plotly_build(tsggplotly(p))

  expect_equal(built$x$data[[1]]$line$color, "rgba(255,0,255,1)")
  expect_true(built$x$data[[1]]$line$width > 10) # default is ~2-4
  expect_equal(built$x$layout$yaxis$gridcolor, "rgba(0,255,0,1)")
  expect_true(built$x$layout$yaxis$gridwidth > 2) # default is < 1
})

test_that("tsggplotly turns the static plot's quarterly-tick layer into shapes (thin) or drops it (auto)", {
  # tsggplot()'s "mid" x-axis label positioning draws quarterly ticks as an
  # actual geom_segment() layer (a real ggplot2 workaround, since ggplot2's
  # native minor-tick support only covers yearly breaks), not a genuine
  # axis element. ggplotly() can't tell that apart from real plotted data,
  # so it converts it into an ordinary, hoverable data trace. In "thin"
  # mode those small marks between the yearly ticks are part of the
  # intended look, so they're redrawn as plain shapes (like the yearly
  # ticks already are); in "auto" mode Plotly places its own ticks, which
  # these fixed positions don't line up with -- they looked like stray,
  # mixed-direction ticks there -- so they're dropped entirely.
  long_ts <- ts(runif(80), start = c(1990, 1), frequency = 4)
  p <- tsggplot(list(A = long_ts), tsr = list(B = long_ts + 1), labs = list(y_right = "right"))
  meta <- attr(p, "tsggplot_meta")
  expect_length(meta$quarterly_tick_marks$y_range, 2)

  built <- list(
    thin = plotly::plotly_build(tsggplotly(p)),
    auto = plotly::plotly_build(tsggplotly(p, x_tick_mode = "auto"))
  )
  for (b in built) {
    names <- vapply(b$x$data, function(d) d$name %||% "", character(1))
    expect_setequal(names[nzchar(names)], c("A", "B"))
    # only the two real series plus the invisible yaxis2 marker -- no
    # leftover, unnamed tick-decoration trace, in either mode
    expect_equal(length(b$x$data), 3)
  }

  # thin: yearly + quarterly marks are both still there, as shapes -- the
  # quarterly ones match the static plot's own positions exactly
  q_shapes <- Filter(function(s) !isTRUE(all.equal(s$y1, 0.015)), built$thin$x$layout$shapes)
  expect_equal(
    sort(vapply(q_shapes, function(s) s$x0, numeric(1))),
    sort(as.numeric(meta$quarterly_tick_marks$x))
  )
  expect_true(all(vapply(q_shapes, function(s) s$y1, numeric(1)) > 0))

  # auto: nothing of the sort -- Plotly's own ticks only
  expect_equal(length(built$auto$x$layout$shapes), 0)

  # quarterly_ticks = FALSE never adds the layer in the first place --
  # nothing for tsggplotly() to find, remove or redraw
  p_no_ticks <- tsggplot(list(A = long_ts), theme = init_tsggplot_theme(quarterly_ticks = FALSE))
  expect_null(attr(p_no_ticks, "tsggplot_meta")$quarterly_tick_marks)
  built_no_ticks <- plotly::plotly_build(tsggplotly(p_no_ticks))
  expect_equal(length(built_no_ticks$x$data), 1)
})

test_that("tsggplotly converts stacked, grouped and sum_as_line bar charts", {
  tsb1 <- ts(runif(20, -30, 20), start = c(2010, 1), frequency = 4)
  tsb2 <- ts(runif(20, 0, 50), start = c(2010, 1), frequency = 4)
  tsb3 <- ts(runif(20, 0, 50), start = c(2010, 1), frequency = 4)

  p_stacked <- tsggplot(tsb1, tsb2, tsb3, left_as_bar = TRUE)
  expect_no_error(fig_stacked <- tsggplotly(p_stacked))
  built_stacked <- plotly::plotly_build(fig_stacked)
  bar_traces <- Filter(function(d) identical(d$type, "bar"), built_stacked$x$data)
  expect_equal(length(bar_traces), 3)
  expect_true(all(vapply(bar_traces, function(d) length(d$x) == length(tsb1), logical(1))))

  p_grouped <- tsggplot(tsb1, tsb2, tsb3, left_as_bar = TRUE, group_bar_chart = TRUE)
  expect_no_error(fig_grouped <- tsggplotly(p_grouped))
  built_grouped <- plotly::plotly_build(fig_grouped)
  bar_traces_grouped <- Filter(function(d) identical(d$type, "bar"), built_grouped$x$data)
  # dodged, not stacked: each series' bars sit at their own offset x
  # positions rather than all three sharing the same x
  x_by_trace <- lapply(bar_traces_grouped, function(d) d$x[1])
  expect_equal(length(unique(x_by_trace)), 3)

  p_sum <- tsggplot(list(tsb1, tsb2, tsb3), left_as_bar = TRUE, theme = init_tsggplot_theme(sum_as_line = TRUE))
  expect_no_error(fig_sum <- tsggplotly(p_sum))
  built_sum <- plotly::plotly_build(fig_sum)
  expect_equal(length(Filter(function(d) identical(d$type, "bar"), built_sum$x$data)), 3)
  expect_true(length(Filter(function(d) identical(d$type, "scatter"), built_sum$x$data)) >= 1)
})

test_that("tsggplotly keeps the subtitle as a styled second title line", {
  x <- ts(rnorm(20), start = c(2010, 1), frequency = 4)
  theme <- init_tsggplot_theme(
    plot.subtitle = ggplot2::element_text(size = 15, colour = "red")
  )

  built <- plotly::plotly_build(tsggplotly(
    tsggplot(list(A = x), labs = list(title = "Main", subtitle = "Sub"), theme = theme)
  ))
  expect_match(built$x$layout$title$text, "Main.*<br><span[^>]*color:rgba\\(255,0,0,1\\)[^>]*>Sub</span>")

  # extra top margin for the second line
  built_no_sub <- plotly::plotly_build(tsggplotly(
    tsggplot(list(A = x), labs = list(title = "Main"), theme = theme)
  ))
  expect_gt(built$x$layout$margin$t, built_no_sub$x$layout$margin$t)
  expect_false(grepl("<br>", built_no_sub$x$layout$title$text))

  # subtitle without a title
  built_only_sub <- plotly::plotly_build(tsggplotly(
    tsggplot(list(A = x), labs = list(subtitle = "Sub"), theme = theme)
  ))
  expect_match(built_only_sub$x$layout$title$text, "Sub</span>")
})

test_that("tsggplotly hides a subtitle the theme hides and escapes HTML in it", {
  x <- ts(rnorm(20), start = c(2010, 1), frequency = 4)

  theme_blank <- init_tsggplot_theme(plot.subtitle = ggplot2::element_blank())
  built <- plotly::plotly_build(tsggplotly(
    tsggplot(list(A = x), labs = list(title = "Main", subtitle = "Sub"), theme = theme_blank)
  ))
  expect_false(grepl("Sub", built$x$layout$title$text))

  built_esc <- plotly::plotly_build(tsggplotly(
    tsggplot(list(A = x), labs = list(title = "Main", subtitle = "a < b & c"))
  ))
  expect_match(built_esc$x$layout$title$text, "a &lt; b &amp; c</span>", fixed = TRUE)
})

test_that("tsggplotly only takes the highlight window out, not other rectangles", {
  x <- ts(rnorm(24), start = c(2018, 1), frequency = 4)
  theme <- init_tsggplot_theme(
    highlight_window = TRUE,
    highlight_window_start = c(2022, 1),
    highlight_window_end = c(2023, 4)
  )
  p <- tsggplot(list(A = x), theme = theme) +
    ggplot2::geom_rect(
      data = data.frame(xmin = 2019, xmax = 2020, ymin = -1, ymax = 1),
      ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax),
      inherit.aes = FALSE
    )
  built <- plotly::plotly_build(tsggplotly(p))
  expect_length(Filter(function(s) identical(s$type, "rect"), built$x$layout$shapes), 1)
  expect_true(any(vapply(built$x$data, function(d) identical(d$fill, "toself"), logical(1))))
})

test_that("tsggplotly draws the highlight window as a shape below the traces", {
  x <- ts(rnorm(24), start = c(2018, 1), frequency = 4)
  theme <- init_tsggplot_theme(
    highlight_window = TRUE,
    highlight_window_start = c(2022, 1),
    highlight_window_end = c(2023, 4),
    highlight_window_color = "red",
    highlight_window_alpha = 0.3
  )
  p <- tsggplot(list(A = x), left_as_bar = TRUE, theme = theme)

  for (mode in c("thin", "auto")) {
    built <- plotly::plotly_build(tsggplotly(p, x_tick_mode = mode))

    # no filled scatter trace for the window (it would be drawn above bars)
    expect_false(any(vapply(built$x$data, function(d) identical(d$fill, "toself"), logical(1))))
    expect_equal(length(Filter(function(d) identical(d$type, "bar"), built$x$data)), 1)

    shapes <- Filter(function(s) identical(s$layer, "below"), built$x$layout$shapes)
    expect_length(shapes, 1)
    expect_equal(shapes[[1]]$fillcolor, "rgba(255,0,0,0.3)")
    expect_equal(c(shapes[[1]]$y0, shapes[[1]]$y1), c(0, 1))
  }

  # numeric x in thin mode, dates in auto mode
  thin <- plotly::plotly_build(tsggplotly(p, x_tick_mode = "thin"))
  s_thin <- Filter(function(s) identical(s$layer, "below"), thin$x$layout$shapes)[[1]]
  expect_equal(s_thin$x0, 2022)
  expect_equal(s_thin$x1, 2024)
  auto <- plotly::plotly_build(tsggplotly(p, x_tick_mode = "auto"))
  s_auto <- Filter(function(s) identical(s$layer, "below"), auto$x$layout$shapes)[[1]]
  expect_match(as.character(s_auto$x0), "^2022-")
})

test_that("tsggplotly makes room in the top margin for every title line", {
  x <- ts(rnorm(20), start = c(2010, 1), frequency = 4)
  margin_t <- function(...) {
    built <- plotly::plotly_build(tsggplotly(tsggplot(list(A = x), labs = list(...))))
    built$x$layout$margin$t
  }

  one <- margin_t(title = "Main", subtitle = "Sub")
  two <- margin_t(title = "Main\nsecond line", subtitle = "Sub")
  expect_gt(two, one)

  # no padding spaces inside the <b> tag, which would indent only line one
  built_title <- plotly::plotly_build(tsggplotly(tsggplot(list(A = x), labs = list(title = "Main"))))
  expect_match(built_title$x$layout$title$text, "^<b>Main</b>$")

  # the title, not just the subtitle, needs the extra room
  expect_gt(margin_t(title = "Main\nsecond line"), margin_t(title = "Main"))

  # title pinned to the top so the block grows downwards into the margin
  built <- plotly::plotly_build(tsggplotly(tsggplot(list(A = x), labs = list(title = "Main", subtitle = "Sub"))))
  expect_equal(built$x$layout$title$yanchor, "top")
  expect_equal(built$x$layout$title$yref, "container")
})
