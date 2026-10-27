# =============================================================================
# distNum: "Comparison of Numeric Variables"
#
# Implementation of the analysis. The option, result and UI definitions live in
# jamovi/distNum.{a,r,u}.yaml; jamovi's compiler (jmvtools) turns the .a.yaml and
# .r.yaml into R/distNum.h.R (generated, do not edit), which defines the
# base classes `distNumOptions`, `distNumResults` and `distNumBase` used here.
#
# HOW AN ANALYSIS RUNS IN JAMOVI
#   1. `.run()` is called whenever an option or the data change. It must be quick:
#      it only prepares the data and hands it to each *enabled* image through
#      `image$setState()`. Nothing is drawn here.
#   2. jamovi then calls each image's render function (`.plot_<key>`, named in
#      distNum.r.yaml via `renderFun`). Those read the state back with
#      `image$state` and print a ggplot2 object to the graphics device that
#      jamovi has opened. Data are NOT available at render time, which is why
#      everything the plots need travels inside the state.
#
# HOW THE PLOTS ARE BUILT (read this before touching any `.p_*` function)
#   Every plot that has one variable per slot on a "variable axis" is drawn on a
#   fixed *canvas*: x = position of the variable (1, 2, 3, ...), y = the value.
#   Orientation is then handled in one place: for "horizontal" the canvas is
#   flipped with coord_flip(), so values run left to right and the variables
#   stack top to bottom. See `.context()` for how positions are assigned and
#   `.flip()` / `.flip_x()` for the flipping.
#   The variable axis is a *numeric* axis whose tick labels are the variable
#   names (`.cat_axis()`), not a discrete one; that is what lets the custom
#   polygon plots (raincloud, ridgeline) and the built-in geoms share one axis.
#
# NOTE ON OPTION NAMES
#   jmvcore's `Options` class already owns the names `theme` and `palette` (for
#   jamovi's global plot settings), so this analysis calls them `plotTheme` and
#   `colourPalette`. Do not rename them back: the global setting silently wins.
#
# Shared helpers (palettes, themes, summary statistics) are in R/utils.R.
# =============================================================================
distNumClass <- R6::R6Class(
    "distNumClass",
    inherit = distNumBase,
    private = list(

        # Plot key -> name of the checkbox option that switches it on. The key is
        # also the suffix of the result item (`plot_<key>`) and of the render
        # function (`.plot_<key>`), so all three stay in sync through this table.
        .switches = c(
            box = "showBox", violin = "showViolin", errorbar = "showErrorBar",
            raincloud = "showRaincloud", ridge = "showRidge", hist = "showHist",
            density = "showDensity", ecdf = "showEcdf", strip = "showStrip",
            barcode = "showBarcode"),

        # ---------------------------------------------------------------------
        # .run(): prepare data and state for every enabled plot (no drawing)
        # ---------------------------------------------------------------------
        .run = function() {
            opts <- self$options
            vars <- opts$vars
            # logical vector, one entry per plot key: is its checkbox ticked?
            on   <- vapply(private$.switches, function(k) isTRUE(opts[[k]]), logical(1))

            # `hint` is an Html result element that is only shown when helpful
            hint <- self$results$hint
            if (length(vars) > 0 && !any(on)) {
                hint$setContent("<p>Tick at least one plot under <b>Plots to show</b>.</p>")
                hint$setVisible(TRUE)
            } else {
                hint$setVisible(FALSE)
            }
            if (length(vars) == 0 || !any(on)) return()

            # Long format: one row per observation, columns `variable` and `value`.
            # Missing / non-finite values are dropped per variable, so variables
            # may have different n (it is shown in the caption).
            long <- do.call(rbind, lapply(vars, function(v) {
                data.frame(variable = v, value = as.numeric(self$data[[v]]),
                           stringsAsFactors = FALSE)
            }))
            long <- long[is.finite(long$value), , drop = FALSE]
            if (nrow(long) == 0) return()

            # Hand the data to each enabled image. Images that are not ticked get
            # no state and (via `visible:` in the .r.yaml) are not shown at all.
            for (key in names(private$.switches)[on]) {
                image <- self$results[[paste0("plot_", key)]]
                sz <- private$.size(key, length(vars))
                image$setSize(sz[1], sz[2])
                image$setState(list(long = long, vars = vars))
            }
        },

        # Plot size in pixels. With "Size plots automatically" the size grows with
        # the number of variables `n`; otherwise the user's width/height are used.
        # The numbers are empirical: they keep labels readable up to ~10 variables.
        .size = function(key, n) {
            o <- self$options
            if (!o$autoSize) return(c(o$plotWidth, o$plotHeight))
            horiz <- o$orientation == "horizontal"
            if (key %in% c("density", "ecdf")) return(c(700, 460))   # single panel
            if (key == "ridge")      # ridges need extra room because they overlap upwards
                return(if (horiz) c(700, max(300, 150 + 80 * n)) else c(max(450, 160 + 120 * n), 500))
            if (key == "hist")       # one facet per variable
                return(if (horiz) c(700, max(320, 100 + 115 * n)) else c(max(480, 110 + 190 * n), 440))
            # all other plots: one slot per variable on the variable axis
            if (horiz) c(700, max(250, 110 + 64 * n)) else c(max(430, 130 + 100 * n), 470)
        },

        # ---------------------------------------------------------------------
        # Render entry points. jamovi calls these by name (renderFun in the
        # .r.yaml); each just forwards to the shared `.render()` with its key.
        # ---------------------------------------------------------------------
        .plot_box       = function(image, ...) private$.render(image, "box"),
        .plot_violin    = function(image, ...) private$.render(image, "violin"),
        .plot_errorbar  = function(image, ...) private$.render(image, "errorbar"),
        .plot_raincloud = function(image, ...) private$.render(image, "raincloud"),
        .plot_ridge     = function(image, ...) private$.render(image, "ridge"),
        .plot_hist      = function(image, ...) private$.render(image, "hist"),
        .plot_density   = function(image, ...) private$.render(image, "density"),
        .plot_ecdf      = function(image, ...) private$.render(image, "ecdf"),
        .plot_strip     = function(image, ...) private$.render(image, "strip"),
        .plot_barcode   = function(image, ...) private$.render(image, "barcode"),

        # Read the state, build the ggplot, print it. Returning FALSE tells jamovi
        # that nothing was drawn; returning TRUE means the plot is on the device.
        # Warnings (e.g. "notch went outside hinges" with tiny n) are muted
        # because they would otherwise surface as noise in the results.
        .render = function(image, key) {
            st <- image$state
            if (is.null(st)) return(FALSE)
            p <- private$.build(key, st$long, st$vars)
            if (is.null(p)) return(FALSE)
            suppressWarnings(suppressMessages(print(p)))
            TRUE
        },

        # ---------------------------------------------------------------------
        # .context(): everything the plots share, computed once per render
        #   horiz : TRUE when values should run horizontally
        #   lv    : variable names in the order they are drawn (after inversion)
        #   pos   : named numeric vector, variable -> position on the variable axis.
        #           Horizontal: the first variable gets the HIGHEST position so
        #           that, after coord_flip(), it appears at the TOP of the plot.
        #           Vertical: the first variable is leftmost (position 1).
        #   cols  : named character vector, variable -> colour
        #   alpha, lw : fill opacity (0-1) and line width in ggplot units
        # ---------------------------------------------------------------------
        .context = function(vars) {
            o     <- self$options
            horiz <- identical(o$orientation, "horizontal")
            lv    <- if (o$invertVars) rev(vars) else vars
            n     <- length(lv)
            pos   <- if (horiz) rev(seq_len(n)) else seq_len(n)
            names(pos) <- lv
            # spread = TRUE drops the pale middle of diverging palettes (see utils.R)
            cols  <- cp_palette(o$colourPalette, n, o$reversePalette, spread = TRUE)
            names(cols) <- lv
            list(o = o, horiz = horiz, lv = lv, n = n, pos = pos, cols = cols,
                 alpha = o$alpha / 100, lw = cp_line_width(o$lineWidth))
        },

        # Build the ggplot for one plot key: attach `variable` (factor, drawing
        # order) and `pos` to the data, dispatch to the `.p_<key>` builder, then
        # apply the theme/labels common to all plots in `.finish()`.
        .build = function(key, long, vars) {
            ctx <- private$.context(vars)
            long$variable <- factor(long$variable, levels = ctx$lv)
            long$pos <- unname(ctx$pos[as.character(long$variable)])
            p <- switch(key,
                box       = private$.p_box(long, ctx),
                violin    = private$.p_violin(long, ctx),
                errorbar  = private$.p_errorbar(long, ctx),
                raincloud = private$.p_raincloud(long, ctx),
                ridge     = private$.p_ridge(long, ctx),
                hist      = private$.p_hist(long, ctx),
                density   = private$.p_density(long, ctx),
                ecdf      = private$.p_ecdf(long, ctx),
                strip     = private$.p_strip(long, ctx),
                barcode   = private$.p_barcode(long, ctx))
            if (is.null(p)) return(NULL)      # a builder may decline (e.g. no usable data)
            private$.finish(p, long, ctx, key)
        },

        # ---------------------------------------------------------------------
        # .finish(): colour scales, theme, title, caption, legend
        # Everything that must be identical across plots lives here. IMPORTANT:
        # a complete ggplot theme (theme_minimal() etc.) *replaces* any theme()
        # tweaks added before it, so all structural theme() calls are made here,
        # after the complete theme.
        # ---------------------------------------------------------------------
        .finish = function(p, long, ctx, key) {
            o <- ctx$o
            overlay <- key %in% c("density", "ecdf")   # variables share one panel -> need a legend
            facets  <- key == "hist"                   # one panel per variable -> strip labels
            p <- p +
                ggplot2::scale_fill_manual(values = ctx$cols, guide = if (overlay) "legend" else "none") +
                ggplot2::scale_colour_manual(values = ctx$cols, guide = if (overlay) "legend" else "none") +
                cp_theme(o$plotTheme, o$textSize) +
                ggplot2::theme(
                    legend.position  = if (overlay) o$legendPos else "none",
                    legend.title     = ggplot2::element_blank(),
                    plot.title       = ggplot2::element_text(face = "bold", hjust = 0.5),
                    panel.grid.minor = ggplot2::element_blank(),
                    plot.caption     = ggplot2::element_text(size = o$textSize * 0.65, colour = "grey45", hjust = 0),
                    plot.margin      = ggplot2::margin(10, 16, 8, 10))

            # histogram facets: horizontal strip text on the left, no boxes
            if (facets) {
                p <- p + ggplot2::theme(
                    strip.background = ggplot2::element_blank(),
                    panel.spacing    = ggplot2::unit(0.7, "lines"),
                    strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1))
            }

            # Grid lines only along the value axis, none along the variable axis.
            # After coord_flip() the theme's x/y elements refer to the screen axes.
            if (!overlay && !facets) {
                p <- p + ggplot2::theme(
                    panel.grid.major.x = if (ctx$horiz) ggplot2::element_line() else ggplot2::element_blank(),
                    panel.grid.major.y = if (ctx$horiz) ggplot2::element_blank() else ggplot2::element_line())
            }

            # Caption: n per variable (optional), plus a description of what the
            # error bars show for the means plot.
            n_per   <- table(long$variable)
            caption <- if (isTRUE(o$showN))
                paste(paste0(names(n_per), " (n=", n_per, ")"), collapse = "   ") else NULL
            if (key == "errorbar") {
                extra <- cp_interval_label(o$ebCenter, o$ebInterval, o$ebLevel, o$ebMult)
                caption <- paste(c(extra, caption), collapse = "\n")
            }
            p + ggplot2::labs(title = cp_title(o$title), caption = caption)
        },

        # ---------------------------------------------------------------------
        # Small building blocks used by several plots
        # ---------------------------------------------------------------------

        # The variable axis: numeric positions labelled with the variable names.
        # `upper` adds extra room at the high-position end (ridges/clouds extend
        # beyond the last variable's baseline).
        .cat_axis = function(ctx, upper = 0.6) {
            ggplot2::scale_x_continuous(
                breaks = unname(ctx$pos), labels = names(ctx$pos),
                expand = ggplot2::expansion(add = c(0.6, upper)))
        },

        # Axis titles for canvas plots: nothing on the variable axis, the user's
        # label on the value axis (ggplot2 keeps `y` for the value even when flipped).
        .value_axis = function(ctx) ggplot2::labs(x = NULL, y = ctx$o$valueLabel),

        # Canvas has variable on x and value on y. Flip when values should run
        # horizontally.
        .flip = function(p, ctx) if (ctx$horiz) p + ggplot2::coord_flip() else p,

        # Plots with the VALUE on x (density, ECDF): already horizontal, so flip
        # only for the vertical orientation.
        .flip_x = function(p, ctx) if (ctx$horiz) p else p + ggplot2::coord_flip(),

        # Jittered raw data points on the variable axis.
        #   colour = NULL -> coloured by variable; otherwise a fixed colour
        #   x             -> name of the column holding the x position (the raincloud
        #                    shifts its "rain" to the side by using a different column)
        # `seed = 1` makes the jitter identical on every render, so the plot does
        # not shimmer when other options change. `inherit.aes = FALSE` plus an
        # explicit `data` lets this layer be added to plots built on ggplot().
        .jitter = function(long, ctx, colour = NULL, alpha = 0.4, x = "pos") {
            o <- ctx$o
            # position_jitter's width is the half-width, hence the / 2
            pj <- ggplot2::position_jitter(width = o$jitterSpread / 100 / 2, height = 0, seed = 1)
            if (is.null(colour)) {
                ggplot2::geom_point(
                    data = long, mapping = ggplot2::aes(x = .data[[x]], y = value, colour = variable),
                    inherit.aes = FALSE, position = pj, size = o$pointSize, alpha = alpha, shape = 16)
            } else {
                ggplot2::geom_point(
                    data = long, mapping = ggplot2::aes(x = .data[[x]], y = value),
                    inherit.aes = FALSE, position = pj, size = o$pointSize, alpha = alpha,
                    colour = colour, shape = 16)
            }
        },

        # Kernel density of one variable, rescaled so its maximum is 1, as a data
        # frame of (v = value, d = relative height). Returns NULL when a density
        # cannot be estimated (fewer than 2 distinct values).
        #   adjust : bandwidth multiplier (the "Bandwidth adjust" option)
        #   trim   : TRUE restricts the curve to the observed range
        # Used by the raincloud and ridgeline, which draw the curve as a polygon.
        .dens = function(x, adjust, trim = FALSE, n = 256) {
            x <- x[is.finite(x)]
            if (length(x) < 2 || length(unique(x)) < 2) return(NULL)
            d <- if (trim) stats::density(x, adjust = adjust, n = n, from = min(x), to = max(x))
                 else stats::density(x, adjust = adjust, n = n)
            data.frame(v = d$x, d = d$y / max(d$y))
        },

        # ── Box plot ───────────────────────────────────────────────────────
        # geom_boxplot needs `group` because the x axis is numeric. When jittered
        # data are shown, the box's own outlier dots are hidden (they would be
        # drawn twice).
        .p_box = function(long, ctx) {
            o <- ctx$o
            p <- ggplot2::ggplot(long, ggplot2::aes(x = pos, y = value, group = pos,
                                                    fill = variable, colour = variable)) +
                ggplot2::geom_boxplot(
                    width = o$boxWidth / 100, alpha = ctx$alpha, linewidth = ctx$lw,
                    notch = o$boxNotch,
                    outlier.shape = if (o$boxOutliers && !o$boxJitter) 19 else NA,
                    outlier.alpha = 0.5, outlier.size = o$pointSize)
            if (o$boxJitter) p <- p + private$.jitter(long, ctx, colour = "grey25", alpha = 0.4)
            if (o$boxMean)   # white diamond at the mean
                p <- p + ggplot2::stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
                                               fill = "white", colour = "grey15", stroke = 0.8)
            p <- p + private$.cat_axis(ctx) + private$.value_axis(ctx)
            private$.flip(p, ctx)
        },

        # ── Violin plot ────────────────────────────────────────────────────
        # Optional narrow white box inside each violin ("Inner element").
        .p_violin = function(long, ctx) {
            o <- ctx$o
            p <- ggplot2::ggplot(long, ggplot2::aes(x = pos, y = value, group = pos,
                                                    fill = variable, colour = variable)) +
                ggplot2::geom_violin(
                    width = o$violinWidth / 100, scale = o$violinScale, trim = o$violinTrim,
                    adjust = o$violinAdjust, alpha = ctx$alpha, linewidth = ctx$lw * 0.6)
            if (o$violinInner == "box")
                p <- p + ggplot2::geom_boxplot(
                    width = 0.12, fill = "white", colour = "grey20", alpha = 0.9,
                    linewidth = ctx$lw * 0.6, outlier.shape = NA)
            if (o$violinJitter) p <- p + private$.jitter(long, ctx, colour = "grey25", alpha = 0.4)
            p <- p + private$.cat_axis(ctx) + private$.value_axis(ctx)
            private$.flip(p, ctx)
        },

        # ── Means (or medians) with error bars ─────────────────────────────
        # Summary statistics come from cp_summary() in utils.R (centre, lower,
        # upper per variable). The raw data can be drawn faintly behind.
        .p_errorbar = function(long, ctx) {
            o <- ctx$o
            s <- do.call(rbind, lapply(ctx$lv, function(v) {
                r <- cp_summary(long$value[long$variable == v], o$ebCenter, o$ebInterval,
                                 o$ebLevel, o$ebMult)
                data.frame(variable = v, pos = unname(ctx$pos[v]), centre = r[1], lo = r[2], hi = r[3])
            }))
            s$variable <- factor(s$variable, levels = ctx$lv)
            s <- s[order(s$pos), ]    # needed so the optional connecting line goes in order

            p <- ggplot2::ggplot()    # empty: every layer below brings its own data
            if (o$ebJitter) p <- p + private$.jitter(long, ctx, alpha = min(ctx$alpha * 0.5, 0.45))
            if (o$ebConnect)
                p <- p + ggplot2::geom_line(data = s, ggplot2::aes(x = pos, y = centre, group = 1),
                                            colour = "grey40", linewidth = ctx$lw * 0.7)
            p <- p +
                ggplot2::geom_errorbar(
                    data = s, ggplot2::aes(x = pos, ymin = lo, ymax = hi, colour = variable),
                    width = o$ebCap / 100, linewidth = ctx$lw * 1.5) +
                # shape 21 = circle with separate fill (variable colour) and outline
                ggplot2::geom_point(
                    data = s, ggplot2::aes(x = pos, y = centre, fill = variable),
                    shape = 21, size = o$ebSize, colour = "grey15", stroke = 0.8)
            if (o$ebLabels)   # value printed next to the marker; nudge_x is on the variable axis
                p <- p + ggplot2::geom_text(
                    data = s, ggplot2::aes(x = pos, y = centre, label = formatC(centre, format = "f", digits = 2)),
                    nudge_x = 0.3, size = o$textSize * 0.28, colour = "grey20")
            p <- p + private$.cat_axis(ctx) + private$.value_axis(ctx)
            private$.flip(p, ctx)
        },

        # ── Raincloud ──────────────────────────────────────────────────────
        # Three parts per variable, all in canvas coordinates:
        #   cloud: half density polygon on the high side of the variable's position
        #   box  : thin box at the position itself
        #   rain : jittered points on the low side (x shifted by -0.2)
        .p_raincloud = function(long, ctx) {
            o <- ctx$o
            h <- o$rcCloudWidth / 100    # height of the tallest cloud, in variable-axis units
            p <- ggplot2::ggplot()
            for (v in ctx$lv) {
                d <- private$.dens(long$value[long$variable == v], o$rcAdjust)
                if (is.null(d)) next
                base <- unname(ctx$pos[v]) + 0.08     # small gap between box and cloud
                # polygon outline: along the curve, then back along the baseline
                poly <- data.frame(x = c(base + h * d$d, rep(base, nrow(d))),
                                   y = c(d$v, rev(d$v)), variable = factor(v, levels = ctx$lv))
                p <- p + ggplot2::geom_polygon(
                    data = poly, ggplot2::aes(x = x, y = y, fill = variable, colour = variable),
                    alpha = ctx$alpha, linewidth = ctx$lw * 0.5)
            }
            if (o$rcRain) {
                long$xr <- long$pos - 0.2             # the "rain" sits beside the box
                p <- p + private$.jitter(long, ctx, alpha = min(ctx$alpha * 0.6, 0.55), x = "xr")
            }
            if (o$rcBox)
                p <- p + ggplot2::geom_boxplot(
                    data = long, ggplot2::aes(x = pos, y = value, group = pos),
                    inherit.aes = FALSE, width = 0.12, fill = "white", colour = "grey20",
                    alpha = 0.9, linewidth = ctx$lw * 0.6, outlier.shape = NA)
            p <- p + private$.cat_axis(ctx, upper = h + 0.2) + private$.value_axis(ctx)
            private$.flip(p, ctx)
        },

        # ── Ridgeline ──────────────────────────────────────────────────────
        # One density polygon per variable standing on its own baseline. Ridges
        # overlap (controlled by "Overlap"), and the ones LOWER on the plot must
        # cover the ones above, like hills seen from the front. In ggplot2 later
        # layers are drawn on top, so we add the layers from the highest position
        # to the lowest. Each ridge also gets an opaque white polygon underneath
        # its translucent coloured one; without it the ridges behind would show
        # through the transparency instead of being hidden.
        .p_ridge = function(long, ctx) {
            o <- ctx$o
            h <- 1 + o$ridgeOverlap / 100     # ridge height in variable-axis units (>1 = overlap)
            p <- ggplot2::ggplot()
            drawn <- 0
            for (v in ctx$lv[order(-ctx$pos[ctx$lv])]) {      # highest position first
                d <- private$.dens(long$value[long$variable == v], o$ridgeAdjust, o$ridgeTrim)
                if (is.null(d)) next
                drawn <- drawn + 1
                base <- unname(ctx$pos[v])
                poly <- data.frame(x = c(base + h * d$d, rep(base, nrow(d))),
                                   y = c(d$v, rev(d$v)), variable = factor(v, levels = ctx$lv))
                # 1) opaque underlay
                p <- p + ggplot2::geom_polygon(data = poly, ggplot2::aes(x = x, y = y),
                                               fill = "white", colour = NA)
                # 2) the visible, translucent ridge
                p <- p + ggplot2::geom_polygon(
                    data = poly, ggplot2::aes(x = x, y = y, fill = variable, colour = variable),
                    alpha = ctx$alpha, linewidth = ctx$lw * 0.6)
                if (o$ridgeMedian) {
                    # line from the baseline up to the curve at the median value
                    m  <- stats::median(long$value[long$variable == v])
                    hm <- stats::approx(d$v, d$d, xout = m, rule = 2)$y * h
                    seg <- data.frame(x = base, xend = base + hm, y = m, yend = m,
                                      variable = factor(v, levels = ctx$lv))
                    p <- p + ggplot2::geom_segment(
                        data = seg, ggplot2::aes(x = x, xend = xend, y = y, yend = yend, colour = variable),
                        linewidth = ctx$lw * 1.2)
                }
            }
            if (drawn == 0) return(NULL)     # no variable had enough distinct values
            p <- p + private$.cat_axis(ctx, upper = h + 0.15) + private$.value_axis(ctx)
            private$.flip(p, ctx)
        },

        # ── Histograms on a shared value axis ──────────────────────────────
        # Same bin width for every variable (range of ALL data / number of bins) so
        # the bars are comparable. One facet per variable: rows when horizontal,
        # columns (with coord_flip) when vertical. `after_stat()` picks the bar
        # height: counts, percent of the variable's own n, or density; for percent
        # we use density * binwidth * 100, which is already per variable.
        .p_hist = function(long, ctx) {
            o  <- ctx$o
            bw <- diff(range(long$value)) / o$histBins
            if (!is.finite(bw) || bw <= 0) return(NULL)
            ymap <- switch(o$histStat,
                count = ggplot2::aes(y = ggplot2::after_stat(count)),
                percent = ggplot2::aes(y = ggplot2::after_stat(density * bw * 100)),
                ggplot2::aes(y = ggplot2::after_stat(density)))
            p <- ggplot2::ggplot(long, ggplot2::aes(x = value, fill = variable)) +
                ggplot2::geom_histogram(ymap, binwidth = bw, boundary = min(long$value),
                                        colour = "white", linewidth = 0.15, alpha = ctx$alpha)
            if (o$histDensity) {
                # density curve rescaled to the same units as the bars
                dmap <- switch(o$histStat,
                    count = ggplot2::aes(y = ggplot2::after_stat(count * bw), colour = variable),
                    percent = ggplot2::aes(y = ggplot2::after_stat(density * bw * 100), colour = variable),
                    ggplot2::aes(y = ggplot2::after_stat(density), colour = variable))
                p <- p + ggplot2::stat_density(dmap, geom = "line", linewidth = ctx$lw, adjust = 1)
            }
            scales <- if (o$histFreeY) "free_y" else "fixed"
            if (ctx$horiz) {
                p <- p + ggplot2::facet_grid(rows = ggplot2::vars(variable), scales = scales, switch = "y")
            } else {
                p <- p + ggplot2::facet_grid(cols = ggplot2::vars(variable), scales = scales) +
                    ggplot2::coord_flip()
            }
            ylab <- switch(o$histStat, count = "Count", percent = "Percent", "Density")
            p + ggplot2::labs(x = o$valueLabel, y = ylab)
        },

        # ── Overlaid densities ─────────────────────────────────────────────
        # All variables in one panel; value on x, so use .flip_x().
        .p_density = function(long, ctx) {
            o <- ctx$o
            p <- ggplot2::ggplot(long, ggplot2::aes(x = value, fill = variable, colour = variable)) +
                ggplot2::geom_density(adjust = o$densAdjust, trim = o$densTrim,
                                      alpha = if (o$densFilled) ctx$alpha * 0.55 else 0,
                                      linewidth = ctx$lw) +
                ggplot2::labs(x = o$valueLabel, y = "Density")
            private$.flip_x(p, ctx)
        },

        # ── ECDF (cumulative distribution) ─────────────────────────────────
        # pad = FALSE: do not extend the step lines to +/- infinity.
        .p_ecdf = function(long, ctx) {
            o <- ctx$o
            p <- ggplot2::ggplot(long, ggplot2::aes(x = value, colour = variable))
            if (o$ecdfMedian)   # dashed guide at 50%: where each curve crosses is its median
                p <- p + ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey60", linewidth = 0.4)
            p <- p + ggplot2::stat_ecdf(geom = "step", linewidth = ctx$lw * 1.3, pad = FALSE) +
                ggplot2::scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
                ggplot2::labs(x = o$valueLabel, y = "Cumulative proportion")
            private$.flip_x(p, ctx)
        },

        # ── Strip (data points only) ───────────────────────────────────────
        # Optional short bar at the mean or median of each variable.
        .p_strip = function(long, ctx) {
            o <- ctx$o
            p <- ggplot2::ggplot() +
                private$.jitter(long, ctx, alpha = ctx$alpha)
            if (o$stripSummary != "none") {
                fun <- if (o$stripSummary == "mean") mean else stats::median
                s <- do.call(rbind, lapply(ctx$lv, function(v) {
                    data.frame(variable = factor(v, levels = ctx$lv), pos = unname(ctx$pos[v]),
                               y = fun(long$value[long$variable == v]))
                }))
                p <- p + ggplot2::geom_segment(
                    data = s, ggplot2::aes(x = pos - 0.35, xend = pos + 0.35, y = y, yend = y),
                    colour = "grey15", linewidth = ctx$lw * 1.6)
            }
            p <- p + private$.cat_axis(ctx) + private$.value_axis(ctx)
            private$.flip(p, ctx)
        },

        # ── Barcode ────────────────────────────────────────────────────────
        # One short tick per observation, across the width of the variable's slot.
        .p_barcode = function(long, ctx) {
            h <- ctx$o$barcodeHeight / 100 / 2     # half the tick length, in variable-axis units
            p <- ggplot2::ggplot(long) +
                ggplot2::geom_segment(
                    ggplot2::aes(x = pos - h, xend = pos + h, y = value, yend = value, colour = variable),
                    linewidth = ctx$lw * 0.6, alpha = min(ctx$alpha, 0.9)) +
                private$.cat_axis(ctx) + private$.value_axis(ctx)
            private$.flip(p, ctx)
        }
    )
)
