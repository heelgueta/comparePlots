distNumClass <- R6::R6Class(
    "distNumClass",
    inherit = distNumBase,
    private = list(

        # plot key -> name of the option that switches it on
        .switches = c(
            box = "showBox", violin = "showViolin", errorbar = "showErrorBar",
            raincloud = "showRaincloud", ridge = "showRidge", hist = "showHist",
            density = "showDensity", ecdf = "showEcdf", strip = "showStrip",
            barcode = "showBarcode"),

        .run = function() {
            opts <- self$options
            vars <- opts$vars
            on   <- vapply(private$.switches, function(k) isTRUE(opts[[k]]), logical(1))

            hint <- self$results$hint
            if (length(vars) > 0 && !any(on)) {
                hint$setContent("<p>Tick at least one plot under <b>Plots to show</b>.</p>")
                hint$setVisible(TRUE)
            } else {
                hint$setVisible(FALSE)
            }
            if (length(vars) == 0 || !any(on)) return()

            long <- do.call(rbind, lapply(vars, function(v) {
                data.frame(variable = v, value = as.numeric(self$data[[v]]),
                           stringsAsFactors = FALSE)
            }))
            long <- long[is.finite(long$value), , drop = FALSE]
            if (nrow(long) == 0) return()

            for (key in names(private$.switches)[on]) {
                image <- self$results[[paste0("plot_", key)]]
                sz <- private$.size(key, length(vars))
                image$setSize(sz[1], sz[2])
                image$setState(list(long = long, vars = vars))
            }
        },

        # automatic plot size from the number of variables
        .size = function(key, n) {
            o <- self$options
            if (!o$autoSize) return(c(o$plotWidth, o$plotHeight))
            horiz <- o$orientation == "horizontal"
            if (key %in% c("density", "ecdf")) return(c(700, 460))
            if (key == "ridge")
                return(if (horiz) c(700, max(300, 150 + 80 * n)) else c(max(450, 160 + 120 * n), 500))
            if (key == "hist")
                return(if (horiz) c(700, max(320, 100 + 115 * n)) else c(max(480, 110 + 190 * n), 440))
            if (horiz) c(700, max(250, 110 + 64 * n)) else c(max(430, 130 + 100 * n), 470)
        },

        # one render function per image (named in the .r.yaml)
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

        .render = function(image, key) {
            st <- image$state
            if (is.null(st)) return(FALSE)
            p <- private$.build(key, st$long, st$vars)
            if (is.null(p)) return(FALSE)
            suppressWarnings(suppressMessages(print(p)))
            TRUE
        },

        # shared context: ordering, positions, colours
        .context = function(vars) {
            o     <- self$options
            horiz <- identical(o$orientation, "horizontal")
            lv    <- if (o$invertVars) rev(vars) else vars
            n     <- length(lv)
            # first variable on top (horizontal) or on the left (vertical)
            pos   <- if (horiz) rev(seq_len(n)) else seq_len(n)
            names(pos) <- lv
            cols  <- vdp_palette(o$palette, n, o$reversePalette, spread = TRUE)
            names(cols) <- lv
            list(o = o, horiz = horiz, lv = lv, n = n, pos = pos, cols = cols,
                 alpha = o$alpha / 100, lw = vdp_line_width(o$lineWidth))
        },

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
            if (is.null(p)) return(NULL)
            private$.finish(p, long, ctx, key)
        },

        # theme, title, caption, legend: identical for every plot
        .finish = function(p, long, ctx, key) {
            o <- ctx$o
            overlay <- key %in% c("density", "ecdf")
            facets  <- key == "hist"
            p <- p +
                ggplot2::scale_fill_manual(values = ctx$cols, guide = if (overlay) "legend" else "none") +
                ggplot2::scale_colour_manual(values = ctx$cols, guide = if (overlay) "legend" else "none") +
                vdp_theme(o$theme, o$textSize) +
                ggplot2::theme(
                    legend.position  = if (overlay) o$legendPos else "none",
                    legend.title     = ggplot2::element_blank(),
                    plot.title       = ggplot2::element_text(face = "bold", hjust = 0.5),
                    panel.grid.minor = ggplot2::element_blank(),
                    plot.caption     = ggplot2::element_text(size = o$textSize * 0.65, colour = "grey45", hjust = 0),
                    plot.margin      = ggplot2::margin(10, 16, 8, 10))

            if (facets) {
                p <- p + ggplot2::theme(
                    strip.background = ggplot2::element_blank(),
                    panel.spacing    = ggplot2::unit(0.7, "lines"),
                    strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1))
            }

            # no grid lines along the variable axis
            if (!overlay && !facets) {
                p <- p + ggplot2::theme(
                    panel.grid.major.x = if (ctx$horiz) ggplot2::element_line() else ggplot2::element_blank(),
                    panel.grid.major.y = if (ctx$horiz) ggplot2::element_blank() else ggplot2::element_line())
            }

            n_per   <- table(long$variable)
            caption <- if (isTRUE(o$showN))
                paste(paste0(names(n_per), " (n=", n_per, ")"), collapse = "   ") else NULL
            if (key == "errorbar") {
                extra <- vdp_interval_label(o$ebCenter, o$ebInterval, o$ebLevel, o$ebMult)
                caption <- paste(c(extra, caption), collapse = "\n")
            }
            p + ggplot2::labs(title = vdp_title(o$title), caption = caption)
        },

        # variable axis (numeric positions labelled with the variable names)
        .cat_axis = function(ctx, upper = 0.6) {
            ggplot2::scale_x_continuous(
                breaks = unname(ctx$pos), labels = names(ctx$pos),
                expand = ggplot2::expansion(add = c(0.6, upper)))
        },

        .value_axis = function(ctx) ggplot2::labs(x = NULL, y = ctx$o$valueLabel),

        # variable on x, value on y: flip when the values should run horizontally
        .flip = function(p, ctx) if (ctx$horiz) p + ggplot2::coord_flip() else p,

        # value on x (density, ECDF): flip when the values should run vertically
        .flip_x = function(p, ctx) if (ctx$horiz) p else p + ggplot2::coord_flip(),

        # jittered raw data on the variable axis
        .jitter = function(long, ctx, colour = NULL, alpha = 0.4, x = "pos") {
            o <- ctx$o
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

        # ── Box plot ───────────────────────────────────────────────────────
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
            if (o$boxMean)
                p <- p + ggplot2::stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
                                               fill = "white", colour = "grey15", stroke = 0.8)
            p <- p + private$.cat_axis(ctx) + private$.value_axis(ctx)
            private$.flip(p, ctx)
        },

        # ── Violin plot ────────────────────────────────────────────────────
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
        .p_errorbar = function(long, ctx) {
            o <- ctx$o
            s <- do.call(rbind, lapply(ctx$lv, function(v) {
                r <- vdp_summary(long$value[long$variable == v], o$ebCenter, o$ebInterval,
                                 o$ebLevel, o$ebMult)
                data.frame(variable = v, pos = unname(ctx$pos[v]), centre = r[1], lo = r[2], hi = r[3])
            }))
            s$variable <- factor(s$variable, levels = ctx$lv)
            s <- s[order(s$pos), ]

            p <- ggplot2::ggplot()
            if (o$ebJitter) p <- p + private$.jitter(long, ctx, alpha = min(ctx$alpha * 0.5, 0.45))
            if (o$ebConnect)
                p <- p + ggplot2::geom_line(data = s, ggplot2::aes(x = pos, y = centre, group = 1),
                                            colour = "grey40", linewidth = ctx$lw * 0.7)
            p <- p +
                ggplot2::geom_errorbar(
                    data = s, ggplot2::aes(x = pos, ymin = lo, ymax = hi, colour = variable),
                    width = o$ebCap / 100, linewidth = ctx$lw * 1.5) +
                ggplot2::geom_point(
                    data = s, ggplot2::aes(x = pos, y = centre, fill = variable),
                    shape = 21, size = o$ebSize, colour = "grey15", stroke = 0.8)
            if (o$ebLabels)
                p <- p + ggplot2::geom_text(
                    data = s, ggplot2::aes(x = pos, y = centre, label = formatC(centre, format = "f", digits = 2)),
                    nudge_x = 0.3, size = o$textSize * 0.28, colour = "grey20")
            p <- p + private$.cat_axis(ctx) + private$.value_axis(ctx)
            private$.flip(p, ctx)
        },

        # density outline of one variable as polygon points in the (pos, value) canvas
        .dens = function(x, adjust, trim = FALSE, n = 256) {
            x <- x[is.finite(x)]
            if (length(x) < 2 || length(unique(x)) < 2) return(NULL)
            d <- if (trim) stats::density(x, adjust = adjust, n = n, from = min(x), to = max(x))
                 else stats::density(x, adjust = adjust, n = n)
            data.frame(v = d$x, d = d$y / max(d$y))
        },

        # ── Raincloud ──────────────────────────────────────────────────────
        .p_raincloud = function(long, ctx) {
            o <- ctx$o
            h <- o$rcCloudWidth / 100
            p <- ggplot2::ggplot()
            for (v in ctx$lv) {
                d <- private$.dens(long$value[long$variable == v], o$rcAdjust)
                if (is.null(d)) next
                base <- unname(ctx$pos[v]) + 0.08
                poly <- data.frame(x = c(base + h * d$d, rep(base, nrow(d))),
                                   y = c(d$v, rev(d$v)), variable = factor(v, levels = ctx$lv))
                p <- p + ggplot2::geom_polygon(
                    data = poly, ggplot2::aes(x = x, y = y, fill = variable, colour = variable),
                    alpha = ctx$alpha, linewidth = ctx$lw * 0.5)
            }
            if (o$rcRain) {
                long$xr <- long$pos - 0.2
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

        # ── Ridgeline: lower ridges are drawn last, so they cover the ones above ──
        .p_ridge = function(long, ctx) {
            o <- ctx$o
            h <- 1 + o$ridgeOverlap / 100
            p <- ggplot2::ggplot()
            drawn <- 0
            for (v in ctx$lv[order(-ctx$pos[ctx$lv])]) {      # highest position first
                d <- private$.dens(long$value[long$variable == v], o$ridgeAdjust, o$ridgeTrim)
                if (is.null(d)) next
                drawn <- drawn + 1
                base <- unname(ctx$pos[v])
                poly <- data.frame(x = c(base + h * d$d, rep(base, nrow(d))),
                                   y = c(d$v, rev(d$v)), variable = factor(v, levels = ctx$lv))
                # opaque underlay so the ridge truly hides what is behind it
                p <- p + ggplot2::geom_polygon(data = poly, ggplot2::aes(x = x, y = y),
                                               fill = "white", colour = NA)
                p <- p + ggplot2::geom_polygon(
                    data = poly, ggplot2::aes(x = x, y = y, fill = variable, colour = variable),
                    alpha = ctx$alpha, linewidth = ctx$lw * 0.6)
                if (o$ridgeMedian) {
                    m  <- stats::median(long$value[long$variable == v])
                    hm <- stats::approx(d$v, d$d, xout = m, rule = 2)$y * h
                    seg <- data.frame(x = base, xend = base + hm, y = m, yend = m,
                                      variable = factor(v, levels = ctx$lv))
                    p <- p + ggplot2::geom_segment(
                        data = seg, ggplot2::aes(x = x, xend = xend, y = y, yend = yend, colour = variable),
                        linewidth = ctx$lw * 1.2)
                }
            }
            if (drawn == 0) return(NULL)
            p <- p + private$.cat_axis(ctx, upper = h + 0.15) + private$.value_axis(ctx)
            private$.flip(p, ctx)
        },

        # ── Histograms on a shared value axis ──────────────────────────────
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
        .p_density = function(long, ctx) {
            o <- ctx$o
            p <- ggplot2::ggplot(long, ggplot2::aes(x = value, fill = variable, colour = variable)) +
                ggplot2::geom_density(adjust = o$densAdjust, trim = o$densTrim,
                                      alpha = if (o$densFilled) ctx$alpha * 0.55 else 0,
                                      linewidth = ctx$lw) +
                ggplot2::labs(x = o$valueLabel, y = "Density")
            private$.flip_x(p, ctx)
        },

        # ── ECDF ───────────────────────────────────────────────────────────
        .p_ecdf = function(long, ctx) {
            o <- ctx$o
            p <- ggplot2::ggplot(long, ggplot2::aes(x = value, colour = variable))
            if (o$ecdfMedian)
                p <- p + ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey60", linewidth = 0.4)
            p <- p + ggplot2::stat_ecdf(geom = "step", linewidth = ctx$lw * 1.3, pad = FALSE) +
                ggplot2::scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
                ggplot2::labs(x = o$valueLabel, y = "Cumulative proportion")
            private$.flip_x(p, ctx)
        },

        # ── Strip (data points) ────────────────────────────────────────────
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
        .p_barcode = function(long, ctx) {
            h <- ctx$o$barcodeHeight / 100 / 2
            p <- ggplot2::ggplot(long) +
                ggplot2::geom_segment(
                    ggplot2::aes(x = pos - h, xend = pos + h, y = value, yend = value, colour = variable),
                    linewidth = ctx$lw * 0.6, alpha = min(ctx$alpha, 0.9)) +
                private$.cat_axis(ctx) + private$.value_axis(ctx)
            private$.flip(p, ctx)
        }
    )
)
