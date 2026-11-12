# =============================================================================
# distGrp: "Compare Groups"
#
# Compares one or more numeric variables across the levels of a grouping factor.
# Colours encode the groups (`grp`), while the three group display modes
# (`groupMode`) control how variables and groups share the canvas:
#   - "dodge"      : one combined plot; variables sit at integer positions on the
#                    variable axis and groups are dodged/overlaid within each
#                    variable's slot.
#   - "facetGroup" : one facet panel per group, each showing all variables on
#                    the shared axis.
#   - "facetVar"   : one facet panel per variable, each comparing the groups on
#                    the shared axis.
# =============================================================================
distGrpClass <- R6::R6Class(
    "distGrpClass",
    inherit = distGrpBase,
    private = list(

        .switches = c(
            box = "showBox", violin = "showViolin", errorbar = "showErrorBar",
            raincloud = "showRaincloud", ridge = "showRidge", hist = "showHist",
            density = "showDensity", ecdf = "showEcdf", strip = "showStrip",
            barcode = "showBarcode"),

        .run = function() {
            opts <- self$options
            vars <- opts$vars
            grp  <- opts$group
            on   <- vapply(private$.switches, function(k) isTRUE(opts[[k]]), logical(1))

            hint <- self$results$hint
            if ((length(vars) > 0 || !is.null(grp)) && (length(vars) == 0 || is.null(grp))) {
                hint$setContent("<p>Select at least one <b>Variable</b> and a <b>Grouping Variable</b> to compare groups.</p>")
                hint$setVisible(TRUE)
                return()
            } else if (length(vars) > 0 && !is.null(grp) && !any(on)) {
                hint$setContent("<p>Open a plot's section below and tick its checkbox to draw it.</p>")
                hint$setVisible(TRUE)
                return()
            } else {
                hint$setVisible(FALSE)
            }
            if (length(vars) == 0 || is.null(grp) || !any(on)) return()

            long <- private$.long(vars, grp, warn = TRUE)
            if (nrow(long) == 0)
                jmvcore::reject("The selected variables and grouping variable have no usable non-missing observations.")
            if (length(unique(long$grp)) < 1)
                jmvcore::reject("The grouping variable has no non-missing levels.")

            for (key in names(private$.switches)[on]) {
                image <- self$results[[paste0("plot_", key)]]
                st <- list(vars = vars, group = grp)
                if (key == "errorbar") st$summary <- private$.summary(long, vars)
                image$setState(st)
            }
        },

        .long = function(vars, grp, warn = FALSE) {
            o <- self$options
            how <- o$transform
            raw_g <- self$data[[grp]]
            if (!is.factor(raw_g)) raw_g <- as.factor(raw_g)
            if (!isTRUE(o$ignoreMissingGroup) && anyNA(raw_g)) {
                lv_g <- c(levels(raw_g), "(Missing)")
                ch_g <- as.character(raw_g)
                ch_g[is.na(ch_g)] <- "(Missing)"
                g_vec <- factor(ch_g, levels = unique(lv_g))
            } else {
                g_vec <- droplevels(raw_g)
            }

            lost <- character(0)
            long <- do.call(rbind, lapply(vars, function(v) {
                raw <- as.numeric(self$data[[v]])
                x <- cmp_transform(raw, how)
                n_lost <- sum(is.finite(raw)) - sum(is.finite(x))
                if (how != "none" && n_lost > 0) lost <<- c(lost, paste0(v, " (", n_lost, ")"))
                data.frame(variable = v, grp = g_vec, value = x, stringsAsFactors = FALSE)
            }))
            if (warn && length(lost) > 0)
                private$.warn(".transformLost", paste0(
                    "The transformation (", cmp_transform_label(how), ") could not be applied to some values, which were left out: ",
                    paste(lost, collapse = ", "), "."))
            keep <- is.finite(long$value) & !is.na(long$grp)
            out <- long[keep, , drop = FALSE]
            out$grp <- droplevels(out$grp)
            out
        },

        .warn = function(name, content) {
            notice <- jmvcore::Notice$new(
                self$options, name = name, type = jmvcore::NoticeType$WARNING, content = content)
            self$results$insert(1, notice)
        },

        .summary = function(long, vars) {
            o <- self$options
            lg <- levels(long$grp)
            do.call(rbind, lapply(vars, function(v) {
                do.call(rbind, lapply(lg, function(g) {
                    vals <- long$value[long$variable == v & long$grp == g]
                    r <- cmp_summary(vals, o$ebCenter, o$ebInterval, o$ebLevel, o$ebMult)
                    data.frame(variable = v, grp = g, centre = r[1], lo = r[2], hi = r[3],
                               stringsAsFactors = FALSE)
                }))
            }))
        },

        .plot_box       = function(image, ggtheme, theme, ...) private$.render(image, "box", ggtheme, theme),
        .plot_violin    = function(image, ggtheme, theme, ...) private$.render(image, "violin", ggtheme, theme),
        .plot_errorbar  = function(image, ggtheme, theme, ...) private$.render(image, "errorbar", ggtheme, theme),
        .plot_raincloud = function(image, ggtheme, theme, ...) private$.render(image, "raincloud", ggtheme, theme),
        .plot_ridge     = function(image, ggtheme, theme, ...) private$.render(image, "ridge", ggtheme, theme),
        .plot_hist      = function(image, ggtheme, theme, ...) private$.render(image, "hist", ggtheme, theme),
        .plot_density   = function(image, ggtheme, theme, ...) private$.render(image, "density", ggtheme, theme),
        .plot_ecdf      = function(image, ggtheme, theme, ...) private$.render(image, "ecdf", ggtheme, theme),
        .plot_strip     = function(image, ggtheme, theme, ...) private$.render(image, "strip", ggtheme, theme),
        .plot_barcode   = function(image, ggtheme, theme, ...) private$.render(image, "barcode", ggtheme, theme),

        .render = function(image, key, ggtheme = NULL, theme = NULL) {
            st <- image$state
            if (is.null(st)) return(FALSE)
            st$width <- image$width
            long <- private$.long(st$vars, st$group)
            if (nrow(long) == 0) return(FALSE)
            p <- private$.build(key, long, st, ggtheme, theme)
            if (is.null(p)) return(FALSE)
            suppressWarnings(suppressMessages(print(p)))
            TRUE
        },

        .context = function(vars, grp_name, long, ggtheme = NULL, theme = NULL, width = 480) {
            o     <- self$options
            horiz <- identical(o$orientation, "horizontal")
            mode  <- o$groupMode

            stat <- switch(o$sortVars,
                mean   = tapply(long$value, long$variable, mean),
                median = tapply(long$value, long$variable, stats::median),
                NULL)
            ord <- cmp_order_vars(vars, o$sortVars, stat)
            lv  <- if (o$invertVars) rev(ord) else ord
            n   <- length(lv)

            lg_raw <- levels(long$grp)
            lg     <- if (o$invertGroups) rev(lg_raw) else lg_raw
            ng     <- length(lg)

            # Variable positions (1..n) and group axis positions (1..ng)
            vpos <- if (horiz) rev(seq_len(n)) else seq_len(n)
            names(vpos) <- lv
            gpos_axis <- if (horiz) rev(seq_len(ng)) else seq_len(ng)
            names(gpos_axis) <- lg

            # Dodge offsets within each variable slot (only used when mode == "dodge")
            span <- if (ng <= 1) 0 else min(0.76, 0.24 * ng)
            goff <- if (ng <= 1 || mode != "dodge") rep(0, ng)
                    else if (horiz) rev(seq(-span / 2, span / 2, length.out = ng))
                    else seq(-span / 2, span / 2, length.out = ng)
            names(goff) <- lg
            w_scale <- if (mode == "dodge" && ng > 1) (span / ng) * 0.88 else 1

            # Group colours
            bw    <- identical(o$colourPalette, "bw")
            cols  <- if (bw) rep("black", ng)
                     else cmp_palette(o$colourPalette, ng, o$reversePalette, spread = TRUE, theme = theme)
            fills <- if (bw) rep("white", ng) else cols
            names(cols)  <- lg
            names(fills) <- lg

            lab <- cmp_var_labels(self$data, lv, o$useDesc)
            glab <- stats::setNames(lg, lg)

            # Axis positions & labels depend on whether the canvas x-axis shows variables or groups
            if (mode == "facetVar") {
                axis_pos <- gpos_axis
                axis_lab <- if (horiz) glab else stats::setNames(
                    cmp_fit_labels(glab, ((if (is.null(width)) 480 else width) - 110) / max(ng, 1), o$textSize), names(glab))
            } else {
                axis_pos <- vpos
                axis_lab <- if (horiz) lab else stats::setNames(
                    cmp_fit_labels(lab, ((if (is.null(width)) 480 else width) - 110) / max(n, 1), o$textSize), names(lab))
            }

            vlab <- if (o$transform == "none") o$valueLabel
                    else paste0(o$valueLabel, " (", cmp_transform_label(o$transform), ")")
            leg_title <- cmp_title(o$legendTitle)
            if (is.null(leg_title)) leg_title <- grp_name

            list(o = o, horiz = horiz, mode = mode, lv = lv, n = n, lg = lg, ng = ng,
                 pos = vpos, gpos_axis = gpos_axis, goff = goff, w_scale = w_scale,
                 axis_pos = axis_pos, axis_lab = axis_lab,
                 cols = cols, fills = fills, bw = bw,
                 lab = lab, glab = glab, vlab = vlab, leg_title = leg_title,
                 ggtheme = ggtheme,
                 alpha = if (bw) 1 else o$alpha / 100,
                 lw = cmp_line_width(o$lineWidth) * (if (bw) 1.5 else 1))
        },

        .build = function(key, long, st, ggtheme = NULL, theme = NULL) {
            ctx <- private$.context(st$vars, st$group, long, ggtheme, theme, st$width)
            ctx$summary <- st$summary
            long$variable <- factor(long$variable, levels = ctx$lv)
            long$grp      <- factor(long$grp, levels = ctx$lg)
            long$pos      <- if (ctx$mode == "facetVar") unname(ctx$gpos_axis[as.character(long$grp)])
                             else unname(ctx$pos[as.character(long$variable)])
            long$gpos     <- long$pos + unname(ctx$goff[as.character(long$grp)])
            long$slot     <- interaction(long$variable, long$grp, drop = TRUE)

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

        .finish = function(p, long, ctx, key) {
            o <- ctx$o
            overlay <- key %in% c("density", "ecdf")
            show_leg <- !identical(o$legendPos, "none")
            leg <- if (show_leg) ggplot2::guide_legend(
                title = ctx$leg_title,
                nrow = if (o$legendPos %in% c("bottom", "top")) max(1L, ceiling(ctx$ng / 4)) else NULL) else "none"

            # Faceting for canvas plots when mode is facetGroup or facetVar
            canvas_plot <- !(key %in% c("hist", "density", "ecdf"))
            if (canvas_plot && ctx$mode == "facetGroup") {
                p <- p + ggplot2::facet_wrap(ggplot2::vars(grp), labeller = ggplot2::as_labeller(ctx$glab))
            } else if (canvas_plot && ctx$mode == "facetVar") {
                p <- p + ggplot2::facet_wrap(ggplot2::vars(variable), labeller = ggplot2::as_labeller(ctx$lab))
            }

            p <- p +
                ggplot2::scale_fill_manual(values = ctx$fills, breaks = ctx$lg, labels = ctx$glab,
                                           name = ctx$leg_title, drop = FALSE, guide = leg) +
                ggplot2::scale_colour_manual(values = ctx$cols, breaks = ctx$lg, labels = ctx$glab,
                                             name = ctx$leg_title, drop = FALSE, guide = leg) +
                ggplot2::scale_linetype_manual(
                    values = stats::setNames(if (ctx$bw) rep_len(c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), ctx$ng)
                                             else rep("solid", ctx$ng), ctx$lg),
                    breaks = ctx$lg, labels = ctx$glab, name = ctx$leg_title,
                    guide = if (overlay && ctx$bw && show_leg) leg else "none") +
                cmp_theme(o$plotTheme, o$textSize, ctx$ggtheme) +
                ggplot2::theme(
                    legend.position  = o$legendPos,
                    plot.title       = ggplot2::element_text(face = "bold", hjust = 0.5),
                    panel.grid.minor = ggplot2::element_blank(),
                    strip.background = ggplot2::element_blank(),
                    strip.text       = ggplot2::element_text(face = "bold"),
                    panel.spacing    = ggplot2::unit(0.8, "lines"),
                    plot.caption     = ggplot2::element_text(size = ggplot2::rel(0.7), colour = "grey45", hjust = 0),
                    plot.margin      = ggplot2::margin(10, 16, 8, 10))

            if (canvas_plot) {
                p <- p + ggplot2::theme(
                    panel.grid.major.x = if (ctx$horiz) ggplot2::element_line() else ggplot2::element_blank(),
                    panel.grid.major.y = if (ctx$horiz) ggplot2::element_blank() else ggplot2::element_line())
            }

            caption <- NULL
            if (isTRUE(o$showN)) {
                # Count distinct observations per group across the dataset
                g_counts <- table(long$grp[long$variable == ctx$lv[1]])
                caption <- paste(strwrap(paste(paste0(names(g_counts), " (n=", g_counts, ")"), collapse = "   "), 55), collapse = "\n")
            }
            if (key == "errorbar") {
                extra <- cmp_interval_label(o$ebCenter, o$ebInterval, o$ebLevel, o$ebMult)
                caption <- paste(c(extra, caption), collapse = "\n")
            }
            p + ggplot2::labs(title = cmp_title(o$title), caption = caption)
        },

        .cat_axis = function(ctx, upper = 0.6) {
            ggplot2::scale_x_continuous(
                breaks = unname(ctx$axis_pos), labels = unname(ctx$axis_lab[names(ctx$axis_pos)]),
                expand = ggplot2::expansion(add = c(0.6, upper)))
        },

        .value_axis = function(ctx) ggplot2::labs(x = NULL, y = ctx$vlab),

        .flip = function(p, ctx) if (ctx$horiz) p + ggplot2::coord_flip() else p,

        .flip_x = function(p, ctx) if (ctx$horiz) p else p + ggplot2::coord_flip(),

        .jitter = function(long, ctx, x = "gpos") {
            o <- ctx$o
            w <- (o$jitterSpread / 100 / 2) * ctx$w_scale
            pj <- ggplot2::position_jitter(width = w, height = 0, seed = 1)
            alpha <- o$pointAlpha / 100
            if (identical(o$pointColour, "grey")) {
                ggplot2::geom_point(
                    data = long, mapping = ggplot2::aes(x = .data[[x]], y = value),
                    inherit.aes = FALSE, position = pj, size = o$pointSize, alpha = alpha,
                    colour = if (ctx$bw) "black" else "grey25", shape = 16)
            } else {
                ggplot2::geom_point(
                    data = long, mapping = ggplot2::aes(x = .data[[x]], y = value, colour = grp),
                    inherit.aes = FALSE, position = pj, size = o$pointSize, alpha = alpha, shape = 16)
            }
        },

        .dens = function(x, adjust, trim = FALSE, n = 256) {
            x <- x[is.finite(x)]
            if (length(x) < 2 || length(unique(x)) < 2) return(NULL)
            d <- if (trim) stats::density(x, adjust = adjust, n = n, from = min(x), to = max(x))
                 else stats::density(x, adjust = adjust, n = n)
            data.frame(v = d$x, d = d$y / max(d$y))
        },

        # ── Box plot ───────────────────────────────────────────────────────
        .p_box = function(long, ctx) {
            o <- ctx$o
            w <- (o$boxWidth / 100) * ctx$w_scale
            p <- ggplot2::ggplot(long, ggplot2::aes(x = gpos, y = value, group = slot,
                                                    fill = grp, colour = grp)) +
                ggplot2::geom_boxplot(
                    width = w, alpha = ctx$alpha, linewidth = ctx$lw,
                    notch = o$boxNotch,
                    outlier.shape = if (o$boxOutliers && !o$boxJitter) 19 else NA,
                    outlier.alpha = o$pointAlpha / 100, outlier.size = o$pointSize)
            if (o$boxJitter) p <- p + private$.jitter(long, ctx)
            if (o$boxMean)
                p <- p + ggplot2::stat_summary(fun = mean, geom = "point", shape = 23, size = 2.6,
                                               fill = "white", colour = "grey15", stroke = 0.8)
            p <- p + private$.cat_axis(ctx) + private$.value_axis(ctx)
            private$.flip(p, ctx)
        },

        # ── Violin plot (with optional Split Violin for 2 groups) ──────────
        .p_violin = function(long, ctx) {
            o <- ctx$o
            if (isTRUE(o$violinSplit) && ctx$ng == 2 && identical(ctx$mode, "dodge")) {
                h <- (o$violinWidth / 100) * 0.45
                p <- ggplot2::ggplot()
                for (v in ctx$lv) {
                    base <- unname(ctx$pos[v])
                    for (gi in seq_along(ctx$lg)) {
                        g <- ctx$lg[gi]
                        vals <- long$value[long$variable == v & long$grp == g]
                        d <- private$.dens(vals, o$violinAdjust, o$violinTrim)
                        if (is.null(d)) next
                        # Group 1 extends to the high side (+1), Group 2 to the low side (-1)
                        dir <- if (gi == 1) (if (ctx$horiz) 1 else -1) else (if (ctx$horiz) -1 else 1)
                        poly <- data.frame(
                            x = c(base + dir * h * d$d, rep(base, nrow(d))),
                            y = c(d$v, rev(d$v)),
                            grp = factor(g, levels = ctx$lg))
                        p <- p + ggplot2::geom_polygon(
                            data = poly, ggplot2::aes(x = x, y = y, fill = grp, colour = grp),
                            alpha = ctx$alpha, linewidth = ctx$lw * 0.6)
                    }
                }
                if (o$violinBox)
                    p <- p + ggplot2::geom_boxplot(
                        data = long, ggplot2::aes(x = gpos, y = value, group = slot),
                        inherit.aes = FALSE, width = 0.10, fill = "white", colour = "grey20",
                        alpha = 0.85, linewidth = ctx$lw * 0.5, outlier.shape = NA)
                if (o$violinJitter) p <- p + private$.jitter(long, ctx)
                p <- p + private$.cat_axis(ctx) + private$.value_axis(ctx)
                return(private$.flip(p, ctx))
            }

            w <- (o$violinWidth / 100) * ctx$w_scale
            p <- ggplot2::ggplot(long, ggplot2::aes(x = gpos, y = value, group = slot,
                                                    fill = grp, colour = grp)) +
                ggplot2::geom_violin(
                    width = w, scale = o$violinScale, trim = o$violinTrim,
                    adjust = o$violinAdjust, alpha = ctx$alpha, linewidth = ctx$lw * 0.6)
            if (o$violinBox)
                p <- p + ggplot2::geom_boxplot(
                    width = 0.14 * ctx$w_scale, fill = "white", colour = "grey20",
                    alpha = 0.85, linewidth = ctx$lw * 0.5, outlier.shape = NA)
            if (o$violinJitter) p <- p + private$.jitter(long, ctx)
            p <- p + private$.cat_axis(ctx) + private$.value_axis(ctx)
            private$.flip(p, ctx)
        },

        # ── Means (or medians) with error bars ─────────────────────────────
        .p_errorbar = function(long, ctx) {
            o <- ctx$o
            s <- ctx$summary
            s <- s[s$variable %in% ctx$lv & s$grp %in% ctx$lg, , drop = FALSE]
            s$variable <- factor(s$variable, levels = ctx$lv)
            s$grp      <- factor(s$grp, levels = ctx$lg)
            s$pos      <- if (ctx$mode == "facetVar") unname(ctx$gpos_axis[as.character(s$grp)])
                          else unname(ctx$pos[as.character(s$variable)])
            s$gpos     <- s$pos + unname(ctx$goff[as.character(s$grp)])
            s <- s[order(s$grp, s$pos), ]

            p <- ggplot2::ggplot()
            if (o$ebJitter) p <- p + private$.jitter(long, ctx)
            if (o$ebConnect) {
                p <- p + ggplot2::geom_line(
                    data = s, ggplot2::aes(x = gpos, y = centre, group = grp, colour = grp),
                    linewidth = ctx$lw * 0.85)
            }
            p <- p +
                ggplot2::geom_errorbar(
                    data = s, ggplot2::aes(x = gpos, ymin = lo, ymax = hi, colour = grp),
                    width = (o$ebCap / 100) * ctx$w_scale, linewidth = ctx$lw * 1.4) +
                ggplot2::geom_point(
                    data = s, ggplot2::aes(x = gpos, y = centre, fill = grp),
                    shape = 21, size = o$ebSize, colour = "grey15", stroke = 0.8)
            if (o$ebLabels)
                p <- p + ggplot2::geom_text(
                    data = s, ggplot2::aes(x = gpos, y = centre, label = formatC(centre, format = "f", digits = 2)),
                    nudge_x = 0.18 * ctx$w_scale, size = o$textSize * 0.26, colour = "grey20")
            p <- p + private$.cat_axis(ctx) + private$.value_axis(ctx)
            private$.flip(p, ctx)
        },

        # ── Raincloud ──────────────────────────────────────────────────────
        .p_raincloud = function(long, ctx) {
            o <- ctx$o
            h <- o$rcCloudWidth / 100
            p <- ggplot2::ggplot()
            if (ctx$mode == "facetVar") {
                for (v in ctx$lv) {
                    for (g in ctx$lg) {
                        d <- private$.dens(long$value[long$variable == v & long$grp == g], o$rcAdjust)
                        if (is.null(d)) next
                        base <- unname(ctx$gpos_axis[g]) + 0.08
                        poly <- data.frame(
                            x = c(base + h * d$d, rep(base, nrow(d))),
                            y = c(d$v, rev(d$v)),
                            variable = factor(v, levels = ctx$lv),
                            grp = factor(g, levels = ctx$lg))
                        p <- p + ggplot2::geom_polygon(
                            data = poly, ggplot2::aes(x = x, y = y, fill = grp, colour = grp),
                            alpha = ctx$alpha, linewidth = ctx$lw * 0.5)
                    }
                }
            } else {
                for (v in ctx$lv) {
                    base <- unname(ctx$pos[v]) + 0.08
                    for (g in ctx$lg) {
                        d <- private$.dens(long$value[long$variable == v & long$grp == g], o$rcAdjust)
                        if (is.null(d)) next
                        poly <- data.frame(
                            x = c(base + h * d$d, rep(base, nrow(d))),
                            y = c(d$v, rev(d$v)),
                            variable = factor(v, levels = ctx$lv),
                            grp = factor(g, levels = ctx$lg))
                        p <- p + ggplot2::geom_polygon(
                            data = poly, ggplot2::aes(x = x, y = y, fill = grp, colour = grp),
                            alpha = ctx$alpha * 0.75, linewidth = ctx$lw * 0.5)
                    }
                }
            }
            if (o$rcRain) {
                long$xr <- long$gpos - 0.16 * ctx$w_scale
                p <- p + private$.jitter(long, ctx, x = "xr")
            }
            if (o$rcBox)
                p <- p + ggplot2::geom_boxplot(
                    data = long, ggplot2::aes(x = gpos, y = value, group = slot, fill = grp),
                    inherit.aes = FALSE, width = 0.12 * ctx$w_scale, colour = "grey20",
                    alpha = 0.85, linewidth = ctx$lw * 0.5, outlier.shape = NA)
            p <- p + private$.cat_axis(ctx, upper = 0.25) + private$.value_axis(ctx)
            private$.flip(p, ctx)
        },

        # ── Ridgeline ──────────────────────────────────────────────────────
        .p_ridge = function(long, ctx) {
            o <- ctx$o
            h <- 1 + o$ridgeOverlap / 100
            p <- ggplot2::ggplot()
            drawn <- 0
            if (ctx$mode == "facetVar") {
                for (v in ctx$lv) {
                    for (g in ctx$lg[order(-ctx$gpos_axis[ctx$lg])]) {
                        vals <- long$value[long$variable == v & long$grp == g]
                        d <- private$.dens(vals, o$ridgeAdjust, o$ridgeTrim)
                        if (is.null(d)) next
                        drawn <- drawn + 1
                        base <- unname(ctx$gpos_axis[g])
                        poly <- data.frame(
                            x = c(base + h * d$d, rep(base, nrow(d))),
                            y = c(d$v, rev(d$v)),
                            variable = factor(v, levels = ctx$lv),
                            grp = factor(g, levels = ctx$lg))
                        p <- p + ggplot2::geom_polygon(
                            data = poly, ggplot2::aes(x = x, y = y, fill = grp, colour = grp),
                            alpha = ctx$alpha, linewidth = ctx$lw * 0.6)
                        if (o$ridgeMedian) {
                            m  <- stats::median(vals)
                            hm <- stats::approx(d$v, d$d, xout = m, rule = 2)$y * h
                            seg <- data.frame(x = base, xend = base + hm, y = m, yend = m,
                                              variable = factor(v, levels = ctx$lv),
                                              grp = factor(g, levels = ctx$lg))
                            p <- p + ggplot2::geom_segment(
                                data = seg, ggplot2::aes(x = x, xend = xend, y = y, yend = yend, colour = grp),
                                linewidth = ctx$lw * 1.2)
                        }
                    }
                }
            } else {
                for (v in ctx$lv[order(-ctx$pos[ctx$lv])]) {
                    base <- unname(ctx$pos[v])
                    for (g in ctx$lg) {
                        vals <- long$value[long$variable == v & long$grp == g]
                        d <- private$.dens(vals, o$ridgeAdjust, o$ridgeTrim)
                        if (is.null(d)) next
                        drawn <- drawn + 1
                        poly <- data.frame(
                            x = c(base + h * d$d, rep(base, nrow(d))),
                            y = c(d$v, rev(d$v)),
                            variable = factor(v, levels = ctx$lv),
                            grp = factor(g, levels = ctx$lg))
                        p <- p + ggplot2::geom_polygon(
                            data = poly, ggplot2::aes(x = x, y = y, fill = grp, colour = grp),
                            alpha = ctx$alpha * 0.65, linewidth = ctx$lw * 0.6)
                        if (o$ridgeMedian) {
                            m  <- stats::median(vals)
                            hm <- stats::approx(d$v, d$d, xout = m, rule = 2)$y * h
                            seg <- data.frame(x = base, xend = base + hm, y = m, yend = m,
                                              variable = factor(v, levels = ctx$lv),
                                              grp = factor(g, levels = ctx$lg))
                            p <- p + ggplot2::geom_segment(
                                data = seg, ggplot2::aes(x = x, xend = xend, y = y, yend = yend, colour = grp),
                                linewidth = ctx$lw * 1.2)
                        }
                    }
                }
            }
            if (drawn == 0) return(NULL)
            p <- p + private$.cat_axis(ctx, upper = 0.15) + private$.value_axis(ctx)
            private$.flip(p, ctx)
        },

        # ── Histograms ─────────────────────────────────────────────────────
        .p_hist = function(long, ctx) {
            o  <- ctx$o
            bw <- diff(range(long$value)) / o$histBins
            if (!is.finite(bw) || bw <= 0) return(NULL)
            ymap <- switch(o$histStat,
                count   = ggplot2::aes(y = ggplot2::after_stat(count)),
                percent = ggplot2::aes(y = ggplot2::after_stat(density * bw * 100)),
                ggplot2::aes(y = ggplot2::after_stat(density)))
            pos_hist <- if (identical(o$histPosition, "dodge")) "dodge" else "identity"
            al <- if (pos_hist == "identity" && ctx$mode != "facetGroup") ctx$alpha * 0.6 else ctx$alpha
            p <- ggplot2::ggplot(long, ggplot2::aes(x = value, fill = grp)) +
                ggplot2::geom_histogram(ymap, binwidth = bw, boundary = min(long$value),
                                        position = pos_hist,
                                        colour = if (ctx$bw) "black" else "white", linewidth = 0.15, alpha = al)
            if (o$histDensity) {
                dmap <- switch(o$histStat,
                    count   = ggplot2::aes(y = ggplot2::after_stat(count * bw), colour = grp),
                    percent = ggplot2::aes(y = ggplot2::after_stat(density * bw * 100), colour = grp),
                    ggplot2::aes(y = ggplot2::after_stat(density), colour = grp))
                p <- p + ggplot2::stat_density(dmap, geom = "line", linewidth = ctx$lw, adjust = 1)
            }
            scales <- if (o$histFreeY) "free_y" else "fixed"
            if (ctx$mode == "facetGroup") {
                p <- p + ggplot2::facet_grid(rows = ggplot2::vars(variable), cols = ggplot2::vars(grp),
                                             scales = scales, switch = "y",
                                             labeller = ggplot2::labeller(variable = ctx$lab, grp = ctx$glab))
            } else if (ctx$horiz) {
                p <- p + ggplot2::facet_grid(rows = ggplot2::vars(variable), scales = scales, switch = "y",
                                             labeller = ggplot2::as_labeller(ctx$lab))
            } else {
                p <- p + ggplot2::facet_grid(cols = ggplot2::vars(variable), scales = scales,
                                             labeller = ggplot2::as_labeller(ctx$lab)) +
                    ggplot2::coord_flip()
            }
            ylab <- switch(o$histStat, count = "Count", percent = "Percent", "Density")
            p + ggplot2::labs(x = ctx$vlab, y = ylab)
        },

        # ── Overlaid densities ─────────────────────────────────────────────
        .p_density = function(long, ctx) {
            o <- ctx$o
            p <- ggplot2::ggplot(long, ggplot2::aes(x = value, fill = grp, colour = grp, linetype = grp)) +
                ggplot2::geom_density(adjust = o$densAdjust, trim = o$densTrim,
                                      alpha = if (o$densFilled && !ctx$bw) ctx$alpha * 0.5 else 0,
                                      linewidth = ctx$lw) +
                ggplot2::labs(x = ctx$vlab, y = "Density")
            if (ctx$mode == "facetGroup") {
                p <- p + ggplot2::facet_grid(rows = ggplot2::vars(variable), cols = ggplot2::vars(grp),
                                             scales = "free_y", switch = "y",
                                             labeller = ggplot2::labeller(variable = ctx$lab, grp = ctx$glab))
            } else if (ctx$n > 1) {
                p <- p + ggplot2::facet_wrap(ggplot2::vars(variable), scales = "free_y",
                                             labeller = ggplot2::as_labeller(ctx$lab))
            }
            private$.flip_x(p, ctx)
        },

        # ── ECDF (cumulative distribution) ─────────────────────────────────
        .p_ecdf = function(long, ctx) {
            o <- ctx$o
            p <- ggplot2::ggplot(long, ggplot2::aes(x = value, colour = grp, linetype = grp))
            if (o$ecdfMedian)
                p <- p + ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey60", linewidth = 0.4)
            p <- p + ggplot2::stat_ecdf(geom = "step", linewidth = ctx$lw * 1.3, pad = FALSE) +
                ggplot2::scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
                ggplot2::labs(x = ctx$vlab, y = "Cumulative proportion")
            if (ctx$mode == "facetGroup") {
                p <- p + ggplot2::facet_grid(rows = ggplot2::vars(variable), cols = ggplot2::vars(grp),
                                             switch = "y",
                                             labeller = ggplot2::labeller(variable = ctx$lab, grp = ctx$glab))
            } else if (ctx$n > 1) {
                p <- p + ggplot2::facet_wrap(ggplot2::vars(variable),
                                             labeller = ggplot2::as_labeller(ctx$lab))
            }
            private$.flip_x(p, ctx)
        },

        # ── Strip (data points only) ───────────────────────────────────────
        .p_strip = function(long, ctx) {
            o <- ctx$o
            p <- ggplot2::ggplot() +
                private$.jitter(long, ctx)
            if (o$stripSummary != "none") {
                fun <- if (o$stripSummary == "mean") mean else stats::median
                s <- do.call(rbind, lapply(ctx$lv, function(v) {
                    do.call(rbind, lapply(ctx$lg, function(g) {
                        vals <- long$value[long$variable == v & long$grp == g]
                        if (length(vals) == 0) return(NULL)
                        base <- if (ctx$mode == "facetVar") unname(ctx$gpos_axis[g]) else unname(ctx$pos[v])
                        gp <- base + unname(ctx$goff[g])
                        data.frame(variable = factor(v, levels = ctx$lv),
                                   grp = factor(g, levels = ctx$lg),
                                   gpos = gp, y = fun(vals))
                    }))
                }))
                if (!is.null(s) && nrow(s) > 0) {
                    hw <- 0.30 * ctx$w_scale
                    p <- p + ggplot2::geom_segment(
                        data = s, ggplot2::aes(x = gpos - hw, xend = gpos + hw, y = y, yend = y),
                        colour = "grey15", linewidth = ctx$lw * 1.6)
                }
            }
            p <- p + private$.cat_axis(ctx) + private$.value_axis(ctx)
            private$.flip(p, ctx)
        },

        # ── Barcode ────────────────────────────────────────────────────────
        .p_barcode = function(long, ctx) {
            h <- (ctx$o$barcodeHeight / 100 / 2) * ctx$w_scale
            p <- ggplot2::ggplot(long) +
                ggplot2::geom_segment(
                    ggplot2::aes(x = gpos - h, xend = gpos + h, y = value, yend = value, colour = grp),
                    linewidth = ctx$lw * 0.6, alpha = min(ctx$alpha, 0.9)) +
                private$.cat_axis(ctx) + private$.value_axis(ctx)
            private$.flip(p, ctx)
        }
    )
)
