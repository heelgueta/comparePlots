# =============================================================================
# Shared engine of the two group analyses: Compare Dichotomous (distDich, exactly
# two groups) and Compare Polytomous (distPoly, three or more). Both compare one
# or more numeric variables across the levels of a grouping factor. Each analysis'
# class (R/distDich.b.R, R/distPoly.b.R) is built from cmp_grp_private(kind).
#
# Colours encode the groups (`grp`), while the three group display modes
# (`groupMode`) control how variables and groups share the canvas:
#   - "dodge"      : one combined plot; variables sit at integer positions on the
#                    variable axis and groups are dodged/overlaid within each
#                    variable's slot.
#   - "facetGroup" : one facet panel per group, each showing all variables on
#                    the shared axis.
#   - "facetVar"   : one facet panel per variable, each comparing the groups on
#                    the shared axis.
# `kind` is "dich" (adds the back-to-back histogram and requires two groups) or
# "poly" (accepts any number of groups).
# =============================================================================
cmp_grp_private <- function(kind) {
    list(


        .kind = kind,

        .switches = c(
            box = "showBox", violin = "showViolin", errorbar = "showErrorBar",
            raincloud = "showRaincloud", ridge = "showRidge", hist = "showHist",
            if (kind == "dich") c(pyramid = "showPyramid", ladder = "showLadder", lead = "showLead"),
            density = "showDensity", ecdf = "showEcdf", strip = "showStrip",
            barcode = "showBarcode"),

        # Size each plot from what it has to show (variables x groups x display
        # mode) so crowded configurations stay legible; users can still resize.
        .init = function() {
            o <- self$options
            vars <- o$vars; grp <- o$group
            if (length(vars) == 0 || is.null(grp)) return()
            g <- tryCatch(self$data[[grp]], error = function(e) NULL)
            if (is.null(g)) return()
            ng <- max(1L, nlevels(droplevels(if (is.factor(g)) g else as.factor(g))))
            if (!isTRUE(o$ignoreMissingGroup) && anyNA(g)) ng <- ng + 1L
            nv <- length(vars)
            mode <- o$groupMode
            horiz <- identical(o$orientation, "horizontal")
            grid <- function(k) {                  # the panel grid facet_wrap() will use
                d <- if (k <= 1) c(1, 1) else ggplot2::wrap_dims(k)
                c(nc = d[[2]], nr = d[[1]])
            }
            for (key in names(private$.switches)) {
                img <- self$results[[paste0("plot_", key)]]
                if (is.null(img)) next
                w <- 480; h <- 380
                if (key %in% c("hist", "density", "ecdf")) {
                    if (mode == "facetGroup") { rows <- nv; cols <- ng }
                    else if (key == "hist") { rows <- if (horiz) nv else 1; cols <- if (horiz) 1 else nv }
                    else { gr <- grid(nv); rows <- gr[["nr"]]; cols <- gr[["nc"]] }
                    w <- max(480, min(1000, 120 + 170 * cols))
                    h <- max(380, min(1400, 110 + 130 * rows))
                } else if (key %in% c("pyramid", "ladder", "lead")) {
                    gr <- grid(nv)
                    w <- max(480, min(1000, 120 + 200 * gr[["nc"]]))
                    h <- max(380, min(1400, 110 + 220 * gr[["nr"]]))
                } else if (mode == "dodge") {
                    span <- nv * (max(ng, 1) * 20 + 36) + 120
                    if (horiz) h <- max(380, min(1400, span)) else w <- max(480, min(1200, span))
                } else {
                    k <- if (mode == "facetGroup") ng else nv
                    gr <- grid(k)
                    w <- max(480, min(1000, 110 + 200 * gr[["nc"]]))
                    h <- max(380, min(1400, 130 + 190 * gr[["nr"]]))
                }
                img$setSize(w, h)
            }
        },

        .run = function() {
            opts <- self$options
            vars <- opts$vars
            grp  <- opts$group
            on   <- vapply(private$.switches, function(k) isTRUE(opts[[k]]), logical(1))

            if (length(vars) > 0 && !is.null(grp)) {
                ng0 <- private$.n_levels(grp)
                if (private$.kind == "dich" && ng0 != 2)
                    jmvcore::reject(paste0(
                        "Compare Dichotomous needs a grouping variable with exactly two levels, but '", grp, "' has ", ng0,
                        if (ng0 == 1) " level" else " levels",
                        if (ng0 > 2) ". For three or more groups use Compare Polytomous." else "."))
                if (private$.kind == "poly" && ng0 == 2)
                    private$.warn(".twoGroups", paste0(
                        "'", grp, "' has only two levels. Compare Dichotomous offers plots made for two groups ",
                        "(split violin, back-to-back histogram)."), jmvcore::NoticeType$INFO)
            }

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
                jmvcore::reject(.("The selected variables and grouping variable have no usable non-missing observations."))
            if (length(unique(long$grp)) < 1)
                jmvcore::reject(.("The grouping variable has no non-missing levels."))
            if (private$.kind == "dich" && nlevels(long$grp) != 2)
                jmvcore::reject(paste0(
                    "Compare Dichotomous needs two groups with usable values, but only ", nlevels(long$grp),
                    " of the grouping variable's levels have any."))

            # Say what silently has nothing to draw.
            cnt <- table(factor(long$variable, levels = vars), long$grp)
            gone <- vars[rowSums(cnt) == 0]
            if (length(gone) > 0)
                private$.warn(".emptyVar", paste0(
                    paste(gone, collapse = ", "), if (length(gone) == 1) " has" else " have",
                    " no usable values and is left out of the plots."))
            empty <- which(cnt[rowSums(cnt) > 0, , drop = FALSE] == 0, arr.ind = TRUE)
            if (nrow(empty) > 0) {
                cn <- cnt[rowSums(cnt) > 0, , drop = FALSE]
                lab <- paste0(rownames(cn)[empty[, 1]], " (", colnames(cn)[empty[, 2]], ")")
                if (length(lab) > 6) lab <- c(lab[1:6], paste0("and ", length(lab) - 6, " more"))
                private$.warn(".emptyCell", paste0(
                    "No data for: ", paste(lab, collapse = ", "), ". These combinations are left blank."))
            }
            if (nlevels(long$grp) > 8)
                private$.warn(".manyGroups", paste0(
                    "The grouping variable has ", nlevels(long$grp),
                    " levels, so colours are hard to tell apart. Panels by group (Appearance) usually reads better."))

            # Plots that need a density curve or a bin width cannot be drawn for a
            # variable-group cell without spread; say so rather than leaving a gap.
            if (any(on[c("violin", "raincloud", "ridge", "density")])) {
                cells <- unique(long[, c("variable", "grp")])
                flat <- character(0)
                for (i in seq_len(nrow(cells))) {
                    x <- long$value[long$variable == cells$variable[i] & long$grp == cells$grp[i]]
                    if (length(unique(x)) < 2)
                        flat <- c(flat, paste0(cells$variable[i], " (", cells$grp[i], ")"))
                }
                if (length(flat) > 0)
                    private$.warn(".noSpread", paste0(
                        paste(flat, collapse = ", "), if (length(flat) == 1) " has" else " have",
                        " fewer than two distinct values, so no density curve (violin, raincloud, ridgeline or overlaid densities) can be drawn for ",
                        if (length(flat) == 1) "it." else "them."))
            }
            if (on[["hist"]] && length(unique(long$value)) < 2)
                private$.warn(".noSpreadHist",
                    .("All values are the same, so no histogram can be drawn."))

            for (key in names(private$.switches)[on]) {
                image <- self$results[[paste0("plot_", key)]]
                st <- list(vars = vars, group = grp)
                if (key == "errorbar") st$summary <- private$.summary(long, vars)
                image$setState(st)
            }
        },

        .n_levels = function(grp) {
            g <- self$data[[grp]]
            if (!is.factor(g)) g <- as.factor(g)
            n <- nlevels(droplevels(g))
            if (!isTRUE(self$options$ignoreMissingGroup) && anyNA(g)) n <- n + 1L
            n
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
                data.frame(variable = v, grp = g_vec, value = x, id = seq_along(x), stringsAsFactors = FALSE)
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

        .warn = function(name, content, type = jmvcore::NoticeType$WARNING) {
            notice <- jmvcore::Notice$new(
                self$options, name = name, type = type, content = content)
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
        .plot_pyramid   = function(image, ggtheme, theme, ...) private$.render(image, "pyramid", ggtheme, theme),
        .plot_ladder    = function(image, ggtheme, theme, ...) private$.render(image, "ladder", ggtheme, theme),
        .plot_lead      = function(image, ggtheme, theme, ...) private$.render(image, "lead", ggtheme, theme),
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
            n_by <- tapply(long$id, long$grp, function(z) length(unique(z)))
            glab <- stats::setNames(
                if (isTRUE(o$showN)) paste0(lg, " (n = ", n_by[lg], ")") else lg, lg)

            ang <- switch(o$xLabelAngle, angle45 = 45, angle90 = 90, 0)

            # Width left for one panel's axis labels: the plot is divided among the panel columns
            # (facet_wrap's grid), then among the labels in a panel.
            k_pan <- if (mode == "facetGroup") ng else if (mode == "facetVar") n else 1
            n_col <- if (k_pan > 1) ggplot2::wrap_dims(k_pan)[[2]] else 1
            panel_px <- ((if (is.null(width)) 480 else width) - 110) / n_col

            # Axis positions & labels depend on whether the canvas x-axis shows variables or groups
            if (mode == "facetVar") {
                axis_pos <- gpos_axis
                axis_lab <- if (horiz) glab else if (ang > 0) stats::setNames(cmp_fit_labels(glab, 260, o$textSize), names(glab))
                            else stats::setNames(
                    cmp_fit_labels(glab, panel_px / max(ng, 1), o$textSize), names(glab))
            } else {
                axis_pos <- vpos
                axis_lab <- if (horiz) lab else if (ang > 0) stats::setNames(cmp_fit_labels(lab, 260, o$textSize), names(lab))
                            else stats::setNames(
                    cmp_fit_labels(lab, panel_px / max(n, 1), o$textSize), names(lab))
            }

            vlab <- if (o$transform == "none") o$valueLabel
                    else paste0(o$valueLabel, " (", cmp_transform_label(o$transform), ")")
            leg_title <- cmp_title(o$legendTitle)
            if (is.null(leg_title)) leg_title <- grp_name

            wrap <- function(x, w = 18) stats::setNames(
                vapply(x, function(z) paste(strwrap(z, w), collapse = "\n"), ""), names(x))
            list(angle = ang, width = if (is.null(width)) 480 else width, lab_strip = wrap(lab), glab_strip = wrap(glab),
                 o = o, horiz = horiz, mode = mode, lv = lv, n = n, lg = lg, ng = ng,
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
                pyramid   = private$.p_pyramid(long, ctx),
                ladder    = private$.p_ladder(long, ctx),
                lead      = private$.p_lead(long, ctx),
                density   = private$.p_density(long, ctx),
                ecdf      = private$.p_ecdf(long, ctx),
                strip     = private$.p_strip(long, ctx),
                barcode   = private$.p_barcode(long, ctx))
            if (is.null(p)) return(NULL)
            p <- p + private$.ref(long, ctx, key)
            private$.finish(p, long, ctx, key)
        },

        .finish = function(p, long, ctx, key) {
            o <- ctx$o
            overlay <- key %in% c("density", "ecdf")
            show_leg <- !identical(o$legendPos, "none")
            leg <- if (show_leg) ggplot2::guide_legend(
                title = ctx$leg_title,
                nrow = if (o$legendPos %in% c("bottom", "top")) {
                    # as many rows as the label widths need to fit the plot width
                    entry <- max(nchar(ctx$glab), nchar(ctx$leg_title)) * 0.45 * o$textSize + 0.04 * o$textSize * 16 + 30
                    per_row <- max(1L, floor((ctx$width - 30) / entry))
                    max(1L, ceiling(ctx$ng / per_row))
                } else NULL) else "none"

            # Faceting for canvas plots when mode is facetGroup or facetVar
            canvas_plot <- !(key %in% c("hist", "density", "ecdf", "pyramid", "ladder", "lead"))
            if (canvas_plot && ctx$mode == "facetGroup") {
                p <- p + ggplot2::facet_wrap(ggplot2::vars(grp), labeller = ggplot2::as_labeller(ctx$glab_strip))
            } else if (canvas_plot && ctx$mode == "facetVar") {
                p <- p + ggplot2::facet_wrap(ggplot2::vars(variable), labeller = ggplot2::as_labeller(ctx$lab_strip))
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

            if (ctx$angle > 0)
                p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(
                    angle = ctx$angle, hjust = 1, vjust = if (ctx$angle == 90) 0.5 else 1))

            if (canvas_plot) {
                p <- p + ggplot2::theme(
                    panel.grid.major.x = if (ctx$horiz) ggplot2::element_line() else ggplot2::element_blank(),
                    panel.grid.major.y = if (ctx$horiz) ggplot2::element_blank() else ggplot2::element_line())
            }

            # Optional notes, each switched on by its own setting (all off by default).
            caps <- character(0)
            if (key == "errorbar" && isTRUE(o$ebNote))
                caps <- c(caps, cmp_interval_label(o$ebCenter, o$ebInterval, o$ebLevel, o$ebMult))
            if (isTRUE(o$refNote) && !identical(o$refLine, "none") && !(key %in% c("pyramid", "ladder", "lead")))
                caps <- c(caps, if (identical(o$refLine, "mean")) .("Dashed line: overall mean of all groups.") else .("Dashed line: overall median of all groups."))
            if (key == "lead" && isTRUE(o$leadNote)) caps <- c(caps, private$.lead_caption(long, ctx))
            if (key == "density" && private$.kind == "dich" && isTRUE(o$densOverlap) && isTRUE(o$densNote) && ctx$mode != "facetGroup") {
                ov <- private$.overlap(long, ctx)
                if (!is.null(ov))
                    caps <- c(caps, paste0("Shaded: overlap of the two groups. ",
                        paste0(names(ov$pct), " ", round(ov$pct), "%", collapse = ", "), "."))
            }
            caption <- if (length(caps) > 0) paste(strwrap(caps, 60), collapse = "\n") else NULL
            p + ggplot2::labs(title = cmp_title(o$title), caption = caption)
        },

        # Dashed reference line at the overall (all groups pooled) mean or median of
        # each variable: across the variable's slot on category-axis plots, vertical
        # on the value-axis plots (histogram, densities, ECDF).
        .ref = function(long, ctx, key) {
            o <- ctx$o
            if (identical(o$refLine, "none") || key %in% c("pyramid", "ladder", "lead")) return(NULL)
            fun <- if (identical(o$refLine, "mean")) mean else stats::median
            val <- vapply(ctx$lv, function(z) fun(long$value[as.character(long$variable) == z]), 0)
            d <- data.frame(variable = factor(ctx$lv, levels = ctx$lv), ref = unname(val))
            if (key %in% c("hist", "density", "ecdf"))
                return(ggplot2::geom_vline(
                    data = d, mapping = ggplot2::aes(xintercept = ref), inherit.aes = FALSE,
                    linetype = "dashed", colour = "grey25", linewidth = 0.5))
            if (ctx$mode == "facetVar") {
                d$x0 <- 0.4; d$x1 <- ctx$ng + 0.6
            } else {
                d$x0 <- unname(ctx$pos[ctx$lv]) - 0.5; d$x1 <- unname(ctx$pos[ctx$lv]) + 0.5
            }
            ggplot2::geom_segment(
                data = d, mapping = ggplot2::aes(x = x0, xend = x1, y = ref, yend = ref), inherit.aes = FALSE,
                linetype = "dashed", colour = "grey25", linewidth = 0.5)
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
            if (private$.kind == "dich" && isTRUE(o$violinSplit) && ctx$ng == 2 && identical(ctx$mode, "dodge")) {
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
                                             labeller = ggplot2::labeller(variable = ctx$lab_strip, grp = ctx$glab_strip))
            } else if (ctx$horiz) {
                p <- p + ggplot2::facet_grid(rows = ggplot2::vars(variable), scales = scales, switch = "y",
                                             labeller = ggplot2::as_labeller(ctx$lab_strip))
            } else {
                p <- p + ggplot2::facet_grid(cols = ggplot2::vars(variable), scales = scales,
                                             labeller = ggplot2::as_labeller(ctx$lab_strip)) +
                    ggplot2::coord_flip()
            }
            ylab <- switch(o$histStat, count = .("Count"), percent = .("Percent"), .("Density"))
            p + ggplot2::labs(x = ctx$vlab, y = ylab)
        },

        # ── Back-to-back histogram (population pyramid), exactly two groups ──
        # Bins are shared by the two groups of a variable. Vertical orientation: the
        # classic pyramid (value axis vertical, first group to the left). Horizontal
        # orientation: value axis horizontal, first group above the line.
        .p_pyramid = function(long, ctx) {
            o <- ctx$o
            if (ctx$ng != 2) return(NULL)
            horiz <- ctx$horiz
            rows <- lapply(ctx$lv, function(v) {
                x <- long$value[as.character(long$variable) == v]
                if (length(x) == 0 || diff(range(x)) <= 0) return(NULL)
                br <- seq(min(x), max(x), length.out = o$pyBins + 1)
                mid <- (br[-1] + br[-length(br)]) / 2
                bw <- br[2] - br[1]
                do.call(rbind, lapply(seq_along(ctx$lg), function(i) {
                    xs <- long$value[as.character(long$variable) == v & as.character(long$grp) == ctx$lg[i]]
                    cnt <- tabulate(findInterval(xs, br, rightmost.closed = TRUE, all.inside = TRUE), nbins = o$pyBins)
                    val <- if (identical(o$pyStat, "percent")) 100 * cnt / max(length(xs), 1) else cnt
                    up <- (i == 1) == horiz            # which side of the zero line this group takes
                    data.frame(variable = factor(v, levels = ctx$lv), grp = factor(ctx$lg[i], levels = ctx$lg),
                               mid = mid, bw = bw, len = if (up) val else -val)
                }))
            })
            d <- do.call(rbind, rows)
            if (is.null(d) || nrow(d) == 0) return(NULL)
            lim <- max(abs(d$len), 1)
            lenlab <- if (identical(o$pyStat, "percent")) .("Percent of group") else .("Count")
            edge <- if (ctx$bw) "black" else "white"
            if (horiz) {
                p <- ggplot2::ggplot(d, ggplot2::aes(xmin = mid - bw / 2, xmax = mid + bw / 2,
                                                     ymin = pmin(0, len), ymax = pmax(0, len), fill = grp)) +
                    ggplot2::geom_rect(colour = edge, linewidth = 0.15, alpha = ctx$alpha) +
                    ggplot2::geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.4) +
                    ggplot2::scale_y_continuous(limits = c(-lim, lim), labels = function(z) abs(z)) +
                    ggplot2::labs(x = ctx$vlab, y = lenlab)
                sc <- "free_x"
            } else {
                p <- ggplot2::ggplot(d, ggplot2::aes(xmin = pmin(0, len), xmax = pmax(0, len),
                                                     ymin = mid - bw / 2, ymax = mid + bw / 2, fill = grp)) +
                    ggplot2::geom_rect(colour = edge, linewidth = 0.15, alpha = ctx$alpha) +
                    ggplot2::geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.4) +
                    ggplot2::scale_x_continuous(limits = c(-lim, lim), labels = function(z) abs(z)) +
                    ggplot2::labs(x = lenlab, y = ctx$vlab)
                sc <- "free_y"
            }
            if (ctx$n > 1)
                p <- p + ggplot2::facet_wrap(ggplot2::vars(variable), scales = sc,
                                             labeller = ggplot2::as_labeller(ctx$lab_strip))
            p
        },

        # ── Percentile ladder (two groups) ─────────────────────────────────
        # One rung per percentile: the two groups' values at that percentile joined by a
        # bar coloured by the group that is higher there. Shows WHERE in the distribution
        # (low, middle, high) the groups differ, and by how much in raw units.
        .p_ladder = function(long, ctx) {
            o <- ctx$o
            if (ctx$ng != 2) return(NULL)
            probs <- switch(o$ladderSteps,
                quartiles = c(0.25, 0.5, 0.75),
                ventiles  = seq(0.05, 0.95, by = 0.05),
                seq(0.1, 0.9, by = 0.1))
            rows <- lapply(ctx$lv, function(v) {
                q <- lapply(ctx$lg, function(g) {
                    x <- long$value[as.character(long$variable) == v & as.character(long$grp) == g]
                    if (length(x) == 0) NULL else unname(stats::quantile(x, probs, type = 7))
                })
                if (any(vapply(q, is.null, TRUE))) return(NULL)
                data.frame(variable = factor(v, levels = ctx$lv), p = round(probs * 100), q1 = q[[1]], q2 = q[[2]],
                           hi = factor(ctx$lg[ifelse(q[[1]] >= q[[2]], 1L, 2L)], levels = ctx$lg))
            })
            d <- do.call(rbind, rows)
            if (is.null(d) || nrow(d) == 0) return(NULL)
            pts <- rbind(
                data.frame(variable = d$variable, p = d$p, q = d$q1, grp = factor(ctx$lg[1], levels = ctx$lg)),
                data.frame(variable = d$variable, p = d$p, q = d$q2, grp = factor(ctx$lg[2], levels = ctx$lg)))
            brk <- round(probs * 100)
            brk <- brk[brk %% 10 == 0 | length(brk) <= 9]
            p <- ggplot2::ggplot() +
                ggplot2::geom_segment(
                    data = d, ggplot2::aes(x = p, xend = p, y = q1, yend = q2, colour = hi),
                    linewidth = ctx$lw * 2.6, alpha = 0.55, lineend = "round") +
                ggplot2::geom_point(
                    data = pts, ggplot2::aes(x = p, y = q, fill = grp), shape = 21,
                    colour = "grey15", size = max(2.5, o$pointSize * 1.8), stroke = 0.6) +
                ggplot2::scale_x_continuous(breaks = brk, labels = format(brk, trim = TRUE),
                                            expand = ggplot2::expansion(add = 5)) +
                ggplot2::labs(x = .("Percentile"), y = ctx$vlab)
            if (ctx$n > 1)
                p <- p + ggplot2::facet_wrap(ggplot2::vars(variable), scales = "free_y",
                                             labeller = ggplot2::as_labeller(ctx$lab_strip))
            private$.flip(p, ctx)
        },

        # ── Lead plot (two groups) ─────────────────────────────────────────
        # Running difference of the two cumulative distributions: at each value, the
        # share of group 1 at or below it minus the share of group 2 (percentage points).
        # Above zero group 1 is "ahead", i.e. tends lower; below zero group 2. Under the
        # curve a two-row barcode shows the observations themselves.
        .lead_calc = function(long, ctx) {
            g1 <- ctx$lg[1]; g2 <- ctx$lg[2]
            res <- lapply(ctx$lv, function(v) {
                x1 <- long$value[as.character(long$variable) == v & as.character(long$grp) == g1]
                x2 <- long$value[as.character(long$variable) == v & as.character(long$grp) == g2]
                grid <- sort(unique(c(x1, x2)))
                if (length(x1) == 0 || length(x2) == 0 || length(grid) < 2) return(NULL)
                d <- (stats::ecdf(x1)(grid) - stats::ecdf(x2)(grid)) * 100
                k <- which.max(abs(d))
                list(v = v, x1 = x1, x2 = x2, grid = grid, d = d, k = k)
            })
            Filter(Negate(is.null), res)
        },

        .lead_caption = function(long, ctx) {
            o <- ctx$o
            lc <- private$.lead_calc(long, ctx)
            if (length(lc) == 0) return(NULL)
            out <- paste0("Curve: share of ", ctx$lg[1], " minus share of ", ctx$lg[2],
                          " at or below each value (percentage points). Above 0, ", ctx$lg[1],
                          " tends lower; below 0, ", ctx$lg[2], ".")
            if (isTRUE(o$leadMark))
                out <- c(out, paste0("Largest gap: ", paste0(vapply(lc, function(z)
                    paste0(z$v, " ", formatC(abs(z$d[z$k]), format = "f", digits = 0), " points at ",
                           formatC(z$grid[z$k], format = "fg", digits = 3)), ""), collapse = "; "), "."))
            out
        },

        .p_lead = function(long, ctx) {
            o <- ctx$o
            if (ctx$ng != 2) return(NULL)
            lc <- private$.lead_calc(long, ctx)
            if (length(lc) == 0) return(NULL)
            paths <- do.call(rbind, lapply(lc, function(z) {
                m <- length(z$grid)
                data.frame(variable = factor(z$v, levels = ctx$lv), x = rep(z$grid, each = 2),
                           y = c(0, rep(z$d[-m], each = 2), z$d[m]))
            }))
            L <- max(abs(paths$y), 1)
            rug <- do.call(rbind, lapply(lc, function(z) {
                rbind(data.frame(variable = factor(z$v, levels = ctx$lv), x = z$x1, g = 1L,
                                 y0 = -1.12 * L, y1 = -1.20 * L),
                      data.frame(variable = factor(z$v, levels = ctx$lv), x = z$x2, g = 2L,
                                 y0 = -1.23 * L, y1 = -1.31 * L))
            }))
            rug$grp <- factor(ctx$lg[rug$g], levels = ctx$lg)
            fills <- if (ctx$bw) c("grey35", "grey80") else unname(ctx$fills[ctx$lg[1:2]])
            pos <- transform(paths, lo = 0, hi = pmax(y, 0), grp = factor(ctx$lg[1], levels = ctx$lg))
            neg <- transform(paths, lo = pmin(y, 0), hi = 0, grp = factor(ctx$lg[2], levels = ctx$lg))
            brk <- pretty(c(-L, L), n = 5)
            brk <- brk[abs(brk) <= L * 1.001]
            p <- ggplot2::ggplot() +
                (if (ctx$bw)
                    ggplot2::geom_ribbon(data = pos, ggplot2::aes(x = x, ymin = lo, ymax = hi),
                                         fill = fills[1], alpha = ctx$alpha, colour = NA)
                 else ggplot2::geom_ribbon(data = pos, ggplot2::aes(x = x, ymin = lo, ymax = hi, fill = grp),
                                           alpha = ctx$alpha, colour = NA)) +
                (if (ctx$bw)
                    ggplot2::geom_ribbon(data = neg, ggplot2::aes(x = x, ymin = lo, ymax = hi),
                                         fill = fills[2], alpha = ctx$alpha, colour = NA)
                 else ggplot2::geom_ribbon(data = neg, ggplot2::aes(x = x, ymin = lo, ymax = hi, fill = grp),
                                           alpha = ctx$alpha, colour = NA)) +
                ggplot2::geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.4) +
                ggplot2::geom_path(data = paths, ggplot2::aes(x = x, y = y), colour = "grey25",
                                   linewidth = ctx$lw * 0.45) +
                ggplot2::geom_segment(data = rug, ggplot2::aes(x = x, xend = x, y = y0, yend = y1, colour = grp),
                                      linewidth = 0.45, alpha = 0.85)
            if (isTRUE(o$leadMark)) {
                mk <- do.call(rbind, lapply(lc, function(z)
                    data.frame(variable = factor(z$v, levels = ctx$lv), x = z$grid[z$k], y = z$d[z$k])))
                p <- p +
                    ggplot2::geom_segment(data = mk, ggplot2::aes(x = x, xend = x, y = 0, yend = y),
                                          colour = "grey15", linewidth = ctx$lw * 1.2, linetype = "dotted") +
                    ggplot2::geom_point(data = mk, ggplot2::aes(x = x, y = y), shape = 21, fill = "white",
                                        colour = "grey15", size = 2.6, stroke = 0.8)
            }
            p <- p + ggplot2::scale_y_continuous(limits = c(-1.34 * L, 1.06 * L), breaks = brk) +
                ggplot2::labs(x = ctx$vlab, y = .("Cumulative difference (points)"))
            if (ctx$n > 1)
                p <- p + ggplot2::facet_wrap(ggplot2::vars(variable), scales = "free_x",
                                             labeller = ggplot2::as_labeller(ctx$lab_strip))
            private$.flip_x(p, ctx)
        },

        # Overlap of the two groups' density curves (shared grid; the area under the lower
        # of the two curves, as a percentage).
        .overlap = function(long, ctx) {
            o <- ctx$o
            res <- lapply(ctx$lv, function(v) {
                x1 <- long$value[as.character(long$variable) == v & as.character(long$grp) == ctx$lg[1]]
                x2 <- long$value[as.character(long$variable) == v & as.character(long$grp) == ctx$lg[2]]
                if (length(unique(x1)) < 2 || length(unique(x2)) < 2) return(NULL)
                rng <- range(c(x1, x2))
                # The number is computed over the full extent of both curves (tails included);
                # only the part inside the data range is shaded, as that is what is drawn.
                pad <- 3 * max(stats::bw.nrd0(x1), stats::bw.nrd0(x2)) * o$densAdjust
                f <- function(x) {
                    d <- stats::density(x, adjust = o$densAdjust, from = rng[1] - pad, to = rng[2] + pad, n = 1024)
                    if (isTRUE(o$densTrim)) d$y[d$x < min(x) | d$x > max(x)] <- 0
                    d
                }
                d1 <- f(x1); d2 <- f(x2)
                m <- pmin(d1$y, d2$y)
                inside <- d1$x >= rng[1] & d1$x <= rng[2]
                list(v = v, pct = 100 * sum(m) * (d1$x[2] - d1$x[1]),
                     data = data.frame(variable = factor(v, levels = ctx$lv), x = d1$x[inside], m = m[inside]))
            })
            res <- Filter(Negate(is.null), res)
            if (length(res) == 0) return(NULL)
            list(data = do.call(rbind, lapply(res, `[[`, "data")),
                 pct = stats::setNames(vapply(res, `[[`, 0, "pct"), vapply(res, `[[`, "", "v")))
        },

        # ── Overlaid densities ─────────────────────────────────────────────
        .p_density = function(long, ctx) {
            o <- ctx$o
            p <- ggplot2::ggplot(long, ggplot2::aes(x = value, fill = grp, colour = grp, linetype = grp)) +
                ggplot2::geom_density(adjust = o$densAdjust, trim = o$densTrim,
                                      alpha = if (o$densFilled && !ctx$bw) ctx$alpha * 0.5 else 0,
                                      linewidth = ctx$lw) +
                ggplot2::labs(x = ctx$vlab, y = .("Density"))
            if (private$.kind == "dich" && isTRUE(o$densOverlap) && ctx$mode != "facetGroup") {
                ov <- private$.overlap(long, ctx)
                if (!is.null(ov))
                    p <- p + ggplot2::geom_ribbon(
                        data = ov$data, ggplot2::aes(x = x, ymin = 0, ymax = m), inherit.aes = FALSE,
                        fill = "grey15", alpha = 0.35, colour = NA)
            }
            if (ctx$mode == "facetGroup") {
                p <- p + ggplot2::facet_grid(rows = ggplot2::vars(variable), cols = ggplot2::vars(grp),
                                             scales = "free_y", switch = "y",
                                             labeller = ggplot2::labeller(variable = ctx$lab_strip, grp = ctx$glab_strip))
            } else if (ctx$n > 1) {
                p <- p + ggplot2::facet_wrap(ggplot2::vars(variable), scales = "free_y",
                                             labeller = ggplot2::as_labeller(ctx$lab_strip))
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
                ggplot2::labs(x = ctx$vlab, y = .("Cumulative percent"))
            if (ctx$mode == "facetGroup") {
                p <- p + ggplot2::facet_grid(rows = ggplot2::vars(variable), cols = ggplot2::vars(grp),
                                             switch = "y",
                                             labeller = ggplot2::labeller(variable = ctx$lab_strip, grp = ctx$glab_strip))
            } else if (ctx$n > 1) {
                p <- p + ggplot2::facet_wrap(ggplot2::vars(variable),
                                             labeller = ggplot2::as_labeller(ctx$lab_strip))
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
}
