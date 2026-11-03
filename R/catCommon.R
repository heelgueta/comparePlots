# =============================================================================
# catCommon.R: the code shared by the categorical analyses (distOrd for ordinal
# variables, distNom for nominal ones). Each analysis class (R/dist*.b.R) takes
# these private methods and adds three fields of its own:
#   .switches  plot key -> checkbox option (which plots the analysis offers)
#   .kind      "ordinal" or "nominal"
# This file must load before the analysis files; R loads R/ files alphabetically,
# which is why it is called catCommon.R.
# The text below is the former body of distOrd.b.R; see that file's header for the
# data model and how the plots are built.
# =============================================================================
cmp_cat_private <- function() {
    list(

        # ---------------------------------------------------------------------
        # .run(): tabulate, decide which plots can be drawn, set the state
        # ---------------------------------------------------------------------
        .run = function() {
            opts <- self$options
            vars <- opts$vars
            on   <- vapply(private$.switches, function(k) isTRUE(opts[[k]]), logical(1))

            # `hint` explains the empty state (no plot ticked); nothing else goes there
            hint <- self$results$hint
            if (length(vars) > 0 && !any(on)) {
                hint$setContent("<p>Open a plot's section below and tick its checkbox to draw it.</p>")
                hint$setVisible(TRUE)
            } else {
                hint$setVisible(FALSE)
            }
            if (length(vars) == 0 || !any(on)) return()

            # Each analysis is for one measure type: ordinal variables arrive as
            # ordered factors, nominal ones as plain factors. Say what to do instead
            # of drawing something misleading.
            is_ord <- vapply(vars, function(v) is.ordered(self$data[[v]]), NA)
            if (identical(private$.kind, "ordinal") && !all(is_ord))
                jmvcore::reject(paste0("Compare Ordinal needs ordinal variables. Set the measure type of ",
                    paste(vars[!is_ord], collapse = ", "), " to Ordinal in the Data tab, or use Compare Nominal."))
            if (identical(private$.kind, "nominal") && any(is_ord))
                jmvcore::reject(paste0("Compare Nominal needs nominal variables. Set the measure type of ",
                    paste(vars[is_ord], collapse = ", "), " to Nominal in the Data tab, or use Compare Ordinal."))

            # make sure every variable is a factor
            data <- self$data[, vars, drop = FALSE]
            for (v in vars) if (!is.factor(data[[v]])) data[[v]] <- as.factor(data[[v]])

            # Common category axis. A column's levels are only the values that occur
            # in it, so the axis has to be merged carefully (see cmp_cat_axis()).
            ax   <- cmp_cat_axis(lapply(stats::setNames(vars, vars), function(v) levels(data[[v]])))
            cats <- ax$cats
            # nominal categories have no natural order: optionally sort the shared
            # axis by overall frequency, most frequent first
            if (identical(private$.opt("catSort"), "frequency") && length(cats) > 1) {
                tot <- vapply(cats, function(cc) sum(vapply(vars, function(v) sum(as.character(data[[v]]) == cc, na.rm = TRUE), 0)), 0)
                cats <- cats[order(-tot, seq_along(cats))]
            }
            if (length(cats) == 0)
                jmvcore::reject("The selected variables have no categories.")
            if (length(ax$conflict) > 0)
                private$.warn(".catConflict", paste0(
                    "The categories of ", paste(ax$conflict, collapse = ", "),
                    " cannot be put in the same order as those of the other variables, so the shared category axis may be misordered."))
            if (length(ax$disjoint) > 0)
                private$.warn(".catDisjoint", paste0(
                    paste(ax$disjoint, collapse = ", "),
                    " share no categories with the other variables; their categories are placed on the same axis by position only."))

            # one row per (variable, category); missing answers are excluded from n
            long <- do.call(rbind, lapply(vars, function(v) {
                x <- data[[v]][!is.na(data[[v]])]
                n <- length(x)
                tab <- table(factor(as.character(x), levels = cats))
                data.frame(variable = v, category = factor(cats, levels = cats),
                           cidx = seq_along(cats), count = as.numeric(tab),
                           pct = if (n > 0) as.numeric(tab) / n * 100 else 0, n = n,
                           stringsAsFactors = FALSE)
            }))
            if (all(long$n == 0))
                jmvcore::reject("The selected variables have no non-missing values.")

            # The mean-category plot treats categories as numbers 1..k (their
            # position on the shared axis), which is only defensible for ordinal
            # variables or for two-category variables (where it is a proportion).
            # `ok` flags the variables that qualify; the others are skipped.
            ok <- vapply(vars, function(v) {
                is.ordered(self$data[[v]]) || sum(long$count[long$variable == v] > 0) == 2
            }, logical(1))
            names(ok) <- vars
            if (isTRUE(private$.opt("showMeans")) && !all(ok)) {
                private$.warn(".meansSkipped", paste0(
                    if (!any(ok)) "Mean category was not drawn: it needs ordinal variables or variables with exactly two categories. "
                    else "Mean category skips nominal variables with more than two categories: ",
                    paste(vars[!ok], collapse = ", "), "."))
            }

            # hand the (small) table to each ticked image; `ok` is read by the means plot
            for (key in names(private$.switches)[on]) {
                if (key == "means" && !any(ok)) next     # nothing to draw
                image <- self$results[[paste0("plot_", key)]]
                image$setState(list(long = long, vars = vars, cats = cats, ok = ok,
                                    labels = cmp_var_labels(self$data, vars, opts$useDesc)))
            }
        },

        # .init() runs before the first draw and before .run(); it sees the options
        # but not the data. With "Fit plot size to the number of variables" ticked the
        # plot sizes are set HERE (never in .run(), which the jamovi review forbids):
        # the size is then right from the first draw and only changes when the number
        # of variables or the grid layout changes. Without it the fixed sizes of the
        # .r.yaml apply (the grid plots still lay their charts out in `gridCols`).
        .init = function() {
            vars <- self$options$vars
            if (!isTRUE(self$options$autoFit) || length(vars) == 0) return()
            n <- length(vars)
            for (key in names(private$.switches)) {
                if (!isTRUE(self$options[[private$.switches[[key]]]])) next
                grid <- key %in% c("waffle", "picto", "parliament", "pie", "donut")
                sz <- private$.size(key, n, grid)
                self$results[[paste0("plot_", key)]]$setSize(sz[1], sz[2])
            }
        },

        # Pixel size for a plot, from the number of variables `n` (see .init()).
        .size = function(key, n, grid) {
            o <- self$options
            if (grid) {
                ncol <- max(1L, min(as.integer(o$gridCols), n))
                nrow <- ceiling(n / ncol)
                cell <- if (key == "parliament") c(190, 120) else c(165, 165)   # width, height of one chart
                return(c(max(450, ncol * cell[1] + 60), nrow * (cell[2] + 30) + 110))
            }
            if (key %in% c("profile", "means")) return(c(450, 350))
            if (identical(o$orientation, "horizontal")) c(500, max(300, 130 + 55 * n))
            else c(max(450, 120 + 85 * n), 380)
        },

        # An option that only some of the categorical analyses have (e.g. `catSort` exists
        # for nominal variables only); NULL when this analysis does not have it.
        .opt = function(name) tryCatch(self$options[[name]], error = function(e) NULL),

        # Report something about the results as a jmvcore Notice (a proper warning
        # in the results tree, in the results theme), at the top of the results.
        .warn = function(name, content) {
            notice <- jmvcore::Notice$new(
                self$options, name = name, type = jmvcore::NoticeType$WARNING, content = content)
            self$results$insert(1, notice)
        },

        # Render entry points; jamovi calls these by name (renderFun in the .r.yaml).
        .plot_bars       = function(image, ggtheme, theme, ...) private$.render(image, "bars", ggtheme, theme),
        .plot_diverging  = function(image, ggtheme, theme, ...) private$.render(image, "diverging", ggtheme, theme),
        .plot_heat       = function(image, ggtheme, theme, ...) private$.render(image, "heat", ggtheme, theme),
        .plot_waffle     = function(image, ggtheme, theme, ...) private$.render(image, "waffle", ggtheme, theme),
        .plot_picto      = function(image, ggtheme, theme, ...) private$.render(image, "picto", ggtheme, theme),
        .plot_parliament = function(image, ggtheme, theme, ...) private$.render(image, "parliament", ggtheme, theme),
        .plot_profile    = function(image, ggtheme, theme, ...) private$.render(image, "profile", ggtheme, theme),
        .plot_means      = function(image, ggtheme, theme, ...) private$.render(image, "means", ggtheme, theme),
        .plot_pie        = function(image, ggtheme, theme, ...) private$.render(image, "pie", ggtheme, theme),
        .plot_donut      = function(image, ggtheme, theme, ...) private$.render(image, "donut", ggtheme, theme),
        .plot_grouped    = function(image, ggtheme, theme, ...) private$.render(image, "grouped", ggtheme, theme),
        .plot_lollipop   = function(image, ggtheme, theme, ...) private$.render(image, "lollipop", ggtheme, theme),

        # Read the state, build, print. FALSE = nothing drawn, TRUE = drawn.
        # Warnings are muted: they would only add noise to the results.
        .render = function(image, key, ggtheme = NULL, theme = NULL) {
            st <- image$state
            if (is.null(st)) return(FALSE)
            st$width <- image$width          # lets the plot decide how many legend rows it needs
            p <- private$.build(key, st, ggtheme, theme)
            if (is.null(p)) return(FALSE)
            suppressWarnings(suppressMessages(print(p)))
            TRUE
        },

        # ---------------------------------------------------------------------
        # .context(): what all plots share
        #   lv        variable names in drawing order (sorted, then maybe inverted)
        #   pos       variable -> position on the variable axis (see header)
        #   labs      axis labels for the variables (wrapped; with n if requested)
        #   cat_cols  category -> colour, from the chosen palette (used by bars,
        #             waffle, ... where COLOUR = CATEGORY)
        #   var_cols  variable -> colour (used by profile and means, where
        #             COLOUR = VARIABLE); `spread` drops the pale middle of
        #             diverging palettes so lines/points stay visible
        #   leg_rows  number of legend rows so a long legend wraps
        # ---------------------------------------------------------------------
        .context = function(st, ggtheme = NULL, theme = NULL) {
            o     <- self$options
            long  <- st$long
            vars  <- st$vars
            cats  <- st$cats
            k     <- length(cats)
            horiz <- identical(o$orientation, "horizontal")

            # "Order variables by": entry order, name, or a statistic of the category
            # distribution (lowest first, or highest first for the % options).
            # "Reverse the order" then flips whichever order was chosen.
            # Categories are coded 1..k by their position on the shared axis.
            stat_of <- function(f) vapply(vars, function(v) f(long[long$variable == v, ]), 0)
            sp <- cmp_split_cats(cats, !identical(private$.opt("divNeutral"), "none"))
            stat <- switch(o$sortVars,
                mean   = stat_of(function(d) sum(d$cidx * d$count) / max(sum(d$count), 1)),
                median = stat_of(function(d) { cs <- cumsum(d$pct); d$cidx[which(cs >= 50 - 1e-9)[1]] }),
                first  = -stat_of(function(d) d$pct[d$cidx == 1]),     # most in first category on top
                last   = -stat_of(function(d) d$pct[d$cidx == k]),
                net    = -stat_of(function(d) sum(d$pct[d$category %in% sp$pos]) - sum(d$pct[d$category %in% sp$neg])),
                top    = -stat_of(function(d) max(d$pct)),                 # most concentrated first
                diverse = -stat_of(function(d) { p <- d$pct[d$pct > 0] / 100; -sum(p * log(p)) }),   # most diverse first
                NULL)
            ord <- cmp_order_vars(vars, o$sortVars, stat)
            lv <- if (o$invertVars) rev(ord) else ord
            n  <- length(lv)
            # first variable on top (horizontal, after coord_flip) or on the left (vertical)
            pos <- if (horiz) rev(seq_len(n)) else seq_len(n)
            names(pos) <- lv

            npv <- tapply(long$n, long$variable, function(z) z[1])   # valid n per variable
            lab  <- st$labels[lv]              # name or description (option), already wrapped
            labs <- if (o$showN) paste0(gsub("\n", " ", lab), " (n=", npv[lv], ")") else lab

            cat_cols <- cmp_palette(o$colourPalette, k, o$reversePalette, theme = theme)
            names(cat_cols) <- cats
            var_cols <- cmp_palette(o$colourPalette, n, o$reversePalette, spread = TRUE, theme = theme)
            names(var_cols) <- lv

            # legend wraps onto several rows when it would be wider than the plot
            leg_rows <- if (o$legendPos %in% c("bottom", "top"))
                max(1L, ceiling(k / max(2, floor(st$width / 130)))) else NULL

            list(o = o, horiz = horiz, lv = lv, n = n, pos = pos, k = k, cats = cats,
                 labs = cmp_wrap(labs), lab = lab, gcols = max(1L, as.integer(o$gridCols)),
                 lab_grid = stats::setNames(cmp_wrap(gsub("\n", " ", lab), 20), names(lab)),
                 cat_cols = cat_cols, var_cols = var_cols,
                 leg_rows = leg_rows, npv = npv, ggtheme = ggtheme)
        },

        # Build the ggplot for one key: prepare the data (+ `pos`), dispatch to the
        # `.p_<key>` builder, then apply the common theme in `.finish()`.
        .build = function(key, st, ggtheme = NULL, theme = NULL) {
            skipped <- character(0)
            if (key == "means") {
                # keep only the variables that qualify (see .run); rebuild the
                # state around them so positions have no gaps
                keep <- st$vars[st$ok[st$vars]]
                if (length(keep) == 0) return(NULL)
                skipped <- setdiff(st$vars, keep)
                st$vars <- keep
                st$long <- st$long[st$long$variable %in% keep, ]
            }
            ctx <- private$.context(st, ggtheme, theme)
            long <- st$long
            long$variable <- factor(long$variable, levels = ctx$lv)
            long$pos <- unname(ctx$pos[as.character(long$variable)])
            p <- switch(key,
                bars       = private$.p_bars(long, ctx),
                diverging  = private$.p_diverging(long, ctx),
                heat       = private$.p_heat(long, ctx),
                waffle     = private$.p_dots(long, ctx, "waffle"),
                picto      = private$.p_dots(long, ctx, "picto"),
                parliament = private$.p_parliament(long, ctx),
                profile    = private$.p_profile(long, ctx),
                means      = private$.p_means(long, ctx, st, skipped),
                pie        = private$.p_pie(long, ctx, FALSE),
                donut      = private$.p_pie(long, ctx, TRUE),
                grouped    = private$.p_grouped(long, ctx),
                lollipop   = private$.p_lollipop(long, ctx))
            if (is.null(p)) return(NULL)
            private$.finish(p, ctx, key)
        },

        # Theme, legend, title: common to all plots. IMPORTANT: a complete ggplot
        # theme (theme_minimal() ...) *replaces* theme() tweaks added before it, so
        # every structural tweak is applied here, after the complete theme.
        .finish = function(p, ctx, key) {
            o <- ctx$o
            legend_key <- key != "means"     # the means plot has no legend: variables are on the axis
            blank <- ggplot2::element_blank()
            # per-plot structural tweaks
            extra <- switch(key,
                bars = , diverging = , means = , grouped = , lollipop = private$.grid_theme(ctx),
                heat = ggplot2::theme(panel.grid = blank),
                waffle = , picto = , parliament = , pie = , donut = ggplot2::theme(
                    panel.grid = blank, axis.ticks = blank, axis.title = blank, axis.text = blank,
                    panel.border = blank, strip.background = blank, strip.text = ggplot2::element_text(size = ggplot2::rel(0.9)),
                    panel.spacing = ggplot2::unit(0.6, "lines")),
                NULL)
            p + cmp_theme(o$plotTheme, o$textSize, ctx$ggtheme) + extra +
                ggplot2::theme(
                    legend.position    = if (legend_key) o$legendPos else "none",
                    legend.title       = if (nchar(trimws(o$legendTitle)) > 0) ggplot2::element_text() else ggplot2::element_blank(),
                    plot.title         = ggplot2::element_text(face = "bold", hjust = 0.5),
                    panel.grid.minor   = ggplot2::element_blank(),
                    plot.caption       = ggplot2::element_text(size = ggplot2::rel(0.7), colour = "grey45", hjust = 0),
                    plot.margin        = ggplot2::margin(10, 16, 8, 10)) +
                ggplot2::labs(title = cmp_title(o$title))
        },

        # ---------------------------------------------------------------------
        # Small building blocks
        # ---------------------------------------------------------------------

        # legend title: NULL (no title) when the option is empty
        .leg_title = function(ctx) cmp_title(ctx$o$legendTitle),

        # fill scale mapping category -> colour, with the legend wrapped in rows
        # (`drop = FALSE` keeps categories with 0% in the legend)
        .fill_scale = function(ctx) {
            ggplot2::scale_fill_manual(
                values = ctx$cat_cols, name = private$.leg_title(ctx), drop = FALSE,
                guide = ggplot2::guide_legend(nrow = ctx$leg_rows))
        },

        # "12%" with the number of decimals chosen by the user
        .pct_fmt = function(x, ctx) paste0(formatC(x, format = "f", digits = ctx$o$decimals), "%"),

        # variable axis: numeric positions labelled with the variable names
        .cat_axis = function(ctx, upper = 0.6) {
            ggplot2::scale_x_continuous(breaks = unname(ctx$pos), labels = ctx$labs,
                                        expand = ggplot2::expansion(add = c(0.6, upper)))
        },

        # Flip the canvas for the horizontal orientation. `clip = "off"` would let
        # labels spill outside the panel (not used at the moment).
        .flip = function(p, ctx, clip = "on") {
            if (ctx$horiz) p + ggplot2::coord_flip(clip = clip)
            else p + ggplot2::coord_cartesian(clip = clip)
        },

        # grid lines along the percentage axis only (none along the variable axis);
        # after coord_flip the theme's x/y refer to the screen axes
        .grid_theme = function(ctx) {
            ggplot2::theme(
                panel.grid.major.x = if (ctx$horiz) ggplot2::element_line(colour = "grey90", linewidth = 0.35) else ggplot2::element_blank(),
                panel.grid.major.y = if (ctx$horiz) ggplot2::element_blank() else ggplot2::element_line(colour = "grey90", linewidth = 0.35))
        },

        # ── Stacked bars (100%) ────────────────────────────────────────────
        # Each variable is one stacked bar. Segments are drawn as rectangles
        # (geom_rect) from explicit lo/hi values instead of geom_col + position_stack:
        # that way the labels use exactly the same geometry, and labels that are
        # hidden for being too small cannot shift the others.
        .p_bars = function(long, ctx) {
            o <- ctx$o
            bw <- o$barWidth / 100
            long <- long[order(long$variable, long$cidx), ]
            long$hi <- ave(long$pct, long$variable, FUN = cumsum)   # running total = segment end
            long$lo <- long$hi - long$pct                           # segment start
            p <- ggplot2::ggplot(long) +
                ggplot2::geom_rect(
                    ggplot2::aes(xmin = pos - bw / 2, xmax = pos + bw / 2, ymin = lo, ymax = hi, fill = category),
                    colour = "grey35", linewidth = 0.2)
            if (o$barLabels) {
                ld <- long[long$pct >= max(o$barMinLabel, 0.01), ]     # skip tiny segments
                ld$label <- private$.pct_fmt(ld$pct, ctx)
                # black or white text depending on the fill it sits on
                ld$tc <- cmp_text_col(ctx$cat_cols[as.character(ld$category)])
                p <- p + ggplot2::geom_text(
                    data = ld, ggplot2::aes(x = pos, y = (lo + hi) / 2, label = label, colour = tc),
                    size = o$textSize * 0.28, fontface = "bold", show.legend = FALSE) +
                    ggplot2::scale_colour_identity()    # `tc` already holds colour names
            }
            p <- p + private$.cat_axis(ctx) +
                ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25),
                                            labels = function(x) paste0(x, "%"),
                                            expand = ggplot2::expansion(0)) +
                private$.fill_scale(ctx) + ggplot2::labs(x = NULL, y = NULL)
            private$.flip(p, ctx)
        },

        # ── Diverging bars (Likert style) ──────────────────────────────────
        # Categories are split into a negative side (drawn leftwards from 0), an
        # optional neutral middle category (centred on 0) and a positive side.
        #   "Centre"       : with an odd number (>= 3) of categories the middle one is
        #                    the neutral band, centred on 0.
        #   "Never centre" : no neutral band; the first floor(k/2) categories go to
        #                    the negative side and the rest (middle included) to
        #                    the positive side.
        # Segments on each side are stacked outward starting next to the centre,
        # so the category closest to neutral is always next to the middle.
        .p_diverging = function(long, ctx) {
            o <- ctx$o
            cats <- ctx$cats
            k <- length(cats)
            if (k < 2) return(NULL)                 # needs at least two categories
            sp <- cmp_split_cats(cats, o$divNeutral == "auto")
            neg <- sp$neg; neu <- sp$neu; pos <- sp$pos

            # per variable: start (lo) and end (hi) of every segment on the % axis
            rows <- lapply(ctx$lv, function(v) {
                d <- long[long$variable == v, ]
                pc <- function(cat) d$pct[d$category == cat]    # percent of one category
                off <- if (length(neu)) pc(neu) / 2 else 0      # half the neutral band
                out <- list()
                if (length(neu))
                    out[[1]] <- data.frame(category = neu, lo = -off, hi = off, pct = pc(neu))
                cum <- off
                for (cat in rev(neg)) {                    # nearest to the centre first
                    out[[length(out) + 1]] <- data.frame(category = cat, lo = -(cum + pc(cat)), hi = -cum, pct = pc(cat))
                    cum <- cum + pc(cat)
                }
                cum <- off
                for (cat in pos) {
                    out[[length(out) + 1]] <- data.frame(category = cat, lo = cum, hi = cum + pc(cat), pct = pc(cat))
                    cum <- cum + pc(cat)
                }
                r <- do.call(rbind, out)
                r$variable <- v
                r$pos <- unname(ctx$pos[v])
                # totals of each side (neutral excluded), shown by "net %"
                r$neg_tot <- sum(d$pct[d$category %in% neg])
                r$pos_tot <- sum(d$pct[d$category %in% pos])
                r
            })
            dv <- do.call(rbind, rows)
            dv$category <- factor(dv$category, levels = cats)
            # symmetric axis, rounded up to a multiple of 10; extra room for the net labels
            ext <- max(abs(c(dv$lo, dv$hi)))
            ext <- max(10, ceiling(ext / 10) * 10)
            lim <- ext + if (o$divNet) 14 else 0
            bw <- o$divWidth / 100

            p <- ggplot2::ggplot(dv) +
                ggplot2::geom_rect(
                    ggplot2::aes(xmin = pos - bw / 2, xmax = pos + bw / 2, ymin = lo, ymax = hi, fill = category),
                    colour = "grey35", linewidth = 0.2) +
                ggplot2::geom_hline(yintercept = 0, colour = "grey25", linewidth = 0.5)   # centre line
            if (o$divLabels) {
                ld <- dv[dv$pct >= max(o$divMinLabel, 0.01), ]
                ld$label <- private$.pct_fmt(ld$pct, ctx)
                ld$tc <- cmp_text_col(ctx$cat_cols[as.character(ld$category)])
                p <- p + ggplot2::geom_text(
                    data = ld, ggplot2::aes(x = pos, y = (lo + hi) / 2, label = label, colour = tc),
                    size = o$textSize * 0.28, fontface = "bold", show.legend = FALSE) +
                    ggplot2::scale_colour_identity()
            }
            if (o$divNet) {
                # italic totals just outside each end of the bars
                nt <- unique(dv[, c("pos", "neg_tot", "pos_tot")])
                p <- p +
                    ggplot2::geom_text(data = nt, ggplot2::aes(x = pos, y = -(ext + 7), label = private$.pct_fmt(neg_tot, ctx)),
                                       size = o$textSize * 0.28, colour = "grey25", fontface = "italic") +
                    ggplot2::geom_text(data = nt, ggplot2::aes(x = pos, y = ext + 7, label = private$.pct_fmt(pos_tot, ctx)),
                                       size = o$textSize * 0.28, colour = "grey25", fontface = "italic")
            }
            brk <- pretty(c(-ext, ext), n = 8)
            brk <- brk[abs(brk) <= ext]       # no tick labels in the room reserved for net labels
            p <- p + private$.cat_axis(ctx) +
                ggplot2::scale_y_continuous(limits = c(-lim, lim), breaks = brk,
                                            labels = function(x) paste0(abs(x), "%"),   # no minus signs
                                            expand = ggplot2::expansion(0)) +
                private$.fill_scale(ctx) + ggplot2::labs(x = NULL, y = NULL)
            private$.flip(p, ctx)
        },

        # ── Heatmap ────────────────────────────────────────────────────────
        # Tiles: variable x category, filled by percentage. The category axis is
        # the canvas y; first category left (horizontal) or top (vertical).
        .p_heat = function(long, ctx) {
            o <- ctx$o
            k <- ctx$k
            # option value -> hcl.colors palette name (Mako/Rocket come from viridisLite)
            hm <- c(blues = "Blues 3", greens = "Greens 3", reds = "Reds 3", purples = "Purples 3",
                    oranges = "Oranges", ylorrd = "YlOrRd", mako = "Mako", rocket = "Rocket", grays = "Grays")
            hp <- hm[[o$heatPalette]]
            # 9 colours from light (low %) to dark (high %)
            cols <- if (hp %in% c("Mako", "Rocket")) rev(viridisLite::viridis(9, option = tolower(hp)))
                    else grDevices::hcl.colors(9, hp, rev = TRUE)
            top <- max(long$pct)     # colour scale runs from 0 to the largest percentage
            long$y <- if (ctx$horiz) long$cidx else k - long$cidx + 1
            gap <- o$heatGap / 100
            p <- ggplot2::ggplot(long, ggplot2::aes(x = pos, y = y, fill = pct)) +
                ggplot2::geom_tile(width = 1 - gap, height = 1 - gap)
            if (o$heatLabels) {
                # label colour chosen from the tile colour the value maps to
                tcol <- cmp_text_col(scales::col_numeric(cols, c(0, top))(long$pct))
                long$label <- private$.pct_fmt(long$pct, ctx)
                p <- p + ggplot2::geom_text(
                    data = long, ggplot2::aes(label = label), colour = tcol,
                    size = o$textSize * 0.28, show.legend = FALSE)
            }
            p <- p + private$.cat_axis(ctx) +
                ggplot2::scale_y_continuous(breaks = if (ctx$horiz) seq_len(k) else k - seq_len(k) + 1,
                                            labels = cmp_wrap(ctx$cats, 14), expand = ggplot2::expansion(add = 0.5)) +
                ggplot2::scale_fill_gradientn(colours = cols, limits = c(0, top),
                                              name = if (is.null(private$.leg_title(ctx))) "%" else private$.leg_title(ctx),
                                              guide = ggplot2::guide_colourbar(barwidth = ggplot2::unit(9, "lines")),
                                              labels = function(x) paste0(x, "%")) +
                ggplot2::labs(x = NULL, y = NULL)
            private$.flip(p, ctx)
        },

        # ── Waffle and pictogram ───────────────────────────────────────────
        # One rows x cols grid of cells per variable; every cell stands for
        # 100 / (rows * cols) percent (1% for the 10 x 10 default). The number of
        # cells given to each category is the percentage rounded with the
        # largest-remainder method (cmp_alloc) so the grid is always exactly full.
        # Cells are filled row by row from the top-left corner in category order.
        # Each variable is one facet of a grid of charts (`gridCols` per row; the
        # variable's name or description is the label under the chart), so the
        # charts get as big as the plot allows. `kind` is "waffle" (squares) or
        # "picto" (dots/other shapes).
        .p_dots = function(long, ctx, kind) {
            o <- ctx$o
            nrow_ <- if (kind == "waffle") o$waffleRows else o$pictoRows
            ncol_ <- if (kind == "waffle") o$waffleCols else o$pictoCols
            cells <- nrow_ * ncol_
            df <- do.call(rbind, lapply(ctx$lv, function(v) {
                d <- long[long$variable == v, ]
                cnt <- cmp_alloc(d$pct, cells)         # cells per category, sums to `cells`
                i <- seq_len(cells) - 1L              # cell index 0..cells-1
                data.frame(variable = factor(v, levels = ctx$lv), cx = i %% ncol_, cy = (nrow_ - 1) - i %/% ncol_,
                           category = factor(rep(as.character(d$category), cnt), levels = ctx$cats))
            }))
            unit <- 100 / cells
            unit_lab <- paste0("1 ", if (kind == "waffle") "square" else "dot", " = ",
                               format(round(unit, 2), nsmall = 0), "%")
            if (kind == "waffle") {
                s <- 1 - o$waffleGap / 100            # tile size < 1 leaves the gap
                p <- ggplot2::ggplot(df, ggplot2::aes(x = cx, y = cy, fill = category)) +
                    ggplot2::geom_tile(width = s, height = s, colour = "grey55", linewidth = 0.2) +
                    private$.fill_scale(ctx)
            } else {
                # shapes 21-24 are filled shapes with a separate outline, so pale
                # categories (e.g. a neutral middle) stay visible on white
                shp <- c(circle = 21, square = 22, diamond = 23, triangle = 24)[[o$pictoShape]]
                p <- ggplot2::ggplot(df, ggplot2::aes(x = cx, y = cy, fill = category)) +
                    ggplot2::geom_point(shape = shp, size = o$pictoSize, colour = "grey45", stroke = 0.25) +
                    ggplot2::scale_fill_manual(
                        values = ctx$cat_cols, name = private$.leg_title(ctx), drop = FALSE,
                        guide = ggplot2::guide_legend(nrow = ctx$leg_rows, override.aes = list(size = 4, shape = 21)))
            }
            p <- p + private$.chart_grid(ctx)
            p + ggplot2::labs(caption = unit_lab)     # tells the reader what one cell is worth
        },

        # ── Parliament (arc) chart ─────────────────────────────────────────
        # Seats on concentric half-circles, one chart per variable. Seats are
        # shared among the rows in proportion to the row's radius (longer arcs get
        # more), then sorted by angle from left to right so each category forms a
        # wedge, as in parliament diagrams. Categories get seats by cmp_alloc().
        .p_parliament = function(long, ctx) {
            o <- ctx$o
            nrow_ <- o$parlRows
            radii <- seq(0.8, 1.8, length.out = max(nrow_, 2))[seq_len(nrow_)]
            if (nrow_ == 1) radii <- 1.5
            per_row <- cmp_alloc(radii, o$parlSeats)       # seats on each row
            seats <- do.call(rbind, lapply(seq_len(nrow_), function(r) {
                m <- per_row[r]
                if (m == 0) return(NULL)
                a <- if (m == 1) pi / 2 else seq(pi * 0.97, pi * 0.03, length.out = m)   # angles, left to right
                data.frame(ang = a, rad = radii[r])
            }))
            seats <- seats[order(-seats$ang, seats$rad), ]       # sweep left to right
            df <- do.call(rbind, lapply(ctx$lv, function(v) {
                d <- long[long$variable == v, ]
                cnt <- cmp_alloc(d$pct, nrow(seats))              # seats per category
                data.frame(variable = factor(v, levels = ctx$lv), cx = seats$rad * cos(seats$ang), cy = seats$rad * sin(seats$ang),
                           category = factor(rep(as.character(d$category), cnt), levels = ctx$cats))
            }))
            p <- ggplot2::ggplot(df, ggplot2::aes(x = cx, y = cy, fill = category)) +
                ggplot2::geom_point(size = o$parlSize, shape = 21, colour = "grey45", stroke = 0.25) +
                ggplot2::scale_fill_manual(
                    values = ctx$cat_cols, name = private$.leg_title(ctx), drop = FALSE,
                    guide = ggplot2::guide_legend(nrow = ctx$leg_rows, override.aes = list(size = 4)))
            p + private$.chart_grid(ctx)
        },

        # Shared by waffle / pictogram / parliament / pie / donut: the variables are
        # the panels of one grid of charts (`gridCols` per row), labelled under each
        # chart, with equal aspect ratio and no axes.
        .chart_grid = function(ctx, equal = TRUE) {
            list(
                ggplot2::facet_wrap(ggplot2::vars(variable), ncol = ctx$gcols, strip.position = "bottom",
                                    labeller = ggplot2::as_labeller(ctx$lab_grid)),
                if (equal) ggplot2::coord_equal(),
                ggplot2::scale_x_continuous(breaks = NULL, expand = ggplot2::expansion(add = 0.6)),
                ggplot2::scale_y_continuous(breaks = NULL, expand = ggplot2::expansion(add = 0.6)),
                ggplot2::labs(x = NULL, y = NULL))
        },

        # ── Pie and donut ──────────────────────────────────────────────────
        # One pie (donut = TRUE: with a hole, `donutHole` % of the radius, and
        # optionally n in the middle) per variable, in the same grid of charts as
        # the waffles. Slices are stacked rectangles bent into a circle by
        # coord_polar; they start at 12 o'clock and run clockwise in category order.
        .p_pie = function(long, ctx, donut = FALSE) {
            o <- ctx$o
            long <- long[order(long$variable, long$cidx), ]
            long$hi <- ave(long$pct, long$variable, FUN = cumsum)
            long$lo <- long$hi - long$pct
            r0 <- if (donut) max(0.05, min(o$donutHole / 100, 0.9)) else 0
            show_labels <- if (donut) isTRUE(o$donutLabels) else isTRUE(o$pieLabels)
            min_label   <- if (donut) o$donutMinLabel else o$pieMinLabel
            p <- ggplot2::ggplot(long) +
                ggplot2::geom_rect(ggplot2::aes(xmin = r0, xmax = 1, ymin = lo, ymax = hi, fill = category),
                                   colour = "grey35", linewidth = 0.2)
            if (show_labels) {
                ld <- long[long$pct >= max(min_label, 0.01), ]
                ld$label <- private$.pct_fmt(ld$pct, ctx)
                ld$tc <- cmp_text_col(ctx$cat_cols[as.character(ld$category)])
                ld$xm <- if (donut) (r0 + 1) / 2 else 0.62      # pie labels sit a bit outside the middle of the radius
                p <- p + ggplot2::geom_text(
                    data = ld, ggplot2::aes(x = xm, y = (lo + hi) / 2, label = label, colour = tc),
                    size = o$textSize * 0.24, fontface = "bold", show.legend = FALSE) +
                    ggplot2::scale_colour_identity()
            }
            if (donut && isTRUE(o$donutN)) {
                nt <- unique(data.frame(variable = long$variable, n = unname(ctx$npv[as.character(long$variable)])))
                p <- p + ggplot2::geom_text(data = nt, ggplot2::aes(x = 0, y = 0, label = paste0("n=", n)),
                                            size = o$textSize * 0.24, colour = "grey25")
            }
            p + ggplot2::coord_polar(theta = "y", start = 0) +
                ggplot2::scale_x_continuous(limits = c(0, 1), expand = ggplot2::expansion(0)) +
                ggplot2::scale_y_continuous(limits = c(0, 100), expand = ggplot2::expansion(0)) +
                ggplot2::facet_wrap(ggplot2::vars(variable), ncol = ctx$gcols, strip.position = "bottom",
                                    labeller = ggplot2::as_labeller(ctx$lab_grid)) +
                private$.fill_scale(ctx) + ggplot2::labs(x = NULL, y = NULL)
        },

        # ── Grouped bars ───────────────────────────────────────────────────
        # Categories on one axis, one bar per variable inside each category
        # (colour = variable). The easiest way to compare how often each category
        # was chosen across variables. When horizontal the first category is on
        # top and, inside a category, the first variable is on top.
        .p_grouped = function(long, ctx) {
            o <- ctx$o
            k <- ctx$k
            long$x <- if (ctx$horiz) k - long$cidx + 1 else long$cidx
            long$variable <- factor(as.character(long$variable), levels = if (ctx$horiz) rev(ctx$lv) else ctx$lv)
            dodge <- ggplot2::position_dodge(width = 0.85)
            p <- ggplot2::ggplot(long, ggplot2::aes(x = x, y = pct, fill = variable)) +
                ggplot2::geom_col(position = dodge, width = o$groupWidth / 100 * 0.85, colour = "grey35", linewidth = 0.2)
            if (isTRUE(o$groupLabels)) {
                ld <- long[long$pct > 0, ]
                ld$label <- private$.pct_fmt(ld$pct, ctx)
                p <- p + ggplot2::geom_text(
                    data = ld, ggplot2::aes(label = label, group = variable), position = dodge,
                    size = o$textSize * 0.22, colour = "grey20",
                    hjust = if (ctx$horiz) -0.15 else 0.5, vjust = if (ctx$horiz) 0.5 else -0.4)
            }
            p <- p +
                ggplot2::scale_x_continuous(breaks = if (ctx$horiz) k - seq_len(k) + 1 else seq_len(k),
                                            labels = cmp_wrap(ctx$cats, 14), expand = ggplot2::expansion(add = 0.5)) +
                ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"), expand = ggplot2::expansion(mult = c(0, 0.15))) +
                private$.var_fill_scale(ctx) + ggplot2::labs(x = NULL, y = NULL)
            private$.flip(p, ctx)
        },

        # ── Lollipop (dot) plot ────────────────────────────────────────────
        # Same layout as the grouped bars, but each value is a dot on a thin stem
        # (colour = variable): less ink, and the dots of different variables line
        # up for an easy comparison.
        .p_lollipop = function(long, ctx) {
            o <- ctx$o
            k <- ctx$k
            long$x <- if (ctx$horiz) k - long$cidx + 1 else long$cidx
            long$variable <- factor(as.character(long$variable), levels = if (ctx$horiz) rev(ctx$lv) else ctx$lv)
            dodge <- ggplot2::position_dodge(width = 0.6)
            p <- ggplot2::ggplot(long, ggplot2::aes(x = x, y = pct, colour = variable, group = variable)) +
                ggplot2::geom_linerange(ggplot2::aes(ymin = 0, ymax = pct), position = dodge, linewidth = 0.8) +
                ggplot2::geom_point(position = dodge, size = o$lolliSize, shape = 16) +
                ggplot2::scale_x_continuous(breaks = if (ctx$horiz) k - seq_len(k) + 1 else seq_len(k),
                                            labels = cmp_wrap(ctx$cats, 14), expand = ggplot2::expansion(add = 0.5)) +
                ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"), expand = ggplot2::expansion(mult = c(0.02, 0.1))) +
                ggplot2::scale_colour_manual(values = ctx$var_cols, breaks = ctx$lv, labels = ctx$lab,
                                             name = private$.leg_title(ctx),
                                             guide = ggplot2::guide_legend(nrow = if (ctx$o$legendPos %in% c("bottom", "top")) max(1L, ceiling(ctx$n / 3)) else NULL)) +
                ggplot2::labs(x = NULL, y = NULL)
            private$.flip(p, ctx)
        },

        # fill scale mapping VARIABLE -> colour (grouped bars), legend wrapped in rows
        .var_fill_scale = function(ctx) {
            ggplot2::scale_fill_manual(
                values = ctx$var_cols, breaks = ctx$lv, labels = ctx$lab, name = private$.leg_title(ctx),
                guide = ggplot2::guide_legend(nrow = if (ctx$o$legendPos %in% c("bottom", "top")) max(1L, ceiling(ctx$n / 3)) else NULL))
        },

        # ── Response profile (lines) ───────────────────────────────────────
        # x = category, y = percent, one line per variable (colour = variable).
        # Horizontal puts the percentage on the horizontal axis (coord_flip), with
        # the first category at the top, hence the reversed x below.
        .p_profile = function(long, ctx) {
            o <- ctx$o
            k <- ctx$k
            long$x <- if (ctx$horiz) k - long$cidx + 1 else long$cidx
            long <- long[order(long$variable, long$cidx), ]    # lines must be drawn in category order
            p <- ggplot2::ggplot(long, ggplot2::aes(x = x, y = pct, colour = variable, group = variable))
            if (o$profLines)  p <- p + ggplot2::geom_line(linewidth = 1.1)
            if (o$profPoints) p <- p + ggplot2::geom_point(size = 2.6)
            if (o$profLabels)
                p <- p + ggplot2::geom_text(ggplot2::aes(label = private$.pct_fmt(pct, ctx)),
                                            nudge_x = 0.0, vjust = -0.9, size = o$textSize * 0.26, show.legend = FALSE)
            p <- p +
                ggplot2::scale_x_continuous(breaks = if (ctx$horiz) k - seq_len(k) + 1 else seq_len(k),
                                            labels = cmp_wrap(ctx$cats, 14),
                                            expand = ggplot2::expansion(add = 0.4)) +
                ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"),
                                            expand = ggplot2::expansion(mult = c(0.02, 0.12))) +
                ggplot2::scale_colour_manual(values = ctx$var_cols, labels = ctx$lab, name = private$.leg_title(ctx),
                                             guide = ggplot2::guide_legend(nrow = if (ctx$o$legendPos %in% c("bottom", "top")) max(1L, ceiling(ctx$n / 5)) else NULL)) +
                ggplot2::labs(x = NULL, y = "Responses")
            private$.flip(p, ctx)
        },

        # ── Mean category with CI (categories coded 1..k) ──────────────────
        # Only called with the variables that qualified (ordinal or two-category;
        # see .run / .build). For each variable: the mean of the category codes
        # and a t-based confidence interval, computed from the counts expanded
        # back to one code per respondent. `skipped` lists variables left out,
        # which are named in the caption.
        .p_means = function(long, ctx, st, skipped = character(0)) {
            o <- ctx$o
            k <- ctx$k
            level_ci <- o$meansLevel / 100
            s <- do.call(rbind, lapply(ctx$lv, function(v) {
                d <- long[long$variable == v, ]
                x <- rep(d$cidx, d$count)          # one code (1..k) per respondent
                n <- length(x)
                m <- mean(x)
                se <- if (n > 1) stats::sd(x) / sqrt(n) else 0
                tq <- if (n > 1) stats::qt(1 - (1 - level_ci) / 2, n - 1) else 0
                data.frame(variable = factor(v, levels = ctx$lv), pos = unname(ctx$pos[v]),
                           m = m, lo = m - tq * se, hi = m + tq * se)
            }))
            p <- ggplot2::ggplot(s)
            if (o$meansCi)
                p <- p + ggplot2::geom_errorbar(ggplot2::aes(x = pos, ymin = lo, ymax = hi, colour = variable),
                                                width = 0.25, linewidth = 1)
            p <- p + ggplot2::geom_point(ggplot2::aes(x = pos, y = m, fill = variable),
                                         shape = 21, size = 3.8, colour = "grey15", stroke = 0.8)
            if (o$meansLabels)
                p <- p + ggplot2::geom_text(ggplot2::aes(x = pos, y = m, label = formatC(m, format = "f", digits = 2)),
                                            nudge_x = 0.3, size = o$textSize * 0.28, colour = "grey20")
            # the value axis is labelled with the category names at 1..k
            p <- p + private$.cat_axis(ctx) +
                ggplot2::scale_y_continuous(breaks = seq_len(k), labels = cmp_wrap(ctx$cats, 14),
                                            limits = c(1, k), expand = ggplot2::expansion(mult = 0.05)) +
                ggplot2::scale_colour_manual(values = ctx$var_cols, guide = "none") +
                ggplot2::scale_fill_manual(values = ctx$var_cols, guide = "none") +
                ggplot2::labs(x = NULL, y = NULL,
                              caption = paste0("Categories coded 1 to ", k, " in the order shown",
                                               if (o$meansCi) paste0("; bars: ", o$meansLevel, "% CI") else "",
                                               if (length(skipped)) paste0("\nNot shown (nominal, more than two categories): ",
                                                                           paste(skipped, collapse = ", ")) else ""))
            private$.flip(p, ctx)
        }
    )
}
