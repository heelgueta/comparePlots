# =============================================================================
# distOrd: "Comparison of Ordinal/Nominal Variables"
#
# Implementation of the analysis. Options, results and UI are defined in
# jamovi/distOrd.{a,r,u}.yaml; jmvtools generates R/distOrd.h.R (do not edit)
# which provides `distOrdOptions`, `distOrdResults` and `distOrdBase`.
#
# HOW AN ANALYSIS RUNS IN JAMOVI (same lifecycle as distNum.b.R)
#   1. `.run()` runs when options/data change. It tabulates the data and hands
#      the result to each ticked plot with `image$setState()`. No drawing.
#   2. jamovi then calls each image's render function (`.plot_<key>`), which
#      reads `image$state` and prints a ggplot2 object. Data are not available
#      at render time, so everything the plots need is in the state.
#
# DATA MODEL
#   `long` has one row per (variable, category): the variable, the category as
#   a factor, its index `cidx` (1..k, the order of the levels), the raw `count`,
#   `pct` (percent of that variable's valid answers) and `n`. All variables are
#   put on the SAME category axis: the union of their levels, in order of first
#   appearance. A variable that lacks a category simply has 0% there.
#
# HOW THE PLOTS ARE BUILT
#   Bar-like plots (stacked, diverging, heatmap, means, profile) use a *canvas*:
#   x = position of the variable (1, 2, 3, ...), y = percentage (or category).
#   For "horizontal" the canvas is flipped with coord_flip(), which makes the
#   bars run left to right and stacks the variables top to bottom. `.context()`
#   assigns the positions; the first variable gets the highest position when
#   horizontal so that it ends up on top after the flip.
#   Grid-like plots (waffle, pictogram, parliament) do not use the canvas: they
#   are laid out by hand on a plain x/y plane (see `.p_dots`, `.p_parliament`).
#   In all cases orientation decides whether the charts of different variables
#   are stacked top to bottom (horizontal) or placed side by side (vertical).
#
# NOTE ON OPTION NAMES
#   jmvcore reserves `theme` and `palette` for jamovi's global plot settings, so
#   the options here are `plotTheme` and `colourPalette`. Do not rename them back.
#
# Shared helpers (palettes, themes, text colour, rounding) are in R/utils.R.
# =============================================================================
distOrdClass <- R6::R6Class(
    "distOrdClass",
    inherit = distOrdBase,
    private = list(

        # Plot key -> checkbox option. The key is also the suffix of the result
        # item (`plot_<key>`) and of the render function (`.plot_<key>`).
        .switches = c(
            bars = "showBars", diverging = "showDiverging", heat = "showHeat",
            waffle = "showWaffle", picto = "showPicto", parliament = "showParliament",
            profile = "showProfile", means = "showMeans"),

        # ---------------------------------------------------------------------
        # .run(): tabulate, decide which plots can be drawn, set the state
        # ---------------------------------------------------------------------
        .run = function() {
            opts <- self$options
            vars <- opts$vars
            on   <- vapply(private$.switches, function(k) isTRUE(opts[[k]]), logical(1))

            # `notes` are messages shown in the Html element `hint` above the plots
            hint <- self$results$hint
            notes <- character(0)
            if (length(vars) > 0 && !any(on))
                notes <- "Tick at least one plot under <b>Plots to show</b>."
            if (length(vars) == 0 || !any(on)) {
                private$.set_hint(hint, notes)
                return()
            }

            # make sure every variable is a factor (jamovi passes ordinal as
            # ordered factors and nominal as plain factors already)
            data <- self$data[, vars, drop = FALSE]
            for (v in vars) if (!is.factor(data[[v]])) data[[v]] <- as.factor(data[[v]])
            # common category axis: union of all levels, in order of appearance
            cats <- unique(unlist(lapply(vars, function(v) levels(data[[v]]))))
            if (length(cats) == 0) return()

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
            if (all(long$n == 0)) return()

            # The mean-category plot treats categories as numbers 1..k, which is
            # only defensible for ordinal variables or for two-category variables
            # (where it is a proportion). `ok` flags the variables that qualify;
            # the others are skipped and named in a note.
            ok <- vapply(vars, function(v) {
                is.ordered(self$data[[v]]) || sum(long$count[long$variable == v] > 0) == 2
            }, logical(1))
            names(ok) <- vars
            if (isTRUE(opts$showMeans) && !all(ok)) {
                skipped <- paste(vars[!ok], collapse = ", ")
                notes <- c(notes, if (!any(ok))
                    paste0("<b>Mean category</b> was not drawn: it needs ordinal variables or variables with exactly two categories (",
                           skipped, " are nominal with more than two).")
                    else paste0("<b>Mean category</b> skips nominal variables with more than two categories: ", skipped, "."))
            }
            private$.set_hint(hint, notes)

            # hand the data to each ticked image; `width` lets the plot decide how
            # many legend rows it needs, `ok` is read by the means plot
            for (key in names(private$.switches)[on]) {
                if (key == "means" && !any(ok)) next     # nothing to draw
                image <- self$results[[paste0("plot_", key)]]
                sz <- private$.size(key, length(vars))
                image$setSize(sz[1], sz[2])
                image$setState(list(long = long, vars = vars, cats = cats, width = sz[1], ok = ok))
            }
        },

        # Show the given HTML notes in the `hint` element, or hide it if none.
        .set_hint = function(hint, notes) {
            if (length(notes) == 0) {
                hint$setVisible(FALSE)
            } else {
                hint$setContent(paste0("<p>", notes, "</p>", collapse = ""))
                hint$setVisible(TRUE)
            }
        },

        # Plot size in pixels. With "Size plots automatically" it grows with the
        # number of variables `n` (and, for waffle/pictogram, with the grid size);
        # otherwise the user's width/height are used. Numbers are empirical.
        .size = function(key, n) {
            o <- self$options
            if (!o$autoSize) return(c(o$plotWidth, o$plotHeight))
            horiz <- o$orientation == "horizontal"
            if (key %in% c("profile", "means")) return(c(700, 460))   # single panel
            if (key %in% c("waffle", "picto")) {
                rows <- if (key == "waffle") o$waffleRows else o$pictoRows
                cols <- if (key == "waffle") o$waffleCols else o$pictoCols
                # horizontal: charts stacked, height grows with n and rows
                # vertical: charts side by side, width grows with n and columns
                return(if (horiz) c(max(520, 190 + 12 * cols), max(240, 120 + n * (22 + 11 * rows)))
                       else c(max(450, 110 + n * (26 + 11 * cols)), max(380, 150 + 13 * rows)))
            }
            if (horiz) {
                per <- switch(key, bars = 56, diverging = 56, heat = 52, 100)   # px per variable
                c(700, max(240, 120 + per * n))
            } else {
                per <- switch(key, bars = 95, diverging = 95, heat = 90, 105)
                c(max(450, 110 + per * n), if (key %in% c("bars", "diverging", "heat")) 480 else 430)
            }
        },

        # Render entry points; jamovi calls these by name (renderFun in the .r.yaml).
        .plot_bars       = function(image, ...) private$.render(image, "bars"),
        .plot_diverging  = function(image, ...) private$.render(image, "diverging"),
        .plot_heat       = function(image, ...) private$.render(image, "heat"),
        .plot_waffle     = function(image, ...) private$.render(image, "waffle"),
        .plot_picto      = function(image, ...) private$.render(image, "picto"),
        .plot_parliament = function(image, ...) private$.render(image, "parliament"),
        .plot_profile    = function(image, ...) private$.render(image, "profile"),
        .plot_means      = function(image, ...) private$.render(image, "means"),

        # Read the state, build, print. FALSE = nothing drawn, TRUE = drawn.
        # Warnings are muted: they would only add noise to the results.
        .render = function(image, key) {
            st <- image$state
            if (is.null(st)) return(FALSE)
            p <- private$.build(key, st)
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
        .context = function(st) {
            o     <- self$options
            long  <- st$long
            vars  <- st$vars
            cats  <- st$cats
            k     <- length(cats)
            horiz <- identical(o$orientation, "horizontal")

            # helpers for the "Sort variables" option
            first <- function(v) long$pct[long$variable == v & long$cidx == 1]
            last  <- function(v) long$pct[long$variable == v & long$cidx == k]
            mean_code <- function(v) {          # mean of the category codes 1..k
                d <- long[long$variable == v, ]
                sum(d$cidx * d$count) / max(sum(d$count), 1)
            }
            ord <- switch(o$sortVars,
                name  = sort(vars),
                first = vars[order(-vapply(vars, first, 0))],   # most in first category on top
                last  = vars[order(-vapply(vars, last, 0))],
                mean  = vars[order(vapply(vars, mean_code, 0))],
                vars)                                           # "none": as entered
            lv <- if (o$invertVars) rev(ord) else ord
            n  <- length(lv)
            # first variable on top (horizontal, after coord_flip) or on the left (vertical)
            pos <- if (horiz) rev(seq_len(n)) else seq_len(n)
            names(pos) <- lv

            npv <- tapply(long$n, long$variable, function(z) z[1])   # valid n per variable
            labs <- if (o$showN) paste0(lv, " (n=", npv[lv], ")") else lv

            cat_cols <- cmp_palette(o$colourPalette, k, o$reversePalette)
            names(cat_cols) <- cats
            var_cols <- cmp_palette(o$colourPalette, n, o$reversePalette, spread = TRUE)
            names(var_cols) <- lv

            # legend wraps onto several rows when it would be wider than the plot
            leg_rows <- if (o$legendPos %in% c("bottom", "top"))
                max(1L, ceiling(k / max(2, floor(st$width / 130)))) else NULL

            list(o = o, horiz = horiz, lv = lv, n = n, pos = pos, k = k, cats = cats,
                 labs = cmp_wrap(labs), cat_cols = cat_cols, var_cols = var_cols,
                 leg_rows = leg_rows, npv = npv)
        },

        # Build the ggplot for one key: prepare the data (+ `pos`), dispatch to the
        # `.p_<key>` builder, then apply the common theme in `.finish()`.
        .build = function(key, st) {
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
            ctx <- private$.context(st)
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
                means      = private$.p_means(long, ctx, st, skipped))
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
                bars = , diverging = , means = private$.grid_theme(ctx),
                heat = ggplot2::theme(panel.grid = blank),
                waffle = , picto = , parliament = ggplot2::theme(
                    panel.grid = blank, axis.ticks = blank, axis.title = blank),
                NULL)
            p + cmp_theme(o$plotTheme, o$textSize) + extra +
                ggplot2::theme(
                    legend.position    = if (legend_key) o$legendPos else "none",
                    legend.title       = if (nchar(trimws(o$legendTitle)) > 0) ggplot2::element_text() else ggplot2::element_blank(),
                    plot.title         = ggplot2::element_text(face = "bold", hjust = 0.5),
                    panel.grid.minor   = ggplot2::element_blank(),
                    plot.caption       = ggplot2::element_text(size = o$textSize * 0.65, colour = "grey45", hjust = 0),
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
            mid <- o$divNeutral == "auto" && k %% 2 == 1 && k >= 3
            if (mid) {
                m <- (k + 1) / 2
                neg <- cats[seq_len(m - 1)]; neu <- cats[m]; pos <- cats[(m + 1):k]
            } else {
                h <- floor(k / 2)
                neg <- cats[seq_len(h)]; neu <- character(0); pos <- cats[(h + 1):k]
            }

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
        # Layout on a plain x/y plane (not the canvas): charts of different
        # variables are offset by (cols+2) or (rows+2) so there is a gap between them;
        # horizontal stacks them (y offset), vertical puts them side by side (x offset).
        # `kind` is "waffle" (squares) or "picto" (dots/other shapes).
        .p_dots = function(long, ctx, kind) {
            o <- ctx$o
            nrow_ <- if (kind == "waffle") o$waffleRows else o$pictoRows
            ncol_ <- if (kind == "waffle") o$waffleCols else o$pictoCols
            cells <- nrow_ * ncol_
            gx <- ncol_ + 2; gy <- nrow_ + 2          # grid pitch including the gap
            df <- do.call(rbind, lapply(ctx$lv, function(v) {
                d <- long[long$variable == v, ]
                cnt <- cmp_alloc(d$pct, cells)         # cells per category, sums to `cells`
                i <- seq_len(cells) - 1L              # cell index 0..cells-1
                ox <- if (ctx$horiz) 0 else (ctx$pos[v] - 1) * gx
                oy <- if (ctx$horiz) (ctx$pos[v] - 1) * gy else 0
                data.frame(variable = v, cx = ox + i %% ncol_, cy = oy + (nrow_ - 1) - i %/% ncol_,
                           category = factor(rep(as.character(d$category), cnt), levels = ctx$cats))
            }))
            # where the variable labels go: the middle of each grid
            centres <- if (ctx$horiz) (ctx$pos[ctx$lv] - 1) * gy + (nrow_ - 1) / 2
                       else (ctx$pos[ctx$lv] - 1) * gx + (ncol_ - 1) / 2
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
            p <- private$.dots_axes(p, ctx, centres)
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
            gx <- 4.4; gy <- 2.3                          # offsets between the charts
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
                ox <- if (ctx$horiz) 0 else (ctx$pos[v] - 1) * gx
                oy <- if (ctx$horiz) (ctx$pos[v] - 1) * gy else 0
                data.frame(variable = v, cx = ox + seats$rad * cos(seats$ang), cy = oy + seats$rad * sin(seats$ang),
                           category = factor(rep(as.character(d$category), cnt), levels = ctx$cats))
            }))
            centres <- if (ctx$horiz) (ctx$pos[ctx$lv] - 1) * gy + 0.9 else (ctx$pos[ctx$lv] - 1) * gx
            p <- ggplot2::ggplot(df, ggplot2::aes(x = cx, y = cy, fill = category)) +
                ggplot2::geom_point(size = o$parlSize, shape = 21, colour = "grey45", stroke = 0.25) +
                ggplot2::scale_fill_manual(
                    values = ctx$cat_cols, name = private$.leg_title(ctx), drop = FALSE,
                    guide = ggplot2::guide_legend(nrow = ctx$leg_rows, override.aes = list(size = 4)))
            private$.dots_axes(p, ctx, centres)
        },

        # Shared by waffle / pictogram / parliament: equal aspect ratio, and the
        # variable names act as the axis labels, placed at the middle of each chart
        # (on the y axis when charts are stacked, on the x axis when side by side).
        # The other axis shows nothing.
        .dots_axes = function(p, ctx, centres) {
            labs <- stats::setNames(ctx$labs, ctx$lv)[ctx$lv]
            p <- p + ggplot2::coord_equal() + ggplot2::labs(x = NULL, y = NULL)
            if (ctx$horiz) {
                p + ggplot2::scale_y_continuous(breaks = unname(centres), labels = unname(labs),
                                                expand = ggplot2::expansion(add = 1)) +
                    ggplot2::scale_x_continuous(breaks = NULL, expand = ggplot2::expansion(add = 1))
            } else {
                p + ggplot2::scale_x_continuous(breaks = unname(centres), labels = unname(labs),
                                                expand = ggplot2::expansion(add = 1)) +
                    ggplot2::scale_y_continuous(breaks = NULL, expand = ggplot2::expansion(add = 1))
            }
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
                ggplot2::scale_colour_manual(values = ctx$var_cols, name = private$.leg_title(ctx),
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
)
