distOrdClass <- R6::R6Class(
    "distOrdClass",
    inherit = distOrdBase,
    private = list(

        .switches = c(
            bars = "showBars", diverging = "showDiverging", heat = "showHeat",
            waffle = "showWaffle", picto = "showPicto", parliament = "showParliament",
            profile = "showProfile", means = "showMeans"),

        .run = function() {
            opts <- self$options
            vars <- opts$vars
            on   <- vapply(private$.switches, function(k) isTRUE(opts[[k]]), logical(1))

            hint <- self$results$hint
            notes <- character(0)
            if (length(vars) > 0 && !any(on))
                notes <- "Tick at least one plot under <b>Plots to show</b>."
            if (length(vars) == 0 || !any(on)) {
                private$.set_hint(hint, notes)
                return()
            }

            data <- self$data[, vars, drop = FALSE]
            for (v in vars) if (!is.factor(data[[v]])) data[[v]] <- as.factor(data[[v]])
            cats <- unique(unlist(lapply(vars, function(v) levels(data[[v]]))))
            if (length(cats) == 0) return()

            # percentages and category codes (1..k) per variable
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

            # mean category is only meaningful for ordinal or two-category variables
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

            for (key in names(private$.switches)[on]) {
                if (key == "means" && !any(ok)) next
                image <- self$results[[paste0("plot_", key)]]
                sz <- private$.size(key, length(vars))
                image$setSize(sz[1], sz[2])
                image$setState(list(long = long, vars = vars, cats = cats, width = sz[1], ok = ok))
            }
        },

        .set_hint = function(hint, notes) {
            if (length(notes) == 0) {
                hint$setVisible(FALSE)
            } else {
                hint$setContent(paste0("<p>", notes, "</p>", collapse = ""))
                hint$setVisible(TRUE)
            }
        },

        .size = function(key, n) {
            o <- self$options
            if (!o$autoSize) return(c(o$plotWidth, o$plotHeight))
            horiz <- o$orientation == "horizontal"
            if (key %in% c("profile", "means")) return(c(700, 460))
            if (key %in% c("waffle", "picto")) {
                rows <- if (key == "waffle") o$waffleRows else o$pictoRows
                cols <- if (key == "waffle") o$waffleCols else o$pictoCols
                return(if (horiz) c(max(520, 190 + 12 * cols), max(240, 120 + n * (22 + 11 * rows)))
                       else c(max(450, 110 + n * (26 + 11 * cols)), max(380, 150 + 13 * rows)))
            }
            if (horiz) {
                per <- switch(key, bars = 56, diverging = 56, heat = 52, 100)
                c(700, max(240, 120 + per * n))
            } else {
                per <- switch(key, bars = 95, diverging = 95, heat = 90, 105)
                c(max(450, 110 + per * n), if (key %in% c("bars", "diverging", "heat")) 480 else 430)
            }
        },

        .plot_bars       = function(image, ...) private$.render(image, "bars"),
        .plot_diverging  = function(image, ...) private$.render(image, "diverging"),
        .plot_heat       = function(image, ...) private$.render(image, "heat"),
        .plot_waffle     = function(image, ...) private$.render(image, "waffle"),
        .plot_picto      = function(image, ...) private$.render(image, "picto"),
        .plot_parliament = function(image, ...) private$.render(image, "parliament"),
        .plot_profile    = function(image, ...) private$.render(image, "profile"),
        .plot_means      = function(image, ...) private$.render(image, "means"),

        .render = function(image, key) {
            st <- image$state
            if (is.null(st)) return(FALSE)
            p <- private$.build(key, st)
            if (is.null(p)) return(FALSE)
            suppressWarnings(suppressMessages(print(p)))
            TRUE
        },

        # ordering, positions and colours shared by all plots
        .context = function(st) {
            o     <- self$options
            long  <- st$long
            vars  <- st$vars
            cats  <- st$cats
            k     <- length(cats)
            horiz <- identical(o$orientation, "horizontal")

            first <- function(v) long$pct[long$variable == v & long$cidx == 1]
            last  <- function(v) long$pct[long$variable == v & long$cidx == k]
            mean_code <- function(v) {
                d <- long[long$variable == v, ]
                sum(d$cidx * d$count) / max(sum(d$count), 1)
            }
            ord <- switch(o$sortVars,
                name  = sort(vars),
                first = vars[order(-vapply(vars, first, 0))],
                last  = vars[order(-vapply(vars, last, 0))],
                mean  = vars[order(vapply(vars, mean_code, 0))],
                vars)
            lv <- if (o$invertVars) rev(ord) else ord
            n  <- length(lv)
            pos <- if (horiz) rev(seq_len(n)) else seq_len(n)
            names(pos) <- lv

            npv <- tapply(long$n, long$variable, function(z) z[1])
            labs <- if (o$showN) paste0(lv, " (n=", npv[lv], ")") else lv

            cat_cols <- cp_palette(o$colourPalette, k, o$reversePalette)
            names(cat_cols) <- cats
            var_cols <- cp_palette(o$colourPalette, n, o$reversePalette, spread = TRUE)
            names(var_cols) <- lv

            leg_rows <- if (o$legendPos %in% c("bottom", "top"))
                max(1L, ceiling(k / max(2, floor(st$width / 130)))) else NULL

            list(o = o, horiz = horiz, lv = lv, n = n, pos = pos, k = k, cats = cats,
                 labs = cp_wrap(labs), cat_cols = cat_cols, var_cols = var_cols,
                 leg_rows = leg_rows, npv = npv)
        },

        .build = function(key, st) {
            skipped <- character(0)
            if (key == "means") {
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

        .finish = function(p, ctx, key) {
            o <- ctx$o
            legend_key <- key != "means"
            blank <- ggplot2::element_blank()
            extra <- switch(key,
                bars = , diverging = , means = private$.grid_theme(ctx),
                heat = ggplot2::theme(panel.grid = blank),
                waffle = , picto = , parliament = ggplot2::theme(
                    panel.grid = blank, axis.ticks = blank, axis.title = blank),
                NULL)
            p + cp_theme(o$plotTheme, o$textSize) + extra +
                ggplot2::theme(
                    legend.position    = if (legend_key) o$legendPos else "none",
                    legend.title       = if (nchar(trimws(o$legendTitle)) > 0) ggplot2::element_text() else ggplot2::element_blank(),
                    plot.title         = ggplot2::element_text(face = "bold", hjust = 0.5),
                    panel.grid.minor   = ggplot2::element_blank(),
                    plot.caption       = ggplot2::element_text(size = o$textSize * 0.65, colour = "grey45", hjust = 0),
                    plot.margin        = ggplot2::margin(10, 16, 8, 10)) +
                ggplot2::labs(title = cp_title(o$title))
        },

        .leg_title = function(ctx) cp_title(ctx$o$legendTitle),

        .fill_scale = function(ctx) {
            ggplot2::scale_fill_manual(
                values = ctx$cat_cols, name = private$.leg_title(ctx), drop = FALSE,
                guide = ggplot2::guide_legend(nrow = ctx$leg_rows))
        },

        .pct_fmt = function(x, ctx) paste0(formatC(x, format = "f", digits = ctx$o$decimals), "%"),

        .cat_axis = function(ctx, upper = 0.6) {
            ggplot2::scale_x_continuous(breaks = unname(ctx$pos), labels = ctx$labs,
                                        expand = ggplot2::expansion(add = c(0.6, upper)))
        },

        .flip = function(p, ctx, clip = "on") {
            if (ctx$horiz) p + ggplot2::coord_flip(clip = clip)
            else p + ggplot2::coord_cartesian(clip = clip)
        },

        .grid_theme = function(ctx) {
            # keep grid lines on the percentage axis only
            ggplot2::theme(
                panel.grid.major.x = if (ctx$horiz) ggplot2::element_line(colour = "grey90", linewidth = 0.35) else ggplot2::element_blank(),
                panel.grid.major.y = if (ctx$horiz) ggplot2::element_blank() else ggplot2::element_line(colour = "grey90", linewidth = 0.35))
        },

        # ── Stacked bars ───────────────────────────────────────────────────
        .p_bars = function(long, ctx) {
            o <- ctx$o
            bw <- o$barWidth / 100
            long <- long[order(long$variable, long$cidx), ]
            long$hi <- ave(long$pct, long$variable, FUN = cumsum)
            long$lo <- long$hi - long$pct
            p <- ggplot2::ggplot(long) +
                ggplot2::geom_rect(
                    ggplot2::aes(xmin = pos - bw / 2, xmax = pos + bw / 2, ymin = lo, ymax = hi, fill = category),
                    colour = "grey35", linewidth = 0.2)
            if (o$barLabels) {
                ld <- long[long$pct >= max(o$barMinLabel, 0.01), ]
                ld$label <- private$.pct_fmt(ld$pct, ctx)
                ld$tc <- cp_text_col(ctx$cat_cols[as.character(ld$category)])
                p <- p + ggplot2::geom_text(
                    data = ld, ggplot2::aes(x = pos, y = (lo + hi) / 2, label = label, colour = tc),
                    size = o$textSize * 0.28, fontface = "bold", show.legend = FALSE) +
                    ggplot2::scale_colour_identity()
            }
            p <- p + private$.cat_axis(ctx) +
                ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25),
                                            labels = function(x) paste0(x, "%"),
                                            expand = ggplot2::expansion(0)) +
                private$.fill_scale(ctx) + ggplot2::labs(x = NULL, y = NULL)
            private$.flip(p, ctx)
        },

        # ── Diverging bars ─────────────────────────────────────────────────
        .p_diverging = function(long, ctx) {
            o <- ctx$o
            cats <- ctx$cats
            k <- length(cats)
            if (k < 2) return(NULL)
            mid <- o$divNeutral == "auto" && k %% 2 == 1 && k >= 3
            if (mid) {
                m <- (k + 1) / 2
                neg <- cats[seq_len(m - 1)]; neu <- cats[m]; pos <- cats[(m + 1):k]
            } else {
                h <- floor(k / 2)
                neg <- cats[seq_len(h)]; neu <- character(0); pos <- cats[(h + 1):k]
            }

            rows <- lapply(ctx$lv, function(v) {
                d <- long[long$variable == v, ]
                pc <- function(cat) d$pct[d$category == cat]
                off <- if (length(neu)) pc(neu) / 2 else 0
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
                r$neg_tot <- sum(d$pct[d$category %in% neg])
                r$pos_tot <- sum(d$pct[d$category %in% pos])
                r
            })
            dv <- do.call(rbind, rows)
            dv$category <- factor(dv$category, levels = cats)
            ext <- max(abs(c(dv$lo, dv$hi)))
            ext <- max(10, ceiling(ext / 10) * 10)
            lim <- ext + if (o$divNet) 14 else 0
            bw <- o$divWidth / 100

            p <- ggplot2::ggplot(dv) +
                ggplot2::geom_rect(
                    ggplot2::aes(xmin = pos - bw / 2, xmax = pos + bw / 2, ymin = lo, ymax = hi, fill = category),
                    colour = "grey35", linewidth = 0.2) +
                ggplot2::geom_hline(yintercept = 0, colour = "grey25", linewidth = 0.5)
            if (o$divLabels) {
                ld <- dv[dv$pct >= max(o$divMinLabel, 0.01), ]
                ld$label <- private$.pct_fmt(ld$pct, ctx)
                ld$tc <- cp_text_col(ctx$cat_cols[as.character(ld$category)])
                p <- p + ggplot2::geom_text(
                    data = ld, ggplot2::aes(x = pos, y = (lo + hi) / 2, label = label, colour = tc),
                    size = o$textSize * 0.28, fontface = "bold", show.legend = FALSE) +
                    ggplot2::scale_colour_identity()
            }
            if (o$divNet) {
                nt <- unique(dv[, c("pos", "neg_tot", "pos_tot")])
                p <- p +
                    ggplot2::geom_text(data = nt, ggplot2::aes(x = pos, y = -(ext + 7), label = private$.pct_fmt(neg_tot, ctx)),
                                       size = o$textSize * 0.28, colour = "grey25", fontface = "italic") +
                    ggplot2::geom_text(data = nt, ggplot2::aes(x = pos, y = ext + 7, label = private$.pct_fmt(pos_tot, ctx)),
                                       size = o$textSize * 0.28, colour = "grey25", fontface = "italic")
            }
            brk <- pretty(c(-ext, ext), n = 8)
            brk <- brk[abs(brk) <= ext]
            p <- p + private$.cat_axis(ctx) +
                ggplot2::scale_y_continuous(limits = c(-lim, lim), breaks = brk,
                                            labels = function(x) paste0(abs(x), "%"),
                                            expand = ggplot2::expansion(0)) +
                private$.fill_scale(ctx) + ggplot2::labs(x = NULL, y = NULL)
            private$.flip(p, ctx)
        },

        # ── Heatmap ────────────────────────────────────────────────────────
        .p_heat = function(long, ctx) {
            o <- ctx$o
            k <- ctx$k
            hm <- c(blues = "Blues 3", greens = "Greens 3", reds = "Reds 3", purples = "Purples 3",
                    oranges = "Oranges", ylorrd = "YlOrRd", mako = "Mako", rocket = "Rocket", grays = "Grays")
            hp <- hm[[o$heatPalette]]
            cols <- if (hp %in% c("Mako", "Rocket")) rev(viridisLite::viridis(9, option = tolower(hp)))
                    else grDevices::hcl.colors(9, hp, rev = TRUE)
            top <- max(long$pct)
            long$y <- if (ctx$horiz) long$cidx else k - long$cidx + 1
            gap <- o$heatGap / 100
            p <- ggplot2::ggplot(long, ggplot2::aes(x = pos, y = y, fill = pct)) +
                ggplot2::geom_tile(width = 1 - gap, height = 1 - gap)
            if (o$heatLabels) {
                tcol <- cp_text_col(scales::col_numeric(cols, c(0, top))(long$pct))
                long$label <- private$.pct_fmt(long$pct, ctx)
                p <- p + ggplot2::geom_text(
                    data = long, ggplot2::aes(label = label), colour = tcol,
                    size = o$textSize * 0.28, show.legend = FALSE)
            }
            p <- p + private$.cat_axis(ctx) +
                ggplot2::scale_y_continuous(breaks = if (ctx$horiz) seq_len(k) else k - seq_len(k) + 1,
                                            labels = cp_wrap(ctx$cats, 14), expand = ggplot2::expansion(add = 0.5)) +
                ggplot2::scale_fill_gradientn(colours = cols, limits = c(0, top),
                                              name = if (is.null(private$.leg_title(ctx))) "%" else private$.leg_title(ctx),
                                              guide = ggplot2::guide_colourbar(barwidth = ggplot2::unit(9, "lines")),
                                              labels = function(x) paste0(x, "%")) +
                ggplot2::labs(x = NULL, y = NULL)
            private$.flip(p, ctx)
        },

        # one rows x cols grid per variable (waffle squares or pictogram dots); every
        # cell stands for 100 / (rows * cols) percent
        .p_dots = function(long, ctx, kind) {
            o <- ctx$o
            nrow_ <- if (kind == "waffle") o$waffleRows else o$pictoRows
            ncol_ <- if (kind == "waffle") o$waffleCols else o$pictoCols
            cells <- nrow_ * ncol_
            gx <- ncol_ + 2; gy <- nrow_ + 2
            df <- do.call(rbind, lapply(ctx$lv, function(v) {
                d <- long[long$variable == v, ]
                cnt <- cp_alloc(d$pct, cells)
                i <- seq_len(cells) - 1L
                # horizontal: charts on top of each other; vertical: side by side
                ox <- if (ctx$horiz) 0 else (ctx$pos[v] - 1) * gx
                oy <- if (ctx$horiz) (ctx$pos[v] - 1) * gy else 0
                data.frame(variable = v, cx = ox + i %% ncol_, cy = oy + (nrow_ - 1) - i %/% ncol_,
                           category = factor(rep(as.character(d$category), cnt), levels = ctx$cats))
            }))
            centres <- if (ctx$horiz) (ctx$pos[ctx$lv] - 1) * gy + (nrow_ - 1) / 2
                       else (ctx$pos[ctx$lv] - 1) * gx + (ncol_ - 1) / 2
            unit <- 100 / cells
            unit_lab <- paste0("1 ", if (kind == "waffle") "square" else "dot", " = ",
                               format(round(unit, 2), nsmall = 0), "%")
            if (kind == "waffle") {
                s <- 1 - o$waffleGap / 100
                p <- ggplot2::ggplot(df, ggplot2::aes(x = cx, y = cy, fill = category)) +
                    ggplot2::geom_tile(width = s, height = s, colour = "grey55", linewidth = 0.2) +
                    private$.fill_scale(ctx)
            } else {
                shp <- c(circle = 21, square = 22, diamond = 23, triangle = 24)[[o$pictoShape]]
                p <- ggplot2::ggplot(df, ggplot2::aes(x = cx, y = cy, fill = category)) +
                    ggplot2::geom_point(shape = shp, size = o$pictoSize, colour = "grey45", stroke = 0.25) +
                    ggplot2::scale_fill_manual(
                        values = ctx$cat_cols, name = private$.leg_title(ctx), drop = FALSE,
                        guide = ggplot2::guide_legend(nrow = ctx$leg_rows, override.aes = list(size = 4, shape = 21)))
            }
            p <- private$.dots_axes(p, ctx, centres)
            p + ggplot2::labs(caption = unit_lab)
        },

        # ── Parliament (arc) chart ─────────────────────────────────────────
        .p_parliament = function(long, ctx) {
            o <- ctx$o
            nrow_ <- o$parlRows
            radii <- seq(0.8, 1.8, length.out = max(nrow_, 2))[seq_len(nrow_)]
            if (nrow_ == 1) radii <- 1.5
            per_row <- cp_alloc(radii, o$parlSeats)
            gx <- 4.4; gy <- 2.3
            seats <- do.call(rbind, lapply(seq_len(nrow_), function(r) {
                m <- per_row[r]
                if (m == 0) return(NULL)
                a <- if (m == 1) pi / 2 else seq(pi * 0.97, pi * 0.03, length.out = m)
                data.frame(ang = a, rad = radii[r])
            }))
            seats <- seats[order(-seats$ang, seats$rad), ]       # sweep left to right
            df <- do.call(rbind, lapply(ctx$lv, function(v) {
                d <- long[long$variable == v, ]
                cnt <- cp_alloc(d$pct, nrow(seats))
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

        # variable labels act as the axis text of the grids
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
        .p_profile = function(long, ctx) {
            o <- ctx$o
            k <- ctx$k
            long$x <- if (ctx$horiz) k - long$cidx + 1 else long$cidx
            long <- long[order(long$variable, long$cidx), ]
            p <- ggplot2::ggplot(long, ggplot2::aes(x = x, y = pct, colour = variable, group = variable))
            if (o$profLines)  p <- p + ggplot2::geom_line(linewidth = 1.1)
            if (o$profPoints) p <- p + ggplot2::geom_point(size = 2.6)
            if (o$profLabels)
                p <- p + ggplot2::geom_text(ggplot2::aes(label = private$.pct_fmt(pct, ctx)),
                                            nudge_x = 0.0, vjust = -0.9, size = o$textSize * 0.26, show.legend = FALSE)
            p <- p +
                ggplot2::scale_x_continuous(breaks = if (ctx$horiz) k - seq_len(k) + 1 else seq_len(k),
                                            labels = cp_wrap(ctx$cats, 14),
                                            expand = ggplot2::expansion(add = 0.4)) +
                ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"),
                                            expand = ggplot2::expansion(mult = c(0.02, 0.12))) +
                ggplot2::scale_colour_manual(values = ctx$var_cols, name = private$.leg_title(ctx),
                                             guide = ggplot2::guide_legend(nrow = if (ctx$o$legendPos %in% c("bottom", "top")) max(1L, ceiling(ctx$n / 5)) else NULL)) +
                ggplot2::labs(x = NULL, y = "Responses")
            private$.flip(p, ctx)
        },

        # ── Mean category with CI (levels coded 1..k) ─────────────────────
        .p_means = function(long, ctx, st, skipped = character(0)) {
            o <- ctx$o
            k <- ctx$k
            level_ci <- o$meansLevel / 100
            s <- do.call(rbind, lapply(ctx$lv, function(v) {
                d <- long[long$variable == v, ]
                x <- rep(d$cidx, d$count)
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
            p <- p + private$.cat_axis(ctx) +
                ggplot2::scale_y_continuous(breaks = seq_len(k), labels = cp_wrap(ctx$cats, 14),
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
