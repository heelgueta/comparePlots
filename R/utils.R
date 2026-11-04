# =============================================================================
# utils.R: internal helpers shared by both analyses (distNum, distOrd and distNom).
# None of these are exported; they are plain functions in the package namespace,
# so the analysis classes (R/dist*.b.R) can call them directly. All are prefixed
# `cmp_` (comparePlots) to avoid clashing with anything else in jamovi's R.
# =============================================================================


# -----------------------------------------------------------------------------
# cmp_palette(): n colours for a palette chosen in the UI.
#   scheme  the option value of `colourPalette` (see the .a.yaml for the list).
#           "jamovi" builds the colours from the user's
#           global jamovi theme, see cmp_jamovi_palette()
#   n       how many colours are needed
#   reverse TRUE when the user ticked "Invert palette colours"
#   spread  TRUE when colours are for individual VARIABLES (lines, points, one
#           box per variable). The pale middle of a diverging palette (RdBu etc.)
#           is unreadable there, so n + 2 colours are generated and the two
#           lightest are dropped. Colours for CATEGORIES (bars, waffle...) keep
#           the full diverging range, so they are called with spread = FALSE.
# Families: viridis-type palettes come from viridisLite; the diverging and
# qualitative ones from grDevices::hcl.colors(); "okabe" is the colour-blind-safe
# Okabe-Ito set (without its black) and is recycled if more colours are needed.
# -----------------------------------------------------------------------------
cmp_palette <- function(scheme, n, reverse = FALSE, spread = FALSE, theme = NULL) {
    n <- max(as.integer(n), 1L)
    if (identical(scheme, "jamovi")) {
        cols <- cmp_jamovi_palette(theme, n)
        return(if (reverse) rev(cols) else cols)
    }
    if (spread && scheme %in% c("rdbu", "rdylgn", "piyg", "prgn", "brbg") && n > 1L) {
        cols <- cmp_palette(scheme, n + 2L, reverse = FALSE)
        rgb  <- grDevices::col2rgb(cols)
        lum  <- 0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ]   # perceived brightness
        cols <- cols[-order(lum, decreasing = TRUE)[1:2]]                 # drop the 2 lightest
        return(if (reverse) rev(cols) else cols)
    }
    # option value -> viridisLite option name
    vir <- c(viridis = "viridis", plasma = "plasma", magma = "magma", inferno = "inferno",
             cividis = "cividis", mako = "mako", rocket = "rocket", turbo = "turbo")
    # option value -> hcl.colors() palette name
    hcl <- c(rdbu = "RdBu", rdylgn = "RdYlGn", piyg = "PiYG", prgn = "PRGn", brbg = "BrBG",
             pastel = "Pastel 1", grays = "Grays")
    cols <- if (scheme %in% names(vir)) {
        viridisLite::viridis(n, option = vir[[scheme]])
    } else if (scheme == "grays") {
        # not hcl's Grays, whose light end is almost white and vanishes on a white page
        grDevices::gray.colors(n, start = 0.12, end = 0.7, gamma = 1)
    } else if (scheme == "okabe") {
        rep_len(unname(grDevices::palette.colors(8, "Okabe-Ito")[-1]), n)
    } else if (scheme %in% names(hcl)) {
        grDevices::hcl.colors(n, hcl[[scheme]])
    } else {
        viridisLite::viridis(n)      # unknown value: fall back to the default palette
    }
    if (reverse) rev(cols) else cols
}


# -----------------------------------------------------------------------------
# cmp_jamovi_palette(): n colours that follow jamovi's global plot settings.
# jamovi hands every render function a `theme` list whose `fill` (and `color`)
# hold the accent colours of the user's chosen theme/palette (jamovi's own plots
# use theme$fill[2] for filled shapes). One colour is not enough to tell several
# variables or categories apart, so n shades are spread from dark to light around
# that accent colour. The ramp stops well short of white (about 40% of the way),
# because pale shades vanish on a white page and made some categories unreadable.
# Without a theme (e.g. in tests) a neutral blue is used.
# -----------------------------------------------------------------------------
cmp_jamovi_palette <- function(theme, n) {
    base <- NULL
    for (cand in list(theme$fill[2], theme$fill[1], theme$color[1]))
        if (length(cand) == 1 && !is.na(cand) && is.character(cand)) { base <- cand; break }
    if (is.null(base)) base <- "#3E6DA6"
    if (n == 1L) return(base)
    rgb <- as.numeric(grDevices::col2rgb(base))
    dark  <- grDevices::rgb(rgb[1] * 0.45, rgb[2] * 0.45, rgb[3] * 0.45, maxColorValue = 255)
    light <- grDevices::rgb(rgb[1] + (255 - rgb[1]) * 0.4, rgb[2] + (255 - rgb[2]) * 0.4,
                            rgb[3] + (255 - rgb[3]) * 0.4, maxColorValue = 255)
    grDevices::colorRampPalette(c(dark, base, light))(n)
}


# -----------------------------------------------------------------------------
# cmp_theme(): the complete ggplot2 theme for the `plotTheme` option.
#   name     option value; "jamovi" (the default) returns `ggtheme`, the theme
#            jamovi passes to the render function, so the plots follow the user's
#            global theme settings. Other values give one of ggplot2's own themes
#            at `size`. Unknown values fall back to theme_minimal.
#   ggtheme  the theme object jamovi supplies (NULL outside jamovi -> minimal)
# Whatever the theme, the plot and legend backgrounds are made transparent, like
# jamovi's own plots, so the results look right on any page colour.
# -----------------------------------------------------------------------------
cmp_theme <- function(name, size = 16, ggtheme = NULL) {
    base <- if (identical(name, "jamovi") && inherits(ggtheme, "theme")) {
        ggtheme
    } else {
        switch(name,
            classic = ggplot2::theme_classic(base_size = size),
            bw      = ggplot2::theme_bw(base_size = size),
            light   = ggplot2::theme_light(base_size = size),
            gray    = ggplot2::theme_gray(base_size = size),
            ggplot2::theme_minimal(base_size = size))
    }
    clear <- ggplot2::element_rect(fill = NA, colour = NA)
    base + ggplot2::theme(plot.background = clear, legend.background = clear,
                          legend.key = clear, legend.box.background = clear)
}


# -----------------------------------------------------------------------------
# cmp_text_col(): "grey15" or "white", whichever is readable on each fill colour.
# Uses the usual luminance weights (0.299 R + 0.587 G + 0.114 B); fills brighter
# than 0.6 get dark text. Vectorised over `fill`.
# -----------------------------------------------------------------------------
cmp_text_col <- function(fill) {
    rgb <- grDevices::col2rgb(fill)
    lum <- (0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ]) / 255
    ifelse(lum > 0.6, "grey15", "white")
}


# -----------------------------------------------------------------------------
# cmp_alloc(): split `total` whole units among proportions with the
# largest-remainder method, so the parts are integers that sum to exactly
# `total`. Used to decide how many waffle squares / pictogram dots / parliament
# seats each category gets (naive rounding can leave the grid over- or under-full).
#   pct    non-negative numbers (need not sum to 100); NA counts as 0
#   total  number of units to distribute
# Example: cmp_alloc(c(33.3, 33.3, 33.4), 100) -> 33 33 34
# -----------------------------------------------------------------------------
cmp_alloc <- function(pct, total = 100L) {
    pct[is.na(pct)] <- 0
    if (sum(pct) <= 0) return(rep(0L, length(pct)))
    raw  <- pct / sum(pct) * total
    base <- floor(raw)                    # everybody gets the whole part first
    rem  <- total - sum(base)             # units still to give out
    if (rem > 0) {                        # ... to the largest fractional parts
        idx <- order(raw - base, decreasing = TRUE)[seq_len(rem)]
        base[idx] <- base[idx] + 1
    }
    as.integer(base)
}


# -----------------------------------------------------------------------------
# cmp_wrap(): break long labels over several lines (strwrap at `width` characters)
# so axis labels do not eat the plot. Returns an unnamed character vector.
# -----------------------------------------------------------------------------
cmp_wrap <- function(x, width = 26) {
    vapply(x, function(s) paste(strwrap(s, width), collapse = "\n"), "", USE.NAMES = FALSE)
}


# -----------------------------------------------------------------------------
# cmp_line_width(): maps the 1-5 "Line thickness" option to a ggplot2 linewidth.
# The index is clamped so an out-of-range value cannot error.
# -----------------------------------------------------------------------------
cmp_line_width <- function(i) c(0.3, 0.55, 0.9, 1.4, 2.2)[max(1L, min(5L, as.integer(i)))]


# -----------------------------------------------------------------------------
# cmp_fit_labels(): keep vertical axis labels from running into each other. Each
# label gets a slot of `slot_px` pixels; labels that would not fit at `size` points
# are cut and end in an ellipsis. Newlines are first turned into spaces.
# -----------------------------------------------------------------------------
cmp_fit_labels <- function(x, slot_px, size = 16) {
    x <- gsub("\n", " ", x)
    max_chars <- max(4L, floor(slot_px / (0.8 * size * 0.55)))   # axis text is ~0.8 x base size, ~0.55 em per character
    ifelse(nchar(x) > max_chars, paste0(substr(x, 1L, max_chars - 1L), "\u2026"), x)
}


# -----------------------------------------------------------------------------
# cmp_title(): NULL (= no title) when the text option is empty or only spaces.
# -----------------------------------------------------------------------------
cmp_title <- function(x) if (nchar(trimws(x)) > 0) x else NULL


# -----------------------------------------------------------------------------
# cmp_with_seed(): evaluate `expr` with a fixed RNG seed, then restore the
# caller's RNG state. Used for bootstrapped intervals so they are reproducible
# (same plot every time) without disturbing the random numbers of anything else
# running in the same R process.
# -----------------------------------------------------------------------------
cmp_with_seed <- function(seed, expr) {
    had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
    old <- if (had) get(".Random.seed", envir = globalenv()) else NULL
    on.exit({                              # runs even if `expr` fails
        if (had) assign(".Random.seed", old, envir = globalenv())
        else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
            rm(".Random.seed", envir = globalenv())
    })
    set.seed(seed)
    expr                                   # lazily evaluated here, after the seed is set
}


# -----------------------------------------------------------------------------
# cmp_summary(): centre and interval for one numeric vector (the "Means and error
# bars" plot). Returns c(centre, lower, upper).
#   center    "mean" or "median"
#   interval  "ci"     confidence interval. Mean: t-based. Median: bootstrap
#                      percentile interval (1000 resamples, fixed seed).
#             "se"     centre +/- mult * standard error (median: SE from bootstrap)
#             "sd"     centre +/- mult * standard deviation
#             "iqr"    25th to 75th percentile of the data
#             "pctile" central `level`% of the data
#             "range"  minimum to maximum
#   level     confidence / coverage level in percent (CI and pctile only)
#   mult      multiplier for se and sd
# With a single observation the interval collapses onto the centre; with none, NA.
# -----------------------------------------------------------------------------
cmp_summary <- function(x, center = "mean", interval = "ci", level = 95, mult = 1) {
    x <- x[is.finite(x)]
    n <- length(x)
    if (n == 0) return(c(NA_real_, NA_real_, NA_real_))
    ctr <- if (center == "median") stats::median(x) else mean(x)
    if (n < 2) return(c(ctr, ctr, ctr))
    lv <- level / 100
    tails <- c((1 - lv) / 2, 1 - (1 - lv) / 2)          # e.g. 0.025 and 0.975 for 95%
    # bootstrap distribution of the median (only evaluated when needed)
    boot <- function() cmp_with_seed(1, replicate(1000, stats::median(sample(x, n, TRUE))))
    se_mean <- stats::sd(x) / sqrt(n)
    lohi <- switch(interval,
        ci = if (center == "mean") {
            ctr + c(-1, 1) * stats::qt(tails[2], n - 1) * se_mean
        } else {
            unname(stats::quantile(boot(), tails))
        },
        se = if (center == "mean") ctr + c(-1, 1) * mult * se_mean
             else ctr + c(-1, 1) * mult * stats::sd(boot()),
        sd = ctr + c(-1, 1) * mult * stats::sd(x),
        iqr = unname(stats::quantile(x, c(0.25, 0.75))),
        pctile = unname(stats::quantile(x, tails)),
        range = range(x),
        ctr + c(-1, 1) * stats::qt(tails[2], n - 1) * se_mean)   # default: same as "ci" for the mean
    c(ctr, lohi)
}


# -----------------------------------------------------------------------------
# cmp_interval_label(): plain-language description of what cmp_summary() drew,
# for the plot caption, e.g. "Mean and 95% CI" or "Median and IQR".
# Arguments are the same options passed to cmp_summary().
# -----------------------------------------------------------------------------
cmp_interval_label <- function(center, interval, level, mult) {
    c_lab <- if (center == "median") "Median" else "Mean"
    i_lab <- switch(interval,
        ci     = paste0(level, "% CI"),
        se     = paste0(if (mult != 1) paste0(mult, " x ") else "", "SE"),
        sd     = paste0(if (mult != 1) paste0(mult, " x ") else "", "SD"),
        iqr    = "IQR",
        pctile = paste0("central ", level, "% of data"),
        range  = "min-max",
        paste0(level, "% CI"))
    paste0(c_lab, " and ", i_lab)
}


# -----------------------------------------------------------------------------
# cmp_cat_axis(): the shared category axis of the ordinal/nominal analysis.
# In jamovi a column's levels are only the values that occur in it, so an item
# nobody answered "3" on has levels 1, 2, 4, 5. A union "in order of appearance"
# would then put 3 last. This builds the union so that every variable keeps its
# own level order:
#   * all levels numeric (integer-coded Likert items): sorted numerically;
#   * otherwise: start from the variable with the most levels and slot each
#     missing level in right after the level that precedes it in its own variable.
# `levs` is a named list, variable name -> character vector of levels. Returns
#   cats      the common axis
#   conflict  variables whose own order cannot be kept (they disagree with another
#             variable about which category comes first)
#   disjoint  variables that share no level at all with the others
# The caller reports conflict/disjoint with a warning; nothing is guessed silently.
# -----------------------------------------------------------------------------
cmp_cat_axis <- function(levs) {
    levs <- lapply(levs, as.character)
    all_lv <- unique(unlist(levs, use.names = FALSE))
    if (length(all_lv) == 0) return(list(cats = character(0), conflict = character(0), disjoint = character(0)))
    num <- suppressWarnings(as.numeric(all_lv))
    cats <- if (!anyNA(num)) {
        all_lv[order(num)]
    } else {
        tmpl <- levs[[which.max(vapply(levs, length, 1L))]]
        res <- tmpl
        for (lv in levs) {
            last <- 0L                                   # index in `res` of the previous level of this variable
            for (x in lv) {
                i <- match(x, res)
                if (is.na(i)) {
                    res <- append(res, x, after = last)
                    last <- last + 1L
                } else {
                    last <- i
                }
            }
        }
        res
    }
    nm <- names(levs)
    conflict <- nm[vapply(levs, function(lv) is.unsorted(match(lv, cats)), logical(1))]
    disjoint <- if (length(levs) > 1)
        nm[vapply(seq_along(levs), function(i) !any(levs[[i]] %in% unlist(levs[-i], use.names = FALSE)), logical(1))]
    else character(0)
    list(cats = cats, conflict = conflict, disjoint = disjoint)
}


# -----------------------------------------------------------------------------
# cmp_order_vars(): the order in which variables are drawn (before "reverse").
#   how    the `sortVars` option: "none" (entry order), "name", or a statistic
#          name; `stat` is then a named numeric vector (one value per variable)
#          and variables are sorted by it, lowest first. NAs go last and ties
#          keep their entry order.
# -----------------------------------------------------------------------------
cmp_order_vars <- function(vars, how = "none", stat = NULL) {
    if (identical(how, "name")) return(vars[order(tolower(vars), vars)])
    if (identical(how, "none") || is.null(stat)) return(vars)
    vars[order(unname(stat[vars]), na.last = TRUE)]
}


# -----------------------------------------------------------------------------
# cmp_split_cats(): which categories count as negative, neutral and positive
# in the diverging bars (and in the "net %" ordering). With an odd number (>= 3)
# of categories and `centre` TRUE the middle one is neutral; otherwise the first
# floor(k/2) categories are negative and the rest positive.
# -----------------------------------------------------------------------------
cmp_split_cats <- function(cats, centre = TRUE) {
    k <- length(cats)
    if (centre && k %% 2 == 1 && k >= 3) {
        m <- (k + 1) / 2
        list(neg = cats[seq_len(m - 1)], neu = cats[m], pos = cats[(m + 1):k])
    } else {
        h <- floor(k / 2)
        list(neg = cats[seq_len(h)], neu = character(0), pos = cats[(h + 1):k])
    }
}


# -----------------------------------------------------------------------------
# cmp_var_labels(): the label shown for each variable. With `use` TRUE the
# variable's description from jamovi's Data tab (attribute "jmv-desc") is used
# when it is not empty; otherwise, and by default, the variable name.
# Returns a named character vector (names = variable names), wrapped so long
# descriptions do not eat the plot.
# -----------------------------------------------------------------------------
cmp_var_labels <- function(data, vars, use = FALSE, width = 30) {
    labs <- vapply(vars, function(v) {
        d <- if (isTRUE(use)) attr(data[[v]], "jmv-desc") else NULL
        if (is.character(d) && length(d) == 1 && !is.na(d) && nzchar(trimws(d))) trimws(d) else v
    }, "")
    stats::setNames(cmp_wrap(labs, width), vars)
}


# -----------------------------------------------------------------------------
# cmp_transform(): re-express one numeric vector before plotting (option
# `transform`, applied per variable). Values the transformation cannot handle
# (log of <= 0, square root of < 0, z of a constant) become NA; the caller counts
# them and reports them.
#   z                   (x - mean) / SD
#   log, sqrt, square   the usual functions
# -----------------------------------------------------------------------------
cmp_transform <- function(x, how = "none") {
    x <- as.numeric(x)
    out <- suppressWarnings(switch(how,
        z      = (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE),
        log    = ifelse(x > 0, log(x), NA_real_),
        sqrt   = ifelse(x >= 0, sqrt(x), NA_real_),
        square = x^2,
        x))
    out[!is.finite(out)] <- NA_real_
    out
}

# short name of a transformation for the value-axis label, e.g. "z-scores"
cmp_transform_label <- function(how) {
    switch(how, z = "z-scores", log = "ln", sqrt = "square root", square = "squared", "")
}
