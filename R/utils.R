# =============================================================================
# utils.R: internal helpers shared by both analyses (distNum and distOrd).
# None of these are exported; they are plain functions in the package namespace,
# so the analysis classes (R/dist*.b.R) can call them directly. All are prefixed
# `cp_` (ComparePlots) to avoid clashing with anything else in jamovi's R.
# =============================================================================


# -----------------------------------------------------------------------------
# cp_palette(): n colours for a palette chosen in the UI.
#   scheme  the option value of `colourPalette` (see the .a.yaml for the list)
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
cp_palette <- function(scheme, n, reverse = FALSE, spread = FALSE) {
    n <- max(as.integer(n), 1L)
    if (spread && scheme %in% c("rdbu", "rdylgn", "piyg", "prgn", "brbg") && n > 1L) {
        cols <- cp_palette(scheme, n + 2L, reverse = FALSE)
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
             set2 = "Set 2", dark2 = "Dark 2", pastel = "Pastel 1", grays = "Grays")
    cols <- if (scheme %in% names(vir)) {
        viridisLite::viridis(n, option = vir[[scheme]])
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
# cp_theme(): the complete ggplot2 theme for the `plotTheme` option.
# Unknown values fall back to theme_minimal, the module's default.
# -----------------------------------------------------------------------------
cp_theme <- function(name, size = 12) {
    switch(name,
        classic = ggplot2::theme_classic(base_size = size),
        bw      = ggplot2::theme_bw(base_size = size),
        light   = ggplot2::theme_light(base_size = size),
        gray    = ggplot2::theme_gray(base_size = size),
        ggplot2::theme_minimal(base_size = size))
}


# -----------------------------------------------------------------------------
# cp_text_col(): "grey15" or "white", whichever is readable on each fill colour.
# Uses the usual luminance weights (0.299 R + 0.587 G + 0.114 B); fills brighter
# than 0.6 get dark text. Vectorised over `fill`.
# -----------------------------------------------------------------------------
cp_text_col <- function(fill) {
    rgb <- grDevices::col2rgb(fill)
    lum <- (0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ]) / 255
    ifelse(lum > 0.6, "grey15", "white")
}


# -----------------------------------------------------------------------------
# cp_alloc(): split `total` whole units among proportions with the
# largest-remainder method, so the parts are integers that sum to exactly
# `total`. Used to decide how many waffle squares / pictogram dots / parliament
# seats each category gets (naive rounding can leave the grid over- or under-full).
#   pct    non-negative numbers (need not sum to 100); NA counts as 0
#   total  number of units to distribute
# Example: cp_alloc(c(33.3, 33.3, 33.4), 100) -> 33 33 34
# -----------------------------------------------------------------------------
cp_alloc <- function(pct, total = 100L) {
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
# cp_wrap(): break long labels over several lines (strwrap at `width` characters)
# so axis labels do not eat the plot. Returns an unnamed character vector.
# -----------------------------------------------------------------------------
cp_wrap <- function(x, width = 26) {
    vapply(x, function(s) paste(strwrap(s, width), collapse = "\n"), "", USE.NAMES = FALSE)
}


# -----------------------------------------------------------------------------
# cp_line_width(): maps the 1-5 "Line thickness" option to a ggplot2 linewidth.
# The index is clamped so an out-of-range value cannot error.
# -----------------------------------------------------------------------------
cp_line_width <- function(i) c(0.3, 0.55, 0.9, 1.4, 2.2)[max(1L, min(5L, as.integer(i)))]


# -----------------------------------------------------------------------------
# cp_title(): NULL (= no title) when the text option is empty or only spaces.
# -----------------------------------------------------------------------------
cp_title <- function(x) if (nchar(trimws(x)) > 0) x else NULL


# -----------------------------------------------------------------------------
# cp_with_seed(): evaluate `expr` with a fixed RNG seed, then restore the
# caller's RNG state. Used for bootstrapped intervals so they are reproducible
# (same plot every time) without disturbing the random numbers of anything else
# running in the same R process.
# -----------------------------------------------------------------------------
cp_with_seed <- function(seed, expr) {
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
# cp_summary(): centre and interval for one numeric vector (the "Means and error
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
cp_summary <- function(x, center = "mean", interval = "ci", level = 95, mult = 1) {
    x <- x[is.finite(x)]
    n <- length(x)
    if (n == 0) return(c(NA_real_, NA_real_, NA_real_))
    ctr <- if (center == "median") stats::median(x) else mean(x)
    if (n < 2) return(c(ctr, ctr, ctr))
    lv <- level / 100
    tails <- c((1 - lv) / 2, 1 - (1 - lv) / 2)          # e.g. 0.025 and 0.975 for 95%
    # bootstrap distribution of the median (only evaluated when needed)
    boot <- function() cp_with_seed(1, replicate(1000, stats::median(sample(x, n, TRUE))))
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
# cp_interval_label(): plain-language description of what cp_summary() drew,
# for the plot caption, e.g. "Mean and 95% CI" or "Median and IQR".
# Arguments are the same options passed to cp_summary().
# -----------------------------------------------------------------------------
cp_interval_label <- function(center, interval, level, mult) {
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
