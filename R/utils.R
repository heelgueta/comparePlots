# Internal helpers shared by both analyses (not exported)

# `spread = TRUE` is for colouring individual variables: the pale middle of a
# diverging palette would be unreadable as a line or a bar, so those are dropped.
cp_palette <- function(scheme, n, reverse = FALSE, spread = FALSE) {
    n <- max(as.integer(n), 1L)
    if (spread && scheme %in% c("rdbu", "rdylgn", "piyg", "prgn", "brbg") && n > 1L) {
        cols <- cp_palette(scheme, n + 2L, reverse = FALSE)
        rgb  <- grDevices::col2rgb(cols)
        lum  <- 0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ]
        cols <- cols[-order(lum, decreasing = TRUE)[1:2]]
        return(if (reverse) rev(cols) else cols)
    }
    vir <- c(viridis = "viridis", plasma = "plasma", magma = "magma", inferno = "inferno",
             cividis = "cividis", mako = "mako", rocket = "rocket", turbo = "turbo")
    hcl <- c(rdbu = "RdBu", rdylgn = "RdYlGn", piyg = "PiYG", prgn = "PRGn", brbg = "BrBG",
             set2 = "Set 2", dark2 = "Dark 2", pastel = "Pastel 1", grays = "Grays")
    cols <- if (scheme %in% names(vir)) {
        viridisLite::viridis(n, option = vir[[scheme]])
    } else if (scheme == "okabe") {
        rep_len(unname(grDevices::palette.colors(8, "Okabe-Ito")[-1]), n)
    } else if (scheme %in% names(hcl)) {
        grDevices::hcl.colors(n, hcl[[scheme]])
    } else {
        viridisLite::viridis(n)
    }
    if (reverse) rev(cols) else cols
}

cp_theme <- function(name, size = 12) {
    switch(name,
        classic = ggplot2::theme_classic(base_size = size),
        bw      = ggplot2::theme_bw(base_size = size),
        light   = ggplot2::theme_light(base_size = size),
        gray    = ggplot2::theme_gray(base_size = size),
        ggplot2::theme_minimal(base_size = size))
}

# black or white text, whichever is readable on the given fill colours
cp_text_col <- function(fill) {
    rgb <- grDevices::col2rgb(fill)
    lum <- (0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ]) / 255
    ifelse(lum > 0.6, "grey15", "white")
}

# largest-remainder allocation of `total` units to proportions
cp_alloc <- function(pct, total = 100L) {
    pct[is.na(pct)] <- 0
    if (sum(pct) <= 0) return(rep(0L, length(pct)))
    raw  <- pct / sum(pct) * total
    base <- floor(raw)
    rem  <- total - sum(base)
    if (rem > 0) {
        idx <- order(raw - base, decreasing = TRUE)[seq_len(rem)]
        base[idx] <- base[idx] + 1
    }
    as.integer(base)
}

cp_wrap <- function(x, width = 26) {
    vapply(x, function(s) paste(strwrap(s, width), collapse = "\n"), "", USE.NAMES = FALSE)
}

cp_line_width <- function(i) c(0.3, 0.55, 0.9, 1.4, 2.2)[max(1L, min(5L, as.integer(i)))]

cp_title <- function(x) if (nchar(trimws(x)) > 0) x else NULL

# run `expr` with a fixed RNG seed without disturbing the caller's RNG stream
cp_with_seed <- function(seed, expr) {
    had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
    old <- if (had) get(".Random.seed", envir = globalenv()) else NULL
    on.exit({
        if (had) assign(".Random.seed", old, envir = globalenv())
        else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
            rm(".Random.seed", envir = globalenv())
    })
    set.seed(seed)
    expr
}

# centre and interval for one numeric vector; returns c(centre, lo, hi)
cp_summary <- function(x, center = "mean", interval = "ci", level = 95, mult = 1) {
    x <- x[is.finite(x)]
    n <- length(x)
    if (n == 0) return(c(NA_real_, NA_real_, NA_real_))
    ctr <- if (center == "median") stats::median(x) else mean(x)
    if (n < 2) return(c(ctr, ctr, ctr))
    lv <- level / 100
    tails <- c((1 - lv) / 2, 1 - (1 - lv) / 2)
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
        ctr + c(-1, 1) * stats::qt(tails[2], n - 1) * se_mean)
    c(ctr, lohi)
}

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
