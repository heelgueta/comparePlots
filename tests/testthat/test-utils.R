# Run from the module root:  Rscript -e 'testthat::test_dir("tests/testthat")'
# The helpers in R/utils.R are plain functions, so they are sourced rather than loaded from a package.
src <- c("R/utils.R", "../../R/utils.R")
source(src[file.exists(src)][1])

test_that("category axis: sparse Likert item does not move the missing category to the end", {
    # the item that comes first never got a "3": its levels are 1, 2, 4, 5
    ax <- cmp_cat_axis(list(a = c("1", "2", "4", "5"), b = c("1", "2", "3", "4", "5")))
    expect_equal(ax$cats, c("1", "2", "3", "4", "5"))
    expect_length(ax$conflict, 0)
    expect_length(ax$disjoint, 0)
})

test_that("category axis: numeric levels are sorted numerically (10 after 9)", {
    ax <- cmp_cat_axis(list(a = c("1", "2", "10"), b = c("2", "9", "10")))
    expect_equal(ax$cats, c("1", "2", "9", "10"))
})

test_that("category axis: text labels keep each variable's order and slot in missing ones", {
    a <- c("Disagree", "Agree")                                   # no "Neutral" in the first item
    b <- c("Disagree", "Neutral", "Agree")
    ax <- cmp_cat_axis(list(a = a, b = b))
    expect_equal(ax$cats, c("Disagree", "Neutral", "Agree"))
    # the other way round: the longer variable comes first
    ax2 <- cmp_cat_axis(list(b = b, a = a))
    expect_equal(ax2$cats, c("Disagree", "Neutral", "Agree"))
})

test_that("category axis: missing end categories are placed at the right end", {
    ax <- cmp_cat_axis(list(a = c("Low", "Mid", "High"), b = c("Mid", "High", "Very high"), c = c("Very low", "Low", "Mid")))
    expect_equal(ax$cats, c("Very low", "Low", "Mid", "High", "Very high"))
    expect_length(ax$conflict, 0)
})

test_that("category axis: contradicting orders are reported, not guessed", {
    ax <- cmp_cat_axis(list(a = c("A", "B", "C"), b = c("C", "B", "A")))
    expect_true(length(ax$conflict) >= 1)
})

test_that("category axis: variables sharing no category are reported", {
    ax <- cmp_cat_axis(list(a = c("x", "y"), b = c("p", "q"), c = c("x", "z")))
    expect_equal(ax$disjoint, "b")
})

test_that("category axis: single variable and empty input", {
    expect_equal(cmp_cat_axis(list(a = c("lo", "hi")))$cats, c("lo", "hi"))
    expect_length(cmp_cat_axis(list(a = character(0)))$cats, 0)
})

test_that("ordering: entry order, name, statistic (lowest first, NA last, ties stable)", {
    vars <- c("b", "C", "a")
    expect_equal(cmp_order_vars(vars, "none"), vars)
    expect_equal(cmp_order_vars(vars, "name"), c("a", "b", "C"))          # case-insensitive
    stat <- c(b = 3, C = 1, a = 3)
    expect_equal(cmp_order_vars(vars, "mean", stat), c("C", "b", "a"))
    expect_equal(cmp_order_vars(vars, "median", c(b = NA, C = 2, a = 1)), c("a", "C", "b"))
})

test_that("splitting categories: neutral middle, or never centre", {
    expect_equal(cmp_split_cats(as.character(1:5))$neu, "3")
    expect_equal(cmp_split_cats(as.character(1:5), centre = FALSE)$pos, c("3", "4", "5"))
    s <- cmp_split_cats(c("no", "yes"))
    expect_equal(s$neg, "no"); expect_equal(s$pos, "yes"); expect_length(s$neu, 0)
})

test_that("cmp_alloc: whole units that sum to the total", {
    expect_equal(cmp_alloc(c(33.3, 33.3, 33.4), 100), c(33L, 33L, 34L))
    expect_equal(sum(cmp_alloc(c(1, 1, 1), 10)), 10L)
    expect_equal(cmp_alloc(c(0, 0), 10), c(0L, 0L))
    expect_equal(cmp_alloc(c(NA, 50, 50), 4), c(0L, 2L, 2L))
})

test_that("cmp_summary: mean CI, median, degenerate input", {
    x <- c(1, 2, 3, 4, 5)
    r <- cmp_summary(x, "mean", "ci", 95)
    expect_equal(r[1], 3)
    expect_equal(r[2:3], 3 + c(-1, 1) * qt(0.975, 4) * sd(x) / sqrt(5))
    expect_equal(cmp_summary(x, "median", "iqr")[1], 3)
    expect_equal(cmp_summary(x, "mean", "range")[2:3], c(1, 5))
    expect_equal(cmp_summary(7, "mean", "ci"), c(7, 7, 7))
    expect_true(all(is.na(cmp_summary(numeric(0)))))
    # the bootstrap is reproducible and leaves the caller's RNG alone
    set.seed(42); s0 <- .Random.seed
    a <- cmp_summary(x, "median", "ci"); b <- cmp_summary(x, "median", "ci")
    expect_equal(a, b)
    expect_identical(.Random.seed, s0)
})

test_that("cmp_palette: right length, jamovi default follows the theme, reverse works", {
    for (s in c("viridis", "rdbu", "set2", "okabe", "grays", "jamovi")) expect_length(cmp_palette(s, 6), 6)
    expect_length(cmp_palette("rdbu", 4, spread = TRUE), 4)
    expect_equal(cmp_palette("viridis", 3, reverse = TRUE), rev(cmp_palette("viridis", 3)))
    th <- list(fill = c("#FFFFFF", "#FF0000"), color = "#000000")
    expect_equal(cmp_palette("jamovi", 1, theme = th), "#FF0000")           # the accent colour itself
    expect_length(unique(cmp_palette("jamovi", 5, theme = th)), 5)
    expect_length(cmp_palette("jamovi", 3, theme = NULL), 3)                # no theme: still works
})

test_that("cmp_theme: jamovi default returns jamovi's theme with transparent backgrounds", {
    th <- cmp_theme("jamovi", 16, ggplot2::theme_bw())
    expect_s3_class(th, "theme")
    expect_equal(th$plot.background$fill, NA)
    expect_s3_class(cmp_theme("jamovi", 16, NULL), "theme")                 # outside jamovi: falls back
    expect_s3_class(cmp_theme("classic", 16), "theme")
})

test_that("helpers: text colour, wrapping, titles", {
    expect_equal(cmp_text_col(c("white", "black")), c("grey15", "white"))
    expect_equal(cmp_wrap("a very long label indeed", 11), "a very\nlong label\nindeed")
    expect_null(cmp_title("  ")); expect_equal(cmp_title("T"), "T")
    expect_equal(cmp_line_width(99), 2.2)
})

test_that("cmp_var_labels: names by default, descriptions on request, wrapped", {
    d <- data.frame(a = 1:2, b = 3:4)
    attr(d$a, "jmv-desc") <- "A long description of the first variable"
    expect_equal(unname(cmp_var_labels(d, c("a", "b"), FALSE)), c("a", "b"))
    l <- cmp_var_labels(d, c("a", "b"), TRUE, width = 20)
    expect_equal(names(l), c("a", "b"))
    expect_equal(unname(l["b"]), "b")                              # no description: falls back to the name
    expect_true(grepl("\n", l[["a"]]))                             # long description is wrapped
    expect_equal(gsub("\n", " ", l[["a"]]), "A long description of the first variable")
})

test_that("jamovi palette stops well short of white", {
    th <- list(fill = c("#FFFFFF", "#3E6DA6"))
    cols <- cmp_palette("jamovi", 8, theme = th)
    lum <- colSums(grDevices::col2rgb(cols) * c(0.299, 0.587, 0.114)) / 255
    expect_true(max(lum) < 0.75)                                   # no near-white shade
})
