# Compare Groups: renders every plot across awkward data and option setups.
# Needs the installed package (the analysis class is not a plain helper):
#   R CMD INSTALL . ; Rscript -e 'testthat::test_dir("tests/testthat")'
skip_if_not_installed("comparePlots")

ex <- c("data/example_groups.csv", "../../data/example_groups.csv")
base <- read.csv(ex[file.exists(ex)][1])
base$Group <- factor(base$Group, levels = c("Control", "CBT", "Medication"))
base$SES <- factor(base$SES, levels = c("Low", "Middle", "High"), ordered = TRUE)

keys <- c(box = "showBox", violin = "showViolin", errorbar = "showErrorBar",
          raincloud = "showRaincloud", ridge = "showRidge", hist = "showHist",
          density = "showDensity", ecdf = "showEcdf", strip = "showStrip",
          barcode = "showBarcode")

# Runs the analysis with every plot ticked; returns notice names and the plots
# that failed to draw (errors or FALSE).
cg_run <- function(d, vars, grp = "Group", ...) {
    args <- c(list(vars = vars, group = grp),
              as.list(stats::setNames(rep(TRUE, length(keys)), keys)), list(...))
    a <- comparePlots:::distGrpClass$new(
        options = do.call(comparePlots:::distGrpOptions$new, args), data = d)
    a$init(); a$run()
    bad <- character(0)
    for (k in names(keys)) {
        img <- a$results[[paste0("plot_", k)]]
        if (is.null(img$state)) next
        f <- tempfile(fileext = ".png")
        ragg::agg_png(f, 600, 400)
        r <- tryCatch(a$.__enclos_env__$private[[paste0(".plot_", k)]](
                img, ggtheme = ggplot2::theme_grey(), theme = list()),
            error = function(e) conditionMessage(e))
        grDevices::dev.off()
        if (!isTRUE(r)) bad <- c(bad, k)
    }
    nt <- vapply(a$results$items, function(i) if (inherits(i, "Notice")) i$name else "", "")
    list(bad = bad, notices = nt[nt != ""], res = a)
}

modes <- c("dodge", "facetGroup", "facetVar")

for (m in modes) {
    test_that(paste("Compare Groups draws all plots in awkward setups:", m), {
        v2 <- c("Depression", "Anxiety")
        g1 <- which(base$Group == levels(base$Group)[1])
        chk <- function(r) { expect_equal(r$bad, character(0)); r }

        chk(cg_run(base, v2, groupMode = m))
        chk(cg_run(base, c("Depression", "Anxiety", "Sleep", "Wellbeing", "ReactionTime", "Change"), groupMode = m))

        d <- base; d$G1 <- factor("only");            chk(cg_run(d, v2, "G1", groupMode = m))
        d <- base; d$G12 <- factor(rep(letters[1:12], length.out = nrow(d)))
        r <- chk(cg_run(d, v2, "G12", groupMode = m)); expect_true(".manyGroups" %in% r$notices)
        d <- base; d$Group[c(1, 5, 9)] <- NA
        chk(cg_run(d, v2, groupMode = m))
        chk(cg_run(d, v2, groupMode = m, ignoreMissingGroup = FALSE))
        d <- base; d$Group <- as.character(d$Group);  chk(cg_run(d, v2, groupMode = m))
        d <- base; d$Group <- factor(d$Group, levels = c(levels(d$Group), "Ghost"))
        chk(cg_run(d, v2, groupMode = m))

        d <- base; d$Depression <- NA_real_
        r <- chk(cg_run(d, v2, groupMode = m)); expect_true(".emptyVar" %in% r$notices)
        d <- base; d$Depression[g1] <- NA
        r <- chk(cg_run(d, v2, groupMode = m)); expect_true(".emptyCell" %in% r$notices)
        d <- base; d$Depression[g1[-1]] <- NA
        r <- chk(cg_run(d, v2, groupMode = m)); expect_true(".noSpread" %in% r$notices)
        chk(cg_run(base[1:2, ], "Depression", groupMode = m))

        for (tr in c("z", "log", "sqrt", "square"))
            chk(cg_run(base, v2, groupMode = m, transform = tr))
        d <- base; d$Neg <- stats::rnorm(nrow(d))
        r <- chk(cg_run(d, c("Neg", "Depression"), groupMode = m, transform = "log"))
        expect_true(".transformLost" %in% r$notices)

        chk(cg_run(base, v2, groupMode = m, orientation = "vertical"))
        chk(cg_run(base, v2, groupMode = m, colourPalette = "bw", legendPos = "bottom", showN = TRUE))
        chk(cg_run(base, v2, groupMode = m, sortVars = "median", invertVars = TRUE, invertGroups = TRUE))
        chk(cg_run(base, v2, groupMode = m, ebCenter = "median", ebInterval = "sd",
                   ebConnect = TRUE, ebLabels = TRUE, ebJitter = TRUE))
        chk(cg_run(base, v2, groupMode = m, histStat = "count", histPosition = "dodge",
                   histDensity = TRUE, histFreeY = TRUE))
        chk(cg_run(base, v2, groupMode = m, boxNotch = TRUE, boxMean = TRUE, boxJitter = TRUE))
        two <- base[base$Group %in% levels(base$Group)[1:2], ]; two$Group <- droplevels(two$Group)
        chk(cg_run(two, v2, groupMode = m, violinSplit = TRUE))
        chk(cg_run(base, v2, groupMode = m, violinSplit = TRUE))
    })
}

test_that("Compare Groups: identical values draw what they can and explain the rest", {
    d <- base; d$Depression <- 7; d$Anxiety <- 7
    r <- cg_run(d, c("Depression", "Anxiety"))
    expect_true(all(r$bad %in% c("ridge", "hist")))
    expect_true(all(c(".noSpread", ".noSpreadHist") %in% r$notices))
})

test_that("Compare Groups: no usable data is rejected, not drawn", {
    d <- base; d$Depression <- NA_real_; d$Anxiety <- NA_real_
    expect_error(cg_run(d, c("Depression", "Anxiety")), "no usable")
})

test_that("Compare Groups: plots are sized from their content", {
    d <- base; d$G12 <- factor(rep(letters[1:12], length.out = nrow(d)))
    size <- function(...) {
        a <- cg_run(d, c("Depression", "Anxiety", "Sleep"), ...)$res
        c(a$results$plot_box$width, a$results$plot_box$height)
    }
    expect_equal(size(grp = "Group", groupMode = "dodge")[2] >= 380, TRUE)
    expect_gt(size(grp = "G12", groupMode = "dodge")[2], 380)
    expect_gt(size(grp = "G12", groupMode = "facetGroup")[2], 380)
})

test_that("Compare Groups: dichotomous, ordinal and polytomous grouping variables", {
    chk <- function(r) { expect_equal(r$bad, character(0)); r }
    for (m in modes) for (g in c("Sex", "SES", "Site")) {
        chk(cg_run(base, c("Depression", "Anxiety"), g, groupMode = m))
        chk(cg_run(base, c("Depression", "Anxiety"), g, groupMode = m, orientation = "vertical", xLabelAngle = "angle45"))
    }
    # the ordinal factor keeps its order (Low < Middle < High), reversible
    lv <- function(...) levels(cg_run(base, "Depression", "SES", ...)$res$.__enclos_env__$private$.long("Depression", "SES")$grp)
    expect_equal(lv(), c("Low", "Middle", "High"))
})

test_that("Compare Groups: x axis label angle option", {
    for (ang in c("horizontal", "angle45", "angle90"))
        for (m in modes)
            expect_equal(cg_run(base, c("Depression", "Anxiety", "Sleep"), "Site", groupMode = m,
                                orientation = "vertical", xLabelAngle = ang)$bad, character(0))
})
