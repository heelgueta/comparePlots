# Regenerates the example datasets (CSV for any tool, OMV with variable types set for jamovi).
# Run from the repository root:  Rscript data/make_examples.R
set.seed(2026)

# ---- numeric: six well-being scales on a common 0-100 range -----------------
n <- 240
clip <- function(x) round(pmin(pmax(x, 0), 100), 1)
numeric_df <- data.frame(
    Mood          = clip(rnorm(n, 62, 14)),
    Energy        = clip(rnorm(n, 55, 18)),
    Sleep_quality = clip(100 * rbeta(n, 5, 2)),                  # piles up near the top
    Stress        = clip(100 * rbeta(n, 2, 5)),                  # right-skewed
    Anxiety       = clip(ifelse(runif(n) < 0.55, rnorm(n, 25, 8), rnorm(n, 70, 10))),  # two groups
    Social_contact = clip(rnorm(n, 48, 7)))                      # narrow spread

# ---- ordinal: a six-item Likert battery -------------------------------------
n <- 300
lik <- c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree")
item <- function(p) {
    x <- factor(sample(lik, n, TRUE, prob = p), levels = lik, ordered = TRUE)
    x[sample(n, 8)] <- NA
    x
}
ordinal_df <- data.frame(
    Enjoyed_the_course   = item(c(.04, .08, .16, .42, .30)),     # leans positive
    Workload_was_fair    = item(c(.26, .34, .20, .14, .06)),     # leans negative
    Teacher_was_clear    = item(c(.01, .03, .08, .28, .60)),     # strongly positive
    Content_was_divisive = item(c(.30, .12, .06, .12, .40)),     # polarised
    Materials_were_useful = item(c(.06, .14, .50, .20, .10)),    # many neutral
    Would_recommend      = item(c(.05, .10, .20, .38, .27)))

# ---- nominal: preferred drink at three moments of the day -------------------
n <- 250
dr <- c("Coffee", "Tea", "Water", "Juice", "Other")
nominal_df <- data.frame(
    Morning   = factor(sample(dr, n, TRUE, prob = c(.55, .20, .08, .12, .05)), levels = dr),
    Afternoon = factor(sample(dr, n, TRUE, prob = c(.30, .30, .22, .10, .08)), levels = dr),
    Evening   = factor(sample(dr, n, TRUE, prob = c(.05, .28, .35, .07, .25)), levels = dr))

out <- list(vdp_numeric_example = numeric_df, vdp_ordinal_example = ordinal_df,
            vdp_nominal_example = nominal_df)
dir <- "data"
for (nm in names(out)) {
    utils::write.csv(out[[nm]], file.path(dir, paste0(nm, ".csv")), row.names = FALSE, na = "")
    jmvReadWrite::write_omv(out[[nm]], file.path(dir, paste0(nm, ".omv")))
}
cat("wrote", length(out), "datasets to", dir, "\n")
