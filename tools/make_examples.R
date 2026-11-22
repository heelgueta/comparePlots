# =============================================================================
# make_examples.R: regenerates the example datasets in data/.
#
# The data are SIMULATED (fixed seed, so the files are reproducible); they are not
# real survey data. Each dataset is written twice:
#   *.csv  for any tool
#   *.omv  for jamovi, with the measure types already set (ordered factor ->
#          ordinal, factor -> nominal), which matters because jamovi would
#          otherwise guess "continuous" for 1-5 codes
# The .omv files are also listed under `datasets:` in jamovi/0000.yaml so they
# show up in jamovi's Data Library.
#
# Run from the repository root (needs the jmvReadWrite package, which ships with
# jamovi):   Rscript tools/make_examples.R
# =============================================================================
set.seed(2026)

# ---- numeric: six well-being scales on a common 0-100 range -----------------
n <- 240
clip <- function(x) round(pmin(pmax(x, 0), 100), 1)
numeric_df <- data.frame(
    Mood          = clip(rnorm(n, 62, 14)),
    Energy        = clip(rnorm(n, 55, 18)),
    Sleep         = clip(100 * rbeta(n, 5, 2)),                  # piles up near the top
    Stress        = clip(100 * rbeta(n, 2, 5)),                  # right-skewed
    Anxiety       = clip(ifelse(runif(n) < 0.55, rnorm(n, 25, 8), rnorm(n, 70, 10))),  # two groups
    Social        = clip(rnorm(n, 48, 7)))                      # narrow spread

# ---- ordinal: a six-item Likert battery -------------------------------------
n <- 300
lik <- c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree")
item <- function(p) {
    x <- factor(sample(lik, n, TRUE, prob = p), levels = lik, ordered = TRUE)
    x[sample(n, 8)] <- NA
    x
}
ordinal_df <- data.frame(
    Enjoyed    = item(c(.04, .08, .16, .42, .30)),     # leans positive
    Workload   = item(c(.26, .34, .20, .14, .06)),     # leans negative
    Clarity    = item(c(.01, .03, .08, .28, .60)),     # strongly positive
    Divisive   = item(c(.30, .12, .06, .12, .40)),     # polarised
    Materials  = item(c(.06, .14, .50, .20, .10)),    # many neutral
    Recommend  = item(c(.05, .10, .20, .38, .27)))

# ---- nominal: preferred drink at three moments of the day -------------------
n <- 250
dr <- c("Coffee", "Tea", "Water", "Juice", "Other")
nominal_df <- data.frame(
    Morning   = factor(sample(dr, n, TRUE, prob = c(.55, .20, .08, .12, .05)), levels = dr),
    Afternoon = factor(sample(dr, n, TRUE, prob = c(.30, .30, .22, .10, .08)), levels = dr),
    Evening   = factor(sample(dr, n, TRUE, prob = c(.05, .28, .35, .07, .25)), levels = dr))

# ---- groups: a three-arm trial for Compare Groups ---------------------------
# Own seed, so the three datasets above stay identical when this one changes.
# Built to exercise the plots: shifts between groups, different shapes, a
# strictly positive skewed variable (try the log transform), a change score
# with negative values, uneven group sizes, a two-level Sex factor (for split
# violins) and a few missing values (group and measures).
set.seed(2027)
n_arm <- c(Control = 100, CBT = 90, Medication = 80)
arm  <- factor(rep(names(n_arm), n_arm), levels = names(n_arm))
n    <- length(arm)
cbt  <- arm == "CBT"; med <- arm == "Medication"
sex  <- factor(sample(c("Female", "Male"), n, TRUE, prob = c(.58, .42)))
clip <- function(x) round(pmin(pmax(x, 0), 100), 1)
groups_df <- data.frame(
    Group        = arm,
    Sex          = sex,
    Depression   = clip(rnorm(n, 62 - 18 * cbt - 12 * med, 13)),
    Anxiety      = clip(100 * rbeta(n, 2.4 - 0.5 * cbt - 0.3 * med, 4.5)),  # right-skewed
    Sleep        = clip(100 * rbeta(n, 4 + 1.2 * cbt + 0.4 * med, 2)),      # piles up near the top
    Wellbeing    = clip(rnorm(n, 45 + 9 * cbt + 6 * med, 7)),               # narrow spread
    ReactionTime = round(exp(rnorm(n, 6.0 + 0.10 * med, 0.28))),            # ms, strictly positive, skewed
    Change       = round(rnorm(n, -1 - 11 * cbt - 8 * med, 9), 1))          # post minus pre, can be negative
groups_df$Group[sample(n, 6)] <- NA
groups_df$Depression[sample(n, 9)] <- NA
groups_df$Wellbeing[sample(n, 7)] <- NA

# name -> data frame; the name becomes the file name
out <- list(example_numeric = numeric_df, example_ordinal = ordinal_df,
            example_nominal = nominal_df, example_groups = groups_df)
dir <- "data"
for (nm in names(out)) {
    utils::write.csv(out[[nm]], file.path(dir, paste0(nm, ".csv")), row.names = FALSE, na = "")
    jmvReadWrite::write_omv(out[[nm]], file.path(dir, paste0(nm, ".omv")))
}
cat("wrote", length(out), "datasets to", dir, "\n")
