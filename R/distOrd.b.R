# =============================================================================
# distOrd: "Compare Ordinal"
#
# Implementation of the analysis. Options, results and UI are defined in
# jamovi/distOrd.{a,r,u}.yaml; jmvtools generates R/distOrd.h.R (do not edit)
# which provides `distOrdOptions`, `distOrdResults` and `distOrdBase`.
#
# HOW AN ANALYSIS RUNS IN JAMOVI (same lifecycle as distNum.b.R)
#   1. `.run()` runs when options/data change. It tabulates the data and hands
#      the result to each ticked plot with `image$setState()`. No drawing, and
#      no resizing: plot sizes are fixed in the .r.yaml.
#   2. jamovi then calls each image's render function (`.plot_<key>`) with the
#      user's `ggtheme` and `theme`. It reads `image$state` (a small table of
#      counts and percentages) and prints a ggplot2 object. The images do not
#      need the raw data, so they do not declare `requiresData`.
#
# DATA MODEL
#   `long` has one row per (variable, category): the variable, the category as
#   a factor, its index `cidx` (1..k, the order of the levels), the raw `count`,
#   `pct` (percent of that variable's valid answers) and `n`. All variables are
#   put on the SAME category axis, built by cmp_cat_axis() (utils.R): the union
#   of their levels in which every variable keeps its own order (numeric levels
#   are sorted numerically). A variable that lacks a category has 0% there.
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
# THE CODE ITSELF lives in R/catCommon.R (shared with distOrd2, distOrd3 and distNom);
# this file only says which plots this analysis offers and which sizing variant it is.
# =============================================================================
distOrdClass <- R6::R6Class(
    "distOrdClass",
    inherit = distOrdBase,
    private = c(cmp_cat_private(), list(
        .kind = "ordinal",
        .variant = 1L,
        # Plot key -> checkbox option. The key is also the suffix of the result item
        # (`plot_<key>`) and of the render function (`.plot_<key>`).
        .switches = c(
            bars = "showBars", diverging = "showDiverging", heat = "showHeat",
            waffle = "showWaffle", picto = "showPicto", parliament = "showParliament",
            profile = "showProfile", means = "showMeans")
    ))
)
