# =============================================================================
# distOrd2: "Compare Ordinal 2"  (sizing variant 2: grid plots sized in .init())
# Options, results and UI: jamovi/distOrd2.{a,r,u}.yaml. All code: R/catCommon.R.
# =============================================================================
distOrd2Class <- R6::R6Class(
    "distOrd2Class",
    inherit = distOrd2Base,
    private = c(cmp_cat_private(), list(
        .kind = "ordinal",
        .variant = 2L,
        # Plot key -> checkbox option. The key is also the suffix of the result item
        # (`plot_<key>`) and of the render function (`.plot_<key>`).
        .switches = c(
            bars = "showBars", diverging = "showDiverging", heat = "showHeat",
            waffle = "showWaffle", picto = "showPicto", parliament = "showParliament",
            profile = "showProfile", means = "showMeans")
    ))
)
