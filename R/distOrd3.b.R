# =============================================================================
# distOrd3: "Compare Ordinal 3"  (sizing variant 3: every plot sized in .init())
# Options, results and UI: jamovi/distOrd3.{a,r,u}.yaml. All code: R/catCommon.R.
# =============================================================================
distOrd3Class <- R6::R6Class(
    "distOrd3Class",
    inherit = distOrd3Base,
    private = c(cmp_cat_private(), list(
        .kind = "ordinal",
        .variant = 3L,
        # Plot key -> checkbox option. The key is also the suffix of the result item
        # (`plot_<key>`) and of the render function (`.plot_<key>`).
        .switches = c(
            bars = "showBars", diverging = "showDiverging", heat = "showHeat",
            waffle = "showWaffle", picto = "showPicto", parliament = "showParliament",
            profile = "showProfile", means = "showMeans")
    ))
)
