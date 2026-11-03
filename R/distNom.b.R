# =============================================================================
# distNom: "Compare Nominal"  (grid plots sized in .init(): sizing variant 2, the recommended one, pending your test)
# Options, results and UI: jamovi/distNom.{a,r,u}.yaml. All code: R/catCommon.R.
# =============================================================================
distNomClass <- R6::R6Class(
    "distNomClass",
    inherit = distNomBase,
    private = c(cmp_cat_private(), list(
        .kind = "nominal",
        .variant = 2L,
        # Plot key -> checkbox option. The key is also the suffix of the result item
        # (`plot_<key>`) and of the render function (`.plot_<key>`).
        .switches = c(
            bars = "showBars", heat = "showHeat", waffle = "showWaffle", picto = "showPicto",
            parliament = "showParliament", pie = "showPie", donut = "showDonut",
            grouped = "showGrouped", lollipop = "showLollipop")
    ))
)
