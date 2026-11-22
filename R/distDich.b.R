# Compare Dichotomous: a numeric variable (or several) across exactly two groups.
# Everything is shared with Compare Polytomous; see R/cmpGrpCommon.R.
distDichClass <- R6::R6Class(
    "distDichClass",
    inherit = distDichBase,
    private = cmp_grp_private("dich"))
