# Compare Polytomous: a numeric variable (or several) across three or more groups.
# Everything is shared with Compare Dichotomous; see R/cmpGrpCommon.R.
distPolyClass <- R6::R6Class(
    "distPolyClass",
    inherit = distPolyBase,
    private = cmp_grp_private("poly"))
