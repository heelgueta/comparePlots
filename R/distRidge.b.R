# =============================================================================
# distRidge: "Ridgeline (green-blue-red)"
#
# The ridgeline plot of Compare Numeric as its own button, with the ridges
# coloured green, blue, red from top to bottom (interpolated when there are more
# or fewer than three variables). It reuses all the code of Compare Numeric:
# the private methods of distNumClass are inherited (R6 keeps them in
# `$private_methods`), with two changes: the only plot is the ridgeline, always on,
# and the colours come from cmp_gbr() instead of a palette option.
# Options, results and UI: jamovi/distRidge.{a,r,u}.yaml (written by tools/gen_ridge_yaml.py).
# =============================================================================
distRidgeClass <- R6::R6Class(
    "distRidgeClass",
    inherit = distRidgeBase,
    private = modifyList(distNumClass$private_methods, list(
        .switches = c(ridge = "showRidge"),
        .on       = function() c(ridge = TRUE),
        .fixed3   = TRUE
    ))
)
