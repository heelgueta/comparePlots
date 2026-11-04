# Open decisions (TEMPORARY: delete this file once they are settled)

State of the branch `jamovi-review-1.0` as of 2026-11-03. Everything below is built and smoke-tested headlessly
(all plots of all three analyses render; unit tests pass) but has **not** been judged by eye inside jamovi.

## To decide

1. **Nominal default palette.** Compare Nominal still defaults to *jamovi current*, which gives shades of one colour
   from dark to light; that suggests an order nominal categories do not have. Suggestion: default to a qualitative
   palette (Set 2 or Okabe-Ito) for nominal only. (Contradicts "jamovi current everywhere", so it is your call.)
2. **Fixed vs fitted plot sizes.** Default is a fixed small size per plot. The checkbox *Fit plot size to the number
   of variables* (all three analyses) sizes the plots in `.init()` instead. Decide: keep it as a checkbox (current),
   make it the default, or drop it. Whichever you choose, the reviewer's rule is only "never resize in `.run()`".
   Tune the numbers in `.size()` (`R/catCommon.R`, `R/distNum.b.R`) after looking at real data with 8+ variables.
3. **Reply to the reviewer** before resubmitting. Points to make: Plots tab done the vijPlots way (own *comparePlots*
   group inside it); sizes set in `.init()`, not `.run()`, when auto-fit is ticked; mean category coded by
   position on the shared axis, not per variable (see `docs/jamovi-review-0.4.0.md`, item 14); three analyses.
4. **Black & white mode** exists for Compare Numeric only. Should the categorical analyses get one (patterns or
   black/white/grey fills with labels)?
5. **Transformations (Compare Numeric).** Offered: centred, z, T, percent of range, percentile rank, ln, square root,
   square. Anything to add (log10, reciprocal, Box-Cox, rank)? Values a transform cannot handle (e.g. log of <= 0) are
   dropped with a warning notice.
6. **Nominal plot set.** Pie, donut, grouped bars, lollipop, stacked bars, heatmap, waffle, pictogram, parliament.
   Not done: treemap, mosaic/Marimekko (extra packages or hand-built geometry).
7. **Variable descriptions as labels** reads the Data-tab description from the column attribute `jmv-desc`.
   Untested with real descriptions in jamovi (tested with a hand-set attribute).
8. **Axis text size** with *jamovi current* theme comes from jamovi's theme, not from the *Text size* option
   (which still drives our own themes and the in-plot labels).

## To verify in jamovi (cannot be checked headlessly)

- comparePlots dropdown appears inside the **Plots** tab with Compare Numeric / Ordinal / Nominal directly in it.
- *jamovi current* theme/palette really follow jamovi's global settings (we read `theme$fill[2]`).
- Plot sizes look right at 450x350 and with *Fit plot size...* ticked.
- Ordinal analysis rejects nominal variables and vice versa with a readable message.

## Done in this round (for the record)

Compare Numeric / Ordinal / Nominal; nominal plots; black & white; transformations; charts-per-row grid; variable
descriptions; auto-fit checkbox (replaces the three sizing test menus); `jamovi current` naming; pale-colour fix;
shorter *Order variables by* labels. See `CHANGELOG.md` and `docs/jamovi-review-0.4.0.md`.
