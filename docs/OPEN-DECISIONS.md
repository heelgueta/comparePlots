# Open decisions (TEMPORARY: delete this file once they are settled)

State of the branch `jamovi-review-1.0` as of 2026-11-04. Everything below is built and smoke-tested headlessly
(all plots of all three analyses render; unit tests pass) but has **not** been judged by eye inside jamovi.

## To decide

1. **Nominal default palette.** Compare Nominal still defaults to *jamovi*, which gives shades of one colour
   from dark to light; that suggests an order nominal categories do not have. Suggestion: default to a qualitative
   palette (Set 2 or Okabe-Ito) for nominal only. (Contradicts "jamovi current everywhere", so it is your call.)
2. **Plot sizes.** The *Fit plot size* checkbox was removed (round 4), so sizes are fixed per plot (450x350 etc.). With
   many variables (8+) the user must resize by hand. Decide whether to bring back automatic sizing in `.init()`
   (reviewer only forbids resizing in `.run()`); the old code is in git history (commit d642c36).
3. **Reply to the reviewer** before resubmitting. Points to make: Plots tab done the vijPlots way (own *comparePlots*
   group inside it); plot sizes are fixed (nothing resizes in `.run()`); mean category coded by
   position on the shared axis, not per variable (see `docs/jamovi-review-0.4.0.md`, item 14); three analyses.
4. **Black & white mode** exists for Compare Numeric only. Should the categorical analyses get one (patterns or
   black/white/grey fills with labels)?
5. **Transformations (Compare Numeric).** Offered: z, ln, square root, square (centred, T, percent of range and percentile
   rank were removed on request). Anything to add (log10, reciprocal, Box-Cox, rank)? Values a transform cannot handle (e.g. log of <= 0) are
   dropped with a warning notice.
6. **Nominal plot set.** Pie, donut, grouped bars, lollipop, stacked bars, heatmap, waffle, pictogram, parliament.
   Not done: treemap, mosaic/Marimekko (extra packages or hand-built geometry).
7. **Variable descriptions as labels** reads the Data-tab description from the column attribute `jmv-desc`.
   Untested with real descriptions in jamovi (tested with a hand-set attribute).
8. **Axis text size** with *jamovi* theme comes from jamovi's theme, not from the *Text size* option
   (which still drives our own themes and the in-plot labels).

9. **UI rearrangement** of the three option panels (asked, not implemented; proposal in the chat of 2026-11-04).
10. **Missing cases.** *Ignore missing cases* makes every bar total 100%. In the user's PANAS_08 example the bar stopped at ~97.5%,
    which looks like one case that is not NA but falls in no category; check that variable's levels/values in jamovi.
11. **Box plot with points in the variable's colour:** points sitting on a box of the same colour are hard to see
    (use *Data point colour: Grey* if that bothers you).
12. **References.** Our entry is in `jamovi/00refs.yaml`; verify the wording in jamovi's References section.

## To verify in jamovi (cannot be checked headlessly)

- comparePlots dropdown appears inside the **Plots** tab with Compare Numeric / Ordinal / Nominal directly in it.
- *jamovi* theme/palette really follow jamovi's global settings (we read `theme$fill[2]`).
- Plot sizes look right at 450x350 and with *Fit plot size...* ticked.
- Ordinal analysis rejects nominal variables and vice versa with a readable message.

## Done in this round (for the record)

Compare Numeric / Ordinal / Nominal; nominal plots; black & white; transformations; charts-per-row grid; variable
descriptions; auto-fit checkbox (replaces the three sizing test menus); `jamovi current` naming; pale-colour fix;
shorter *Order variables by* labels. See `CHANGELOG.md` and `docs/jamovi-review-0.4.0.md`.
