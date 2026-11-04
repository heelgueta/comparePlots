# jamovi library review of comparePlots 0.4.0: tracking

Submission 0.4.0 (15a7aac) was not accepted on 2026-11-02; the reviewer liked the module and listed changes.
This file records every point, what we decided (with the maintainer's answers to the questions we asked first),
and the status. Work happens on branch `jamovi-review-1.0`; target release **1.0.0**.

Status key: **done** = implemented in the source, **needs check** = implemented but only verifiable by building
the module in jamovi (no R/jamovi was available when the changes were written), **open** = not done.

> **Before building:** `R/distNum.h.R` and `R/distOrd.h.R` are generated from the yaml files and are now stale.
> Run `jmvtools::prepare()` (or `jmvtools::install()`) once to regenerate them, then commit them.

## Reviewer's list

| # | Point | Decision | Status |
|---|-------|----------|--------|
| 1 | Titles: *Compare Numeric*, *Compare Categorical*; drop the subtitles | As asked. Applied in `0000.yaml`, `.a.yaml`, `.r.yaml`, `.u.yaml`, README | done |
| 2 | Module belongs in the Plots tab | `category: plots`, `menuGroup: .` (same as jamovi's own plots) in `0000.yaml` and both `.a.yaml` | needs check (does the entry land in the Plots tab?) |
| 3 | Smaller default plot sizes, set correctly from the start, never resized in `.run()` | Fixed size per plot in `.r.yaml` (450x350, hist/ridge/waffle/pictogram 450x400); all `setSize()` calls removed | done, needs visual check |
| 4 | Remove manual plot-size options | `autoSize`, `plotWidth`, `plotHeight` removed (also from `clearWith`). **Maintainer wants to explore first** whether fixed sizes look good; an optional "Advanced" size setting at the end of the panel may come back later | done; see *Open ideas* |
| 5 | Fonts too small | Default text size 12 -> 16 (range 8-32); caption/ title sizes are relative to the theme | needs check |
| 6 | Transparent backgrounds | `cmp_theme()` makes plot, legend and key backgrounds transparent for every theme | done |
| 7 | Use jamovi's built-in themes | New first/default item **jamovi current** in *Theme*: uses the `ggtheme` jamovi passes to the render function. Our own themes stay selectable | needs check |
| 8 | (maintainer) Palette: do not force jamovi's palettes, but fit with them by default | New first/default item **jamovi current** in *Palette*: n shades around the accent colour of jamovi's `theme$fill`. Our palettes (Viridis, diverging, Okabe-Ito...) stay selectable and unchanged | needs check (how jamovi fills `theme`) |
| 9 | Plot checkbox inside the plot's own collapsible section | Each plot now has one CollapseBox that starts with its checkbox; the "Plots to show" block is gone. Box Plot / Stacked Bars start expanded | done |
| 10 | Remove units from labels (`%` is already after the box) | Titles stripped of "(%)"; every percent TextBox has `suffix: '%'` | done |
| 11 | Transparency *or* opacity | **Opacity** (higher = more solid). Option name `alpha` kept so saved analyses keep working; title is now *Opacity* | done |
| 12 | Version >= 1.0 | 1.0.0 in `DESCRIPTION`, `0000.yaml`, both `.a.yaml` | done |
| 13 | HIGH: shared category axis misorders categories | New `cmp_cat_axis()` in `utils.R`: numeric levels sorted numerically; otherwise levels merged keeping each variable's order; conflicts / disjoint variables reported with a `jmvcore::Notice` warning. Tests in `tests/testthat/test-utils.R` | done, needs check |
| 14 | Mean-category coding from own levels (1..k_v) | **Deviation, deliberately:** categories are coded by position on the (now correctly ordered) shared axis, because the plot's y axis is labelled with that axis; per-variable 1..k_v codes would put means at the wrong labels for sparse items. Binary nominal variables stay position-coded. See *Open questions* | done (with deviation) |
| 15 | `refs: ggplot2` has no citation | All 18 `refs:` removed | done |
| 16 | State / `requiresData` | Numeric: Solution B. `requiresData: true` kept, long data rebuilt from `self$data` in `.render()`, state is only the variable names (+ the error-bar summary table). Ordinal: `requiresData` removed, state already small. Error-bar summary (incl. median bootstrap) computed once in `.run()` | done, needs check |
| 17 | No usable data -> blank plots | `jmvcore::reject()` in both `.run()` (all missing / no categories) | done |
| 18 | "Mean category skipped" belongs in a Notice | `jmvcore::Notice` (WARNING) via `private$.warn()` | done |
| 19 | Theme/palette follow jamovi's global settings | Same as 7 and 8 | needs check |
| 20 | Dependent options enabled while their parent is off | Combined `enable:` conditions (CI level, smallest % to label x2, multiplier, mean-category CI level) | done |
| 21 | Checkbox labels name the action | Verbs dropped (Outliers, Data points (jittered), n per variable, Centre values, Rain, Box, Mean marker, % labels, Net % ..., Confidence interval, Mean values; also Points, % values, % in the tiles, Median marker). Option names unchanged | done |
| 22 | Group headings in title case | All CollapseBox labels title-cased | done |
| 23 | British/American spelling | Grey / Greys in titles (values `gray`, `grays` unchanged) | done |
| 24 | `.jmo` files committed | Already removed from the repo before this branch; `.gitignore` now also lists `dist/` (`build/`, `*.jmo` were already there). Attach builds to GitHub Releases | done |
| 25 | No unit tests | `tests/testthat/test-utils.R`: category axis, ordering, split, alloc, summary, palette, theme, helpers | done, **not run** (no R here): run `Rscript -e 'testthat::test_dir("tests/testthat")'` |

## Maintainer's additions

| Item | Design | Status |
|------|--------|--------|
| Order numeric variables by: entry order / alphabetical / median / mean | New option `sortVars` in *Compare Numeric* (lowest first). The existing *Reverse the order* checkbox (was *Invert variable order*) flips any choice | done, needs check |
| Order ordinal/nominal variables | `sortVars` in *Compare Categorical* extended to one list: entry order, alphabetical, mean category, median category, % in first category, % in last category, **net %** (positive minus negative, as in the diverging bars). Old values (`none`, `name`, `first`, `last`, `mean`) unchanged | done, needs check |
| Document everything | This file, `CHANGELOG.md`, README | done |

## Decisions taken with the maintainer (before coding)

- Sizes: fixed small defaults first, explore, then maybe an advanced option.
- Palette/theme: jamovi's settings by default; our palettes only when chosen.
- Version 1.0.0. Opacity (not transparency).
- Numeric ordering keeps the *Reverse* checkbox for direction.
- Ordinal ordering: one unified list (see above).
- Apply every polish item. Add tests. Track in CHANGELOG + this file.

## Open questions / ideas

1. **Binary nominal variables in the mean-category plot** are still coded by position on the shared axis, so their "mean" is only a proportion when the axis has two categories. Options: restrict the plot to ordinal variables, or add a 0/1 proportion mode.
2. **Advanced size setting** (maintainer's idea): decide after trying the fixed sizes in jamovi with many variables. Reviewer says jamovi will add typed sizes itself, so this may not be needed.
3. **Text size** now matters only for our own themes and in-plot labels; with *jamovi current* the axis text size comes from jamovi's theme.
4. Reply to the reviewer's email when resubmitting, noting the deviation in item 14.

## Build log (2026-11-03)

- Tooling: jamovi's bundled R 4.5.0 (`/Applications/jamovi.app/Contents/Frameworks/R.framework`) plus `jmvtools` 28.4 installed in `~/jmvtools-lib`; environment in `~/.jmvenv.sh` (`source` it, then add `R_LIBS=$HOME/jmvtools-lib:$R_LIBS`).
- `jmvtools::install(home = "/Applications/jamovi.app")` builds `comparePlots_1.0.0.jmo` (git-ignored) and installs it. It rewrites `jamovi/0000.yaml` without comments: `git checkout jamovi/0000.yaml` afterwards.
- Unit tests: all pass. Headless smoke test: all 10 numeric and 8 categorical plots render; a sparse first item (no "Neutral") keeps the category axis in order.
- Fixed while testing: the n-per-variable caption ran off the edge of the smaller plots (now wrapped).
- Generated `R/dist*.h.R` regenerated and committed.

## Round 2 (2026-11-03, after the first test in jamovi)

| # | Request | What was done | Status |
|---|---------|---------------|--------|
| 1 | Is the Plots tab required? | **Yes**: the reviewer wrote "that's where this module belongs". vijPlots (accepted) uses `category: plots` + `menuGroup: vijPlots`, i.e. its own named dropdown *inside* the Plots tab. We do the same: `category: plots`, `menuGroup: comparePlots`, sub-groups Numeric / Ordinal / Nominal | needs check in jamovi |
| 2 | Three analyses | **Compare Numeric**, **Compare Ordinal**, **Compare Nominal**. Each rejects variables of the wrong measure type with a message saying what to do. Binary-nominal mean-category question is gone (mean category is ordinal-only) | done |
| 2b | Nominal plots | Stacked bars, heatmap, waffle, pictogram, parliament **plus** pie, donut (hole size, n in the centre), grouped bars, lollipop. Option *Order categories by* (data order / overall frequency). Variables can be ordered by largest category share or by diversity | done |
| 3 | "jamovi default" -> "jamovi current" | Renamed in the option lists, code comments and docs. The option value is still `jamovi` | done |
| 4 | Palette too pale | jamovi-current shades now run from 45% of the accent colour (dark) to only 40% of the way to white (before: 70%). Our own palettes untouched | needs visual check |
| 5 | Strong black & white | Palette item **Black & white (no colour)** in Compare Numeric: black lines/points, white fills, thicker lines. Density and ECDF (variables share one panel) use line types plus their normal legend; every other plot needs no legend | done |
| 6 | Waffle / dot / parliament too small | Charts are now facets of one grid, **Charts per row** (default 3), label under each chart. Pie and donut use the same grid. Three sizing strategies to test side by side (submenu *Ordinal (test)*): **Compare Ordinal** = fixed size; **2** = grid plots sized in `.init()` from variables and charts per row; **3** = every plot sized in `.init()`. Nominal currently uses strategy 2. Rationale for pushing back: the reviewer's rule is "never resize in `.run()`"; `.init()` runs before the first draw, so the plot never jumps | **decision pending** (you test, then we delete two) |
| 6b | Labels | Option *Variable descriptions as labels* (default off) uses the description from jamovi's Data tab (column attribute `jmv-desc`); falls back to the name | needs check with real descriptions |
| 7 | "Order variables by" too long | Labels shortened (Entry order, Alphabetical, Mean, Median, % in first category, % in last category, Net %) | done |

### Notes
- `tools/gen_categorical_yaml.py` writes the yaml of Compare Ordinal 2/3 and Compare Nominal from the Compare Ordinal master; `tools/gen_numeric_ui.py` writes the numeric UI. Edit the master, then rerun.
- All categorical code is in `R/catCommon.R`; `R/distOrd*.b.R` and `R/distNom.b.R` only say which plots each analysis offers and which sizing variant it is.
- Not done / ideas: treemap and mosaic plots for nominal data (need extra packages or a lot of hand-built geometry); word cloud rejected (category counts are tiny); text labels in black & white mode for the categorical plots.

## Round 3 (2026-11-03)

- The three sizing test menus are gone. **Compare Ordinal** is the default; the behaviour of test version 3 is now the checkbox **Fit plot size to the number of variables** in the shared options of all three analyses (sizes set in `.init()`).
- **Compare Numeric** was sitting outside the comparePlots dropdown (`menuGroup: .` left in `distNum.a.yaml`). Now all three analyses are direct items of the **comparePlots** group in the Plots tab, no sub-menus.
- New **Transform values** option in Compare Numeric: centred, z-scores, T-scores, percent of range, percentile rank, ln, square root, square. The value axis says which transformation was applied; values a transformation cannot handle are left out with a warning.
- Everything still undecided is in `docs/OPEN-DECISIONS.md` (temporary file). Build instructions for another machine: `docs/dev-setup.md`.
