# Changelog

## 1.0.0 (unreleased) - changes requested by the jamovi library review

### Round 4 (2026-11-04)
- **Renamed to comparePlots** (module name, menu group, package, icon). Rename the GitHub repo to `heelgueta/comparePlots` too.
- Ridgeline and raincloud: removed the large empty space above the top plot.
- Vertical orientation: variable names (and category names) that would collide are cut with an ellipsis; example data now has short variable names.
- Numeric: data points take the variable's colour (new *Data point colour*: As variable / Grey), default point size 0.8, new shared *Data point opacity* (default 30%, was a fixed 40%).
- Palettes: Greys no longer ends near white; *Okabe-Ito* without brackets; no "(diverging)" suffixes; "jamovi current" is now just "jamovi".
- Categorical: *Opacity* (default 80%) and *Line thickness* for everything filled; *Ignore missing cases* (on by default: every variable's bar totals 100%); *Charts per row* moved into the waffle, pictogram, parliament, pie and donut sections; *Order categories by* gained A-Z (nominal); lollipop dot size capped at 6 and the plot is just "Lollipop plot".
- Removed: *Mean category and CI* plot, the *Fit plot size* checkbox, *n per variable* is off by default everywhere.
- Shorter option labels in the *Order variables* and *Transform values* lists so the UI does not break.
- Shared options are one open *Appearance* box with section headings (Variables / Appearance / Data points / Text and labels); the plots keep their own collapsible sections.
- Transform values reduced to none, z-scores, ln, square root, square. Default *Data point opacity* 50%. "several" dropped from the module description.
- Violin: *Inner box* is a checkbox (on by default) and the inner box has no fill. Section headings in the Appearance box are bold.
- Fixed: a stacked bar (and pie) could lose its last slice when the running percentages summed to 100.00000000000001 (the axis limits dropped it); this was the PANAS_08 case. Removed the Set 2 and Dark 2 palettes. New module icon in the colours of jamovi's own plot icons. Bold "Variables" heading in Ordinal/Nominal.
- Our own entry in the results' References (`jamovi/00refs.yaml`, cited by all three analyses).

Full tracking of the review, decisions and status: [docs/jamovi-review-0.4.0.md](docs/jamovi-review-0.4.0.md).

### Added
- *Transform values* (Compare Numeric): z-scores, ln, square root, square.
- Three analyses: **Compare Numeric**, **Compare Ordinal**, **Compare Nominal** in a *ComparePlots* group inside the Plots tab (the same arrangement as vijPlots). Variables of the wrong measure type are rejected with a hint.
- Nominal plots: pie, donut, grouped bars, lollipop; category order by frequency.
- *Black & white (no colour)* palette for the numeric plots.
- *Variable descriptions as labels* and *Charts per row* (waffle, pictogram, parliament, pie, donut are now a grid of charts).
- **Order variables by** in both analyses. Numeric: entry order, alphabetical, median, mean. Categorical: entry order, alphabetical, mean category, median category, % in first / last category, net %. *Reverse the order* flips any of them.
- *jamovi current* theme and palette (the new defaults): plots follow the user's jamovi theme settings. All previous themes and palettes remain available.
- Unit tests (`tests/testthat`).

### Changed
- Analyses renamed **Compare Numeric** and **Compare Categorical**, moved to jamovi's Plots tab; menu subtitles removed.
- Each plot's checkbox now sits inside its own collapsible section.
- Smaller fixed default plot sizes; plots are no longer resized while running. The manual/automatic size options were removed.
- Larger default text (16), transparent plot backgrounds.
- *Transparency (opacity %)* is now simply **Opacity**.
- Labels: units removed where `%` follows the box, checkbox verbs dropped, headings in title case, Grey spelling, dependent options greyed out together with the option they depend on.
- Numeric plots no longer store a copy of the data in each plot; the means-and-error-bars summary (including the median bootstrap) is computed once.
- Version 1.0.0.

### Fixed
- The shared category axis of *Compare Categorical* no longer misorders categories when an item lacks a category (e.g. nobody chose "3"); this affected stacked/diverging bars, heatmap, waffle, pictogram, parliament, profile and the mean-category plot. Conflicting orders and variables with no shared category now raise a warning.

### Removed
- Broken `ggplot2` references, `dist/` from version control.
