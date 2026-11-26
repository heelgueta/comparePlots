# Changelog

## 1.1.1 (2026-11-26)

**Fix**
- The optional notes under some plots are now settings of the plots that have them, instead of one shared checkbox in Text and Labels (1.1.0). All are off by default: *Note: what the bars show* (Means and Error Bars), *Note: what one square / dot is worth* (Waffle Chart, Dot Pictogram), *Explanatory note* (Lead Plot, Compare Dichotomous), *Note: overlap percentages* (Overlaid Densities, Compare Dichotomous) and *Describe in a note* next to the reference line in the group analyses. The shared *Explanatory note below the plot* setting of 1.1.0 is gone.

## 1.1.0 (2026-11-26)

**New analyses (a second section of the menu: Compare Groups)**
- The menu now has two sections: *Compare Variables* (Numeric, Ordinal, Nominal) and *Compare Groups*.
- **Compare Dichotomous** and **Compare Polytomous** compare numeric variables across the levels of a grouping variable (two groups, or three or more), with box, violin, means with error bars, raincloud, ridgeline, histogram, overlaid densities, ECDF, data points and barcode plots. Groups can be shown side by side within each variable, or in panels by group or by variable. Compare Dichotomous requires exactly two groups (and says so otherwise) and adds plots made for two groups:
  - split violin;
  - **back-to-back histogram** (population pyramid, e.g. age by sex; counts or percent of group; classic upright layout with vertical orientation, mirrored above/below a horizontal axis with horizontal orientation);
  - **percentile ladder**: the two groups' values at each percentile (quartiles, deciles or every 5th) joined by a bar coloured by the higher group, so you see where in the distribution they differ and by how much;
  - **lead plot**: at each value, the share of group 1 at or below it minus the share of group 2 (the running difference of the two cumulative distributions), with the largest gap marked and a two-row barcode of the observations underneath;
  - *Shade the overlap* in Overlaid Densities, with the overlap of the two curves as a percentage.
   Compare Polytomous accepts any number of groups (and points to Compare Dichotomous when there are two).
- Both have the same layout as Compare Numeric: no separate Data box, everything shared in one Appearance box, then one collapsible box per plot.
- *Reference line* (Colours and Lines): dashed line at the overall mean or median of all groups together.
- *Show n in group labels*: legend, panel and axis labels read e.g. "Control (n = 96)".
- *Angle of x axis labels* (Text and Labels): horizontal (default), 45 or 90 degrees. Bottom legends wrap to fit long group names.
- Plots are sized from the number of variables, groups and the display mode, long variable and group names wrap in panel headers, and the analyses warn about variables or variable-by-group combinations with no data, about variables with fewer than two distinct values in a group, and about grouping variables with more than 8 levels.

**Layout (Compare Numeric, Ordinal, Nominal)**
- The three analyses have the 1.0.0 arrangement again, and the two group analyses follow it: one Appearance box whose first section, *Variables*, holds the data options (*Transform values* in Compare Numeric, *Ignore missing cases* in Ordinal and Nominal, both in the group analyses). This undoes the 1.0.1 changes (a separate *Data* box in Compare Numeric; *Ignore missing cases* first in a section called *Data, Order and Labels* in Ordinal and Nominal). 1.0.1 was never submitted to jamovi, so 1.1.0 also carries its audit fixes.

**Languages**
- Spanish translation of the interface (`jamovi/i18n/es.po`): menus, options, results titles, palette names and example-data descriptions, and a few plot texts and notices. The sentence-style notices and plot captions are still English.

**Plots**
- *Explanatory note below the plot* (Text and Labels, all five analyses), **off by default**: the notes under some plots (what the error bars show, what one waffle or pictogram cell is worth, and in the group analyses the reference line, the lead plot and the density overlap) are shown only when ticked. In 1.0.x the error-bar and waffle/pictogram notes were always shown. *Show n* is unchanged.
- The heatmap's colour bar is drawn as vector rectangles (sharp in PDF and SVG exports) and has about four labelled breaks, so its labels no longer overprint.

**Example data**
- New `example_groups` dataset (a three-arm therapy trial, n = 270) for trying Compare Dichotomous and Compare Polytomous: four grouping variables of different kinds: *Group* (Control / CBT / Medication, uneven sizes), *Sex* (dichotomous), *SES* (ordinal: Low, Middle, High, order respected) and *Site* (nominal, four levels with long names), and and six numeric measures with different shapes, including strictly positive reaction times (try the log transform) and a change score with negative values. It has a few missing values. The other three example datasets are unchanged.

## 1.0.1 (2026-11-19)

Follow-up to the jamovi library audit of 1.0.0. No options were added or removed, so saved analyses keep working.

**Fixes**
- With *Ignore missing cases* unticked, the waffle, pictogram and parliament plots show the missing share as unfilled cells (as the bars do), the median ordering uses the answers' own distribution, *n* is the number of answers per variable, and variables that are entirely missing are rejected.
- Compare Numeric warns when a variable has fewer than two distinct values and so has no density curve or histogram.

**Layout and labels**
- Compare Numeric: *Transform values* sits in its own *Data* box, outside Appearance; the Appearance section formerly headed *Variables* is now *Order and Labels*.
- Compare Ordinal and Nominal: *Ignore missing cases* (ticked by default) is the first option in Appearance, in a section now headed *Data, Order and Labels*.
- Compare Ordinal and Nominal: bars, tiles, slices and dots have black outlines instead of grey, and *Opacity* no longer fades the outlines of the dot pictogram and parliament charts.
- Axis and option labels: *% in first / last category*, *Percent* (response profile), *Cumulative percent* (ECDF).

## 1.0.0 (2026-11-10)

First release for the jamovi library. It builds on 0.4.0, which was reviewed by the jamovi team; their feedback
(menu placement, plot sizes, fonts, themes, palettes, wording of the options, data handling, tests) shaped most
of the changes below. Version 1.0.0 also adds features of our own.

### Compared with 0.4.0

**Structure**
- The module is named **comparePlots** (it was ComparePlots).
- Three analyses instead of two, grouped in a **comparePlots** dropdown inside jamovi's Plots tab: **Compare Numeric**, **Compare Ordinal** and **Compare Nominal**. Variables of the wrong measure type are rejected with a hint (`Compare Ordinal` suggests ordinal variables while still permitting factors so nominal variables get a clear rejection message).
- Compare Nominal is new, with pie, donut, grouped bars and lollipop plots, and categories ordered by data order, frequency or A-Z.
- Each plot's checkbox now sits inside its own collapsible section, together with that plot's settings.
- The shared options are one collapsed *Appearance* box with native `Label` section headings (*Variables*, *Colours and Lines*, *Data Points*, *Text and Labels*).
- The module cites itself and ggplot2 in the results' References.

**Look and feel (jamovi's feedback)**
- Plots follow the user's jamovi theme and palette by default (*jamovi* theme and palette); our own themes and palettes remain selectable (Viridis, diverging, Okabe-Ito, Greys, Rainbow...).
- Transparent plot backgrounds, larger default text (`Text size` also scales axis, legend and title text under jamovi's default theme), smaller fixed default plot sizes (plots are never resized while running; the manual size options were removed).
- *Transparency* is now **Opacity**; labels follow jamovi's conventions (no units where `%` follows the box, no verbs on checkboxes such as *Density curve*, *Filled curves* and *Connecting lines*, title case headings, Grey spelling); dependent options are greyed out with their parent.

**New features**
- Order variables (entry order, A-Z, mean, median; for ordinal also % in first / last category and net %), with *Reverse the order*.
- *Transform values* (Compare Numeric): z-scores, ln, square root, square.
- Variable descriptions from the Data tab can be used as labels; long names are cut in the vertical orientation so they do not overlap.
- Compare Numeric: data points in the variable's colour with their own opacity and size, a black & white palette, an optional inner box in the violin plot.
- Compare Ordinal and Nominal: opacity, line thickness, *Ignore missing cases*, and *Charts per row* for the waffle, pictogram, parliament, pie and donut plots.
- The mean-category plot of 0.4.0 was removed because it was easy to misread.
- Unit tests (`tests/testthat`) and example datasets with example output.

**Fixes**
- The shared category axis no longer misorders categories when an item lacks one (e.g. nobody chose "3"); conflicting orders and variables with no shared category raise a warning.
- Waffle, pictogram and parliament plots keep a full, unfilled grid when an individual variable has no non-missing values.
- Stacked bars and pies could lose their last slice because of floating-point rounding.
- Variables with no usable data now give a message instead of blank plots; notes about skipped or conflicting data are proper jamovi notices.
- Plots no longer store a copy of the data, and the error-bar summary (including the median bootstrap) is computed once.

### Removed
- The mean-category plot, the manual plot size options and the broken `ggplot2` references.
- Prebuilt `.jmo` files are no longer kept in the repository; they are attached to GitHub releases.
