# Changelog

## 1.1.0 (unreleased)

**New analysis**
- **Compare Groups**: compares the distributions of numeric variables across the levels of a grouping factor, with box, violin, means with error bars, raincloud, ridgeline, histogram, overlaid densities, ECDF, data points and barcode plots. Groups can be shown side by side within each variable, or in panels by group or by variable. It sits in its own section of the comparePlots menu.
- Like the other analyses, its *Transform values* and *Ignore missing group cases* options are in a *Data* box outside Appearance, and it warns when a variable has fewer than two distinct values in a group.

**Example data**
- The numeric example dataset has a *Group* column (Control / Intervention), so Compare Groups can be tried on it. Its values were regenerated, so example output from Compare Numeric differs from 1.0.x.

## 1.0.1 (2026-11-17)

Follow-up to the jamovi library audit of 1.0.0. No options were added or removed, so saved analyses keep working.

**Fixes**
- With *Ignore missing cases* unticked, the waffle, pictogram and parliament plots show the missing share as unfilled cells (as the bars do), the median ordering uses the answers' own distribution, *n* is the number of answers per variable, and variables that are entirely missing are rejected.
- Compare Numeric warns when a variable has fewer than two distinct values and so has no density curve or histogram.

**Layout and labels**
- Options that change the plotted data (*Transform values*, *Ignore missing cases*) sit in their own *Data* box, outside Appearance; the Appearance section formerly headed *Variables* is now *Order and Labels*.
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
