# Changelog

## 1.0.0 (unreleased) - changes requested by the jamovi library review

Full tracking of the review, decisions and status: [docs/jamovi-review-0.4.0.md](docs/jamovi-review-0.4.0.md).

### Added
- **Order variables by** in both analyses. Numeric: entry order, alphabetical, median, mean. Categorical: entry order, alphabetical, mean category, median category, % in first / last category, net %. *Reverse the order* flips any of them.
- *jamovi default* theme and palette (the new defaults): plots follow the user's jamovi theme settings. All previous themes and palettes remain available.
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
