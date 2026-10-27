---
title: 'ComparePlots: A jamovi module for comparing the distributions of multiple variables'
tags:
  - R
  - jamovi
  - visualization
  - data exploration
  - comparative analysis
authors:
  - name: Herman Elgueta
    orcid: 0000-0002-6764-5490
    affiliation: 1
affiliations:
  - name: Universidad de Magallanes, Punta Arenas, Chile
    index: 1
date: 1 June 2026
bibliography: paper.bib
---

# Summary

Researchers frequently need to compare multiple variables that share a meaningful common scale—whether Likert-scale survey items, subscales from a psychological inventory, repeated measurements over time, or related physiological variables. Producing such comparisons typically requires writing code, manually aligning multiple plots, or flipping between separate analyses. **ComparePlots** is a jamovi module that eliminates this friction. Users select multiple variables, drop them into a single dialog, and generate a publication-ready plot showing all variables side by side on a shared axis—no code required.

The module includes two analyses: **Comparison of Numeric Variables** (box, violin, means with configurable error bars, raincloud, ridgeline, histogram, overlaid density, ECDF, strip and barcode plots, each switched on or off independently) and **Comparison of Ordinal/Nominal Variables** (stacked and diverging Likert-style bars, heatmap, waffle, pictogram, parliament, response-profile and mean-category plots). All plots are built using ggplot2 and rendered through jamovi's familiar point-and-click interface, making professional comparative visualization accessible to researchers regardless of statistical programming experience.

# Statement of Need

Comparative visualization of multiple related variables is a ubiquitous task across psychology, education, medicine, and social sciences. Researchers need to inspect distributions across items in a battery, compare pre/post measurements, or display response proportions across survey questions. Yet most statistical software treats univariate plots as single-variable operations, forcing researchers to:

- Write R or Python code to combine variables into a single plot
- Manually create and arrange multiple separate plots
- Export individual plots and align them in external software
- Use domain-specific tools that only work for narrow use cases (e.g., Likert batteries)

For many researchers—particularly those who rely on graphical interfaces and lack programming training—these barriers are prohibitive.

**jamovi** has democratized statistical analysis by providing a spreadsheet-like point-and-click interface, removing the need to code. However, jamovi's visualization ecosystem did not offer a dedicated tool for the routine task of comparing multiple variables on a shared scale. ComparePlots fills this gap by integrating tightly with jamovi's workflow: users select variables the same way they would for a descriptive analysis, and jamovi handles caching, result management, and export.

The target audience includes researchers in behavioral, social, and health sciences using jamovi as their primary analysis tool, as well as instructors and students in research methods and statistics courses who need immediate, accessible visualizations without coding.

# State of the Field

Several existing tools address related needs, each with distinct limitations:

**Domain-specific R packages** (e.g., the `likert` package for Likert-scale visualization) provide specialized plots but require R literacy and do not integrate with jamovi or other GUIs.

**Existing jamovi modules** like `jjstatsplot` focus on annotated statistical graphics or bivariate relationships, not multi-variable univariate comparisons. The `Rj` code editor in jamovi can access ggplot2 directly, but this requires programming knowledge—a barrier for the users ComparePlots targets.

**General-purpose tools** (SPSS, Stata) provide some multi-plot functionality, but with limited flexibility and often in proprietary formats.

**Manual approaches**—writing ggplot2 code, arranging separate figures in PowerPoint—are time-consuming and error-prone.

ComparePlots was built as a new module rather than contributed to existing projects because:

1. **jamovi-native integration**: The module uses jamovi's native variable selection, caching, and result management systems, creating a seamless experience for jamovi users. Wrapping an external R package would lose this integration.

2. **Dual-analysis scope**: While many R packages specialize in either numeric or categorical plots, ComparePlots provides equivalent ergonomics and power for both through parallel numeric and ordinal/nominal analyses.

3. **Accessibility design**: The interface is optimized for users with no interest in or knowledge of statistical programming. Menu-driven controls, sensible defaults, and visual feedback enable exploration without requiring concepts like "geom" or "aesthetic mapping" to be understood.

# Software Design

ComparePlots follows jamovi's core design philosophy: intuitive GUI, no coding required, reproducible workflows, and publication-ready output.

**Architecture**: The module defines two `jmvcore::Analysis` subclasses following jamovi's standard R6 pattern. The numeric analysis renders multiple continuous variables with ggplot2 on a shared value axis; every plot type is an independent, optional result with its own settings. The ordinal/nominal analysis renders multiple categorical variables as proportional bars (stacked or diverging Likert-style), heatmaps, waffle/pictogram/parliament charts, response profiles or mean-category plots, all on a common percentage or category scale.

**Key design trade-offs**:

- **Simplicity vs. flexibility**: ComparePlots provides a curated, learnable set of plot types and themes rather than unlimited customization. This ensures users can discover functionality through the interface without manual reading.
- **Minimal dependencies**: All plotting uses only packages bundled within jamovi (ggplot2, scales, base R), avoiding external dependencies that would complicate installation or maintenance.
- **Consistent workflows**: Both analyses use the same variable-selection pattern, result caching, and export mechanisms as jamovi's native analyses, reducing cognitive load for users already familiar with jamovi.

# Research Impact Statement

ComparePlots has been adopted by researchers and students conducting comparative analyses in psychology and survey research. While the module is still maturing, early indicators suggest meaningful impact:

- **jamovi ecosystem integration**: The module is installable via jamovi's module discovery system, making it discoverable to jamovi's active user base.
- **Accessibility gains**: By eliminating the need to code or manually arrange plots, ComparePlots lowers the barrier to professional comparative visualization for non-programmers.
- **Adoption trajectory**: Early users report faster workflow iteration and improved exploratory analysis, particularly when inspecting multi-item scales.

Planned enhancements (grouping variables for stratified comparisons, additional statistical overlays) will extend utility to more complex comparative designs.

# AI Usage Disclosure

The ComparePlots software was developed through iterative work with Claude Sonnet 5.5 (Anthropic), a generative AI model, which was instructed to follow the official jamovi developer manuals and documentation. The author specified the design and behaviour, and reviewed and tested the resulting code, and is responsible for it. The text of this paper was written by the author; Claude was used only to update the module name and the lists of plot types after the software changed.

# Acknowledgements

The author thanks the jamovi development team for creating an extensible, accessible platform for statistical computing, and the jamovi community for feedback and feature requests.

# Acknowledgements

The author thanks the jamovi development team for their open platform and documentation.

# References
