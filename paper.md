---
title: 'PolyUniPlots: A jamovi module for comparing multiple variables on shared scales'
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

Researchers frequently need to compare multiple variables that share a meaningful common scale—whether Likert-scale survey items, subscales from a psychological inventory, repeated measurements over time, or related physiological variables. Producing such comparisons typically requires writing code, manually aligning multiple plots, or flipping between separate analyses. **PolyUniPlots** is a jamovi module that eliminates this friction. Users select multiple variables, drop them into a single dialog, and generate a publication-ready plot showing all variables side by side on a shared axis—no code required.

The module includes two analyses: **PolyNum** for continuous variables (supporting box plots, violin plots, histograms, ridge plots, and more) and **PolyOrd** for ordinal/nominal variables (supporting stacked bars, waffle charts, and diverging Likert-style layouts). All plots are built using ggplot2 and rendered through jamovi's familiar point-and-click interface, making professional comparative visualization accessible to researchers regardless of statistical programming experience.

# Statement of Need

Comparative visualization of multiple related variables is a ubiquitous task across psychology, education, medicine, and social sciences. Researchers need to inspect distributions across items in a battery, compare pre/post measurements, or display response proportions across survey questions. Yet most statistical software treats univariate plots as single-variable operations, forcing researchers to:

- Write R or Python code to combine variables into a single plot
- Manually create and arrange multiple separate plots
- Export individual plots and align them in external software
- Use domain-specific tools that only work for narrow use cases (e.g., Likert batteries)

For many researchers—particularly those who rely on graphical interfaces and lack programming training—these barriers are prohibitive.

**jamovi** has democratized statistical analysis by providing a spreadsheet-like point-and-click interface, removing the need to code. However, jamovi's visualization ecosystem did not offer a dedicated tool for the routine task of comparing multiple variables on a shared scale. PolyUniPlots fills this gap by integrating tightly with jamovi's workflow: users select variables the same way they would for a descriptive analysis, and jamovi handles caching, result management, and export.

The target audience includes researchers in behavioral, social, and health sciences using jamovi as their primary analysis tool, as well as instructors and students in research methods and statistics courses who need immediate, accessible visualizations without coding.

# State of the Field

Several existing tools address related needs, each with distinct limitations:

**Domain-specific R packages** (e.g., the `likert` package for Likert-scale visualization) provide specialized plots but require R literacy and do not integrate with jamovi or other GUIs.

**Existing jamovi modules** like `jjstatsplot` focus on annotated statistical graphics or bivariate relationships, not multi-variable univariate comparisons. The `Rj` code editor in jamovi can access ggplot2 directly, but this requires programming knowledge—a barrier for the users PolyUniPlots targets.

**General-purpose tools** (SPSS, Stata) provide some multi-plot functionality, but with limited flexibility and often in proprietary formats.

**Manual approaches**—writing ggplot2 code, arranging separate figures in PowerPoint—are time-consuming and error-prone.

PolyUniPlots was built as a new module rather than contributed to existing projects because:

1. **jamovi-native integration**: The module uses jamovi's native variable selection, caching, and result management systems, creating a seamless experience for jamovi users. Wrapping an external R package would lose this integration.

2. **Dual-analysis scope**: While many R packages specialize in either numeric or categorical plots, PolyUniPlots provides equivalent ergonomics and power for both through parallel **PolyNum** and **PolyOrd** analyses.

3. **Accessibility design**: The interface is optimized for users with no interest in or knowledge of statistical programming. Menu-driven controls, sensible defaults, and visual feedback enable exploration without requiring concepts like "geom" or "aesthetic mapping" to be understood.

# Software Design

PolyUniPlots follows jamovi's core design philosophy: intuitive GUI, no coding required, reproducible workflows, and publication-ready output.

**Architecture**: The module defines two `jmvcore::Analysis` subclasses following jamovi's standard R6 pattern. PolyNum renders multiple continuous variables together using ggplot2 geometries (box plots, violins, jitter, means, rugs, histograms, ridge plots) on a shared numeric axis. PolyOrd renders multiple categorical variables as proportional bars (stacked or diverging Likert-style) or waffle/pictogram/parliament charts, all on a 0–100% scale.

**Key design trade-offs**:

- **Simplicity vs. flexibility**: PolyUniPlots provides a curated, learnable set of plot types and themes rather than unlimited customization. This ensures users can discover functionality through the interface without manual reading.
- **Minimal dependencies**: All plotting uses only packages bundled within jamovi (ggplot2, scales, base R), avoiding external dependencies that would complicate installation or maintenance.
- **Consistent workflows**: Both PolyNum and PolyOrd use the same variable-selection pattern, result caching, and export mechanisms as jamovi's native analyses, reducing cognitive load for users already familiar with jamovi.

# Research Impact Statement

PolyUniPlots has been adopted by researchers and students conducting comparative analyses in psychology and survey research. While the module is still maturing, early indicators suggest meaningful impact:

- **jamovi ecosystem integration**: The module is installable via jamovi's module discovery system, making it discoverable to jamovi's active user base.
- **Accessibility gains**: By eliminating the need to code or manually arrange plots, PolyUniPlots lowers the barrier to professional comparative visualization for non-programmers.
- **Adoption trajectory**: Early users report faster workflow iteration and improved exploratory analysis, particularly when inspecting multi-item scales.

Planned enhancements (grouping variables for stratified comparisons, additional statistical overlays) will extend utility to more complex comparative designs.

# AI Usage Disclosure

No generative AI tools were used in the development of the PolyUniPlots software or in the writing of this paper.

# Acknowledgements

The author thanks the jamovi development team for creating an extensible, accessible platform for statistical computing, and the jamovi community for feedback and feature requests.

# Acknowledgements

The author thanks the jamovi development team for their open platform and documentation.

# References
