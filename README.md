# centrimpactvis

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE.md)

**centrimpactvis** is an R package for creating publication-quality, semantically consistent visualizations for CEnTR-IMPACT projects. It provides custom color palettes, typography, and a suite of specialized `ggplot2` geoms and dashboard layouts for community-engaged research reporting.

---

## Features

- **Custom Color Palettes:** Harmonized color schemes for fills, lines, and backgrounds.
- **Typography System:** Google Fonts integration for consistent, professional text rendering.
- **Specialized Geoms:** 
  - `geom_indicators()` for indicator circles
  - `geom_rose()` and `geom_stamen()` for rose plots
  - `geom_slopegraph()` for slope/bump charts
  - `geom_cascade_spiral()` for cascade/spiral visualizations
- **Dashboard Layouts:** Functions to combine multiple plots into composite dashboards.
- **Ready for Patchwork:** Seamless integration with the `patchwork` package for flexible layouts.

---

## Installation

```r
# Install from GitHub (requires remotes or devtools)
remotes::install_github("yourusername/centrimpactvis")
```

---

## Quick Start

```r
library(centrimpactvis)
library(ggplot2)

# Use the custom color palette
colors <- define_colors()
main_colors <- colors$solid_palette

# Example: Indicator visualization
df <- data.frame(
  indicator = c("A", "B", "C"),
  value = c(10, 20, 30)
)

ggplot(df, aes(x = indicator, r = value)) +
  geom_indicators() +
  scale_fill_manual(values = main_colors)
```

---

## Documentation

- See function documentation with `?define_colors`, `?geom_indicators`, etc.
- For advanced dashboards, see `visualize_composite()` and related functions.

---

## License

MIT © 2025 Jeremy Price