# Carbon Footprints — DAC Analytics Dashboard

An R Shiny dashboard for visualizing Direct Air Capture (DAC) installations and their operational performance.

## Features

- **Interactive Dashboard** — KPIs for total CO2 captured, efficiency, and costs
- **Geospatial Map** — Leaflet map showing DAC installation locations worldwide
- **Performance Analytics** — Time series analysis of capture rates by facility
- **Environmental Correlations** — Temperature and humidity impact on efficiency

## Installations Tracked

| Facility | Company | Capacity (tons/year) | Technology |
|----------|---------|---------------------|------------|
| Orca | Climeworks | 4,000 | Solid Sorbent |
| Mammoth | Climeworks | 36,000 | Solid Sorbent |
| Stratos | Occidental/1PointFive | 500,000 | Liquid Solvent |
| Project Bison | CarbonCapture | 5,000 | Solid Sorbent |
| Heirloom Tracy | Heirloom Carbon | 1,000 | Mineral Carbonation |

## Quick Start

```r
# Install dependencies
install.packages("pacman")

# Run the app
shiny::runApp("app.R")
```

## Tech Stack

- **R Shiny** + shinydashboard
- **Leaflet** for maps
- **Plotly** for interactive charts
- **dplyr/tidyr** for data wrangling

## Data Sources

See [data_sources.md](data_sources.md) for planned data integrations.

## License

MIT — See [LICENSE](LICENSE)
