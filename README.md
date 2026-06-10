
<img src="man/figures/logo.png" align="right" height="139"/>

# agropvR

agropvR is an R package for mapping and analysing the environmental
impacts of agrovoltaic facilities on agricultural fields in Bavaria and
Baden-Württemberg, Germany, using Sentinel-2 satellite imagery.

## Installation

``` r
# install from GitHub
devtools::install_github("marlenecss/agropvR")
```

## Functions

- `collect_site_data()` — downloads and caches Sentinel-2 data for one
  site across April–October
- `load_site_results()` — loads cached results for a site, optionally
  filtered by month
- `compare_site()` — runs the full analysis for one site and one month
- `map_indices()` — maps NDVI, NDMI and BSI for one site and month
- `plot_indices()` — plots index time series comparing site vs reference
  field
- `compare_system_types()` — compares index statistics between system
  types
- `plot_system_types()` — plots index comparison between system types

## Example

``` r
library(agropvR)

# collect data for site 1
collect_site_data(sites[1,], fields[1,], site_id = 1)

# load and plot results
results <- load_site_results(site_id = 1)
plot_indices(results, site_id = 1)

# map indices for July 2024
map_indices(site_id = 1, month = "2024-07")
```
