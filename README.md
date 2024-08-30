
<!-- README.md is generated from README.Rmd. Please edit that file -->

# paisaje <img align="right" width="180" src="man/figures/paisaje_logo.png">

<!-- badges: start -->
<!-- badges: end -->

The goal of paisaje is to provide tools for landscape analysis.

## Installation

You can install the development version of paisaje from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ManuelSpinola/crhexgrids")
```

## Example

This is a basic example which shows you how to use the package:

``` r
library(paisaje)
library(tidyverse)
library(sf)
library(h3jsr)
```

Let’s bring an sf object

``` r
nc = st_read(system.file("shape/nc.shp", package="sf"))
#> Reading layer `nc' from data source 
#>   `/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/sf/shape/nc.shp' 
#>   using driver `ESRI Shapefile'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
```

Create an h3 grid of resolution 4

``` r
h3_grid_nc <- get_h3_grid(nc, resolution = 4)
```

Make a map

``` r
ggplot() +
  theme_minimal() +
  geom_sf(data = nc, color = "blue", linewidth = 0.5) +
  geom_sf(data = h3_grid_nc, alpha = 0.4, color = "coral", linewidth = 0.5)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />
