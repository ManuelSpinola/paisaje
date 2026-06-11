# Generate an H3 Hexagonal Grid for an sf Object

Generates a hexagonal grid of H3 cells at a specified resolution that
intersect with a given \`sf\` object. This is a wrapper for functions
from the h3jsr package.

## Usage

``` r
get_h3_grid(sf_object, resolution = 6, expand_factor = 0.1)
```

## Arguments

- sf_object:

  (`sf`) An sf object defining the area of interest. Must have a valid
  coordinate reference system (CRS).

- resolution:

  (`integer`) H3 resolution level (1–16). Default is 6. Lower values
  produce fewer, larger hexagons (faster processing, coarser grid).

- expand_factor:

  (`numeric`) Expands bounding box to ensure coverage. Default is 0.1.

## Value

(`sf`) An sf object containing the hexagonal grid polygons covering the
input area. Each polygon represents an H3 cell at the specified
resolution, with a column containing the H3 index.

## Details

Reducing the resolution (e.g., 5 or 6) can greatly reduce processing
time and memory usage, especially for large AOIs. Each decrease in
resolution increases the size of individual hexagons exponentially.

## Examples

``` r
# \donttest{
library(sf)
nc <- st_read(system.file("shape/nc.shp", package="sf"))
#> Reading layer `nc' from data source 
#>   `/home/runner/work/_temp/Library/sf/shape/nc.shp' using driver `ESRI Shapefile'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
h3_grid_sf <- get_h3_grid(nc, resolution = 6)
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries
# }
```
