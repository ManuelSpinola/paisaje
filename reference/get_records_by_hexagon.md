# Retrieve species records aggregated by H3 hexagons

Downloads species occurrence data within a specified Area of Interest
(AOI) and aggregates these records into H3 hexagonal grid cells at a
given resolution. Returns an \`sf\` object with one polygon per hexagon
and columns containing species occurrence counts.

## Usage

``` r
get_records_by_hexagon(
  species, aoi_sf, res = 6,
  providers = NULL, remove_duplicates = FALSE,
  date = NULL, expand_factor = 0.1, limit = 500
)
```

## Arguments

- species:

  character vector. Species names to query.

- aoi_sf:

  sf object. Area of Interest polygon.

- res:

  integer. H3 resolution level (1–16). Default: 6.

- providers:

  character vector. Data providers to query. Default: NULL (all).

- remove_duplicates:

  logical. Remove duplicate records. Default: FALSE.

- date:

  character vector of length two. Start and end dates for filtering
  records.

- expand_factor:

  numeric. Expand AOI bounding box. Default: 0.1.

- limit:

  integer. Maximum number of occurrence records per species. Default:
  500.

## Value

sf object. H3 hex grid with species occurrence counts.

## Details

This function is useful for spatial biodiversity analyses where data
should be aggregated into a uniform spatial grid. The H3 grid system
enables multi-resolution analysis and efficient spatial summarization of
point occurrence data.

## Examples

``` r
# \donttest{
library(sf)
nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#> Reading layer `nc' from data source 
#>   `/home/runner/work/_temp/Library/sf/shape/nc.shp' using driver `ESRI Shapefile'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
hex_counts <- get_records_by_hexagon(
  species = c("Lynx rufus"),
  aoi_sf = nc,
  res = 6,
  limit = 200
)
print(hex_counts)
#> Simple feature collection with 5705 features and 2 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32377 ymin: 33.88216 xmax: -75.45658 ymax: 36.58977
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>         h3_address Lynx_rufus                       geometry
#> 1  862a99217ffffff          0 MULTIPOLYGON (((-81.70905 3...
#> 2  862a992efffffff          0 MULTIPOLYGON (((-81.59396 3...
#> 3  862a99227ffffff          0 MULTIPOLYGON (((-81.56735 3...
#> 4  8644db267ffffff          0 MULTIPOLYGON (((-81.3142 36...
#> 5  862a99237ffffff          0 MULTIPOLYGON (((-81.63851 3...
#> 6  862a9930fffffff          0 MULTIPOLYGON (((-81.51986 3...
#> 7  8644db34fffffff          0 MULTIPOLYGON (((-81.33057 3...
#> 8  862a99247ffffff          1 MULTIPOLYGON (((-81.49647 3...
#> 9  862a9931fffffff          0 MULTIPOLYGON (((-81.59216 3...
#> 10 8644db35fffffff          0 MULTIPOLYGON (((-81.26766 3...
# }
```
