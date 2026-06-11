# Calculate Landscape Complexity Metrics (IT Metrics) per Polygon

Calculates specified landscape complexity metrics (a subset of
Information Theory metrics) from a categorical land-cover raster for
each input polygon using
[`landscapemetrics::sample_lsm()`](https://r-spatialecology.github.io/landscapemetrics/reference/sample_lsm.html).
This function ensures a safe, alignment-guaranteed join of the results
back to the original geometry.

## Usage

``` r
calculate_it_metrics(landscape_raster, aoi_sf)
```

## Arguments

- landscape_raster:

  A `SpatRaster` object representing the categorical landscape (e.g.,
  LULC).

- aoi_sf:

  An `sf` object containing polygonal geometries (e.g., H3 hexagons) for
  which the landscape metrics should be calculated.

## Value

An `sf` object identical to `aoi_sf`, but with new columns appended. The
new columns represent the calculated landscape metrics (e.g.,
`lsm_shdi`) with an `lsm_` prefix.

## Details

This function calculates metrics at the `"landscape"` level, filtering
for `"complexity metric"` types. The function prioritizes data integrity
by adding a temporary `plot_id` column based on row index, which is used
by `landscapemetrics`.

Crucially, the function uses
[`dplyr::left_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
with this `plot_id` for merging the results. This \*\*robust join
method\*\* prevents data misalignment that could occur if rows were
dropped during metric calculation, which is a significant improvement
over the unsafe `cbind` method. The temporary `plot_id` column is
removed before the final object is returned.

## See also

[`sample_lsm`](https://r-spatialecology.github.io/landscapemetrics/reference/sample_lsm.html)
for available metrics.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'lulc' is a SpatRaster and 'hex_grid_sf' is an sf polygon grid
# metrics_sf <- calculate_it_metrics(lulc, hex_grid_sf)
# head(metrics_sf)
} # }
```
