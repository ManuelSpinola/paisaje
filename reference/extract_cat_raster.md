# Calculate Area Proportions for Categorical Raster Classes (Generic)

Extracts and calculates the \*\*area proportion\*\* of each categorical
class (e.g., LULC) found within each input polygon. This function uses
area-weighting to ensure highly accurate, sub-pixel zonal statistics.

## Usage

``` r
extract_cat_raster(spat_raster_cat, sf_hex_grid, proportion = TRUE)
```

## Arguments

- spat_raster_cat:

  A single-layer `SpatRaster` object containing categorical values.

- sf_hex_grid:

  An `sf` object containing polygonal geometries. The function will use
  `h3_address` if present, otherwise it creates and uses a temporary
  `ID` column for joining.

- proportion:

  Logical. If `TRUE` (default), the output values are the proportion of
  the polygon area covered by each category (summing to 1 for the
  covered area). If `FALSE`, the output is the raw sum of the coverage
  fraction (area).

## Value

An `sf` object identical to `sf_hex_grid`, but with new columns appended
for each categorical value found in the raster. Column names follow the
pattern `<layer_name>_prop_<category_value>`. Columns are
\*\*numerically ordered\*\* by the category value.

## Details

This function replaces the simplistic, non-area-weighted
[`table()`](https://rdrr.io/r/base/table.html) counting method with a
robust custom function utilizing `dplyr` and the `coverage_fraction`
column from `exactextractr`. Key features include:

- \*\*Area-Weighted Accuracy:\*\* Uses `coverage_fraction` for precise
  results.

- \*\*NA Filtering:\*\* Excludes `NA` raster values to prevent a
  `prop_NaN` column.

- \*\*Numerical Ordering:\*\* Sorts the final output columns by category
  number (e.g., 70 before 80).

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'lulc' is a categorical SpatRaster and 'hex_grid' is an sf polygon grid
# cat_data_p <- extract_cat_raster(lulc, hex_grid)
# head(cat_data_p)
} # }
```
