# Create Categorical Land Cover Raster from Copernicus ESA WorldCover Data

This function takes a \`SpatRaster\` object containing Copernicus ESA
WorldCover land cover data, reclassifies it into categorical land cover
classes based on predefined schemes, and returns the resulting
categorical raster.

## Usage

``` r
create_cat_esa_10m(land_cover)
```

## Arguments

- land_cover:

  A \`SpatRaster\` object representing the input land cover raster from
  Copernicus ESA WorldCover. This raster should contain land cover
  classes as defined by the Copernicus program.

## Value

A \`SpatRaster\` object containing the reclassified categorical land
cover raster. Each pixel will have a category corresponding to a defined
land cover type.

## Details

The function uses a predefined classification scheme for ESA WorldCover
data, assigning numeric or categorical values to represent different
land cover types. The resulting raster can be used for further spatial
analysis or landscape ecology studies.

## References

Zanaga, D., Van De Kerchove, R., De Keersmaecker, W., et al. (2021). ESA
WorldCover 10 m 2020 v100. https://doi.org/10.5281/zenodo.5571936
Zanaga, D., Van De Kerchove, R., Daems, D., et al. (2022). ESA
WorldCover 10 m 2021 v200. https://doi.org/10.5281/zenodo.7254221 ESA
WorldCover project 2020 and 2021. Contains modified Copernicus Sentinel
data processed by ESA WorldCover consortium. [ESA
WorldCover](https://esa-worldcover.org/en)

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'land_cover_raster' is a SpatRaster object from ESA WorldCover
cat_raster <- create_cat_esa_10m(land_cover_raster)
} # }

```
