# Download and Retrieve Nightlight Data from NASA Black Marble

Downloads NASA Black Marble nighttime lights raster data for a given
area of interest (`aoi_sf`), time period, and temporal resolution
(daily, monthly, or annual). Internally wraps
[`bm_raster`](https://worldbank.github.io/blackmarbler/reference/bm_raster.html)
from the blackmarbler package (World Bank), which handles tile
discovery, download, mosaicing, and cloud/quality masking automatically.

The result is a `SpatRaster` cropped and masked to the AOI, ready to be
passed directly to
[`extract_num_raster`](https://manuelspinola.github.io/paisaje/reference/extract_num_raster.md)
or any `terra`-based workflow in paisaje.

## Usage

``` r
get_nightlight_data(
  aoi_sf,
  year,
  month = NULL,
  product_id = "VNP46A3",
  bearer,
  variable = NULL,
  quality_flag_rm = NULL,
  destination_dir = NULL,
  timeout = 1200
)
```

## Arguments

- aoi_sf:

  \`sf\`. An `sf` object defining the area of interest (AOI). Can be any
  polygon geometry (country outline, H3 grid extent, custom boundary).
  The raster will be cropped and masked to this extent.

- year:

  \`numeric\` or \`character\`. The year of interest (e.g., `2022`).
  Used together with `month` to build the `date` argument passed to
  `bm_raster`.

- month:

  \`numeric\` or `NULL`. Month of the year (1–12). Required when
  `product_id = "VNP46A3"` (monthly composite). Ignored for annual
  products (`"VNP46A4"`). Default is `NULL`.

- product_id:

  \`character\`. NASA Black Marble product identifier. Available
  options:

  - `"VNP46A1"` — Daily, at-sensor radiance (500 m).

  - `"VNP46A2"` — Daily, BRDF-corrected and gap-filled (500 m). Default
    variable: `Gap_Filled_DNB_BRDF-Corrected_NTL`.

  - `"VNP46A3"` — **Monthly** composite, snow-free (500 m). Default
    variable: `NearNadir_Composite_Snow_Free`.

  - `"VNP46A4"` — **Annual** composite, snow-free (500 m). Default
    variable: `NearNadir_Composite_Snow_Free`.

  Default: `"VNP46A3"` (monthly).

- bearer:

  \`character\`. NASA LAADS DAAC bearer token required for
  authentication. Obtain a free token at
  <https://ladsweb.modaps.eosdis.nasa.gov/> under *Login \> Generate
  Token*. It is strongly recommended to store this token as an
  environment variable (e.g., `NASA_BEARER`) and retrieve it with
  `Sys.getenv("NASA_BEARER")` rather than hardcoding it in scripts.

- variable:

  \`character\` or `NULL`. Specific variable (layer) to extract from the
  HDF5 product. If `NULL` (default), the package default for each
  `product_id` is used (see parameter description above). Pass `""` to
  trigger an informative error listing all valid variable names.

- quality_flag_rm:

  \`integer vector\` or `NULL`. Quality flag values for which pixels
  will be set to `NA`. Lower quality values can be removed to reduce
  noise. Default is `NULL` (no quality filtering).

- destination_dir:

  \`character\` or `NULL`. Directory where the output `.tif` and
  intermediate HDF5 tiles will be cached. If `NULL` (default), the
  system's temporary directory
  ([`tempdir`](https://rdrr.io/r/base/tempfile.html)) is used and a
  message is emitted.

- timeout:

  \`numeric\`. Maximum time in seconds allowed for HTTP downloads.
  Temporarily overrides `getOption("timeout")` and restores the original
  value on exit. Default: `1200` (20 minutes).

## Value

A `SpatRaster` object cropped and masked to `aoi_sf`, containing the
requested nighttime lights variable. Layer name reflects the product and
date. Also written to `destination_dir` as a `.tif`. Returns `NULL`
invisibly if an error occurs, with an informative message.

## Details

\## Why NASA Black Marble over EOG? The previous implementation
downloaded a global monthly raster (~500 MB) from the Earth Observation
Group (EOG, Colorado School of Mines) via HTML scraping, regardless of
the AOI size. NASA Black Marble improves on this in several ways:

- **AOI-aware**: only downloads the MODIS/VIIRS tiles that intersect
  `aoi_sf`, dramatically reducing download size for regional studies.

- **Higher scientific quality**: applies lunar irradiance modeling,
  atmospheric correction, BRDF correction, and cloud masking at the
  algorithm level (not post-hoc).

- **Daily, monthly, and annual** products under a unified interface.

- **Stable API**: accesses NASA LAADS DAAC via token-authenticated
  `httr2` requests — no fragile HTML scraping.

- **Resolution**: 500 m (vs. ~750 m for the EOG monthly_notile product).

\## Bearer token setup The NASA bearer token is free but required.
Recommended setup:


    # In .Renviron (open with usethis::edit_r_environ()):
    NASA_BEARER=your_token_here

    # Then in your script:
    bearer <- Sys.getenv("NASA_BEARER")

\## Integration with paisaje The returned `SpatRaster` is ready to be
passed directly to
[`extract_num_raster`](https://manuelspinola.github.io/paisaje/reference/extract_num_raster.md)
to summarize nightlight values per polygon or H3 hexagon grid.

## References

Román, M. O., et al. (2018). NASA's Black Marble nighttime lights
product suite. *Remote Sensing of Environment*, 210, 113–143.
[doi:10.1016/j.rse.2018.03.017](https://doi.org/10.1016/j.rse.2018.03.017)

Marty, R., & Vicente, G. S. (2024). *blackmarbler: Georeferenced Rasters
and Statistics of Nighttime Lights from NASA Black Marble*. R package
v0.2.5. World Bank. <https://worldbank.github.io/blackmarbler/>

## See also

- [`bm_raster`](https://worldbank.github.io/blackmarbler/reference/bm_raster.html)
  — underlying download function.

- [`extract_num_raster`](https://manuelspinola.github.io/paisaje/reference/extract_num_raster.md)
  — extract area-weighted means per polygon.

- [`get_worldclim_historic`](https://manuelspinola.github.io/paisaje/reference/get_worldclim_historic.md)
  — analogous function for climate data.

- [`get_esa_10m`](https://manuelspinola.github.io/paisaje/reference/get_esa_10m.md)
  — analogous function for land cover data.

- NASA Black Marble portal: <https://blackmarble.gsfc.nasa.gov/>

- LAADS DAAC token: <https://ladsweb.modaps.eosdis.nasa.gov/>

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)

# Use Costa Rica outline (included in paisaje)
aoi <- paisaje::cr_outline_c

# Bearer token from environment variable (recommended)
bearer <- Sys.getenv("NASA_BEARER")

# Monthly composite — March 2022
ntl <- get_nightlight_data(
  aoi_sf     = aoi,
  year       = 2022,
  month      = 3,
  product_id = "VNP46A3",
  bearer     = bearer
)

# Annual composite — 2021
ntl_anual <- get_nightlight_data(
  aoi_sf     = aoi,
  year       = 2021,
  product_id = "VNP46A4",
  bearer     = bearer
)

# Extract mean nightlight per H3 hexagon
h7     <- paisaje::get_h3_grid(aoi, res = 7)
h7_ntl <- extract_num_raster(ntl, h7)
} # }
```
