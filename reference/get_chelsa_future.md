# Download and Process Future Climate Variables from CHELSA v2.1 (CMIP6)

Downloads future bioclimatic variables from CHELSA v2.1 under CMIP6
climate scenarios. Data are available for three SSP scenarios, five GCMs
(ISIMIP3b selection), and three future periods. Files are served as
Cloud Optimized GeoTIFFs (COGs) from the Swiss WSL EnviCloud, enabling
efficient spatial subsetting via `/vsicurl/` without downloading global
files.

One or more bioclimatic variables (bio1–bio19) can be requested in a
single call. The result is a multi-layer `SpatRaster` optionally cropped
and masked to the AOI, consistent with the interface of
[`get_worldclim_future`](https://manuelspinola.github.io/paisaje/reference/get_worldclim_future.md).

## Usage

``` r
get_chelsa_future(
  var = "bio1",
  scenario = "ssp585",
  period = "2041-2070",
  gcm = "MPI-ESM1-2-HR",
  aoi = NULL,
  destination_dir = NULL,
  timeout = 300
)
```

## Arguments

- var:

  \`character\` vector. One or more CHELSA bioclimatic variable names
  (`"bio1"` through `"bio19"`), or `"all"` to download all 19. Default:
  `"bio1"`.

- scenario:

  \`character\`. SSP emission scenario. Options:

  - `"ssp126"` - SSP1-2.6 (low emissions, sustainable development).

  - `"ssp370"` - SSP3-7.0 (high emissions, regional rivalry).

  - `"ssp585"` - SSP5-8.5 (very high emissions, fossil-fueled growth).

  Default: `"ssp585"`.

- period:

  \`character\`. Future climatological period. Options:

  - `"2011-2040"` - Near future.

  - `"2041-2070"` - Mid future.

  - `"2071-2100"` - Far future.

  Default: `"2041-2070"`.

- gcm:

  \`character\`. Global Circulation Model following the ISIMIP3b
  selection. Options:

  - `"GFDL-ESM4"` - Priority 1 (highest priority).

  - `"IPSL-CM6A-LR"` - Priority 2.

  - `"MPI-ESM1-2-HR"` - Priority 3.

  - `"MRI-ESM2-0"` - Priority 4.

  - `"UKESM1-0-LL"` - Priority 5.

  When fewer than five models are used, selection should follow priority
  order. Default: `"MPI-ESM1-2-HR"`.

- aoi:

  \`sf\` or \`SpatVector\` or \`NULL\`. Area of interest used to crop
  and mask the raster. If `NULL` (default), the global raster is
  returned. Providing an AOI is strongly recommended given file sizes.

- destination_dir:

  \`character\` or \`NULL\`. Directory where the output `.tif` will be
  saved. If `NULL`, a temporary directory is used.

- timeout:

  \`numeric\`. Maximum time in seconds per HTTP request. Default: `300`.

## Value

A `SpatRaster` with one layer per requested variable. If `aoi` is
provided, the raster is cropped and masked to it. Also written to
`destination_dir` as a single multi-layer `.tif`. Returns `NULL`
invisibly on error.

## Details

\## Spatial resolution CHELSA v2.1 future projections are at a \*\*fixed
resolution of 30 arc-seconds (~1 km)\*\*. There is no `res` parameter -
unlike WorldClim, CHELSA does not offer coarser resolutions. To
downsample, use
[`terra::aggregate()`](https://rspatial.github.io/terra/reference/aggregate.html)
on the returned `SpatRaster`.

\## GCM availability CHELSA v2.1 future projections follow the ISIMIP3b
model selection, which provides five GCMs covering a range of climate
sensitivities and regional performance. Not all SSP x GCM x period
combinations are guaranteed to be available on the server. If a
combination is unavailable, the function emits a warning and returns
`NULL` for that variable.

\## SSP scenarios available CHELSA v2.1 provides SSP126, SSP370, and
SSP585. Note that SSP245 (available in WorldClim) is \*\*not
available\*\* in CHELSA v2.1 future bioclimatic variables.

\## COG streaming Files use the `/vsicurl/` GDAL virtual filesystem to
stream only the tiles covering `aoi` from the remote COG, avoiding full
file downloads.

\## Comparison with get_worldclim_future()

- CHELSA uses mechanistic downscaling; WorldClim uses statistical
  interpolation.

- CHELSA offers SSP126/370/585; WorldClim offers SSP126/245/370/585.

- CHELSA provides 5 GCMs (ISIMIP3b); WorldClim provides 23 GCMs.

- Both are at ~1 km (30 arc-second) resolution.

- For regions with complex terrain, CHELSA is generally preferred.

## References

Karger, D. N., Conrad, O., B00f6hner, J., Kawohl, T., Kreft, H.,
Soria-Auza, R. W., Zimmermann, N. E., Linder, P., & Kessler, M. (2017).
Climatologies at high resolution for the earth's land surface areas
(CHELSA). *Scientific Data*, 4, 170122.
[doi:10.1038/sdata.2017.122](https://doi.org/10.1038/sdata.2017.122)

Brun, P., Zimmermann, N. E., Hari, C., Pellissier, L., & Karger, D. N.
(2022). Global climate-related predictors at kilometre resolution for
the past and future. *Earth System Science Data*, 14, 5573-5603.
[doi:10.5194/essd-14-5573-2022](https://doi.org/10.5194/essd-14-5573-2022)

## See also

- [`get_chelsa_historic`](https://manuelspinola.github.io/paisaje/reference/get_chelsa_historic.md) -
  CHELSA 1981-2010 baseline.

- [`get_worldclim_future`](https://manuelspinola.github.io/paisaje/reference/get_worldclim_future.md) -
  WorldClim v2.1 future projections.

- [`extract_num_raster`](https://manuelspinola.github.io/paisaje/reference/extract_num_raster.md) -
  extract area-weighted means per polygon.

- CHELSA CMIP6: <https://chelsa-climate.org/>

- EnviCloud browser:
  <https://envicloud.wsl.ch/#/?bucket=https://os.zhdk.cloud.switch.ch/chelsav2/>

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)

# Use Costa Rica outline (included in paisaje)
aoi <- paisaje::cr_outline_c

# Single variable - Annual mean temperature, mid-century, pessimistic
bio1_fut <- get_chelsa_future(
  var      = "bio1",
  scenario = "ssp585",
  period   = "2041-2070",
  gcm      = "MPI-ESM1-2-HR",
  aoi      = aoi
)

# Multiple variables - near future, optimistic
bio_stack <- get_chelsa_future(
  var      = c("bio1", "bio12", "bio15"),
  scenario = "ssp126",
  period   = "2011-2040",
  gcm      = "GFDL-ESM4",
  aoi      = aoi
)

# Compare historic vs future bio1 for Costa Rica
bio1_hist <- get_chelsa_historic(var = "bio1", aoi = aoi)
bio1_diff <- bio1_fut - bio1_hist
terra::plot(bio1_diff, main = "Temperature change (bio1): 2041-2070 SSP585")

# Extract per H3 hexagon
h7      <- paisaje::get_h3_grid(aoi, res = 7)
h7_fut  <- extract_num_raster(bio_stack, h7)
} # }
```
