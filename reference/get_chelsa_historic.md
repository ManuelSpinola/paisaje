# Download and Process Historic Climate Variables from CHELSA v2.1

Downloads historic bioclimatic variables from CHELSA v2.1 (Climatologies
at High Resolution for the Earth's Land Surface Areas) for the reference
period 1981-2010. The data are served as Cloud Optimized GeoTIFFs (COGs)
from the Swiss WSL EnviCloud object store, which allows this function to
retrieve only the spatial subset covering `aoi` without downloading the
global file (~110 MB per variable).

One or more bioclimatic variables (bio1-bio19) can be requested in a
single call. The result is a multi-layer `SpatRaster` optionally cropped
and masked to the AOI, consistent with the interface of
[`get_worldclim_historic`](https://manuelspinola.github.io/paisaje/reference/get_worldclim_historic.md).

## Usage

``` r
get_chelsa_historic(
  var = "bio1",
  aoi = NULL,
  destination_dir = NULL,
  timeout = 300
)
```

## Arguments

- var:

  \`character\` vector. One or more CHELSA bioclimatic variable names.
  Accepted values: `"bio1"` through `"bio19"`, or `"all"` (downloads all
  19 variables). Variable names are case-insensitive. Default: `"bio1"`.

  - `bio1` – Annual mean temperature (degC x 10)

  - `bio2` – Mean diurnal temperature range

  - `bio3` – Isothermality

  - `bio4` – Temperature seasonality

  - `bio5` – Max temperature of warmest month

  - `bio6` – Min temperature of coldest month

  - `bio7` – Temperature annual range

  - `bio8` – Mean temperature of wettest quarter

  - `bio9` – Mean temperature of driest quarter

  - `bio10` – Mean temperature of warmest quarter

  - `bio11` – Mean temperature of coldest quarter

  - `bio12` – Annual precipitation (kg m-2 yr-1)

  - `bio13` – Precipitation of wettest month

  - `bio14` – Precipitation of driest month

  - `bio15` – Precipitation seasonality

  - `bio16` – Precipitation of wettest quarter

  - `bio17` – Precipitation of driest quarter

  - `bio18` – Precipitation of warmest quarter

  - `bio19` – Precipitation of coldest quarter

- aoi:

  \`sf\` or \`SpatVector\` or \`NULL\`. Area of interest used to crop
  and mask the raster. If `NULL` (default), the global raster is
  returned. Providing an AOI is strongly recommended – each CHELSA
  variable is ~110 MB globally, and the COG format enables efficient
  spatial subsetting.

- destination_dir:

  \`character\` or \`NULL\`. Directory where the output `.tif` will be
  saved. If `NULL` (default), a temporary directory is used and a
  message is emitted.

- timeout:

  \`numeric\`. Maximum time in seconds for each HTTP request. Default:
  `300`.

## Value

A `SpatRaster` with one layer per requested variable, named after the
CHELSA filename convention (e.g., `CHELSA_bio1_1981-2010_V.2.1`). If
`aoi` is provided, the raster is cropped and masked to it. Also written
to `destination_dir` as a single multi-layer `.tif`. Returns `NULL`
invisibly on error.

## Details

\## Spatial resolution CHELSA v2.1 is provided at a \*\*fixed resolution
of 30 arc-seconds (~1 km)\*\* globally. Unlike
[`get_worldclim_historic`](https://manuelspinola.github.io/paisaje/reference/get_worldclim_historic.md),
there is no `res` parameter – CHELSA does not offer coarser resolutions
(2.5, 5, or 10 arc-minutes). If you need multi-resolution data, use
[`get_worldclim_historic`](https://manuelspinola.github.io/paisaje/reference/get_worldclim_historic.md)
instead, or downsample the CHELSA output with
[`terra::aggregate()`](https://rspatial.github.io/terra/reference/aggregate.html).

\## Why CHELSA over WorldClim? CHELSA v2.1 and WorldClim v2.1 are both
high-resolution (~1 km) global climatologies, but differ in their
downscaling methodology:

- CHELSA uses a \*\*mechanistic downscaling\*\* approach based on
  atmospheric dynamics and orographic effects, which tends to perform
  better in complex terrain (mountains, coasts).

- WorldClim uses \*\*statistical interpolation\*\* (thin-plate splines),
  which is faster but less physically grounded.

- For tropical regions with complex topography (e.g., Costa Rica),
  CHELSA is generally considered more accurate.

\## COG streaming – no full download required CHELSA files are Cloud
Optimized GeoTIFFs hosted on the Swiss WSL EnviCloud. This function uses
the `/vsicurl/` virtual filesystem prefix from GDAL (via `terra`) to
stream only the tiles that cover `aoi`, avoiding downloading the entire
global file. When `aoi` is provided, spatial subsetting is done in
memory before writing to disk.

\## Reference period All historic CHELSA v2.1 bioclimatic variables use
the \*\*1981-2010\*\* climatological normal period. For future
projections see
[`get_chelsa_future`](https://manuelspinola.github.io/paisaje/reference/get_chelsa_future.md).

## References

Karger, D. N., Conrad, O., Bohner, J., Kawohl, T., Kreft, H.,
Soria-Auza, R. W., Zimmermann, N. E., Linder, P., & Kessler, M. (2017).
Climatologies at high resolution for the earth's land surface areas
(CHELSA). *Scientific Data*, 4, 170122.
[doi:10.1038/sdata.2017.122](https://doi.org/10.1038/sdata.2017.122)

Brun, P., Zimmermann, N. E., Hari, C., Pellissier, L., & Karger, D. N.
(2022). Global climate-related predictors at kilometre resolution for
the past and future. *Earth System Science Data*, 14, 5573-5603.
[doi:10.5194/essd-14-5573-2022](https://doi.org/10.5194/essd-14-5573-2022)

## See also

- [`get_chelsa_future`](https://manuelspinola.github.io/paisaje/reference/get_chelsa_future.md)
  – CHELSA future projections (CMIP6).

- [`get_worldclim_historic`](https://manuelspinola.github.io/paisaje/reference/get_worldclim_historic.md)
  – WorldClim v2.1 historic data.

- [`extract_num_raster`](https://manuelspinola.github.io/paisaje/reference/extract_num_raster.md)
  – extract area-weighted means per polygon.

- CHELSA website: <https://chelsa-climate.org>

- EnviCloud browser:
  <https://envicloud.wsl.ch/#/?bucket=https://os.zhdk.cloud.switch.ch/chelsav2/>

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)

# Use Costa Rica outline (included in paisaje)
aoi <- paisaje::cr_outline_c

# Single variable -- Annual mean temperature
bio1 <- get_chelsa_historic(var = "bio1", aoi = aoi)

# Multiple variables
bio_stack <- get_chelsa_historic(var = c("bio1", "bio12", "bio15"), aoi = aoi)

# All 19 bioclimatic variables
bio_all <- get_chelsa_historic(var = "all", aoi = aoi)

# Extract mean values per H3 hexagon
h7      <- paisaje::get_h3_grid(aoi, res = 7)
h7_clim <- extract_num_raster(bio_stack, h7)
} # }
```
