# Download and process future environmental variables from WorldClim v2.1

Downloads future climate data from WorldClim based on CMIP6 climate
models and SSP scenarios. The data can be retrieved at various spatial
resolutions and optionally clipped to a specified area of interest
(AOI).

## Usage

``` r
get_worldclim_future(
  var = "bioc",
  res = "30s",
  scenario = "585",
  time_range = "2021-2040",
  gcm = "ACCESS-CM2",
  aoi = NULL,
  retries = 3,
  timeout = 300,
  destination_dir = NULL
)
```

## Arguments

- var:

  character Climate variable to download. Options:

  - "bioc" — Bioclimatic variables (19 variables)

  - "prec" — Precipitation

  - "tavg" — Average temperature

  - "tmin" — Minimum temperature

  - "tmax" — Maximum temperature

  Default is "bioc".

- res:

  character Spatial resolution of the data. Options:

  - "30s" — ~1 km (30 arc-seconds)

  - "2.5m" — ~5 km (2.5 arc-minutes)

  - "5m" — ~10 km (5 arc-minutes)

  - "10m" — ~20 km (10 arc-minutes)

  Default is "30s".

- scenario:

  character SSP scenario. Options:

  - "126" — SSP1-2.6 (low emissions)

  - "245" — SSP2-4.5 (intermediate emissions)

  - "370" — SSP3-7.0 (high emissions)

  - "585" — SSP5-8.5 (very high emissions)

  Default is "585".

- time_range:

  character Time period. Options:

  - "2021-2040"

  - "2041-2060"

  - "2061-2080"

  - "2081-2100"

  Default is "2021-2040".

- gcm:

  character General Circulation Model. Options: "ACCESS-CM2",
  "ACCESS-ESM1-5", "AWI-CM-1-1-MR", "BCC-CSM2-MR", "CanESM5",
  "CanESM5-CanOE", "CMCC-ESM2", "CNRM-CM6-1", "CNRM-CM6-1-HR",
  "CNRM-ESM2-1", "EC-Earth3-Veg", "EC-Earth3-Veg-LR", "FIO-ESM-2-0",
  "GFDL-ESM4", "GISS-E2-1-G", "GISS-E2-1-H", "HadGEM3-GC31-LL",
  "INM-CM4-8", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC-ES2L", "MIROC6",
  "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MRI-ESM2-0", "UKESM1-0-LL". Default
  is "ACCESS-CM2".

- aoi:

  sf or SpatRaster Optional area of interest to clip the data. Default
  is NULL (no clipping).

- retries:

  integer Number of attempts to retry download in case of failure.
  Default is 3.

- timeout:

  numeric Download timeout in seconds. Default is 300.

- destination_dir:

  character Directory where downloaded data will be stored. Default is
  NULL (uses a temporary directory).

## Value

SpatRaster object containing the selected climate variables, optionally
clipped to the specified AOI.

## References

Fick, S. E., & Hijmans, R. J. (2017). WorldClim 2: new 1-km spatial
resolution climate surfaces for global land areas. International Journal
of Climatology, 37(12), 4302–4315.
[doi:10.1002/joc.5086](https://doi.org/10.1002/joc.5086)

## Examples

``` r
# \donttest{
nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
#> Reading layer `nc' from data source 
#>   `/home/runner/work/_temp/Library/sf/shape/nc.shp' using driver `ESRI Shapefile'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
nc <- sf::st_transform(nc, crs = 4326)

climate_future <- paisaje::get_worldclim_future(
  var = "tmin", res = "10m", scenario = "585",
  time_range = "2021-2040", gcm = "ACCESS-CM2", aoi = nc
)
#> No destination_dir provided. Using temporary directory: /tmp/RtmpPIbJHF
#> Download URL: https://geodata.ucdavis.edu/cmip6/10m/ACCESS-CM2/ssp585/wc2.1_10m_tmin_ACCESS-CM2_ssp585_2021-2040.tif
#> Raster saved at: /tmp/RtmpPIbJHF/wc2.1_10m_tmin_ACCESS-CM2_ssp585_2021-2040.tif
# }
```
