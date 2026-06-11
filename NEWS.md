# paisaje 0.3.0

## New functions

* `get_chelsa_historic()` — Downloads CHELSA v2.1 bioclimatic variables
  (bio1–bio19) for the 1981–2010 reference period. Uses Cloud Optimized
  GeoTIFF (COG) streaming via `/vsicurl/`, so only the spatial subset
  covering the AOI is retrieved — no full global download required.
  Accepts `var = "all"` to request all 19 variables in a single call.

* `get_chelsa_future()` — Downloads CHELSA v2.1 future bioclimatic variables
  under CMIP6 scenarios (SSP126, SSP370, SSP585) for three periods
  (2011–2040, 2041–2070, 2071–2100) and five ISIMIP3b GCMs
  (GFDL-ESM4, IPSL-CM6A-LR, MPI-ESM1-2-HR, MRI-ESM2-0, UKESM1-0-LL).
  Also uses COG streaming. Note: SSP245 is not available in CHELSA v2.1.

## Breaking changes

* `get_nightlight_data()` — Complete rewrite. The function now wraps
  `blackmarbler::bm_raster()` (World Bank) instead of scraping the EOG
  (Earth Observation Group) website. **Interface change**: a new required
  `aoi_sf` argument (an `sf` object) replaces `year`/`month` as the first
  argument, and a `bearer` argument (NASA LAADS DAAC token) is now required.
  The function returns a `SpatRaster` cropped and masked to the AOI instead
  of a file path. See `?get_nightlight_data` for details on obtaining a
  bearer token.

## Improvements

* `get_nightlight_data()` now supports daily (`VNP46A1`, `VNP46A2`), monthly
  (`VNP46A3`), and annual (`VNP46A4`) NASA Black Marble products at 500 m
  resolution, with built-in quality filtering via `quality_flag_rm`.

* Migrated from `magrittr` pipe (`%>%`) to the native R pipe (`|>`) throughout
  the package (`extract_cat_raster()`, `get_records_by_hexagon()`). Requires
  R >= 4.1.0 (already declared in `Depends`).

## Dependency changes

* **Added**: `blackmarbler` (replaces EOG scraping for nightlight data).
* **Removed**: `rvest` (no longer needed after EOG scraper was dropped).
* **Removed**: `magrittr` (replaced by native pipe `|>`).

# paisaje 0.2.0

* First CRAN release (2026-01-07).
* Functions for downloading environmental variables: `get_worldclim_historic()`,
  `get_worldclim_future()`, `get_esa_10m()`, `get_nightlight_data()`.
* Hexagonal grid tools: `get_h3_grid()`, `get_records_by_hexagon()`.
* Zonal statistics: `extract_num_raster()`, `extract_cat_raster()`.
* Landscape metrics: `calculate_it_metrics()`.
* Biodiversity data: `get_records()`, `count_points_in_polygons()`.
* Included dataset: `cr_outline_c` (Costa Rica continental outline).
