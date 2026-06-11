# Changelog

## paisaje 0.3.0

### New functions

- [`get_chelsa_historic()`](https://manuelspinola.github.io/paisaje/reference/get_chelsa_historic.md)
  – Downloads CHELSA v2.1 bioclimatic variables (bio1-bio19) for the
  1981-2010 reference period. Uses Cloud Optimized GeoTIFF (COG)
  streaming via `/vsicurl/`, so only the spatial subset covering the AOI
  is retrieved – no full global download required. Accepts `var = "all"`
  to request all 19 variables in a single call. Resolution is fixed at
  30 arc-seconds (~1 km); no `res` parameter.

- [`get_chelsa_future()`](https://manuelspinola.github.io/paisaje/reference/get_chelsa_future.md)
  – Downloads CHELSA v2.1 future bioclimatic variables under CMIP6
  scenarios (SSP126, SSP370, SSP585) for three periods (2011-2040,
  2041-2070, 2071-2100) and five ISIMIP3b GCMs (GFDL-ESM4, IPSL-CM6A-LR,
  MPI-ESM1-2-HR, MRI-ESM2-0, UKESM1-0-LL). Also uses COG streaming.
  Note: SSP245 is not available in CHELSA v2.1. Resolution is fixed at
  30 arc-seconds (~1 km).

- [`get_cr_outline()`](https://manuelspinola.github.io/paisaje/reference/get_cr_outline.md)
  – Downloads the Costa Rica boundary from GADM 4.1 via
  [`geodata::gadm()`](https://rspatial.github.io/geodata/reference/gadm.html)
  and returns it as an `sf` object. The `continental` argument (default
  `TRUE`) controls whether to return only the continental landmass
  (excluding Isla del Coco and minor islands) or the full national
  territory. The downloaded file is cached locally.

### New datasets

- `cr_outline` – Costa Rica full outline (continental landmass + Isla
  del Coco and all minor oceanic islands), derived from GADM 4.1.

### Breaking changes

- [`get_nightlight_data()`](https://manuelspinola.github.io/paisaje/reference/get_nightlight_data.md)
  – Complete rewrite. The function now wraps
  [`blackmarbler::bm_raster()`](https://worldbank.github.io/blackmarbler/reference/bm_raster.html)
  (World Bank) instead of scraping the EOG (Earth Observation Group)
  website. **Interface change**: a new required `aoi_sf` argument (an
  `sf` object) replaces `year`/`month` as the first argument, and a
  `bearer` argument (NASA LAADS DAAC token) is now required. The
  function returns a `SpatRaster` cropped and masked to the AOI instead
  of a file path. See
  [`?get_nightlight_data`](https://manuelspinola.github.io/paisaje/reference/get_nightlight_data.md)
  for details on obtaining a bearer token.

### Improvements

- [`get_nightlight_data()`](https://manuelspinola.github.io/paisaje/reference/get_nightlight_data.md)
  now supports daily (`VNP46A1`, `VNP46A2`), monthly (`VNP46A3`), and
  annual (`VNP46A4`) NASA Black Marble products at 500 m resolution,
  with built-in quality filtering via `quality_flag_rm`.

- `cr_outline_c` dataset regenerated from GADM 4.1 with a fully
  reproducible script in `data-raw/cr_outline.R`.

- Migrated from `magrittr` pipe (`%>%`) to the native R pipe (`|>`)
  throughout the package
  ([`extract_cat_raster()`](https://manuelspinola.github.io/paisaje/reference/extract_cat_raster.md),
  [`get_records_by_hexagon()`](https://manuelspinola.github.io/paisaje/reference/get_records_by_hexagon.md)).
  Requires R \>= 4.1.0 (already declared in `Depends`).

### Dependency changes

- **Added**: `blackmarbler` (replaces EOG scraping for nightlight data).
- **Added**: `geodata` (used by
  [`get_cr_outline()`](https://manuelspinola.github.io/paisaje/reference/get_cr_outline.md)).
- **Removed**: `rvest` (no longer needed after EOG scraper was dropped).
- **Removed**: `magrittr` (replaced by native pipe `|>`).

## paisaje 0.2.0

CRAN release: 2026-01-07

- First CRAN release (2026-01-07).
- Functions for downloading environmental variables:
  [`get_worldclim_historic()`](https://manuelspinola.github.io/paisaje/reference/get_worldclim_historic.md),
  [`get_worldclim_future()`](https://manuelspinola.github.io/paisaje/reference/get_worldclim_future.md),
  [`get_esa_10m()`](https://manuelspinola.github.io/paisaje/reference/get_esa_10m.md),
  [`get_nightlight_data()`](https://manuelspinola.github.io/paisaje/reference/get_nightlight_data.md).
- Hexagonal grid tools:
  [`get_h3_grid()`](https://manuelspinola.github.io/paisaje/reference/get_h3_grid.md),
  [`get_records_by_hexagon()`](https://manuelspinola.github.io/paisaje/reference/get_records_by_hexagon.md).
- Zonal statistics:
  [`extract_num_raster()`](https://manuelspinola.github.io/paisaje/reference/extract_num_raster.md),
  [`extract_cat_raster()`](https://manuelspinola.github.io/paisaje/reference/extract_cat_raster.md).
- Landscape metrics:
  [`calculate_it_metrics()`](https://manuelspinola.github.io/paisaje/reference/calculate_it_metrics.md).
- Biodiversity data:
  [`get_records()`](https://manuelspinola.github.io/paisaje/reference/get_records.md),
  [`count_points_in_polygons()`](https://manuelspinola.github.io/paisaje/reference/count_points_in_polygons.md).
- Included dataset: `cr_outline_c` (Costa Rica continental outline).
