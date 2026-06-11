## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* macOS (local), R 4.x.x
* win-builder (devel and release)

## Revisions

This is a new minor release (0.2.0 -> 0.3.0).

### New functions
- get_chelsa_historic(): CHELSA v2.1 historic bioclimatic variables via COG streaming
- get_chelsa_future(): CHELSA v2.1 future bioclimatic variables (CMIP6) via COG streaming
- get_cr_outline(): Costa Rica outline from GADM with continental option

### New datasets
- cr_outline: full Costa Rica outline including all islands

### Breaking changes
- get_nightlight_data(): rewritten as NASA Black Marble wrapper (blackmarbler)
  requires aoi_sf and bearer arguments, returns SpatRaster instead of file path

### Dependency changes
- Added: blackmarbler, geodata
- Removed: rvest, magrittr
