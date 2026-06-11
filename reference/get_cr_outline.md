# Download Costa Rica Outline from GADM

Downloads the Costa Rica country boundary from GADM 4.1 via
[`gadm`](https://rspatial.github.io/geodata/reference/gadm.html) and
returns it as an `sf` object. By default returns only the continental
landmass, excluding the Isla del Coco and other minor oceanic islands.
The downloaded file is cached locally to avoid repeated downloads.

This function provides a reproducible, always up-to-date alternative to
the static
[`cr_outline_c`](https://manuelspinola.github.io/paisaje/reference/cr_outline_c.md)
dataset included in the package, and ensures consistency across packages
that depend on a Costa Rica AOI (e.g., paisaje and h3sdm).

## Usage

``` r
get_cr_outline(continental = TRUE, path = tempdir())
```

## Arguments

- continental:

  \`logical\`. If `TRUE` (default), returns only the continental
  landmass by selecting the polygon with the largest area. If `FALSE`,
  returns the full GADM boundary including all islands (Isla del Coco
  and minor Pacific/Caribbean islands).

- path:

  \`character\`. Directory where the GADM file will be cached. Defaults
  to [`tempdir()`](https://rdrr.io/r/base/tempfile.html). For persistent
  caching across sessions, provide a permanent directory (e.g.,
  `"data/"`).

## Value

An `sf` object with one feature (POLYGON geometry) in WGS 84
(EPSG:4326), representing the Costa Rica outline.

## Details

\## Caching GADM files are cached by
[`geodata::gadm()`](https://rspatial.github.io/geodata/reference/gadm.html)
in `path`. If the file already exists, it is loaded from disk without
downloading again. For persistent caching, set `path` to a permanent
directory.

\## Consistency across packages Both paisaje and h3sdm use Costa Rica as
the default study area in examples and vignettes. Using
`get_cr_outline()` in both packages ensures the same geometry is used,
derived from the same GADM version, rather than relying on static copies
that may diverge over time.

\## Island exclusion When `continental = TRUE`, the Isla del Coco (~550
km offshore in the Pacific Ocean) and any other minor islands are
excluded by retaining only the polygon with the largest area after
casting to individual `POLYGON` geometries.

## References

Global Administrative Areas (GADM) version 4.1.
<https://gadm.org/license.html>

## See also

- [`cr_outline_c`](https://manuelspinola.github.io/paisaje/reference/cr_outline_c.md)
  — static continental outline (no islands).

- [`cr_outline`](https://manuelspinola.github.io/paisaje/reference/cr_outline.md)
  — static full outline (with islands).

- [`gadm`](https://rspatial.github.io/geodata/reference/gadm.html) —
  underlying download function.

- [`get_h3_grid`](https://manuelspinola.github.io/paisaje/reference/get_h3_grid.md)
  — generate H3 grid over the returned AOI.

- GADM: <https://gadm.org>

## Examples

``` r
if (FALSE) { # \dontrun{
# Continental outline only (default)
cr <- get_cr_outline()
plot(sf::st_geometry(cr), main = "Costa Rica (continental)")

# Full outline including islands
cr_full <- get_cr_outline(continental = FALSE)
plot(sf::st_geometry(cr_full), main = "Costa Rica (all territory)")

# Use as AOI — consistent with h3sdm
h7  <- get_h3_grid(cr, res = 7)
bio <- get_chelsa_historic(var = "bio1", aoi = cr)

# Persistent cache
cr <- get_cr_outline(path = "data/gadm")
} # }
```
