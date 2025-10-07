#' @name get_h3_grid
#' @title Generate an H3 Hexagonal Grid for an sf Object
#' @description
#' Generates a hexagonal grid of H3 cells at a specified resolution
#' that intersect with a given `sf` object. This is a wrapper for
#' functions from the \pkg{h3jsr} package.
#'
#' @usage get_h3_grid(sf_object, resolution = 6, expand_factor = 0.1)
#'
#' @param sf_object (\code{sf}) An sf object defining the area of interest.
#'   Must have a valid coordinate reference system (CRS).
#' @param resolution (\code{integer}) H3 resolution level (1–16). Default is 6.
#'   Lower values produce fewer, larger hexagons (faster processing, coarser grid).
#' @param expand_factor (\code{numeric}) Expands bounding box to ensure coverage.
#'   Default is 0.1.
#'
#' @return (\code{sf}) An sf object containing the hexagonal grid polygons
#'   covering the input area. Each polygon represents an H3 cell at the specified
#'   resolution, with a column containing the H3 index.
#'
#' @details
#' Reducing the resolution (e.g., 5 or 6) can greatly reduce processing time and memory
#' usage, especially for large AOIs. Each decrease in resolution increases the size of
#' individual hexagons exponentially.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#' h3_grid_sf <- get_h3_grid(nc, resolution = 6)
#' }

get_h3_grid <- function(sf_object, resolution = 6, expand_factor = 0.1) {
  # Validaciones iniciales
  if (!inherits(sf_object, "sf")) stop("Input must be an sf object")
  if (resolution < 1 || resolution > 16) stop("Resolution must be between 1 and 16")

  # Transformar a WGS84 y asegurar geometrías válidas
  sf_object <- sf::st_transform(sf_object, 4326)
  sf_object <- sf::st_make_valid(sf_object)

  # Expandir bbox ligeramente
  bbox <- sf::st_bbox(sf_object)
  bbox_expanded <- bbox + c(-expand_factor, -expand_factor, expand_factor, expand_factor)
  bbox_poly <- sf::st_as_sfc(bbox_expanded)

  # Crear hexágonos H3 sobre el bbox expandido
  h3_cells <- h3jsr::polygon_to_cells(bbox_poly, res = resolution)
  hexagons <- h3jsr::cell_to_polygon(h3_cells, simple = FALSE)

  # Recortar hexágonos al AOI
  hexagons <- sf::st_intersection(hexagons, sf_object)

  # Unir fragmentos por celda para que siempre sea MULTIPOLYGON
  hexagons <- sf::st_union(hexagons, by_feature = TRUE)

  # Asegurar que seguimos teniendo geometrías válidas
  hexagons <- sf::st_make_valid(hexagons)

  # Devolver
  return(hexagons)
}
