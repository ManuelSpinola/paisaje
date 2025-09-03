#' @name get_h3_grid
#' @title Generate an H3 Hexagonal Grid for an sf Object
#' @description
#' Generates a hexagonal grid of H3 cells at a specified resolution
#' that intersect with a given `sf` object. Wrapper for h3jsr functions.
#' @usage get_h3_grid(sf_object, resolution = 7, expand_factor = 0.1)
#' @param sf_object `sf` object. Must have a valid CRS.
#' @param resolution Integer. H3 resolution level (1–16). Default 7.
#' @param expand_factor Numeric. Expands bounding box to ensure coverage. Default 0.1.
#' @export
#' @examples
#' \dontrun{
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#' h3_grid_sf <- get_h3_grid(nc, resolution = 7)
#' }

get_h3_grid <- function(sf_object, resolution = 7, expand_factor = 0.1) {
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
