#'
#' @name get_h3_grid
#'
#' @title Generate an H3 Hexagonal Grid for an sf Object
#'
#' @description
#' This function generates a hexagonal grid of H3 cells at a
#' specified resolution level that intersect with a given `sf`
#' object. The function allows for optional removal of duplicate
#' hexagons based on geometry.
#'
#' This function is a wrapper of functions from
#' the \href{https://obrl-soil.github.io/h3jsr/}{h3jsr} package.
#'
#' @usage get_h3_grid(sf_object, resolution)
#'
#' @param sf_object `sf` object. The spatial object for which the
#' H3 grid will be generated. Must have a valid CRS.
#' @param resolution Integer. The H3 resolution level for the
#' hexagonal grid. Higher values create smaller hexagons.
#' Default is 7. It should between 1 and 16
#'
#' @references
#' O'Brien L (2023). h3jsr: Access Uber's H3 Library. R
#' package version 1.3.1,
#' <https://CRAN.R-project.org/package=h3jsr>.
#'
#' @details
#' \href{https://h3geo.org}{H3, a hexagonal hierarchical geospatial indexing system}
#'
#' @export
#'
#' @examples
#'
#' library(sf)
#'
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#'
#' h3_grid_sf <- get_h3_grid(nc, resolution = 7)
#'
#'

get_h3_grid <- function(sf_object, resolution = 7) {

  # Load the necessary packages
  library(sf)
  library(h3jsr)

  # Check if the input is an sf object
  if (!inherits(sf_object, "sf")) {
    stop("Input must be an sf object")
  }

  # Ensure the sf object is in the correct CRS (WGS84)
  sf_object <- sf::st_transform(sf_object, 4326)

  # Get the bounding box of the sf object
  bbox <- sf::st_bbox(sf_object)

  # Create a polygon from the bounding box
  bbox_poly <- sf::st_as_sfc(st_bbox(sf_object))

  # Get H3 hexagons for the bounding box
  h3_hexagons <- h3jsr::polygon_to_cells(bbox_poly, res = resolution)

  # Convert H3 hexagons to sf object
  h3_sf <- h3jsr::cell_to_polygon(h3_hexagons, simple = FALSE)

  # Intersect H3 hexagons with the original sf object
  h3_intersect <- sf::st_intersects(h3_sf, sf_object)

  # Keep only the hexagons that intersect with the sf object
  h3_grid <- h3_sf[lengths(h3_intersect) > 0, ]

  h3_grid <- st_as_sf(h3_grid)

  return(h3_grid)
}


