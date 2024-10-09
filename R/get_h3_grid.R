#'
#' @name get_h3_grid
#'
#' @title Generate an H3 Hexagonal Grid for an sf Object
#'
#' @description
#' This function generates a hexagonal grid of H3 cells at a
#' specified resolution level that intersect with a given `sf`
#' object.
#'
#' This function is a wrapper of functions from
#' the \href{https://obrl-soil.github.io/h3jsr/}{h3jsr} package.
#'
#' @usage get_h3_grid(sf_object, resolution = 7, expand_factor = 0.1)
#'
#' @param sf_object `sf` object. The spatial object for which the
#' H3 grid will be generated. Must have a valid CRS.
#' @param resolution Integer. The H3 resolution level for the
#' hexagonal grid. Higher values create smaller hexagons.
#' Default is 7. It should between 1 and 16
#' @param expand_factor Numeric. A small value that expands
#' the bounding box of the `sf_object` to ensure complete
#' coverage by the hexagonal grid.
#' This helps avoid edge cases where the grid might not fully
#' cover the input object. Default is 0.1, which adjusts the
#' bounding box by this amount on all sides.
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
#' \dontrun{
#' library(sf)
#'
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#'
#' h3_grid_sf <- get_h3_grid(nc, resolution = 7,
#' expand_factor = 0.1)
#' }
#'

get_h3_grid <- function(sf_object,
                        resolution = 7,
                        expand_factor = 0.1) {
  # Check if the input is an sf object
  if (!inherits(sf_object, "sf")) {
    stop("Input must be an sf object")
  }

  # Ensure the sf object is in the correct CRS (WGS84)
  sf_object <- sf::st_transform(sf_object, 4326)

  # Expand the bounding box slightly to ensure complete hexagon coverage
  bbox <- sf::st_bbox(sf_object)
  bbox_expanded <- bbox + c(-expand_factor, -expand_factor, expand_factor, expand_factor)

  # Create a polygon from the expanded bounding box
  bbox_poly <- sf::st_as_sfc(bbox_expanded)

  # Get H3 hexagons for the expanded bounding box
  h3_hexagons <- h3jsr::polygon_to_cells(bbox_poly, res = resolution)

  # Convert H3 hexagons to sf object
  hexagons <- h3jsr::cell_to_polygon(h3_hexagons, simple = FALSE)

  # Perform a true geometric intersection to trim hexagons to fit within the original sf object
  trimmed_hexagons <- sf::st_intersection(hexagons, sf_object)

  return(trimmed_hexagons)
}


