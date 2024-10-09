#'
#' @name count_points_in_polygons
#'
#' @title Count Points within Polygons
#'
#' @description
#' This function takes an sf point object and an sf polygon object,
#' counts the number of points within each polygon, and returns
#' the polygon sf object with an additional column containing
#' the count of points.
#'
#' @usage count_points_in_polygons(points, polygons)
#'
#' @param points_sf An sf object representing point geometries.
#' @param polygons_sf An sf object representing polygon geometries.
#'
#' @return An sf polygon object with an additional column `point_count` representing the number of points within each polygon.
#'
#' @examples
#' \dontrun{
#' # Example usage assuming 'points' and 'polygons' are sf objects
#' library(sf)
#'
#' # Create some example data
#' points_sf <- st_as_sf(data.frame(
#'  id = 1:5,
#'  x = c(1, 2, 3, 4, 5),
#'  y = c(1, 2, 3, 4, 5)
#' ), coords = c("x", "y"), crs = 4326)
#'
#' polygons_sf <- st_as_sf(data.frame(
#'  id = 1:2,
#'  wkt = c("POLYGON((0 0, 3 0, 3 3, 0 3, 0 0))",
#'          "POLYGON((3 3, 6 3, 6 6, 3 6, 3 3))")
#' ), wkt = "wkt", crs = 4326)
#'
#' # Count points within polygons
#' result <- count_points_in_polygons(points_sf, polygons_sf)
#' print(result)
#' }
#'
#' @export
#'


count_points_in_polygons <- function(points_sf, polygons_sf) {

  # Ensure that the inputs are sf objects
  if (!inherits(points_sf, "sf") || !inherits(polygons_sf, "sf")) {
    stop("Both inputs must be 'sf' objects.")
  }

  # Ensure both objects have the same CRS
  if (sf::st_crs(points_sf) != sf::st_crs(polygons_sf)) {
    stop("Both inputs must have the same CRS.")
  }

  # Use st_intersects to find points within polygons
  intersections <- sf::st_intersects(polygons_sf, points_sf)

  # Count the number of points within each polygon
  point_count <- lengths(intersections)

  # Add the point count as a new column to the polygon sf object
  polygons_sf$point_count <- point_count

  # Return the polygon sf object with the point count column
  return(polygons_sf)
}
