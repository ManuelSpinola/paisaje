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
#' @usage count_points_in_polygons(points_sf, polygons_sf)
#'
#' @param points_sf An sf object representing point geometries.
#' @param polygons_sf An sf object representing polygon geometries.
#'
#' @return An sf polygon object with an additional column `point_count` representing the number of points within each polygon.
#'
#' @examples
#' \dontrun{
#' # Example usage assuming 'points' and 'polygons' are sf objects
#'
#' library(sf)
#'
#' # Create example points_sf with species column
#' points_sf <- st_as_sf(data.frame(
#'   id = 1:6,
#'   species = c("Panthera onca", "Panthera onca", "Felis catus",
#'               "Felis catus", "Felis catus", "Panthera leo"),
#'   x = c(1, 2, 3, 4, 5, 6),
#'   y = c(1, 2, 3, 4, 5, 6)
#' ), coords = c("x", "y"), crs = 4326)
#'
#' # Create example polygons_sf
#' polygons_sf <- st_as_sf(data.frame(
#'   id = 1:2,
#'   geometry = st_sfc(
#'     st_polygon(list(rbind(c(0, 0), c(3, 0), c(3, 3), c(0, 3), c(0, 0)))),
#'     st_polygon(list(rbind(c(3, 3), c(6, 3), c(6, 6), c(3, 6), c(3, 3))))
#'   )
#' ), crs = 4326)
#'
#' # Run the function
#' result <- count_points_in_polygons(points_sf, polygons_sf)
#'
#' # View the result
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

  # Check if 'species' column exists in points_sf
  if (!"species" %in% colnames(points_sf)) {
    stop("The 'points_sf' object must contain a column named 'species'.")
  }

  # Unique species names
  species_list <- unique(points_sf$species)

  # Replace spaces with underscores in species names for column naming
  species_columns <- gsub(" ", "_", species_list)

  # Initialize columns for each species in polygons_sf
  for (col in species_columns) {
    polygons_sf[[col]] <- 0
  }

  # Loop through each species and count points within polygons
  for (i in seq_along(species_list)) {
    species <- species_list[i]
    col_name <- species_columns[i]

    # Subset points for the current species
    species_points <- points_sf[points_sf$species == species, ]

    # Use st_intersects to find points of the current species within polygons
    intersections <- sf::st_intersects(polygons_sf, species_points)

    # Count the number of points for the current species within each polygon
    polygons_sf[[col_name]] <- lengths(intersections)
  }

  # Return the polygon sf object with the species count columns
  return(polygons_sf)
}
