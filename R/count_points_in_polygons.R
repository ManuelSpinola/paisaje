#' @name count_points_in_polygons
#' @title Count Points within Polygons by Species
#' @description
#' Counts the number of points per species within each polygon.
#' If the points dataset contains a `species` column, a separate
#' column is created for each species with the counts inside each polygon.
#' Spaces in species names are replaced with underscores for naming columns.
#'
#' This function is particularly useful in ecological studies where
#' species have different spatial distributions. It accounts for the
#' possibility that some species may not be present in all polygons,
#' producing zero counts in those cases.
#'
#' @param points_sf An `sf` object containing point geometries. Must include a `species` column.
#' @param polygons_sf An `sf` object containing polygon geometries.
#'
#' @return An `sf` object containing the original polygons and additional
#' columns for each species count. Column names follow the format
#' `species_name_count`, with spaces replaced by underscores.
#'
#' @details
#' The function performs a spatial join to count occurrences of each species
#' within each polygon. For species absent in a polygon, the count will be zero.
#' This approach allows for flexible analysis of species distributions
#' across landscape units.
#'
#' @examples
#' \donttest{
#' library(sf)
#'
#' points_sf <- st_as_sf(data.frame(
#'   id = 1:6,
#'   species = c("Panthera onca", "Panthera onca", "Felis catus",
#'               "Felis catus", "Felis catus", "Panthera leo"),
#'   x = c(1, 2, 3, 4, 5, 6),
#'   y = c(1, 2, 3, 4, 5, 6)
#' ), coords = c("x", "y"), crs = 4326)
#'
#' polygons_sf <- st_as_sf(data.frame(
#'   id = 1:2,
#'   geometry = st_sfc(
#'     st_polygon(list(rbind(c(0,0), c(3,0), c(3,3), c(0,3), c(0,0)))),
#'     st_polygon(list(rbind(c(3,3), c(6,3), c(6,6), c(3,6), c(3,3))))
#'   )
#' ), crs = 4326)
#'
#' result <- count_points_in_polygons(points_sf, polygons_sf)
#' print(result)
#' }
#'
#' @export

count_points_in_polygons <- function(points_sf, polygons_sf) {
  # Validate inputs
  if (!inherits(points_sf, "sf") || !inherits(polygons_sf, "sf")) {
    stop("Both inputs must be 'sf' objects.")
  }
  if (sf::st_crs(points_sf) != sf::st_crs(polygons_sf)) {
    stop("Both inputs must have the same CRS.")
  }
  if (!"species" %in% colnames(points_sf)) {
    stop("points_sf must contain a 'species' column.")
  }

  # Compute intersection matrix: polygons x points
  inter <- sf::st_intersects(polygons_sf, points_sf, sparse = FALSE)

  # Unique species and clean column names
  species_list <- unique(points_sf$species)
  species_cols <- gsub(" ", "_", species_list)

  # Vectorized counting per species
  for (i in seq_along(species_list)) {
    sp <- species_list[i]
    col_name <- species_cols[i]
    polygons_sf[[col_name]] <- rowSums(inter[, points_sf$species == sp, drop = FALSE])
  }

  polygons_sf
}
