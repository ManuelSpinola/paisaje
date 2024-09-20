#'
#' @name get_records
#'
#' @title Retrieve Species Occurrence Records Within an Area
#' of Interest
#'
#' @description
#' This function retrieves species occurrence records from
#' specified data providers within a given area of interest (AOI).
#' The records are returned as an `sf` object, optionally with
#' duplicates removed based on geometry.
#' This function is wrapper of the occ function from the
#'  \href{https://cran.r-project.org/web/packages/spocc/index.html}{spocc package}.
#'
#'
#' @usage get_records(species_name,
#' aoi_sf,
#' providers = NULL,
#' remove_duplicates = FALSE))
#'
#' @param species_name Character. The scientific name of the
#' species to search for.
#' @param aoi_sf `sf` object. The area of interest as a simple
#' features (sf) object. Must have a valid CRS.
#' @param providers Character vector. Data providers to query
#' for species occurrence data (e.g., `c("gbif", "inat")`).
#' See the `spocc` package documentation for available options.
#' @param remove_duplicates Logical. If `TRUE`, removes
#' duplicate records based on geometry to avoid counting the same
#' location multiple times. Default is `FALSE`.
#'
#' @return An `sf` object containing species occurrence records
#' that intersect with the AOI.
#'
#'
#' @return An `sf` object containing species occurrence records
#' that intersect with the AOI.
#'
#' @export
#'
#' @examples
#'
#' nc = sf::st_read(system.file("shape/nc.shp", package="sf"))
#'
#' records <- get_records("Lynx rufus", nc, providers = c("gbif",
#' "inat"))
#'
#'


get_records <- function(species_name,
                        aoi_sf,
                        providers = NULL,
                        remove_duplicates = FALSE) {
  # Ensure the AOI is an sf object
  if (!inherits(aoi_sf, "sf")) {
    stop("The AOI must be an 'sf' object.")
  }

  # Ensure that the AOI has a valid CRS
  if (is.na(sf::st_crs(aoi_sf))) {
    stop("The AOI must have a valid CRS.")
  }

  # Transform the AOI to WGS84 if necessary
  if (sf::st_crs(aoi_sf)$epsg != 4326) {
    aoi_sf <- sf::st_transform(aoi_sf, 4326)
  }

  # Create a bounding box from the AOI
  bbox <- sf::st_bbox(aoi_sf)

  # Obtain species occurrence data from specified providers
  species_data <- spocc::occ(
    query = species_name,
    from = providers,
    geometry = bbox,
    has_coords = TRUE,
    limit = 100000
  )

  # Convert occurrences to data frame
  df <- spocc::occ2df(species_data)

  # Check if there are any records returned
  if (nrow(df) == 0) {
    warning("No records returned for the specified species.")
    return(NULL)
  }

  # Convert the data frame to an sf object
  df_sf <- sf::st_as_sf(
    df,
    coords = c("longitude", "latitude"),
    crs = 4326
  )

  # Ensure the geometry column is correctly set
  if (!"geometry" %in% colnames(df_sf)) {
    df_sf <- sf::st_set_geometry(df_sf, st_geometry(df_sf))
  }

  # Intersect with the AOI
  df_sf_within_aoi <- sf::st_intersection(df_sf, aoi_sf)

  # Optionally remove duplicate geometries
  if (remove_duplicates) {
    df_sf_within_aoi <- dplyr::distinct(df_sf_within_aoi, geometry, .keep_all = TRUE)
  }

  return(df_sf_within_aoi)
}
