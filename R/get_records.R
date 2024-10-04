#'
#' @name get_records
#'
#' @title Retrieve species occurrence records within an Area
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
#' @usage get_records(species_name, aoi_sf, providers = NULL,
#' remove_duplicates = FALSE))
#'
#' @param species_name A character vector of species names to
#' query.
#' @param aoi_sf An `sf` object representing the area of
#' interest.
#' @param providers A character vector of data providers
#' to query (e.g., "gbif", "inat").
#' @param date A character vector of length 2 specifying the
#' date range (e.g., c("YYYY-MM-DD", "YYYY-MM-DD")). Records outside
#' this range will be excluded.
#' @param remove_duplicates A logical value indicating whether
#' to remove duplicate geometries from the resulting `sf` object.
#' Default is `FALSE`.
#'
#' @return An `sf` object containing the species occurrence
#' records that fall within the specified AOI and meet the
#' query criteria. Returns `NULL` if no records are found or
#' if there are issues with the input.
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


get_records <- function(species_name, aoi_sf, providers = NULL, date = NULL, remove_duplicates = FALSE) {
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
    limit = 100000,
    date = date  # Pass date argument directly to occ
  )

  # Convert occurrences to a data frame
  combined_df <- spocc::occ2df(species_data)

  # Check if there are any records returned
  if (nrow(combined_df) == 0) {
    warning("No records returned for the specified species.")
    return(NULL)
  }

  # Ensure longitude and latitude are numeric
  combined_df$longitude <- as.numeric(combined_df$longitude)
  combined_df$latitude <- as.numeric(combined_df$latitude)

  # Remove rows with NA coordinates
  combined_df <- combined_df[!is.na(combined_df$longitude) & !is.na(combined_df$latitude), ]

  # Convert the combined data frame to an sf object
  df_sf <- sf::st_as_sf(
    combined_df,
    coords = c("longitude", "latitude"),
    crs = 4326
  )

  # Intersect with the AOI
  df_sf_within_aoi <- sf::st_intersection(df_sf, aoi_sf)

  # Optionally remove duplicate geometries
  if (remove_duplicates) {
    df_sf_within_aoi <- dplyr::distinct(df_sf_within_aoi, geometry, .keep_all = TRUE)
  }

  return(df_sf_within_aoi)
}
