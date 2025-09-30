#' @name get_records
#' @title Retrieve species occurrence records within an Area of Interest
#' @description
#' This function retrieves species occurrence records from
#' specified data providers within a given area of interest (AOI).
#' The records are returned as an `sf` object, optionally with
#' duplicates removed based on geometry.
#' This function is a wrapper of the occ function from the
#' \href{https://CRAN.R-project.org/package=spocc}{spocc package}
#'
#' @usage get_records(species, aoi_sf, providers = NULL,
#' limit = 500, remove_duplicates = FALSE, date = NULL)
#'
#' @param species A character vector of species names to query.
#' @param aoi_sf An `sf` object representing the area of interest.
#' @param providers A character vector of data providers to query (e.g., "gbif", "inat").
#' @param limit An integer specifying the maximum number of records to retrieve from each provider. Default is 500.
#' @param remove_duplicates A logical value indicating whether to remove duplicate geometries from the resulting `sf` object. Default is FALSE.
#' @param date A character vector of length 2 specifying the date range (e.g., c("YYYY-MM-DD", "YYYY-MM-DD")). Records outside this range will be excluded.
#'
#' @return An `sf` object containing the species occurrence records that fall within the specified AOI and meet the query criteria. Returns `NULL` if no records are found or if there are issues with the input.
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' \dontrun{
#' nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' records <- get_records(species = "Lynx rufus", nc, providers = c("gbif", "inat"))
#' }

get_records <- function(species,
                        aoi_sf,
                        providers = NULL,
                        limit = 500,
                        remove_duplicates = FALSE,
                        date = NULL) {

  stopifnot(inherits(aoi_sf, "sf"))

  if (sf::st_crs(aoi_sf)$epsg != 4326) {
    aoi_sf <- sf::st_transform(aoi_sf, 4326)
  }

  # Query species occurrences
  records <- spocc::occ(
    query = species,
    from = providers,
    geometry = sf::st_bbox(aoi_sf),
    has_coords = TRUE,
    limit = limit,
    date = date
  ) |> spocc::occ2df()

  records <- records[!is.na(records$longitude) & !is.na(records$latitude), ]

  records_sf <- sf::st_as_sf(records, coords = c("longitude","latitude"), crs = 4326)

  if (remove_duplicates) {
    records_sf <- dplyr::distinct(records_sf, .data$geometry, .keep_all = TRUE)
  }

  sf::st_intersection(records_sf, aoi_sf)
}
