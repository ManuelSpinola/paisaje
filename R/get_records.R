#' @name get_records
#' @title Retrieve species occurrence records within an Area of Interest
#' @description
#' Retrieves species occurrence records from specified data providers
#' within a given Area of Interest (AOI). The results are returned as
#' an `sf` object containing point geometries. Duplicates can be removed
#' based on geometry. This is a wrapper of the `occ` function from the
#' \href{https://CRAN.R-project.org/package=spocc}{spocc package}.
#'
#' @usage get_records(species, aoi_sf, providers = NULL,
#'   limit = 500, remove_duplicates = FALSE, date = NULL)
#'
#' @param species (`character`) Vector of species names to query.
#' @param aoi_sf (`sf`) An `sf` object representing the Area of Interest.
#'   Must have a valid CRS.
#' @param providers (`character`) Data providers to query (e.g., `"gbif"`, `"inat"`).
#'   Default is `NULL`, which queries all available providers.
#' @param limit (`integer`) Maximum number of records to retrieve per provider.
#'   Default is 500.
#' @param remove_duplicates (`logical`) Whether to remove duplicate geometries.
#'   Default is `FALSE`.
#' @param date (`character`) Vector of length two specifying the date range
#'   (e.g., `c("YYYY-MM-DD", "YYYY-MM-DD")`). Records outside this range will
#'   be excluded. Default is `NULL` (no date filtering).
#'
#' @return (`sf`) An `sf` object containing species occurrence records
#'   within the specified AOI that match the query criteria. Returns `NULL`
#'   if no records are found or if input parameters are invalid.
#'
#' @details
#' This function simplifies retrieving occurrence records by wrapping the
#' functionality of the `spocc::occ` function. It handles AOI spatial
#' filtering and optional removal of duplicates.
#'
#' @value
#' An `sf` object of class `sf` with point geometries representing
#' species occurrence records. Additional attributes include metadata
#' about the source provider and date of observation.
#'
#' @examples
#' \donttest{
#' library(sf)
#' nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' records <- get_records(
#'   species = "Lynx rufus",
#'   aoi_sf = nc,
#'   providers = c("gbif", "inat"),
#'   limit = 200
#' )
#' head(records)
#' }
#'
#' @export

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
