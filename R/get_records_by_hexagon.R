#' @name get_records_by_hexagon
#' @title Retrieve species records aggregated by H3 hexagons
#' @description
#' Downloads species occurrence data within a specified Area of Interest (AOI)
#' and aggregates these records into H3 hexagonal grid cells at a given resolution.
#' Returns an `sf` object with one polygon per hexagon and columns containing
#' species occurrence counts.
#'
#' @usage
#' get_records_by_hexagon(
#'   species, aoi_sf, res = 6,
#'   providers = NULL, remove_duplicates = FALSE,
#'   date = NULL, expand_factor = 0.1, limit = 500
#' )
#'
#' @param species character vector. Species names to query.
#' @param aoi_sf sf object. Area of Interest polygon.
#' @param res integer. H3 resolution level (1–16). Default: 6.
#' @param providers character vector. Data providers to query. Default: NULL (all).
#' @param remove_duplicates logical. Remove duplicate records. Default: FALSE.
#' @param date character vector of length two. Start and end dates for filtering records.
#' @param expand_factor numeric. Expand AOI bounding box. Default: 0.1.
#' @param limit integer. Maximum number of occurrence records per species. Default: 500.
#'
#' @return sf object. H3 hex grid with species occurrence counts.
#'
#' @details
#' This function is useful for spatial biodiversity analyses where
#' data should be aggregated into a uniform spatial grid.
#' The H3 grid system enables multi-resolution analysis and efficient
#' spatial summarization of point occurrence data.
#'
#' @examples
#' \donttest{
#' library(sf)
#' nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' hex_counts <- get_records_by_hexagon(
#'   species = c("Lynx rufus"),
#'   aoi_sf = nc,
#'   res = 6,
#'   limit = 200
#' )
#' print(hex_counts)
#' }
#'
#' @export

get_records_by_hexagon <- function(species,
                                   aoi_sf,
                                   res = 6,
                                   providers = NULL,
                                   remove_duplicates = FALSE,
                                   date = NULL,
                                   expand_factor = 0.1,
                                   limit = 500) {
  # Validations
  if (!inherits(aoi_sf, "sf")) stop("aoi_sf must be an sf object")
  if (!is.character(species)) stop("species must be a character vector")

  # Clean species names for column names
  species_clean <- gsub(" ", "_", species)

  # 1️⃣ Create hex grid over AOI
  hex_grid <- suppressWarnings(get_h3_grid(aoi_sf, resolution = res, expand_factor = expand_factor))
  hex_grid <- hex_grid[, c("h3_address", "geometry")]
  hex_grid <- suppressWarnings(sf::st_cast(hex_grid, "MULTIPOLYGON"))

  # 2️⃣ Initialize species count columns
  for (sp in species_clean) {
    hex_grid[[sp]] <- 0
  }

  # 3️⃣ Iterate over each species
  for (i in seq_along(species)) {
    sp_orig <- species[i]
    sp_col <- species_clean[i]

    # Get species records within AOI
    sp_sf <- get_records(
      species = sp_orig,
      aoi_sf = aoi_sf,
      providers = providers,
      remove_duplicates = remove_duplicates,
      date = date,
      limit = limit
    )

    if (nrow(sp_sf) == 0) {
      warning("No records found for species: ", sp_orig)
      next
    }

    # Keep only geometry for spatial join
    sp_sf <- sp_sf[, "geometry"]

    # 4️⃣ Spatial join: assign records to hexes
    joined <- suppressWarnings(sf::st_join(sp_sf, hex_grid, left = FALSE))

    # 5️⃣ Count records per hex
    rec_count <- joined %>%
      sf::st_drop_geometry() %>%
      dplyr::group_by(hex_id = h3_address) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop")

    # 6️⃣ Merge counts into full hex grid
    hex_grid <- dplyr::left_join(hex_grid, rec_count, by = c("h3_address" = "hex_id"))
    hex_grid[[sp_col]][!is.na(hex_grid$n)] <- hex_grid$n[!is.na(hex_grid$n)]
    hex_grid$n <- NULL
  }

  # 7️⃣ Return hex grid with species counts
  return(hex_grid)
}
