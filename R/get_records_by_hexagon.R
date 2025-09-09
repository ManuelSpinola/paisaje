#' @name get_records_by_hexagon
#' @title Get species records by hexagon
#' @description
#' Retrieves species occurrence data within a given AOI and aggregates
#' the records into H3 hexagonal grid cells. Counts the number of occurrences
#' per species within each hexagon. Returns an `sf` object.
#'
#' @usage get_records_by_hexagon(species, aoi_sf, res = 6,
#' providers = NULL, remove_duplicates = FALSE, date = NULL,
#' expand_factor = 0.1, limit = 500)
#'
#' @param species Character vector of species names.
#' @param aoi_sf `sf` polygon of the AOI.
#' @param res H3 resolution (1–16). Default 6.
#' @param providers Character vector of data providers (e.g., "gbif").
#' @param remove_duplicates Logical. Remove duplicate geometries. Default FALSE.
#' @param date Character vector c("start","end") to filter by date.
#' @param expand_factor Numeric. Expand AOI bbox for full hex coverage. Default 0.1.
#' @param limit Integer. Maximum number of records to download per species. Default 500.
#'
#' @return `sf` object with hexagons and counts per species.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' hex_counts <- get_records_by_hexagon(
#'   species = c("Lynx rufus"),
#'   aoi_sf = nc,
#'   res = 6,
#'   limit = 200
#' )
#' }
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
