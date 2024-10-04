#'
#' @name get_records_by_hexagon
#'
#' @title Get species records by hexagon
#'
#' @description
#' This function retrieves species occurrence data
#' within a specified area of interest (AOI) and aggregates
#' the records into hexagonal grid cells. The function uses
#' the H3 spatial index to generate hexagonal cells at a
#' specified resolution, counts the number of occurrence
#' records within each hexagon, and returns an `sf` object
#' with the record counts.
#' This function is wrapper of the occ function from the
#'  \href{https://cran.r-project.org/web/packages/spocc/index.html}{spocc package}.
#'
#'
#' @usage
#' get_records_by_hexagon(species_name, aoi_sf, res = 6,
#' providers = NULL, remove_duplicates = FALSE,
#' expand_factor = 0.1, date = NULL)
#'
#' @param species_name A character vector of one or more species
#' names to search for.
#' @param aoi_sf An 'sf' object representing the area of
#' interest. The AOI must have a valid Coordinate Reference
#' System (CRS).
#' @param res An integer specifying the resolution of the
#' hexagons (default is 6).
#' @param providers A character vector specifying the data
#' providers to search (e.g., "gbif", "inat"). If NULL, uses
#' all available providers.
#' @param remove_duplicates A logical indicating whether to
#' remove duplicate geometries from the results (default is
#' FALSE).
#' @param expand_factor A numeric value indicating how much
#' to expand the bounding box around the AOI (default is 0.1).
#' @param date A character vector specifying the date range
#' for filtering records in the
#' format `c("start_date", "end_date")`. The dates should be
#' in "YYYY-MM-DD" format.
#'
#' @return An 'sf' object containing hexagons with record counts
#' for each species. If no records are found, the function
#' returns hexagons with a count of 0. Each species will have
#' its own column, with spaces replaced by underscores.
#'
#'
#'
#'
#' @details
#' The function performs the following steps:
#' 1. Converts the area of interest to H3 cells at the
#' specified resolution.
#' 2. Queries species occurrence data within the bounding
#' box of the AOI from specified providers.
#' 3. Converts the species data into an `sf` object and
#' optionally removes duplicate records based on geometry.
#' 4. Counts the number of occurrence points that fall
#' within each hexagon and adds this count to the hexagon grid.
#'
#' @note
#' Ensure that the AOI is provided as an `sf` object with a
#' valid CRS. The function automatically transforms the AOI to WGS84 (EPSG:4326) if necessary.
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' nc = sf::st_read(system.file("shape/nc.shp", package="sf"))
#'
#' rec_hex <- get_records_by_hexagon("Lynx rufus", nc, res = 6,
#' providers = c("gbif", "inat"), remove_duplicates = FALSE,
#' expand_factor = 0.1)
#' }
#'
#'


get_records_by_hexagon <- function(species_name,
                                   aoi_sf,
                                   res = 6,
                                   providers = NULL,
                                   remove_duplicates = FALSE,
                                   expand_factor = 0.1,
                                   date = NULL) {
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

  # Expand the bounding box slightly to ensure complete hexagon coverage
  bbox <- sf::st_bbox(aoi_sf)
  bbox_expanded <- bbox + c(-expand_factor, -expand_factor, expand_factor, expand_factor)

  # Create a polygon from the expanded bounding box
  bbox_poly <- sf::st_as_sfc(bbox_expanded)

  # Convert AOI to H3 cells using the expanded bounding box
  h3_cells <- h3jsr::polygon_to_cells(bbox_poly, res = res)

  # Convert the H3 cells back to polygons and create an sf object
  hexagons <- h3jsr::cell_to_polygon(h3_cells, simple = FALSE)

  # Perform a true geometric intersection to trim hexagons to fit exactly within the original AOI
  hexagons <- sf::st_intersection(hexagons, aoi_sf)

  # Ensure species_name is a character vector
  if (!is.character(species_name)) {
    stop("species_name must be a character vector.")
  }

  # Create a query for species
  species_query <- paste(species_name, collapse = ", ")

  # Obtain species occurrence data using the spocc package
  species_data <- spocc::occ(
    query = species_name,
    from = providers,
    geometry = sf::st_bbox(aoi_sf),
    has_coords = TRUE,
    limit = 100000,
    date = date
  )

  # Extract data to a data frame
  gbif_data <- spocc::occ2df(species_data)

  # Check if there are any records returned
  if (nrow(gbif_data) == 0) {
    warning("No records returned for the specified species.")
    hexagons$record_count <- rep(0, nrow(hexagons))  # Set count to zero when no records are returned
    hexagons$species_name <- species_query  # Add a species_name column with the queried species
    return(hexagons)
  }

  # Convert the GBIF data to an sf object using the correct columns for latitude and longitude
  gbif_sf <- sf::st_as_sf(gbif_data, coords = c("longitude", "latitude"), crs = 4326)

  # Optionally remove duplicate geometries
  if (remove_duplicates) {
    gbif_sf <- dplyr::distinct(gbif_sf, geometry, .keep_all = TRUE)
  }

  # Initialize record count columns for each species with underscores in names
  for (species in species_name) {
    # Replace spaces with underscores in species names for column names
    species_name_fixed <- gsub(" ", "_", species)
    hexagons[[species_name_fixed]] <- 0  # Create a column for each species with initial count of 0
  }

  # Calculate the intersection of hexagons and occurrence data
  for (species in species_name) {
    # Filter for the current species
    species_records <- gbif_sf[gbif_data$name == species, ]
    if (nrow(species_records) > 0) {
      inter <- sf::st_intersects(hexagons, species_records, sparse = FALSE)
      species_name_fixed <- gsub(" ", "_", species)
      hexagons[[species_name_fixed]] <- rowSums(inter)  # Update the respective species count column
    }
  }

  # Return the hexagonal grid with the record counts as an sf object
  return(hexagons)
}
