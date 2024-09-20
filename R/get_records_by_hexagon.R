#'
#' @name get_records_by_hexagon
#'
#' @title Get Species Records by Hexagonal Grid
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
#' @param species_name Character. The scientific name of the
#' species to search for.
#' @param aoi_sf `sf` object. The area of interest as a
#' simple features (sf) object. Must have a valid CRS.
#' @param res Integer. The H3 resolution level for the
#' hexagonal grid. Higher values create smaller hexagons.
#' @param providers Character vector. Data providers to
#' query for species occurrence data (e.g., `c("gbif", "inat")`).
#' See the `spocc` package documentation for options.
#' @param remove_duplicates Logical. If `TRUE`, removes
#' duplicate records based on geometry to avoid counting
#' the same location multiple times. Default is `FALSE`.
#'
#' @return An `sf` object representing the hexagonal grid
#' with a `record_count` column indicating the number of
#' species occurrence records in each hexagon.
#'
#' @details
#' The function performs the following steps:
#' 1. Converts the area of interest to H3 cells at the specified resolution.
#' 2. Queries species occurrence data within the bounding box of the AOI from specified providers.
#' 3. Converts the species data into an `sf` object and optionally removes duplicate records based on geometry.
#' 4. Counts the number of occurrence points that fall within each hexagon and adds this count to the hexagon grid.
#'
#' @note
#' Ensure that the AOI is provided as an `sf` object with a
#' valid CRS. The function automatically transforms the AOI to WGS84 (EPSG:4326) if necessary.
#'
#'
#' @export
#'
#' @examples
#'
#' Get species records within the AOI using a hexagonal grid
#' of resolution 6
#'
#' nc = sf::st_read(system.file("shape/nc.shp", package="sf"))
#'
#' rec_hex <- get_records_by_hexagon("Lynx rufus", nc, res = 6,
#' providers = c("gbif", "inat"))
#'
#'
#'


get_records_by_hexagon <- function(species_name,
                                   aoi_sf,
                                   res = NULL,
                                   providers = NULL,
                                   remove_duplicates = FALSE) {

  # Load the necessary packages
  library(sf)
  library(spocc)
  library(h3jsr)

  # Ensure the AOI is an sf object
  if (!inherits(aoi_sf, "sf")) {
    stop("The AOI must be an 'sf' object.")
  }

  # Ensure that the AOI has a valid CRS
  if (is.na(st_crs(aoi_sf))) {
    stop("The AOI must have a valid CRS.")
  }

  # Transform the AOI to WGS84 if necessary
  if (st_crs(aoi_sf)$epsg != 4326) {
    aoi_sf <- st_transform(aoi_sf, 4326)
  }

  # Convert AOI to H3 cells
  h3_cells <- polygon_to_cells(aoi_sf, res = res)

  # Convert the H3 cells back to polygons and create an sf object
  hexagons <- cell_to_polygon(h3_cells, simple = FALSE)

  # Create a bounding box from the AOI
  bbox <- st_bbox(aoi_sf)

  # Obtain species occurrence data using the spocc package with bbox and verbose output
  species_data <- occ(query = species_name, from = providers, geometry = bbox, has_coords = TRUE, limit = 100000)

  # Extract GBIF data
  gbif_data <- occ2df(species_data)

  # Check if there are any records returned
  if (nrow(gbif_data) == 0) {
    warning("No records returned for the specified species.")
    hexagons$record_count <- 0
    return(hexagons)
  }

  # Convert the GBIF data to an sf object using the correct columns for latitude and longitude
  gbif_sf <- st_as_sf(gbif_data, coords = c("longitude", "latitude"), crs = 4326)

  # Optionally remove duplicate geometries
  if (remove_duplicates) {
    gbif_sf <- gbif_sf %>% dplyr::distinct(geometry, .keep_all = TRUE)
  }

  # Intersection (first argument map, then points)
  inter <- sf::st_intersects(hexagons, gbif_sf)

  # Add point count to each polygon
  hexagons$record_count <- lengths(inter)

  # Return the hexagonal grid with the record counts as an sf object
  return(hexagons)
}

