#'
#' @name get_records_by_hexagon
#'
#' @title Download species records by hexagon of a hexagonal grid
#' of a specified resolution
#'
#' @description
#' This function is wrapper of the occ function from the
#'  \href{https://cran.r-project.org/web/packages/spocc/index.html}{spocc package}.
#'
#' @param species_name (\code{character}) Species name.
#' @param aoi_sf (\code{character}) The spatial area of interest as an sf object.
#' @param res (\code{numeric}) The option for the resolution of image to
#' download. Should between 1 and 16.
#' @param providers (\code{character}) The providers argument allows you to specify
#' multiple data sources (e.g., "gbif", "inat", etc.).
#'
#' @return An sf object
#'
#' @export
#'
#' @examples
#'
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

