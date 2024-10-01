#'
#' @name extract_cat_raster
#'
#' @title Extract Categorical Raster Data and Summarize by Grid
#'
#' @description
#' This function extracts categorical data (e.g., land cover or land use) from a raster using a spatial grid object (in `sf` format) and summarizes the number or proportion of pixels for each category in each grid cell.
#'
#' @usage
#' extract_cat_raster(cat_raster, grid_sf, proportion = FALSE)
#'
#' @param cat_raster A categorical raster layer (e.g., land cover or land use) from which the values will be extracted.
#' @param grid_sf A spatial grid object of class `sf` that defines the areas over which the raster values will be summarized. The geometries should be `POLYGON` or `MULTIPOLYGON`.
#' @param proportion Logical. If `TRUE`, the function returns the proportion of each land cover category in each grid cell. If `FALSE`, it returns the raw pixel counts. Default is `FALSE`.
#'
#' @return An `sf` object with the original grid geometries and additional columns for each land cover category, containing either the number of pixels or the proportion of pixels in each category per grid cell.
#'
#' @details
#' The function first checks that the geometries in the `grid_sf` object are valid, and ensures that the geometries are either `POLYGON` or `MULTIPOLYGON`. It then uses the `terra::extract()` function to extract the categorical data from the raster and aggregates the results either as counts or proportions, depending on the value of the `proportion` argument.
#'
#' @examples
#' \dontrun{
#' # Load a categorical raster (e.g., land cover)
#' land_cover_raster <- terra::rast("path_to_land_cover_raster.tif")
#'
#' # Load an sf grid object
#' grid_sf <- sf::st_read("path_to_grid_shapefile.shp")
#'
#' # Extract land cover counts for each grid cell
#' result_sf <- extract_cat_raster(land_cover_raster, grid_sf, proportion = FALSE)
#'
#' # Extract land cover proportions for each grid cell
#' result_sf_proportion <- extract_cat_raster(land_cover_raster, grid_sf, proportion = TRUE)
#' }
#'
#' @export
#'
#'

extract_cat_raster <- function(cat_raster, grid_sf, proportion = FALSE) {

  # Validate geometries
  grid_sf <- sf::st_make_valid(grid_sf)  # Ensure geometries are valid

  # Check if geometries are POLYGON or MULTIPOLYGON; no need to extract if already these types
  geom_types <- sf::st_geometry_type(grid_sf)

  if (!all(geom_types %in% c("POLYGON", "MULTIPOLYGON"))) {
    # If there are geometry collections or other types, extract only polygons
    grid_sf <- sf::st_collection_extract(grid_sf, type = "POLYGON")
  }

  # Ensure grid has an ID field of type character
  if (!"ID" %in% colnames(grid_sf)) {
    grid_sf$ID <- as.character(1:nrow(grid_sf))
  }

  # Use terra::extract() directly on the sf object
  extracted_df <- terra::extract(cat_raster, grid_sf, fun = table, na.rm = TRUE, ID = TRUE)

  # If proportion is TRUE, convert counts to proportions
  if (proportion) {
    total_pixels <- rowSums(extracted_df[,-1])  # Sum across land cover columns, excluding ID
    extracted_df[,-1] <- extracted_df[,-1] / total_pixels  # Convert counts to proportions
  }

  # Join the extracted data back to the original sf object
  result_sf <- dplyr::left_join(grid_sf, extracted_df, by = "ID")

  return(result_sf)
}
