#'
#' @name extract_num_raster
#'
#' @title Extract Numerical Raster Data and Summarize by Grid
#' with Custom Function
#'
#' @description
#' This function extracts numerical data (e.g., temperature,
#' elevation) from a raster using a spatial grid object (in `sf`
#' format) and applies a custom function (e.g., mean, sum,
#' median) to summarize the raster values for each grid cell.
#'
#' @usage
#' extract_num_raster(num_raster, grid_sf, fun = mean)
#'
#' @param num_raster A numerical raster layer (e.g., temperature, elevation) from which the values will be extracted.
#' @param grid_sf A spatial grid object of class `sf` that defines the areas over which the raster values will be summarized. The geometries should be `POLYGON` or `MULTIPOLYGON`.
#' @param fun The summary function to apply to the raster values for each grid cell (e.g., mean, sum, median). Default is `mean`.
#'
#' @return An `sf` object with the original grid geometries and an additional column containing the summarized raster values for each grid cell.
#'
#' @details
#' The function first checks that the geometries in the `grid_sf` object are valid, and ensures that the geometries are either `POLYGON` or `MULTIPOLYGON`. It then uses the `terra::extract()` function to extract the numerical data from the raster and applies the specified function to summarize the raster values for each grid cell.
#'
#' @examples
#' \dontrun{
#' # Load a numerical raster (e.g., temperature)
#' temp_raster <- terra::rast("path_to_temp_raster.tif")
#'
#' # Load an sf grid object
#' grid_sf <- sf::st_read("path_to_grid_shapefile.shp")
#'
#' # Extract mean temperature for each grid cell
#' result_sf_mean <- extract_num_raster(temp_raster, grid_sf, fun = mean)
#'
#' # Extract sum of temperature values for each grid cell
#' result_sf_sum <- extract_num_raster(temp_raster, grid_sf, fun = sum)
#' }
#'
#' @export


extract_num_raster <- function(num_raster, grid_sf, fun = mean) {

  # Validate geometries
  grid_sf <- sf::st_make_valid(grid_sf)  # Ensure geometries are valid

  # Check if geometries are POLYGON or MULTIPOLYGON
  geom_types <- sf::st_geometry_type(grid_sf)

  if (!all(geom_types %in% c("POLYGON", "MULTIPOLYGON"))) {
    # If there are geometry collections or other types, extract only polygons
    grid_sf <- sf::st_collection_extract(grid_sf, type = "POLYGON")
  }

  # Ensure grid has an ID field of type character
  if (!"ID" %in% colnames(grid_sf)) {
    grid_sf$ID <- as.character(1:nrow(grid_sf))  # Create ID as character
  } else {
    grid_sf$ID <- as.character(grid_sf$ID)  # Convert existing ID to character
  }

  # Use terra::extract() with the custom function passed as an argument
  extracted_df <- terra::extract(num_raster,
                                 grid_sf,
                                 fun = fun,
                                 weights = TRUE,
                                 exact = TRUE,
                                 touches = TRUE,
                                 na.rm = TRUE,
                                 ID = TRUE)

  # Convert the ID column in the extracted_df to character
  extracted_df$ID <- as.character(extracted_df$ID)

  # Join the extracted summarized values back to the original sf object
  result_sf <- dplyr::left_join(grid_sf, extracted_df, by = "ID")

  return(result_sf)
}
