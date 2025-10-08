#' @name extract_num_raster
#' @title Extract Numeric Raster Values by Polygons
#' @description
#' Extracts numeric raster values for each polygon in an sf object.
#' Uses exactextractr to compute the weighted mean using the area of overlap.
#'
#' @param spat_raster_multi A SpatRaster object (single or multilayer numeric raster).
#' @param sf_hex_grid An sf object with polygon geometries (e.g., H3 hexagons).
#'
#' @return An sf object with additional columns for each raster layer.
#'
#' @examples
#' \dontrun{
#'
#' r <- terra::rast(system.file("raster/bio.tif", package = "spData"))
#' grid_sf <- get_h3_grid(st_as_sf(st_bbox(r)), resolution = 6)
#' result_sf <- extract_num_raster(r, grid_sf)
#' head(result_sf)
#' }
#'
#' @importFrom dplyr bind_cols
#' @importFrom exactextractr exact_extract
#' @export

extract_num_raster <- function(spat_raster_multi, sf_hex_grid) {

  if (!inherits(spat_raster_multi, "SpatRaster")) {
    stop("The first argument must be a SpatRaster object.")
  }
  if (!inherits(sf_hex_grid, "sf")) {
    stop("The second argument must be an sf object with polygons.")
  }

  # Extract area-weighted values, forcing data.frame
  extracted_df <- exactextractr::exact_extract(
    x = spat_raster_multi,
    y = sf_hex_grid,
    fun = "weighted_mean",
    weights = "area",
    force_df = TRUE
  )

  # Ensure correct column names
  colnames(extracted_df) <- names(spat_raster_multi)[seq_len(ncol(extracted_df))]

  # Combine with the original sf grid
  sf_hex_grid_with_data <- dplyr::bind_cols(sf_hex_grid, extracted_df)

  return(sf_hex_grid_with_data)
}
