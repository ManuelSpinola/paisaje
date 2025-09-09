#' @name extract_num_raster
#' @title Extract Numeric Raster Values by Polygons
#' @description
#' Extract numeric raster values for each polygon in an `sf` object.
#' Uses `exactextractr` to compute the weighted mean based on area overlap.
#'
#' @param spat_raster A `SpatRaster` object (single or multilayer numeric raster).
#' @param polygons_sf An `sf` object with polygon geometries (e.g., H3 hexagons).
#'
#' @return An `sf` object with additional columns for each raster layer.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(terra)
#' library(exactextractr)
#'
#' r <- rast(system.file("raster/bio.tif", package = "spData"))
#' grid_sf <- get_h3_grid(st_as_sf(st_bbox(r)), resolution = 6)
#' result_sf <- extract_num_raster(r, grid_sf)
#' head(result_sf)
#' }
#'
#' @importFrom dplyr bind_cols
#' @importFrom exactextractr exact_extract
#' @export
extract_num_raster <- function(spat_raster, polygons_sf) {

  # Input checks
  if (!inherits(spat_raster, "SpatRaster")) {
    stop("`spat_raster` must be a SpatRaster object.")
  }
  if (!inherits(polygons_sf, "sf")) {
    stop("`polygons_sf` must be an sf object with polygons.")
  }

  # Extract weighted mean for each polygon
  extracted_list <- exactextractr::exact_extract(
    x = spat_raster,
    y = polygons_sf,
    fun = "weighted_mean",
    weights = "area"
  )

  # Combine list into a data.frame
  extracted_df <- do.call(rbind, lapply(extracted_list, as.data.frame))

  # Assign layer names
  layer_names <- names(spat_raster)
  if (is.null(layer_names)) {
    layer_names <- paste0("layer_", seq_len(terra::nlyr(spat_raster)))
  }
  colnames(extracted_df) <- layer_names

  # Bind results to polygons
  polygons_sf <- dplyr::bind_cols(polygons_sf, extracted_df)

  return(polygons_sf)
}
