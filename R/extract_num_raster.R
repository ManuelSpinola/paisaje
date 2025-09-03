#' @name extract_num_raster
#' @title Extract raster values using weighted mean
#' @description Extracts numeric raster values for each polygon in `grid_sf`
#'   and computes a weighted mean using `exactextractr`.
#' @param num_raster A numeric SpatRaster
#' @param grid_sf An sf object with polygons
#' @return An sf object with raster values added
#' @importFrom dplyr left_join
#' @importFrom purrr map
#' @importFrom sf st_make_valid st_geometry_type st_collection_extract
#' @importFrom exactextractr exact_extract
#' @export
#' @examples
#' \dontrun{
#' library(terra)
#' library(sf)
#' library(exactextractr)
#'
#' r <- rast(system.file("ex/elev.tif", package = "terra"))
#' poly <- st_as_sf(st_sfc(st_polygon(list(rbind(
#'   c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)
#' ))), crs = crs(r)))
#'
#' result <- extract_num_raster(r, poly)
#' }

extract_num_raster <- function(spat_raster_multi, sf_hex_grid) {

  if (!inherits(spat_raster_multi, "SpatRaster")) {
    stop("El primer argumento debe ser un objeto SpatRaster.")
  }
  if (!inherits(sf_hex_grid, "sf")) {
    stop("El segundo argumento debe ser un objeto sf de polígonos.")
  }

  # Cambiar 'fraction' por 'area'
  extracted_values <- exactextractr::exact_extract(
    x = spat_raster_multi,
    y = sf_hex_grid,
    fun = 'weighted_mean',
    weights = 'area'
  )

  # El resto de la función para formatear y unir los datos
  if (is.matrix(extracted_values)) {
    extracted_df <- as.data.frame(extracted_values)
    colnames(extracted_df) <- names(spat_raster_multi)
  } else if (is.data.frame(extracted_values)) {
    extracted_df <- extracted_values
  } else {
    extracted_df <- as.data.frame(extracted_values)
  }

  sf_hex_grid_with_data <- dplyr::bind_cols(sf_hex_grid, extracted_df)

  return(sf_hex_grid_with_data)
}
