#' @name extract_cat_raster
#' @title Extract Categorical Raster Values by Polygons
#' @description
#' Extract values from a categorical raster for each polygon in an sf object.
#' Returns the proportion of each category within each polygon using exactextractr.
#'
#' @param spat_raster_cat A categorical raster (SpatRaster).
#' @param sf_hex_grid An sf object with polygon geometries.
#'
#' @return An sf object with additional columns, one per raster category,
#' representing proportion of coverage.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(terra)
#' library(paisaje)
#'
#' r <- rast("landcover.tif")
#' bbox <- st_bbox(r) |> st_as_sfc(crs = st_crs(4326)) |> st_as_sf()
#' grid_sf <- get_h3_grid(bbox, resolution = 6)
#' result_sf <- extract_cat_raster(r, grid_sf)
#' head(result_sf)
#' }
#'
#' @export
extract_cat_raster <- function(spat_raster_cat, sf_hex_grid) {

  if (!inherits(spat_raster_cat, "SpatRaster")) {
    stop("spat_raster_cat must be a SpatRaster object.")
  }
  if (!inherits(sf_hex_grid, "sf")) {
    stop("sf_hex_grid must be an sf object.")
  }

  # 1. Get unique categories from raster
  raster_values <- terra::values(spat_raster_cat)
  categories <- sort(unique(raster_values[!is.na(raster_values)]))

  # 2. Create a numeric copy to pass to exact_extract
  spat_raster_num <- spat_raster_cat
  terra::values(spat_raster_num) <- raster_values

  # 3. Extract fractional coverage for each category
  extracted_fractions <- exactextractr::exact_extract(
    x = spat_raster_num,
    y = sf_hex_grid,
    fun = function(values, coverage_fractions) {
      sapply(categories, function(cat) sum(coverage_fractions[values == cat], na.rm = TRUE))
    },
    progress = TRUE
  )

  # 4. Convert to data.frame and name columns
  extracted_df <- do.call(rbind, extracted_fractions)
  colnames(extracted_df) <- paste0("frac_", categories)

  # 5. Bind results to original polygons
  sf_hex_grid_with_data <- dplyr::bind_cols(sf_hex_grid, as.data.frame(extracted_df))

  return(sf_hex_grid_with_data)
}
