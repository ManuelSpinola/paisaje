#' @name extract_cat_raster
#' @title Extract Categorical Raster Values by Polygons
#' @description
#' Extracts values from a categorical raster for each polygon
#' in an sf object. The function calculates the proportion
#' of each land cover category within each polygon using
#' exactextractr for precise zonal statistics.
#'
#' @usage extract_cat_raster(spat_raster_cat, sf_hex_grid)
#'
#' @param spat_raster_cat A categorical raster (`SpatRaster`),
#'   where pixel values represent discrete land cover classes.
#' @param sf_hex_grid An `sf` object containing polygon geometries
#'   over which the raster values will be extracted.
#'
#' @return An `sf` object containing the original polygons
#'   and additional columns — one per raster category —
#'   representing the proportion of coverage for that category
#'   within each polygon.
#'
#' @details
#' This function uses the \pkg{exactextractr} package to compute
#' precise proportions of categorical raster values within
#' polygon boundaries. It is useful for landscape ecology studies,
#' habitat analysis, and spatial modeling where category proportions
#' are important explanatory variables.
#'
#' @examples
#' \donttest{
#' library(sf)
#' library(terra)
#' library(paisaje)
#'
#' # Load categorical raster
#' r <- rast("landcover.tif")
#'
#' # Create polygon grid
#' bbox <- st_bbox(r) |>
#'   st_as_sfc(crs = st_crs(4326)) |>
#'   st_as_sf()
#' grid_sf <- get_h3_grid(bbox, resolution = 6)
#'
#' # Extract category proportions
#' result_sf <- extract_cat_raster(r, grid_sf)
#' head(result_sf)
#' }
#'
#' @value An `sf` object with original polygon geometries and
#'   additional proportion columns for each raster category.
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
