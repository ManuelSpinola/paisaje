#'
#' @name calculate_it_metrics
#'
#' @title Calculate information theory landscape metrics
#'
#' @param landscape_raster A categorical raster object: SpatRaster.
#' @param aoi_sf The spatial area of interest as an sf object.
#'
#' @return An sf object
#'
#' @export
#'
#' @examples
#'
#' library(sf)
#' library(spDataLarge)
#'
#' landscape_raster <- rast(system.file("raster/nlcd.tif",
#' package = "spDataLarge"))
#'
#' bbox <- st_bbox(landscape_raster) |>
#' st_as_sfc(crs = EPSG:26912) |>
#' st_as_sf()
#'
#' h3_bbox <- get_h3_grid(bbox, resolution = 6) |>
#' st_transform(26912)
#'
#'
#' result_sf <- calculate_it_metrics(landscape_raster, h3_bbox)
#'


# Install and load necessary packages
if (!requireNamespace("landscapemetrics", quietly = TRUE)) {
  install.packages("landscapemetrics")
}
if (!requireNamespace("terra", quietly = TRUE)) {
  install.packages("terra")
}
if (!requireNamespace("sf", quietly = TRUE)) {
  install.packages("sf")
}


calculate_it_metrics <- function(landscape_raster, aoi_sf) {
  # Ensure the landscape raster is loaded as a SpatRaster object
  if (!inherits(landscape_raster, "SpatRaster")) {
    stop("The landscape raster must be a 'SpatRaster' object.")
  }

  # Ensure the AOI is an sf object
  if (!inherits(aoi_sf, "sf")) {
    stop("The AOI must be an 'sf' object.")
  }

  # Calculate landscape metrics
  it_metrics <- sample_lsm(landscape_raster,
                           aoi_sf,
                           level = "landscape",
                           type = "complexity metric")

  it_metrics_w <- it_metrics |>
    distinct() |>
    pivot_wider(id_cols = plot_id,
                names_from = metric,
                values_from = value)

  it_metrics_sf <- cbind(aoi_sf, it_metrics_w)

  return(it_metrics_sf)


}

