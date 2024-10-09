#'
#' @name calculate_it_metrics
#'
#' @title Calculate 5 information theory landscape metrics
#'
#' @description
#' This function allow to calculate 5 information theory
#' landscape metrics
#'
#' @usage calculate_it_metrics(landscape_raster, aoi_sf)
#'
#' @param landscape_raster A categorical raster object: SpatRaster.
#' @param aoi_sf The spatial area of interest as an sf object.
#'
#' @details
#'
#' Calculate the landscape metrics: condent, ent, joinent,
#' mutinf, and relmutinf.
#'
#' @return An sf object
#'
#' @note This is a wrapper of the function "sample_lsm" from the
#'  landscapemetrics package (see References)
#'
#' @importFrom rlang .data
#'
#' @export
#'
#' @references Hesselbarth, M.H.K., Sciaini, M., With, K.A.,
#' Wiegand, K., Nowosad, J. 2019. landscapemetrics: an
#' open‐source R tool to calculate landscape metrics.
#' Ecography, 42: 1648-1657 (v2.1.4).
#'
#' Nowosad J., TF Stepinski. 2019. Information theory as a
#' consistent framework for quantification and classification
#' of landscape patterns.
#' https://doi.org/10.1007/s10980-019-00830-x
#'
#' \href{https://r-spatialecology.github.io/landscapemetrics/articles/new_metrics.html}{Information theory-based framework for the analysis of landscape patterns}
#'
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(spDataLarge)
#'
#' landscape_raster <- terra::rast(system.file("raster/nlcd.tif",
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
#' }
#'


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
  it_metrics <- landscapemetrics::sample_lsm(landscape_raster,
                                             aoi_sf,
                                             level = "landscape",
                                             type = "complexity metric")

  it_metrics_w <- it_metrics |>
    dplyr::distinct() |>
    tidyr::pivot_wider(id_cols = .data$plot_id,
                names_from = .data$metric,
                values_from = .data$value)

  it_metrics_sf <- cbind(aoi_sf, it_metrics_w)

  return(it_metrics_sf)


}

