#' @name calculate_it_metrics
#'
#' @title Calculate Landscape Complexity Metrics (IT Metrics) per Polygon
#'
#' @description
#' Calculates specified landscape complexity metrics (a subset of Information Theory
#' metrics) from a categorical land-cover raster for each input polygon using
#' \code{landscapemetrics::sample_lsm()}. This function ensures a safe, alignment-guaranteed
#' join of the results back to the original geometry.
#'
#' @param landscape_raster A \code{SpatRaster} object representing the categorical
#'   landscape (e.g., LULC).
#' @param aoi_sf An \code{sf} object containing polygonal geometries (e.g., H3
#'   hexagons) for which the landscape metrics should be calculated.
#'
#' @return An \code{sf} object identical to \code{aoi_sf}, but with new columns
#'   appended. The new columns represent the calculated landscape metrics (e.g.,
#'   \code{lsm_shdi}) with an \code{lsm_} prefix.
#' @export
#'
#' @details
#' This function calculates metrics at the \code{"landscape"} level, filtering for
#' \code{"complexity metric"} types. The function prioritizes data integrity by
#' adding a temporary \code{plot_id} column based on row index, which is used
#' by \code{landscapemetrics}.
#'
#' Crucially, the function uses \code{dplyr::left_join} with this \code{plot_id}
#' for merging the results. This **robust join method** prevents data misalignment
#' that could occur if rows were dropped during metric calculation, which is a significant
#' improvement over the unsafe \code{cbind} method. The temporary \code{plot_id} column
#' is removed before the final object is returned.
#'
#' @seealso \code{\link[landscapemetrics]{sample_lsm}} for available metrics.
#'
#' @examples
#' \dontrun{
#' # Assuming 'lulc' is a SpatRaster and 'hex_grid_sf' is an sf polygon grid
#' # metrics_sf <- calculate_it_metrics(lulc, hex_grid_sf)
#' # head(metrics_sf)
#' }
calculate_it_metrics <- function(landscape_raster, aoi_sf) {

  # --- 1️⃣ Check inputs and Prepare for Safe Join ---
  if (!inherits(landscape_raster, "SpatRaster")) {
    stop("The landscape raster must be a 'SpatRaster' object.")
  }
  if (!inherits(aoi_sf, "sf")) {
    stop("The AOI must be an 'sf' object.")
  }

  # Add a temporary ID based on row index.
  # 'landscapemetrics' will use this index as its 'plot_id'.
  aoi_sf_with_id <- aoi_sf |>
    dplyr::mutate(plot_id = seq_len(dplyr::n()))

  # --- 2️⃣ Calculate landscape metrics ---
  it_metrics <- landscapemetrics::sample_lsm(
    landscape_raster,
    aoi_sf_with_id, # Use the object with the guaranteed plot_id
    level = "landscape",
    type = "complexity metric"
  )

  # --- 3️⃣ Pivot to wide format ---
  it_metrics_w <- it_metrics |>
    dplyr::distinct() |>
    tidyr::pivot_wider(
      # Use 'plot_id' from landscapemetrics output
      id_cols = .data$plot_id,
      names_from = .data$metric,
      values_from = .data$value,
      names_prefix = "lsm_" # Recommended: Add prefix for clarity
    )

  # --- 4️⃣ Robust Join and Cleanup ---
  # Perform a LEFT JOIN using the 'plot_id' column for guaranteed alignment.
  it_metrics_sf <- aoi_sf_with_id |>
    dplyr::left_join(it_metrics_w, by = "plot_id") |>
    dplyr::select(-.data$plot_id) # Remove the temporary ID column

  return(it_metrics_sf)
}
