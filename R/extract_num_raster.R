#' @name extract_num_raster
#'
#' @title Extract Area-Weighted Mean from Numeric Raster Stack for Polygons
#'
#' @description
#' Calculates the area-weighted mean value for each layer in a numeric \code{SpatRaster}
#' (or single layer) within each polygon feature of an \code{sf} object. This function
#' is designed for high-precision zonal statistics of continuous variables
#' (e.g., bioclimatic data).
#'
#' @param spat_raster_multi A \code{SpatRaster} object from the \code{terra} package.
#'   Must contain numeric layers (can be a single layer or a stack/brick).
#' @param sf_hex_grid An \code{sf} object containing polygonal geometries (e.g., H3
#'   hexagons).
#'
#' @return An \code{sf} object identical to \code{sf_hex_grid}, but with new columns
#'   appended. The new column names match the original \code{SpatRaster} layer names.
#'   The values represent the area-weighted mean for that variable within each polygon.
#' @export
#'
#' @details
#' The function uses \code{exactextractr::exact_extract} with \code{fun = "weighted_mean"}
#' and \code{weights = "area"} to ensure the most accurate sub-pixel summary. A critical
#' security check is implemented before binding columns (\code{bind_cols}) to prevent
#' data misalignment in case of row count discrepancies between the input features and
#' the extracted results.
#'
#' @examples
#' \dontrun{
#' # Assuming 'bio' is a SpatRaster stack and 'h7' is an sf hexagon grid
#' # bio_p <- extract_num_raster(bio, h7)
#' # head(bio_p)
#' }
extract_num_raster <- function(spat_raster_multi, sf_hex_grid) {

  # 1️⃣ Input Validation and Setup
  if (!inherits(spat_raster_multi, "SpatRaster")) {
    stop("The first argument must be a 'SpatRaster' object.")
  }
  if (!inherits(sf_hex_grid, "sf")) {
    stop("The second argument must be an 'sf' object with polygons.")
  }

  # Ensure valid geometries (good practice)
  sf_hex_grid <- sf::st_make_valid(sf_hex_grid)

  # 2️⃣ Extract area-weighted mean values
  # CRUCIAL: 'weights = "area"' is required when using 'weighted_mean'
  extracted_df <- exactextractr::exact_extract(
    x = spat_raster_multi,
    y = sf_hex_grid,
    fun = "weighted_mean",
    weights = "area",
    force_df = TRUE
  )

  # 3️⃣ Security Check and Column Naming

  # CRITICAL SECURITY CHECK: Ensure row counts match before binding by position
  if (nrow(extracted_df) != nrow(sf_hex_grid)) {
    stop("Row count mismatch: Extracted data does not match the number of input polygons. Cannot safely bind columns.")
  }

  original_names <- names(spat_raster_multi)
  num_extracted_cols <- ncol(extracted_df)

  # Rename columns using original raster layer names (robust for 1 or N layers)
  if (num_extracted_cols > 0 && num_extracted_cols <= length(original_names)) {
    colnames(extracted_df) <- original_names[1:num_extracted_cols]
  } else {
    # This warning is an added layer of safety in case of unexpected extraction result
    warning("Could not rename columns using raster layer names due to mismatch or empty extraction.")
  }

  # 4️⃣ Combine with original sf grid (Safe after row count verification)
  sf_hex_grid_with_data <- dplyr::bind_cols(sf_hex_grid, extracted_df)

  return(sf_hex_grid_with_data)
}
