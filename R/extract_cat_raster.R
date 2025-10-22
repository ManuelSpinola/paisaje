#' @name extract_cat_raster
#'
#' @title Calculate Area Proportions for Categorical Raster Classes (Generic)
#'
#' @description
#' Extracts and calculates the **area proportion** of each categorical class (e.g., LULC)
#' found within each input polygon. This function uses area-weighting to ensure
#' highly accurate, sub-pixel zonal statistics.
#'
#' @param spat_raster_cat A single-layer \code{SpatRaster} object containing categorical
#'   values.
#' @param sf_hex_grid An \code{sf} object containing polygonal geometries. The function
#'   will use \code{h3_address} if present, otherwise it creates and uses a temporary \code{ID}
#'   column for joining.
#' @param proportion Logical. If \code{TRUE} (default), the output values are the
#'   proportion of the polygon area covered by each category (summing to 1 for the
#'   covered area). If \code{FALSE}, the output is the raw sum of the coverage fraction (area).
#'
#' @return An \code{sf} object identical to \code{sf_hex_grid}, but with new columns
#'   appended for each categorical value found in the raster. Column names follow the
#'   pattern \code{<layer_name>_prop_<category_value>}. Columns are **numerically ordered**
#'   by the category value.
#' @export
#'
#' @details
#' This function replaces the simplistic, non-area-weighted \code{table()} counting
#' method with a robust custom function utilizing \code{dplyr} and the \code{coverage_fraction}
#' column from \code{exactextractr}. Key features include:
#' \itemize{
#'   \item **Area-Weighted Accuracy:** Uses \code{coverage_fraction} for precise results.
#'   \item **NA Filtering:** Excludes \code{NA} raster values to prevent a \code{prop_NaN} column.
#'   \item **Numerical Ordering:** Sorts the final output columns by category number (e.g., 70 before 80).
#' }
#'
#' @examples
#' \dontrun{
#' # Assuming 'lulc' is a categorical SpatRaster and 'hex_grid' is an sf polygon grid
#' # cat_data_p <- extract_cat_raster(lulc, hex_grid)
#' # head(cat_data_p)
#' }
extract_cat_raster <- function(spat_raster_cat, sf_hex_grid, proportion = TRUE) {

  # 1️⃣ Robust Validation and Setup
  if (!inherits(spat_raster_cat, "SpatRaster")) stop("spat_raster_cat must be a SpatRaster")
  if (!inherits(sf_hex_grid, "sf")) stop("sf_hex_grid must be an sf object")

  # Determine the primary join key
  join_key <- if ("h3_address" %in% names(sf_hex_grid)) "h3_address" else "ID"

  # Ensure the join key is present or created
  if (!join_key %in% names(sf_hex_grid)) {
    sf_hex_grid[[join_key]] <- as.character(seq_len(nrow(sf_hex_grid)))
  }

  sf_hex_grid <- sf::st_make_valid(sf_hex_grid)
  layer_name <- names(spat_raster_cat)[1]

  # 2️⃣ Extraction and Consolidation (Area-Weighted Custom Function)
  extracted <- exactextractr::exact_extract(
    x = spat_raster_cat,
    y = sf_hex_grid,
    # Use summarize_df=TRUE to pass dataframes per feature
    summarize_df = TRUE,
    # Include the determined join key
    include_cols = join_key,

    fun = function(df) {
      if (nrow(df) == 0) return(NULL)

      # KEY FILTER: Exclude NA pixels (to prevent the prop_NaN column)
      df <- df %>%
        dplyr::filter(!is.na(.data$value))

      if (nrow(df) == 0) return(NULL)

      # Calculate the sum of coverage (weighted area) for each category
      df_summary <- df %>%
        dplyr::group_by(.data$value, .data[[join_key]]) %>%
        dplyr::summarize(
          sum_coverage = sum(.data$coverage_fraction, na.rm = TRUE),
          .groups = "drop_last"
        )

      # Calculate the proportion
      df_summary <- df_summary %>%
        dplyr::mutate(
          # Total area of the cell (only covered by defined categories)
          total_area_cell = sum(.data$sum_coverage),
          # Proportion = Category Area / Total Polygon Area
          freq = if (proportion) .data$sum_coverage / .data$total_area_cell else .data$sum_coverage
        ) %>%
        dplyr::ungroup() %>%
        # Select only necessary columns for pivoting
        dplyr::select(dplyr::all_of(c(join_key, "value", "freq")))

      return(df_summary)
    },
    progress = TRUE
  )

  # 3️⃣ Pivot to Wide Format
  if (is.null(extracted) || nrow(extracted) == 0) {
    warning("No raster data extracted or all extractions were empty.")
    return(sf_hex_grid)
  }

  extracted_wide <- extracted %>%
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(join_key),
      names_from = .data$value,
      values_from = .data$freq,
      names_prefix = paste0(layer_name, "_prop_"),
      values_fill = 0
    )

  # 4️⃣ Join to Geometry and Order Columns
  result_sf <- sf_hex_grid %>%
    dplyr::left_join(extracted_wide, by = join_key)

  # Fill NAs (polygons that didn't intersect any defined category) with 0
  prop_cols <- names(result_sf)[grepl(paste0(layer_name, "_prop_"), names(result_sf))]
  result_sf <- result_sf %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(prop_cols), ~tidyr::replace_na(.x, 0)))

  # EXTRA STEP: NUMERICAL COLUMN ORDERING
  if (length(prop_cols) > 0) {
    # 1. Extract only the numeric part (e.g., "10" from "lulc_prop_10")
    prop_numbers <- gsub(paste0(layer_name, "_prop_"), "", prop_cols)

    # 2. Order column names based on the NUMERIC value
    ordered_indices <- order(as.numeric(prop_numbers))
    ordered_prop_cols <- prop_cols[ordered_indices]

    # 3. Reorder the dataframe: ID, Ordered Columns, Geometry, Other Columns
    cols_to_select <- c(join_key, ordered_prop_cols, "geometry",
                        setdiff(names(result_sf), c(join_key, ordered_prop_cols, "geometry")))

    result_sf <- result_sf %>%
      dplyr::select(dplyr::all_of(cols_to_select))
  }

  return(sf::st_cast(result_sf, "MULTIPOLYGON"))
}
