#' @name extract_cat_raster
#' @title Extract Categorical Raster Values by Polygons
#' @description
#' Extract values from a categorical raster for each polygon in an sf object.
#' Returns the proportion of each category within each polygon, accurately
#' accounting for partial coverage using exactextractr.
#'
#' @param cat_raster A categorical raster (RasterLayer, SpatRaster, etc.).
#' @param grid_sf An sf object with polygon geometries.
#' @param proportion Logical, whether to return proportions (default TRUE).
#'
#' @return An sf object with additional columns, one per raster category,
#' representing proportion of coverage (or raw counts if proportion = FALSE).
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(terra)
#' library(paisaje)
#'
#' # Load example categorical raster
#' r <- terra::rast(system.file("raster/nlcd.tif", package = "spDataLarge"))
#'
#' # Create example polygon grid (H3 grid or any polygons)
#' bbox <- st_bbox(r) |> st_as_sfc(crs = st_crs(4326)) |> st_as_sf()
#' grid_sf <- get_h3_grid(bbox, resolution = 6)
#'
#' # Extract category proportions
#' result_sf <- extract_cat_raster(r, grid_sf)
#'
#' # View first rows
#' head(result_sf)
#' }
#'
#' @export
extract_cat_raster <- function(spat_raster_cat, sf_hex_grid) {

  # 1. Obtener la tabla de categorías
  if (is.factor(spat_raster_cat)) {
    categories_df <- terra::cats(spat_raster_cat)[[1]]
  } else {
    stop("El raster no tiene una tabla de colores. Asumimos que es numérico.")
  }

  # 2. Convertir el raster a numérico
  spat_raster_num <- terra::as.numeric(spat_raster_cat)

  # 3. Extraer la fracción de área para cada categoría con el raster numérico
  extracted_fractions <- exactextractr::exact_extract(
    x = spat_raster_num,
    y = sf_hex_grid,
    fun = 'frac',
    progress = TRUE
  )

  # 4. Unir los resultados y dar formato
  extracted_df <- as.data.frame(extracted_fractions)

  # 5. Renombrar las columnas con las etiquetas de las categorías
  # `names(extracted_fractions)` ya contiene los valores numericos
  # Buscamos en categories_df los nombres de las etiquetas para cada valor
  new_names <- paste0(
    "frac_",
    categories_df$category[match(as.integer(names(extracted_df)), categories_df$ID)]
  )
  colnames(extracted_df) <- new_names

  # 6. Unir los resultados a la geometría original
  sf_hex_grid_with_data <- dplyr::bind_cols(sf_hex_grid, extracted_df)

  return(sf_hex_grid_with_data)
}
