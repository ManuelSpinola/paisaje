#' @name extract_cat_raster
#' @title Extract categorical raster values by polygons or hexagons
#'
#' @description
#' Extracts categorical raster values (e.g., land cover classes) for each polygon
#' in a given grid (e.g., H3 hexagons or administrative units). It returns either
#' the proportion of each category within each polygon or the raw counts.
#'
#' @param spat_raster_cat A categorical raster of class `SpatRaster` from the **terra** package.
#' @param sf_hex_grid An object of class `sf` representing the polygons (e.g., hexagonal grid).
#' @param proportion Logical. If `TRUE` (default), returns the proportion of each
#' category within each polygon. If `FALSE`, returns counts instead.
#'
#' @return An `sf` object with the extracted values joined to the input grid.
#' Each polygon will contain the calculated proportions or counts of the categorical
#' raster values that fall within its area.
#'
#' @details
#' This function uses `exactextractr::exact_extract()` to accurately compute the
#' overlap between polygons and raster cells. Invalid geometries are automatically
#' fixed using `sf::st_make_valid()`, and only `POLYGON` or `MULTIPOLYGON`
#' geometries are processed.
#'
#' @examples
#' \dontrun{
#'
#' # Example categorical raster
#' r <- terra::rast(nrows = 10, ncols = 10)
#' terra::values(r) <- sample(1:3, terra::ncell(r), replace = TRUE)
#'
#' # Example grid (hexagons)
#' bbox <- sf::st_as_sfc(sf::st_bbox(terra::as.polygons(r)))
#' hex <- sf::st_make_grid(bbox, cellsize = 0.2, square = FALSE)
#' hex_sf <- sf::st_sf(ID = 1:length(hex), geometry = hex)
#'
#' # Extract proportions of land cover classes per hexagon
#' result <- extract_cat_raster(r, hex_sf, proportion = TRUE)
#' }
#'
#' @importFrom dplyr left_join
#' @importFrom sf st_make_valid st_geometry_type st_collection_extract
#' @importFrom exactextractr exact_extract
#' @importFrom terra rast ncell values as.polygons
#' @export

extract_cat_raster <- function(spat_raster_cat, sf_hex_grid, proportion = TRUE) {

  # Validar geometrías
  sf_hex_grid <- sf::st_make_valid(sf_hex_grid)

  # Filtrar solo POLYGON o MULTIPOLYGON
  geom_types <- sf::st_geometry_type(sf_hex_grid)
  if (!all(geom_types %in% c("POLYGON", "MULTIPOLYGON"))) {
    sf_hex_grid <- sf::st_collection_extract(sf_hex_grid, type = "POLYGON")
  }

  # Asegurar ID único
  if (!"ID" %in% colnames(sf_hex_grid)) {
    sf_hex_grid$ID <- as.character(1:nrow(sf_hex_grid))
  }

  # Extraer valores categóricos con exactextractr
  extraction <- exactextractr::exact_extract(spat_raster_cat, sf_hex_grid, progress = TRUE)

  # Crear dataframe con proporciones
  prop_list <- lapply(seq_along(extraction), function(i) {
    df <- extraction[[i]]
    if (nrow(df) == 0) return(NULL)
    t <- table(df$value, dnn = "value")
    prop <- as.numeric(t) * df$coverage_fraction[1]  # peso inicial
    data.frame(value = as.numeric(names(t)), prop = sum(df$coverage_fraction[df$value == names(t)]))
  })

  prop_df <- do.call(rbind, lapply(seq_along(prop_list), function(i) {
    if (is.null(prop_list[[i]])) return(NULL)
    cbind(ID = sf_hex_grid$ID[i], prop_list[[i]])
  }))

  prop_df <- data.frame(prop_df)

  # Unir proporciones al grid
  result_sf <- dplyr::left_join(sf_hex_grid, prop_df, by = "ID")

  return(result_sf)
}
