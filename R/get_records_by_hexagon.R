#' @name get_records_by_hexagon
#' @title Get species records by hexagon
#' @description
#' Retrieves species occurrence data within a given AOI and aggregates
#' the records into H3 hexagonal grid cells. Counts the number of occurrences
#' per species within each hexagon. Returns an `sf` object.
#' @usage get_records_by_hexagon(species_names, aoi_sf, res = 6,
#' providers = NULL, remove_duplicates = FALSE,
#' expand_factor = 0.1, date = NULL)
#' @param species_names Character vector of species names.
#' @param aoi_sf `sf` polygon of the AOI.
#' @param res H3 resolution (1–16). Default 6.
#' @param providers Character vector of data providers (e.g., "gbif").
#' @param remove_duplicates Logical. Remove duplicate geometries. Default FALSE.
#' @param expand_factor Numeric. Expand AOI bbox for full hex coverage. Default 0.1.
#' @param date Character vector c("start","end") to filter by date.
#' @return `sf` object with hexagons and counts per species.
#' @export
#' @examples
#' \dontrun{
#' nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' hex_counts <- get_records_by_hexagon(c("Lynx rufus"), nc, res = 6)
#' }

get_records_by_hexagon <- function(species_names,
                                   aoi_sf,
                                   res = 6,
                                   providers = NULL,
                                   remove_duplicates = FALSE,
                                   date = NULL,
                                   expand_factor = 0.1) {
  # -------------------------------------------
  # Validaciones iniciales
  # -------------------------------------------
  if (!inherits(aoi_sf, "sf")) stop("aoi_sf must be an sf object")
  if (!is.character(species_names)) stop("species_names must be a character vector")

  # -------------------------------------------
  # Normalizar nombres de especies para columnas válidas en R
  # Reemplaza espacios por guiones bajos
  # -------------------------------------------
  species_names_clean <- gsub(" ", "_", species_names)

  # -------------------------------------------
  # 1. Crear la malla de hexágonos sobre el AOI
  # Se aplica expand_factor para ampliar el AOI si es necesario
  # -------------------------------------------
  hex_grid <- suppressWarnings(get_h3_grid(aoi_sf, resolution = res, expand_factor = expand_factor))

  # Mantener solo columnas necesarias
  hex_grid <- hex_grid[, c("h3_address", "geometry")]

  # Forzar geometrías a MULTIPOLYGON, suprimiendo warnings
  hex_grid <- suppressWarnings(sf::st_cast(hex_grid, "MULTIPOLYGON"))

  # -------------------------------------------
  # 2. Inicializar columnas de conteo para cada especie
  # Todas comienzan en 0
  # -------------------------------------------
  for (sp in species_names_clean) {
    hex_grid[[sp]] <- 0
  }

  # -------------------------------------------
  # 3. Iterar sobre cada especie
  # -------------------------------------------
  for (i in seq_along(species_names)) {
    sp_original <- species_names[i]
    sp_clean <- species_names_clean[i]

    # Obtener registros de la especie dentro del AOI
    # Se pasan providers, remove_duplicates y date a get_records()
    sp_sf <- get_records(sp_original,
                         aoi_sf,
                         providers = providers,
                         remove_duplicates = remove_duplicates,
                         date = date)

    # Si no hay registros, emitir warning y continuar
    if (nrow(sp_sf) == 0) {
      warning("No records found for species: ", sp_original)
      next
    }

    # Mantener solo geometría para evitar warnings en st_join
    sp_sf <- sp_sf[, "geometry"]

    # -------------------------------------------
    # 4. Spatial join: asignar registros a hexágonos
    # Suprimir warnings de geometría y atributos
    # -------------------------------------------
    joined <- suppressWarnings(sf::st_join(sp_sf, hex_grid, left = FALSE))

    # -------------------------------------------
    # 5. Contar registros por hexágono
    # -------------------------------------------
    rec_count <- joined %>%
      sf::st_drop_geometry() %>%
      dplyr::group_by(hex_id = h3_address) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop")

    # -------------------------------------------
    # 6. Unir conteo con la malla completa
    # -------------------------------------------
    hex_grid <- dplyr::left_join(hex_grid, rec_count, by = c("h3_address" = "hex_id"))

    # Rellenar la columna de la especie con los conteos encontrados
    hex_grid[[sp_clean]][!is.na(hex_grid$n)] <- hex_grid$n[!is.na(hex_grid$n)]

    # Eliminar columna temporal de conteo
    hex_grid$n <- NULL
  }

  # -------------------------------------------
  # 7. Devolver hexágonos con columnas de conteo por especie
  # -------------------------------------------
  return(hex_grid)
}
