#' @name get_worldclim_historic
#' @title Descargar y procesar variables climáticas históricas de WorldClim v2.1
#' @description
#' Descarga datos climáticos históricos de WorldClim v2.1 y los procesa
#' según los parámetros especificados. Soporta múltiples variables climáticas
#' y resoluciones espaciales. Opcionalmente recorta los datos a un área de interés (AOI).
#'
#' @usage get_worldclim_historic(
#'   var = "bio",
#'   res = 10,
#'   aoi = NULL,
#'   retries = 3,
#'   timeout = 300,
#'   destination_dir = NULL
#' )
#'
#' @param var Character. Variable climática a descargar. Opciones:
#'   \itemize{
#'     \item "bio" — Variables bioclimáticas.
#'     \item "tavg" — Temperatura media.
#'     \item "tmin" — Temperatura mínima.
#'     \item "tmax" — Temperatura máxima.
#'     \item "prec" — Precipitación.
#'     \item "srad" — Radiación solar.
#'     \item "wind" — Velocidad del viento.
#'     \item "vapr" — Presión de vapor.
#'   }
#'   Por defecto: `"bio"`.
#' @param res Numeric. Resolución espacial en minutos de arco.
#'   Valores válidos: `0.5`, `2.5`, `5`, `10`.
#'   Estos valores se mapean internamente a cadenas aceptadas por WorldClim:
#'   \itemize{
#'     \item 0.5 → "30s"
#'     \item 2.5 → "2.5m"
#'     \item 5   → "5m"
#'     \item 10  → "10m"
#'   }
#'   Por defecto: `10`.
#' @param aoi sf o SpatRaster opcional. Área de interés para recortar los datos.
#' @param retries Integer. Número de intentos de descarga en caso de fallo.
#'   Por defecto: `3`.
#' @param timeout Numeric. Tiempo máximo de descarga en segundos.
#'   Por defecto: `300`.
#' @param destination_dir Character. Carpeta donde guardar los datos descargados.
#'   Si NULL, se usa un directorio temporal.
#'
#' @return Un objeto `SpatRaster` con las variables climáticas históricas.
#'   Si se especifica `aoi`, los datos se recortan a esa área.
#'
#' @references
#' Fick, S. E., & Hijmans, R. J. (2017). WorldClim 2: new 1-km spatial resolution climate surfaces for global land areas.
#' International Journal of Climatology, 37(12), 4302–4315. \doi{10.1002/joc.5086}
#'
#' @examples
#' \donttest{
#' nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' nc <- sf::st_transform(nc, crs = 4326)
#'
#' climate_historic <- get_worldclim_historic(
#'   var = "tmin",
#'   res = 5,
#'   aoi = nc
#' )
#' }
#'
#' @export


get_worldclim_historic <- function(var = "bio",
                                   res = 10,
                                   aoi = NULL,
                                   retries = 3,
                                   timeout = 300,
                                   destination_dir = NULL) {

  # Restaurar opción timeout al salir
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = timeout)

  # Carpeta segura por defecto
  if (is.null(destination_dir)) {
    destination_dir <- tempdir()
    message("No destination_dir provided. Using temporary directory: ", destination_dir)
  }

  # Validar inputs
  stopifnot(var %in% c("tavg", "tmin", "tmax", "prec", "srad", "wind", "vapr", "bio"))
  stopifnot(is.numeric(res))
  stopifnot(res %in% c(0.5, 2.5, 5, 10))

  # Mapear resolución numérica a cadena válida
  res_map <- c(
    "0.5" = "30s",
    "2.5" = "2.5m",
    "5"   = "5m",
    "10"  = "10m"
  )
  res_str <- res_map[as.character(res)]

  base_url <- "https://geodata.ucdavis.edu/climate/worldclim/2_1/base"
  zip_name <- sprintf("wc2.1_%s_%s.zip", res_str, var)
  url <- file.path(base_url, zip_name)

  message("Download URL: ", url)

  # Lógica de reintentos
  download_success <- FALSE
  for (i in seq_len(retries)) {
    try({
      temp_file <- tempfile(fileext = ".zip")
      utils::download.file(url, temp_file, mode = "wb")
      download_success <- TRUE
      break
    }, silent = TRUE)

    if (!download_success) {
      message("Attempt ", i, " failed. Retrying...")
      Sys.sleep(5)
    }
  }

  if (!download_success) {
    stop("Failed to download data after ", retries, " attempts.")
  }

  # Descomprimir en carpeta temporal
  unzip_dir <- tempfile()
  utils::unzip(temp_file, exdir = unzip_dir)

  # Cargar rasters
  raster_files <- list.files(unzip_dir, pattern = "\\.tif$", full.names = TRUE)
  climate_rasters <- terra::rast(raster_files)

  # Si hay AOI, recortar y aplicar máscara
  if (!is.null(aoi)) {
    if (inherits(aoi, "sf")) {
      aoi <- terra::vect(aoi)
    }
    climate_rasters <- terra::crop(climate_rasters, aoi)
    climate_rasters <- terra::mask(climate_rasters, aoi)
  }

  # Guardar raster como .tif
  destfile <- file.path(destination_dir, sprintf("wc2.1_%s_%s.tif", res_str, var))
  terra::writeRaster(climate_rasters, destfile, overwrite = TRUE)
  message("Raster saved at: ", destfile)

  return(climate_rasters)
}
