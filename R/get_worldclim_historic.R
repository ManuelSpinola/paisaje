#' @name get_worldclim_historic
#' @title Download and process historic environmental variables from WorldClim v2.1
#'
#' @description
#' Downloads historic climate data from WorldClim v2.1 and processes
#' it according to the specified parameters. Supports a variety of
#' climate variables and resolutions. Optionally clips data to a specified
#' area of interest (AOI) and includes retry logic for download reliability.
#'
#' @usage get_worldclim_historic(
#'   var = "bio", res = "30s", aoi = NULL,
#'   retries = 3, timeout = 300
#' )
#'
#' @param var Character. Climate variable to download. Options:
#'   \itemize{
#'     \item "bio" — Bioclimatic variables
#'     \item "tavg" — Average temperature
#'     \item "tmin" — Minimum temperature
#'     \item "tmax" — Maximum temperature
#'     \item "prec" — Precipitation
#'     \item "srad" — Solar radiation
#'     \item "wind" — Wind speed
#'     \item "vapr" — Vapor pressure
#'   }
#'   Default is "bio".
#' @param res Character. Spatial resolution of the data. Options:
#'   \itemize{
#'     \item "30s" — ~1 km (30 arc-seconds)
#'     \item "2.5m" — ~5 km (2.5 arc-minutes)
#'     \item "5m" — ~10 km (5 arc-minutes)
#'     \item "10m" — ~20 km (10 arc-minutes)
#'   }
#'   Default is "30s".
#' @param aoi An `sf` or `SpatRaster` object representing the area of interest. Default is NULL (no clipping).
#' @param retries Integer. Number of attempts to retry download in case of failure. Default is 3.
#' @param timeout Numeric. Download timeout in seconds. Default is 300.
#'
#' @return A `SpatRaster` object containing the selected historic climate variables,
#' optionally clipped to the specified AOI.
#'
#' @references
#' Fick, S. E., & Hijmans, R. J. (2017). WorldClim 2: new 1-km spatial resolution climate surfaces for global land areas.
#' International Journal of Climatology, 37(12), 4302–4315. \doi{10.1002/joc.5086}
#'
#' @examples
#' \donttest{
#' library(sf)
#' library(terra)
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#' nc <- st_transform(nc, crs = 4326)
#'
#' climate_historic <- get_worldclim_historic(
#'   var = "tmin", res = "5m", aoi = nc
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
  stopifnot(res %in% c(0.5, 2.5, 5, 10))

  # Manejar resolución
  res_str <- if (res == 0.5) "30s" else sprintf("%sm", res)

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

  # Guardar si se especifica carpeta válida
  destfile <- file.path(destination_dir, zip_name)
  terra::writeRaster(climate_rasters, destfile, overwrite = TRUE)
  message("Raster saved at: ", destfile)

  return(climate_rasters)
}
