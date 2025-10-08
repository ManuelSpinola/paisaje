#' @name get_worldclim_future
#' @title Download and process future environmental variables from WorldClim v2.1
#'
#' @description
#' Downloads future climate data from WorldClim based on CMIP6 climate models
#' and SSP scenarios. The data can be retrieved at various spatial resolutions
#' and optionally clipped to a specified area of interest (AOI).
#'
#' @usage get_worldclim_future(
#'   var = "bioc",
#'   res = "30s",
#'   scenario = "585",
#'   time_range = "2021-2040",
#'   gcm = "ACCESS-CM2",
#'   aoi = NULL,
#'   retries = 3,
#'   timeout = 300,
#'   destination_dir = NULL
#' )
#'
#' @param var character Climate variable to download. Options:
#'   \itemize{
#'     \item "bioc" — Bioclimatic variables (19 variables)
#'     \item "prec" — Precipitation
#'     \item "tavg" — Average temperature
#'     \item "tmin" — Minimum temperature
#'     \item "tmax" — Maximum temperature
#'   }
#'   Default is "bioc".
#'
#' @param res character Spatial resolution of the data. Options:
#'   \itemize{
#'     \item "30s" — ~1 km (30 arc-seconds)
#'     \item "2.5m" — ~5 km (2.5 arc-minutes)
#'     \item "5m" — ~10 km (5 arc-minutes)
#'     \item "10m" — ~20 km (10 arc-minutes)
#'   }
#'   Default is "30s".
#'
#' @param scenario character SSP scenario. Options:
#'   \itemize{
#'     \item "126" — SSP1-2.6 (low emissions)
#'     \item "245" — SSP2-4.5 (intermediate emissions)
#'     \item "370" — SSP3-7.0 (high emissions)
#'     \item "585" — SSP5-8.5 (very high emissions)
#'   }
#'   Default is "585".
#'
#' @param time_range character Time period. Options:
#'   \itemize{
#'     \item "2021-2040"
#'     \item "2041-2060"
#'     \item "2061-2080"
#'     \item "2081-2100"
#'   }
#'   Default is "2021-2040".
#'
#' @param gcm character General Circulation Model. Options:
#'   "ACCESS-CM2", "ACCESS-ESM1-5", "AWI-CM-1-1-MR", "BCC-CSM2-MR",
#'   "CanESM5", "CanESM5-CanOE", "CMCC-ESM2", "CNRM-CM6-1",
#'   "CNRM-CM6-1-HR", "CNRM-ESM2-1", "EC-Earth3-Veg", "EC-Earth3-Veg-LR",
#'   "FIO-ESM-2-0", "GFDL-ESM4", "GISS-E2-1-G", "GISS-E2-1-H",
#'   "HadGEM3-GC31-LL", "INM-CM4-8", "INM-CM5-0", "IPSL-CM6A-LR",
#'   "MIROC-ES2L", "MIROC6", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR",
#'   "MRI-ESM2-0", "UKESM1-0-LL".
#'   Default is "ACCESS-CM2".
#'
#' @param aoi sf or SpatRaster Optional area of interest to clip the data. Default is NULL (no clipping).
#' @param retries integer Number of attempts to retry download in case of failure. Default is 3.
#' @param timeout numeric Download timeout in seconds. Default is 300.
#' @param destination_dir character Directory where downloaded data will be stored. Default is NULL (uses a temporary directory).
#'
#' @return SpatRaster object containing the selected climate variables,
#' optionally clipped to the specified AOI.
#'
#' @references
#' Fick, S. E., & Hijmans, R. J. (2017). WorldClim 2: new 1-km spatial resolution climate surfaces for global land areas.
#' International Journal of Climatology, 37(12), 4302–4315. \doi{10.1002/joc.5086}
#'
#' @examples
#' \donttest{
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
#' nc <- sf::st_transform(nc, crs = 4326)
#'
#' climate_future <- paisaje::get_worldclim_future(
#'   var = "tmin", res = "10m", scenario = "585",
#'   time_range = "2021-2040", gcm = "ACCESS-CM2", aoi = nc
#' )
#' }
#'
#' @export

get_worldclim_future <- function(var = "bioc",
                                 res = "30s",
                                 scenario = "585",
                                 time_range = "2021-2040",
                                 gcm = "ACCESS-CM2",
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

  valid_gcms <- c('ACCESS-CM2', 'ACCESS-ESM1-5', 'AWI-CM-1-1-MR', 'BCC-CSM2-MR', 'CanESM5', 'CanESM5-CanOE',
                  'CMCC-ESM2', 'CNRM-CM6-1', 'CNRM-CM6-1-HR', 'CNRM-ESM2-1', 'EC-Earth3-Veg', 'EC-Earth3-Veg-LR',
                  'FIO-ESM-2-0', 'GFDL-ESM4', 'GISS-E2-1-G', 'GISS-E2-1-H', 'HadGEM3-GC31-LL', 'INM-CM4-8',
                  'INM-CM5-0', 'IPSL-CM6A-LR', 'MIROC-ES2L', 'MIROC6', 'MPI-ESM1-2-HR', 'MPI-ESM1-2-LR',
                  'MRI-ESM2-0', 'UKESM1-0-LL')

  # Validaciones
  stopifnot(var %in% c("bioc", "prec", "tavg", "tmin", "tmax"))
  stopifnot(res %in% c("30s", "2.5m", "5m", "10m"))
  stopifnot(scenario %in% c("126", "245", "370", "585"))
  stopifnot(time_range %in% c("2021-2040", "2041-2060", "2061-2080", "2081-2100"))
  stopifnot(gcm %in% valid_gcms)

  base_url <- "https://geodata.ucdavis.edu/cmip6"
  tif_name <- sprintf("wc2.1_%s_%s_%s_%s_%s.tif", res, var, gcm, paste0("ssp", scenario), time_range)
  url <- file.path(base_url, res, gcm, paste0("ssp", scenario), tif_name)

  message("Download URL: ", url)

  download_success <- FALSE
  temp_file <- tempfile(fileext = ".tif")

  for (i in seq_len(retries)) {
    try({
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

  climate_raster <- terra::rast(temp_file)

  if (!is.null(aoi)) {
    if (inherits(aoi, "sf")) {
      aoi <- terra::vect(aoi)
    }
    climate_raster <- terra::crop(climate_raster, aoi)
    climate_raster <- terra::mask(climate_raster, aoi)
  }

  destfile <- file.path(destination_dir, basename(tif_name))
  terra::writeRaster(climate_raster, destfile, overwrite = TRUE)
  message("Raster saved at: ", destfile)

  return(climate_raster)
}
