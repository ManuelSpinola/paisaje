#'
#' @name get_worldclim_future
#'
#' @title Download and process future environmental variables
#' made by worldclim version 2.1.
#'
#' @description
#' This function downloads future climate data from WorldClim
#' based on CMIP6 climate models and scenarios.
#' The data can be downloaded at various resolutions and
#' clipped to a specific area of interest (AOI) if provided.
#'
#'
#' @param var Character. Climate variable to download. Available
#' options are:
#'   \itemize{
#'     \item "bio" - Bioclimatic variables (19 variables)
#'     \item "prec" - Precipitation
#'     \item "tavg" - Average temperature
#'     \item "tmin" - Minimum temperature
#'     \item "tmax" - Maximum temperature
#'   }
#'   Default is "bio".
#' @param res Character. Spatial resolution of the data.
#' Available options are:
#'   \itemize{
#'     \item "30s" - ~1 km (30 arc-seconds)
#'     \item "2.5m" - ~5 km (2.5 arc-minutes)
#'     \item "5m" - ~10 km (5 arc-minutes)
#'     \item "10m" - ~20 km (10 arc-minutes)
#'   }
#'   Default is "30s".
#' @param scenario Character. SSP scenario to use for future
#' climate data. Available options are:
#'   \itemize{
#'     \item "126" - SSP1-2.6 (low emissions)
#'     \item "245" - SSP2-4.5 (intermediate emissions)
#'     \item "370" - SSP3-7.0 (high emissions)
#'     \item "585" - SSP5-8.5 (very high emissions)
#'   }
#'   Default is "585".
#' @param time_range Character. Time period for the future
#' scenario. Available options are:
#'   \itemize{
#'     \item "2021-2040"
#'     \item "2041-2060"
#'     \item "2061-2080"
#'     \item "2081-2100"
#'   }
#'   Default is "2021-2040".
#' @param gcm Character. The General Circulation Model (GCM)
#' to use. Available options include:
#'   \itemize{
#'     \item "ACCESS-CM2"
#'     \item "ACCESS-ESM1-5"
#'     \item "AWI-CM-1-1-MR"
#'     \item "BCC-CSM2-MR"
#'     \item "CanESM5"
#'     \item "CanESM5-CanOE"
#'     \item "CMCC-ESM2"
#'     \item "CNRM-CM6-1"
#'     \item "CNRM-CM6-1-HR"
#'     \item "CNRM-ESM2-1"
#'     \item "EC-Earth3-Veg"
#'     \item "EC-Earth3-Veg-LR"
#'     \item "FIO-ESM-2-0"
#'     \item "GFDL-ESM4"
#'     \item "GISS-E2-1-G"
#'     \item "GISS-E2-1-H"
#'     \item "HadGEM3-GC31-LL"
#'     \item "INM-CM4-8"
#'     \item "INM-CM5-0"
#'     \item "IPSL-CM6A-LR"
#'     \item "MIROC-ES2L"
#'     \item "MIROC6"
#'     \item "MPI-ESM1-2-HR"
#'     \item "MPI-ESM1-2-LR"
#'     \item "MRI-ESM2-0"
#'     \item "UKESM1-0-LL"
#'   }
#'   Default is "ACCESS-CM2".
#' @param aoi An `sf` or `terra` vector object representing
#' the area of interest (AOI) to which the data will be
#' clipped. Default is `NULL` (no clipping).
#' @param retries Integer. The number of retry attempts
#' for downloading the data in case of failure. Default is 3.
#' @param timeout Numeric. Timeout in seconds for downloading
#' the data. Default is 300 seconds.
#'
#' @return A `terra` raster object representing the climate
#' data, optionally clipped to the AOI.
#'
#' @references
#' Fick, Stephen E., and Robert J.
#' Hijmans. "WorldClim 2: new 1-km spatial resolution climate
#' surfaces for global land areas." \emph{International journal
#' of climatology} 37.12 (2017): 4302-4315.\doi{10.1002/joc.5086}
#'
#' @examples
#'
#' nc <- st_read(system.file("shape/nc.shp", package="sf")) |>
#' st_transform(4326)
#'
#' climate_future <- get_worldclim_future(var = "tmin",
#' res = "10m", scenario = "585", time_range = "2021-2040",
#' gcm = "ACCESS-CM2", aoi = nc)
#'
#'
#' @export
#'
#'
#'


get_worldclim_future <- function(var = "bio",
                                 res = "30s",
                                 scenario = "585",
                                 time_range = "2021-2040",
                                 gcm = "ACCESS-CM2",
                                 aoi = NULL,
                                 retries = 3,
                                 timeout = 300) {

  # Expanded list of GCM models
  valid_gcms <- c('ACCESS-CM2', 'ACCESS-ESM1-5', 'AWI-CM-1-1-MR', 'BCC-CSM2-MR', 'CanESM5', 'CanESM5-CanOE',
                  'CMCC-ESM2', 'CNRM-CM6-1', 'CNRM-CM6-1-HR', 'CNRM-ESM2-1', 'EC-Earth3-Veg', 'EC-Earth3-Veg-LR',
                  'FIO-ESM-2-0', 'GFDL-ESM4', 'GISS-E2-1-G', 'GISS-E2-1-H', 'HadGEM3-GC31-LL', 'INM-CM4-8',
                  'INM-CM5-0', 'IPSL-CM6A-LR', 'MIROC-ES2L', 'MIROC6', 'MPI-ESM1-2-HR', 'MPI-ESM1-2-LR',
                  'MRI-ESM2-0', 'UKESM1-0-LL')

  # Validate inputs
  stopifnot(var %in% c("bio", "prec", "tavg", "tmin", "tmax"))
  stopifnot(res %in% c("30s", "2.5m", "5m", "10m"))
  stopifnot(scenario %in% c("126", "245", "370", "585"))  # SSP scenarios
  stopifnot(time_range %in% c("2021-2040", "2041-2060", "2061-2080", "2081-2100"))  # Available time ranges
  stopifnot(gcm %in% valid_gcms)

  # Set timeout option
  options(timeout = timeout)

  # Prepare download URL
  base_url <- "https://geodata.ucdavis.edu/cmip6"
  tif_name <- sprintf("wc2.1_%s_%s_%s_%s_%s.tif", res, var, gcm, paste0("ssp", scenario), time_range)
  url <- file.path(base_url, res, gcm, paste0("ssp", scenario), tif_name)

  print(paste("Download URL:", url))  # Print the URL for debugging

  # Retry logic for downloading the file
  download_success <- NULL
  for (i in 1:retries) {
    try({
      temp_file <- tempfile(fileext = ".tif")
      download_success <- download.file(url, temp_file, mode = "wb")
      break  # Exit the loop if download succeeds
    }, silent = TRUE)

    if (inherits(download_success, "try-error")) {
      message("Attempt ", i, " failed. Retrying...")
      Sys.sleep(5)  # Wait before retrying
    }
  }

  # Check if download was successful after retries
  if (inherits(download_success, "try-error")) {
    stop("Failed to download data after ", retries, " attempts.")
  }

  # Load the raster data using terra
  climate_raster <- rast(temp_file)

  # If AOI is provided, crop and mask the data
  if (!is.null(aoi)) {
    if (inherits(aoi, "sf")) {
      aoi <- vect(aoi)
    }
    climate_raster <- crop(climate_raster, aoi)
    climate_raster <- mask(climate_raster, aoi)
  }

  # Return the terra raster object
  return(climate_raster)
}


