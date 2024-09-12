#'
#' @name get_worldclim_historic
#'
#' @title Download and process environmental variables made by worldclim
#' version 2.1.
#'
#' @description
#' This function downloads historic worldclim version 2.1
#' and processes it according to the specified
#' parameters. It supports various climate variables and
#' resolutions, and can clip the downloaded data to a
#' specified area of interest (AOI). The function is
#' designed to handle download issues with retry logic
#' and customizable timeouts.
#'
#' @param var A character string specifying the climate variable
#'  to download. Options include "tavg", "tmin", "tmax", "prec", "srad", "wind", "vapr", "bio".
#' @param res A numeric value specifying the resolution of
#'  the data to download. Options are 0.5, 2.5, 5, or 10.
#' @param aoi A spatial object representing the area of
#'  interest. Can be an `sf` or `terra` vector.
#' @param retries An integer specifying the number of times
#'  to retry the download if it fails. Default is 3.
#' @param timeout A numeric value specifying the maximum
#'  time (in seconds) to wait for a server response. Default is 300 seconds.
#'
#' @return A `SpatRaster` object containing the downloaded
#'  climate data, clipped to the specified AOI.
#'
#'
#' @references
#' Fick, Stephen E., and Robert J.
#' Hijmans. "WorldClim 2: new 1-km spatial resolution climate surfaces for
#' global land areas." \emph{International journal of climatology}
#' 37.12 (2017): 4302-4315.\doi{10.1002/joc.5086}
#'
#' @examples
#' nc <- st_read(system.file("shape/nc.shp", package="sf")) |>
#' st_transform(4326)
#' climate_raster <- download_worldclim_data_aoi(var =
#'  "tmin", res = 5, aoi = nc)
#'
#' @export
#'


get_worldclim_historic <- function(var = "bio", res = 10,
                                   aoi = NULL, retries = 3,
                                   timeout = 300) {

  # Validate inputs
  stopifnot(var %in% c("tavg", "tmin", "tmax", "prec", "srad", "wind", "vapr", "bio"))
  stopifnot(res %in% c(0.5, 2.5, 5, 10))

  # Set timeout option
  options(timeout = timeout)

  # Handle resolution input
  if (res == 0.5) {
    res <- "30s"
  } else {
    res <- sprintf("%sm", res)
  }

  # Prepare download URL with updated base URL
  base_url <- "https://geodata.ucdavis.edu/climate/worldclim/2_1/base"
  zip_name <- sprintf("wc2.1_%s_%s.zip", res, var)
  url <- file.path(base_url, zip_name)

  # Retry logic for downloading the file
  download_success <- NULL
  for (i in 1:retries) {
    try({
      temp_file <- tempfile(fileext = ".zip")
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

  # Unzip the file to a temporary directory
  unzip_dir <- tempfile()  # Use a temporary directory
  unzip(temp_file, exdir = unzip_dir)

  # Load raster data using terra
  raster_files <- list.files(unzip_dir, pattern = "\\.tif$", full.names = TRUE)
  climate_rasters <- rast(raster_files)

  # If AOI is provided, crop and mask the data
  if (!is.null(aoi)) {
    if (inherits(aoi, "sf")) {
      aoi <- vect(aoi)
    }
    climate_rasters <- crop(climate_rasters, aoi)
    climate_rasters <- mask(climate_rasters, aoi)
  }

  # Return the terra raster object
  return(climate_rasters)
}

