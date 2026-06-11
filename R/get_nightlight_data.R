#' @name get_nightlight_data
#' @title Download and Retrieve Nightlight Data from NASA Black Marble
#'
#' @description
#' Downloads NASA Black Marble nighttime lights raster data for a given area of
#' interest (\code{aoi_sf}), time period, and temporal resolution (daily, monthly,
#' or annual). Internally wraps \code{\link[blackmarbler]{bm_raster}} from the
#' \pkg{blackmarbler} package (World Bank), which handles tile discovery,
#' download, mosaicing, and cloud/quality masking automatically.
#'
#' The result is a \code{SpatRaster} cropped and masked to the AOI, ready to be
#' passed directly to \code{\link{extract_num_raster}} or any \code{terra}-based
#' workflow in \pkg{paisaje}.
#'
#' @param aoi_sf `sf`. An \code{sf} object defining the area of interest (AOI).
#'   Can be any polygon geometry (country outline, H3 grid extent, custom boundary).
#'   The raster will be cropped and masked to this extent.
#' @param year `numeric` or `character`. The year of interest (e.g., \code{2022}).
#'   Used together with \code{month} to build the \code{date} argument passed to
#'   \code{bm_raster}.
#' @param month `numeric` or \code{NULL}. Month of the year (1–12). Required when
#'   \code{product_id = "VNP46A3"} (monthly composite). Ignored for annual
#'   products (\code{"VNP46A4"}). Default is \code{NULL}.
#' @param product_id `character`. NASA Black Marble product identifier.
#'   Available options:
#'   \itemize{
#'     \item \code{"VNP46A1"} — Daily, at-sensor radiance (500 m).
#'     \item \code{"VNP46A2"} — Daily, BRDF-corrected and gap-filled (500 m).
#'       Default variable: \code{Gap_Filled_DNB_BRDF-Corrected_NTL}.
#'     \item \code{"VNP46A3"} — \strong{Monthly} composite, snow-free (500 m).
#'       Default variable: \code{NearNadir_Composite_Snow_Free}.
#'     \item \code{"VNP46A4"} — \strong{Annual} composite, snow-free (500 m).
#'       Default variable: \code{NearNadir_Composite_Snow_Free}.
#'   }
#'   Default: \code{"VNP46A3"} (monthly).
#' @param bearer `character`. NASA LAADS DAAC bearer token required for
#'   authentication. Obtain a free token at
#'   \url{https://ladsweb.modaps.eosdis.nasa.gov/} under \emph{Login > Generate Token}.
#'   It is strongly recommended to store this token as an environment variable
#'   (e.g., \code{NASA_BEARER}) and retrieve it with
#'   \code{Sys.getenv("NASA_BEARER")} rather than hardcoding it in scripts.
#' @param variable `character` or \code{NULL}. Specific variable (layer) to
#'   extract from the HDF5 product. If \code{NULL} (default), the package default
#'   for each \code{product_id} is used (see parameter description above). Pass
#'   \code{""} to trigger an informative error listing all valid variable names.
#' @param quality_flag_rm `integer vector` or \code{NULL}. Quality flag values
#'   for which pixels will be set to \code{NA}. Lower quality values can be
#'   removed to reduce noise. Default is \code{NULL} (no quality filtering).
#' @param destination_dir `character` or \code{NULL}. Directory where the output
#'   \code{.tif} and intermediate HDF5 tiles will be cached. If \code{NULL}
#'   (default), the system's temporary directory (\code{\link[base]{tempdir}})
#'   is used and a message is emitted.
#' @param timeout `numeric`. Maximum time in seconds allowed for HTTP downloads.
#'   Temporarily overrides \code{getOption("timeout")} and restores the original
#'   value on exit. Default: \code{1200} (20 minutes).
#'
#' @return A \code{SpatRaster} object cropped and masked to \code{aoi_sf},
#'   containing the requested nighttime lights variable. Layer name reflects the
#'   product and date. Also written to \code{destination_dir} as a \code{.tif}.
#'   Returns \code{NULL} invisibly if an error occurs, with an informative message.
#'
#' @details
#' ## Why NASA Black Marble over EOG?
#' The previous implementation downloaded a global monthly raster (~500 MB) from
#' the Earth Observation Group (EOG, Colorado School of Mines) via HTML scraping,
#' regardless of the AOI size. NASA Black Marble improves on this in several ways:
#' \itemize{
#'   \item \strong{AOI-aware}: only downloads the MODIS/VIIRS tiles that intersect
#'         \code{aoi_sf}, dramatically reducing download size for regional studies.
#'   \item \strong{Higher scientific quality}: applies lunar irradiance modeling,
#'         atmospheric correction, BRDF correction, and cloud masking at the
#'         algorithm level (not post-hoc).
#'   \item \strong{Daily, monthly, and annual} products under a unified interface.
#'   \item \strong{Stable API}: accesses NASA LAADS DAAC via token-authenticated
#'         \code{httr2} requests — no fragile HTML scraping.
#'   \item \strong{Resolution}: 500 m (vs. ~750 m for the EOG monthly_notile product).
#' }
#'
#' ## Bearer token setup
#' The NASA bearer token is free but required. Recommended setup:
#' \preformatted{
#' # In .Renviron (open with usethis::edit_r_environ()):
#' NASA_BEARER=your_token_here
#'
#' # Then in your script:
#' bearer <- Sys.getenv("NASA_BEARER")
#' }
#'
#' ## Integration with paisaje
#' The returned \code{SpatRaster} is ready to be passed directly to
#' \code{\link{extract_num_raster}} to summarize nightlight values per polygon
#' or H3 hexagon grid.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link[blackmarbler]{bm_raster}} — underlying download function.
#'   \item \code{\link{extract_num_raster}} — extract area-weighted means per polygon.
#'   \item \code{\link{get_worldclim_historic}} — analogous function for climate data.
#'   \item \code{\link{get_esa_10m}} — analogous function for land cover data.
#'   \item NASA Black Marble portal: \url{https://blackmarble.gsfc.nasa.gov/}
#'   \item LAADS DAAC token: \url{https://ladsweb.modaps.eosdis.nasa.gov/}
#' }
#'
#' @references
#' Román, M. O., et al. (2018). NASA's Black Marble nighttime lights product suite.
#' \emph{Remote Sensing of Environment}, 210, 113–143.
#' \doi{10.1016/j.rse.2018.03.017}
#'
#' Marty, R., & Vicente, G. S. (2024). \emph{blackmarbler: Georeferenced Rasters
#' and Statistics of Nighttime Lights from NASA Black Marble}. R package v0.2.5.
#' World Bank. \url{https://worldbank.github.io/blackmarbler/}
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Use Costa Rica outline (included in paisaje)
#' aoi <- paisaje::cr_outline_c
#'
#' # Bearer token from environment variable (recommended)
#' bearer <- Sys.getenv("NASA_BEARER")
#'
#' # Monthly composite — March 2022
#' ntl <- get_nightlight_data(
#'   aoi_sf     = aoi,
#'   year       = 2022,
#'   month      = 3,
#'   product_id = "VNP46A3",
#'   bearer     = bearer
#' )
#'
#' # Annual composite — 2021
#' ntl_anual <- get_nightlight_data(
#'   aoi_sf     = aoi,
#'   year       = 2021,
#'   product_id = "VNP46A4",
#'   bearer     = bearer
#' )
#'
#' # Extract mean nightlight per H3 hexagon
#' h7     <- paisaje::get_h3_grid(aoi, res = 7)
#' h7_ntl <- extract_num_raster(ntl, h7)
#' }
#'
#' @importFrom blackmarbler bm_raster
#' @importFrom terra crop mask vect crs writeRaster
#' @importFrom sf st_transform st_make_valid
#'
#' @export

get_nightlight_data <- function(aoi_sf,
                                year,
                                month           = NULL,
                                product_id      = "VNP46A3",
                                bearer,
                                variable        = NULL,
                                quality_flag_rm = NULL,
                                destination_dir = NULL,
                                timeout         = 1200) {

  # -- Restore timeout on exit -----------------------------------------------
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = timeout)

  # -- Input validation -------------------------------------------------------
  if (!inherits(aoi_sf, "sf")) {
    stop("`aoi_sf` must be an 'sf' object.")
  }

  valid_products <- c("VNP46A1", "VNP46A2", "VNP46A3", "VNP46A4")
  if (!product_id %in% valid_products) {
    stop("`product_id` must be one of: ", paste(valid_products, collapse = ", "))
  }

  if (product_id == "VNP46A3" && is.null(month)) {
    stop("`month` is required for product_id 'VNP46A3'.")
  }

  if (!nzchar(bearer)) {
    stop(
      "A NASA LAADS DAAC bearer token is required.\n",
      "Obtain one at: https://ladsweb.modaps.eosdis.nasa.gov/\n",
      "Store it with: Sys.setenv(NASA_BEARER = 'your_token') or in .Renviron."
    )
  }

  # -- Destination directory --------------------------------------------------
  if (is.null(destination_dir)) {
    destination_dir <- tempdir()
    message("No destination_dir provided. Using temporary directory: ",
            destination_dir)
  }

  if (!dir.exists(destination_dir)) {
    dir.create(destination_dir, recursive = TRUE)
    message("Created destination_dir: ", destination_dir)
  }

  # -- Build date argument for bm_raster -------------------------------------
  year <- as.integer(year)

  date <- if (product_id == "VNP46A4") {
    year                                             # annual: integer year
  } else {
    month <- sprintf("%02d", as.integer(month))
    as.Date(paste0(year, "-", month, "-01"))         # daily/monthly: Date object
  }

  # -- Ensure valid AOI geometry ---------------------------------------------
  aoi_sf <- sf::st_make_valid(aoi_sf)

  # -- Download via blackmarbler ---------------------------------------------
  message("Requesting NASA Black Marble '", product_id, "' for: ", date)

  tryCatch({

    ntl_raster <- blackmarbler::bm_raster(
      roi_sf                = aoi_sf,
      product_id            = product_id,
      date                  = date,
      bearer                = bearer,
      variable              = variable,
      quality_flag_rm       = quality_flag_rm,
      check_all_tiles_exist = TRUE
    )

    # -- Crop and mask to AOI ------------------------------------------------
    aoi_vect   <- terra::vect(sf::st_transform(aoi_sf, terra::crs(ntl_raster)))
    ntl_raster <- terra::crop(ntl_raster, aoi_vect)
    ntl_raster <- terra::mask(ntl_raster, aoi_vect)

    # -- Cache to disk -------------------------------------------------------
    out_file <- file.path(
      destination_dir,
      sprintf("nightlight_%s_%s.tif",
              product_id,
              gsub("-", "", as.character(date)))
    )
    terra::writeRaster(ntl_raster, out_file, overwrite = TRUE)
    message("Raster saved at: ", out_file)

    return(ntl_raster)

  }, error = function(e) {
    message("Error downloading NASA Black Marble data: ", e$message)
    return(invisible(NULL))
  })
}
