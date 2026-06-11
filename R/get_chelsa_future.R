#' @name get_chelsa_future
#' @title Download and Process Future Climate Variables from CHELSA v2.1 (CMIP6)
#'
#' @description
#' Downloads future bioclimatic variables from CHELSA v2.1 under CMIP6 climate
#' scenarios. Data are available for three SSP scenarios, five GCMs (ISIMIP3b
#' selection), and three future periods. Files are served as Cloud Optimized
#' GeoTIFFs (COGs) from the Swiss WSL EnviCloud, enabling efficient spatial
#' subsetting via \code{/vsicurl/} without downloading global files.
#'
#' One or more bioclimatic variables (bio1--bio19) can be requested in a single
#' call. The result is a multi-layer \code{SpatRaster} optionally cropped and
#' masked to the AOI, consistent with the interface of
#' \code{\link{get_worldclim_future}}.
#'
#' @param var `character` vector. One or more CHELSA bioclimatic variable names
#'   (\code{"bio1"} through \code{"bio19"}), or \code{"all"} to download all 19.
#'   Default: \code{"bio1"}.
#' @param scenario `character`. SSP emission scenario. Options:
#'   \itemize{
#'     \item \code{"ssp126"} - SSP1-2.6 (low emissions, sustainable development).
#'     \item \code{"ssp370"} - SSP3-7.0 (high emissions, regional rivalry).
#'     \item \code{"ssp585"} - SSP5-8.5 (very high emissions, fossil-fueled growth).
#'   }
#'   Default: \code{"ssp585"}.
#' @param period `character`. Future climatological period. Options:
#'   \itemize{
#'     \item \code{"2011-2040"} - Near future.
#'     \item \code{"2041-2070"} - Mid future.
#'     \item \code{"2071-2100"} - Far future.
#'   }
#'   Default: \code{"2041-2070"}.
#' @param gcm `character`. Global Circulation Model following the ISIMIP3b
#'   selection. Options:
#'   \itemize{
#'     \item \code{"GFDL-ESM4"}    - Priority 1 (highest priority).
#'     \item \code{"IPSL-CM6A-LR"} - Priority 2.
#'     \item \code{"MPI-ESM1-2-HR"} - Priority 3.
#'     \item \code{"MRI-ESM2-0"}   - Priority 4.
#'     \item \code{"UKESM1-0-LL"}  - Priority 5.
#'   }
#'   When fewer than five models are used, selection should follow priority order.
#'   Default: \code{"MPI-ESM1-2-HR"}.
#' @param aoi `sf` or `SpatVector` or `NULL`. Area of interest used to crop and
#'   mask the raster. If \code{NULL} (default), the global raster is returned.
#'   Providing an AOI is strongly recommended given file sizes.
#' @param destination_dir `character` or `NULL`. Directory where the output
#'   \code{.tif} will be saved. If \code{NULL}, a temporary directory is used.
#' @param timeout `numeric`. Maximum time in seconds per HTTP request.
#'   Default: \code{300}.
#'
#' @return A \code{SpatRaster} with one layer per requested variable. If
#'   \code{aoi} is provided, the raster is cropped and masked to it. Also
#'   written to \code{destination_dir} as a single multi-layer \code{.tif}.
#'   Returns \code{NULL} invisibly on error.
#'
#' @details
#' ## Spatial resolution
#' CHELSA v2.1 future projections are at a **fixed resolution of 30 arc-seconds
#' (~1 km)**. There is no \code{res} parameter - unlike WorldClim, CHELSA does
#' not offer coarser resolutions. To downsample, use \code{terra::aggregate()}
#' on the returned \code{SpatRaster}.
#'
#' ## GCM availability
#' CHELSA v2.1 future projections follow the ISIMIP3b model selection, which
#' provides five GCMs covering a range of climate sensitivities and regional
#' performance. Not all SSP x GCM x period combinations are guaranteed to be
#' available on the server. If a combination is unavailable, the function
#' emits a warning and returns \code{NULL} for that variable.
#'
#' ## SSP scenarios available
#' CHELSA v2.1 provides SSP126, SSP370, and SSP585. Note that SSP245 (available
#' in WorldClim) is **not available** in CHELSA v2.1 future bioclimatic variables.
#'
#' ## COG streaming
#' Files use the \code{/vsicurl/} GDAL virtual filesystem to stream only the
#' tiles covering \code{aoi} from the remote COG, avoiding full file downloads.
#'
#' ## Comparison with get_worldclim_future()
#' \itemize{
#'   \item CHELSA uses mechanistic downscaling; WorldClim uses statistical interpolation.
#'   \item CHELSA offers SSP126/370/585; WorldClim offers SSP126/245/370/585.
#'   \item CHELSA provides 5 GCMs (ISIMIP3b); WorldClim provides 23 GCMs.
#'   \item Both are at ~1 km (30 arc-second) resolution.
#'   \item For regions with complex terrain, CHELSA is generally preferred.
#' }
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{get_chelsa_historic}} - CHELSA 1981-2010 baseline.
#'   \item \code{\link{get_worldclim_future}} - WorldClim v2.1 future projections.
#'   \item \code{\link{extract_num_raster}} - extract area-weighted means per polygon.
#'   \item CHELSA CMIP6: \url{https://chelsa-climate.org/cmip6/}
#'   \item EnviCloud browser: \url{https://envicloud.wsl.ch/#/?bucket=https://os.zhdk.cloud.switch.ch/chelsav2/}
#' }
#'
#' @references
#' Karger, D. N., Conrad, O., B00f6hner, J., Kawohl, T., Kreft, H., Soria-Auza,
#' R. W., Zimmermann, N. E., Linder, P., & Kessler, M. (2017). Climatologies
#' at high resolution for the earth's land surface areas (CHELSA).
#' \emph{Scientific Data}, 4, 170122. \doi{10.1038/sdata.2017.122}
#'
#' Brun, P., Zimmermann, N. E., Hari, C., Pellissier, L., & Karger, D. N.
#' (2022). Global climate-related predictors at kilometre resolution for the
#' past and future. \emph{Earth System Science Data}, 14, 5573-5603.
#' \doi{10.5194/essd-14-5573-2022}
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Use Costa Rica outline (included in paisaje)
#' aoi <- paisaje::cr_outline_c
#'
#' # Single variable - Annual mean temperature, mid-century, pessimistic
#' bio1_fut <- get_chelsa_future(
#'   var      = "bio1",
#'   scenario = "ssp585",
#'   period   = "2041-2070",
#'   gcm      = "MPI-ESM1-2-HR",
#'   aoi      = aoi
#' )
#'
#' # Multiple variables - near future, optimistic
#' bio_stack <- get_chelsa_future(
#'   var      = c("bio1", "bio12", "bio15"),
#'   scenario = "ssp126",
#'   period   = "2011-2040",
#'   gcm      = "GFDL-ESM4",
#'   aoi      = aoi
#' )
#'
#' # Compare historic vs future bio1 for Costa Rica
#' bio1_hist <- get_chelsa_historic(var = "bio1", aoi = aoi)
#' bio1_diff <- bio1_fut - bio1_hist
#' terra::plot(bio1_diff, main = "Temperature change (bio1): 2041-2070 SSP585")
#'
#' # Extract per H3 hexagon
#' h7      <- paisaje::get_h3_grid(aoi, res = 7)
#' h7_fut  <- extract_num_raster(bio_stack, h7)
#' }
#'
#' @importFrom terra rast crop mask vect project crs writeRaster
#' @importFrom sf st_make_valid
#'
#' @export

get_chelsa_future <- function(var             = "bio1",
                              scenario        = "ssp585",
                              period          = "2041-2070",
                              gcm             = "MPI-ESM1-2-HR",
                              aoi             = NULL,
                              destination_dir = NULL,
                              timeout         = 300) {

  # -- Restore timeout on exit -----------------------------------------------
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = timeout)

  # -- Expand "all" shortcut -------------------------------------------------
  all_vars <- paste0("bio", 1:19)
  if (length(var) == 1 && tolower(var) == "all") var <- all_vars

  # -- Input validation -------------------------------------------------------
  var <- tolower(var)
  invalid_var <- setdiff(var, all_vars)
  if (length(invalid_var) > 0) {
    stop("Invalid variable(s): ", paste(invalid_var, collapse = ", "),
         "\nValid options: ", paste(all_vars, collapse = ", "), " or 'all'.")
  }

  valid_scenarios <- c("ssp126", "ssp370", "ssp585")
  if (!scenario %in% valid_scenarios) {
    stop("`scenario` must be one of: ", paste(valid_scenarios, collapse = ", "),
         "\nNote: SSP245 is not available in CHELSA v2.1 future bioclimatics.")
  }

  valid_periods <- c("2011-2040", "2041-2070", "2071-2100")
  if (!period %in% valid_periods) {
    stop("`period` must be one of: ", paste(valid_periods, collapse = ", "))
  }

  valid_gcms <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR",
                  "MRI-ESM2-0", "UKESM1-0-LL")
  if (!gcm %in% valid_gcms) {
    stop("`gcm` must be one of: ", paste(valid_gcms, collapse = ", "),
         "\nThese follow the ISIMIP3b model selection (priority 1-5).")
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

  # -- Base URL (COG via /vsicurl/) ------------------------------------------
  # Confirmed pattern: .../climatologies/<period>/<GCM>/<scenario>/bio/
  # Filename: CHELSA_<var>_<period>_<gcm_lowercase>_<scenario>_V.2.1.tif
  base_url <- sprintf(
    "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/%s/%s/%s/bio",
    period, gcm, scenario
  )

  # -- Prepare AOI -----------------------------------------------------------
  if (!is.null(aoi)) {
    if (inherits(aoi, "sf")) {
      aoi <- sf::st_make_valid(aoi)
      aoi_vect <- terra::vect(aoi)
    } else if (inherits(aoi, "SpatVector")) {
      aoi_vect <- aoi
    } else {
      stop("`aoi` must be an 'sf' or 'SpatVector' object.")
    }
  }

  # -- Stream each variable via COG ------------------------------------------
  layers <- lapply(var, function(v) {

    filename <- sprintf("CHELSA_%s_%s_%s_%s_V.2.1.tif", v, period, tolower(gcm), scenario)
    cog_url  <- paste0("/vsicurl/", base_url, "/", filename)

    message("Reading: ", filename)

    tryCatch({
      r <- terra::rast(cog_url)

      if (!is.null(aoi)) {
        aoi_proj <- terra::project(aoi_vect, terra::crs(r))
        r <- terra::crop(r, aoi_proj)
        r <- terra::mask(r, aoi_proj)
      }

      return(r)

    }, error = function(e) {
      warning("Failed to retrieve '", v, "' (", gcm, " / ", scenario, " / ",
              period, "): ", e$message)
      return(NULL)
    })
  })

  # -- Remove failed layers --------------------------------------------------
  failed <- sapply(layers, is.null)
  if (all(failed)) {
    message("All variable downloads failed. ",
            "Verify the combination: gcm='", gcm, "', scenario='", scenario,
            "', period='", period, "' is available at:\n", base_url)
    return(invisible(NULL))
  }
  if (any(failed)) {
    warning("Some variables could not be retrieved: ",
            paste(var[failed], collapse = ", "))
  }
  layers <- layers[!failed]

  # -- Stack and save --------------------------------------------------------
  result <- if (length(layers) == 1) layers[[1]] else terra::rast(layers)

  out_file <- file.path(
    destination_dir,
    sprintf("chelsa_future_%s_%s_%s_%s.tif",
            gcm, scenario, period,
            paste(var[!failed], collapse = "-"))
  )
  terra::writeRaster(result, out_file, overwrite = TRUE)
  message("Raster saved at: ", out_file)

  return(result)
}
