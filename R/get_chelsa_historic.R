#' @name get_chelsa_historic
#' @title Download and Process Historic Climate Variables from CHELSA v2.1
#'
#' @description
#' Downloads historic bioclimatic variables from CHELSA v2.1 (Climatologies at
#' High Resolution for the Earth's Land Surface Areas) for the reference period
#' 1981–2010. The data are served as Cloud Optimized GeoTIFFs (COGs) from the
#' Swiss WSL EnviCloud object store, which allows this function to retrieve only
#' the spatial subset covering \code{aoi} without downloading the global file
#' (~110 MB per variable).
#'
#' One or more bioclimatic variables (bio1–bio19) can be requested in a single
#' call. The result is a multi-layer \code{SpatRaster} optionally cropped and
#' masked to the AOI, consistent with the interface of
#' \code{\link{get_worldclim_historic}}.
#'
#' @param var `character` vector. One or more CHELSA bioclimatic variable names.
#'   Accepted values: \code{"bio1"} through \code{"bio19"}, or \code{"all"}
#'   (downloads all 19 variables). Variable names are case-insensitive.
#'   Default: \code{"bio1"}.
#'   \itemize{
#'     \item \code{bio1}  — Annual mean temperature (°C × 10)
#'     \item \code{bio2}  — Mean diurnal temperature range
#'     \item \code{bio3}  — Isothermality
#'     \item \code{bio4}  — Temperature seasonality
#'     \item \code{bio5}  — Max temperature of warmest month
#'     \item \code{bio6}  — Min temperature of coldest month
#'     \item \code{bio7}  — Temperature annual range
#'     \item \code{bio8}  — Mean temperature of wettest quarter
#'     \item \code{bio9}  — Mean temperature of driest quarter
#'     \item \code{bio10} — Mean temperature of warmest quarter
#'     \item \code{bio11} — Mean temperature of coldest quarter
#'     \item \code{bio12} — Annual precipitation (kg m⁻² yr⁻¹)
#'     \item \code{bio13} — Precipitation of wettest month
#'     \item \code{bio14} — Precipitation of driest month
#'     \item \code{bio15} — Precipitation seasonality
#'     \item \code{bio16} — Precipitation of wettest quarter
#'     \item \code{bio17} — Precipitation of driest quarter
#'     \item \code{bio18} — Precipitation of warmest quarter
#'     \item \code{bio19} — Precipitation of coldest quarter
#'   }
#' @param aoi `sf` or `SpatVector` or `NULL`. Area of interest used to crop and
#'   mask the raster. If \code{NULL} (default), the global raster is returned.
#'   Providing an AOI is strongly recommended — each CHELSA variable is ~110 MB
#'   globally, and the COG format enables efficient spatial subsetting.
#' @param destination_dir `character` or `NULL`. Directory where the output
#'   \code{.tif} will be saved. If \code{NULL} (default), a temporary directory
#'   is used and a message is emitted.
#' @param timeout `numeric`. Maximum time in seconds for each HTTP request.
#'   Default: \code{300}.
#'
#' @return A \code{SpatRaster} with one layer per requested variable, named
#'   after the CHELSA filename convention (e.g., \code{CHELSA_bio1_1981-2010_V.2.1}).
#'   If \code{aoi} is provided, the raster is cropped and masked to it.
#'   Also written to \code{destination_dir} as a single multi-layer \code{.tif}.
#'   Returns \code{NULL} invisibly on error.
#'
#' @details
#' ## Spatial resolution
#' CHELSA v2.1 is provided at a **fixed resolution of 30 arc-seconds (~1 km)**
#' globally. Unlike \code{\link{get_worldclim_historic}}, there is no \code{res}
#' parameter — CHELSA does not offer coarser resolutions (2.5, 5, or 10
#' arc-minutes). If you need multi-resolution data, use
#' \code{\link{get_worldclim_historic}} instead, or downsample the CHELSA output
#' with \code{terra::aggregate()}.
#'
#' ## Why CHELSA over WorldClim?
#' CHELSA v2.1 and WorldClim v2.1 are both high-resolution (~1 km) global
#' climatologies, but differ in their downscaling methodology:
#' \itemize{
#'   \item CHELSA uses a **mechanistic downscaling** approach based on
#'         atmospheric dynamics and orographic effects, which tends to perform
#'         better in complex terrain (mountains, coasts).
#'   \item WorldClim uses **statistical interpolation** (thin-plate splines),
#'         which is faster but less physically grounded.
#'   \item For tropical regions with complex topography (e.g., Costa Rica),
#'         CHELSA is generally considered more accurate.
#' }
#'
#' ## COG streaming — no full download required
#' CHELSA files are Cloud Optimized GeoTIFFs hosted on the Swiss WSL EnviCloud.
#' This function uses the \code{/vsicurl/} virtual filesystem prefix from GDAL
#' (via \code{terra}) to stream only the tiles that cover \code{aoi}, avoiding
#' downloading the entire global file. When \code{aoi} is provided, spatial
#' subsetting is done in memory before writing to disk.
#'
#' ## Reference period
#' All historic CHELSA v2.1 bioclimatic variables use the **1981–2010**
#' climatological normal period. For future projections see
#' \code{\link{get_chelsa_future}}.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{get_chelsa_future}} — CHELSA future projections (CMIP6).
#'   \item \code{\link{get_worldclim_historic}} — WorldClim v2.1 historic data.
#'   \item \code{\link{extract_num_raster}} — extract area-weighted means per polygon.
#'   \item CHELSA website: \url{https://chelsa-climate.org}
#'   \item EnviCloud browser: \url{https://envicloud.wsl.ch/#/?bucket=https://os.zhdk.cloud.switch.ch/chelsav2/}
#' }
#'
#' @references
#' Karger, D. N., Conrad, O., Böhner, J., Kawohl, T., Kreft, H., Soria-Auza,
#' R. W., Zimmermann, N. E., Linder, P., & Kessler, M. (2017). Climatologies
#' at high resolution for the earth's land surface areas (CHELSA).
#' \emph{Scientific Data}, 4, 170122. \doi{10.1038/sdata.2017.122}
#'
#' Brun, P., Zimmermann, N. E., Hari, C., Pellissier, L., & Karger, D. N.
#' (2022). Global climate-related predictors at kilometre resolution for the
#' past and future. \emph{Earth System Science Data}, 14, 5573–5603.
#' \doi{10.5194/essd-14-5573-2022}
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Use Costa Rica outline (included in paisaje)
#' aoi <- paisaje::cr_outline_c
#'
#' # Single variable — Annual mean temperature
#' bio1 <- get_chelsa_historic(var = "bio1", aoi = aoi)
#'
#' # Multiple variables
#' bio_stack <- get_chelsa_historic(var = c("bio1", "bio12", "bio15"), aoi = aoi)
#'
#' # All 19 bioclimatic variables
#' bio_all <- get_chelsa_historic(var = "all", aoi = aoi)
#'
#' # Extract mean values per H3 hexagon
#' h7      <- paisaje::get_h3_grid(aoi, res = 7)
#' h7_clim <- extract_num_raster(bio_stack, h7)
#' }
#'
#' @importFrom terra rast crop mask vect writeRaster
#' @importFrom sf st_transform st_make_valid
#'
#' @export

get_chelsa_historic <- function(var             = "bio1",
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
  invalid <- setdiff(var, all_vars)
  if (length(invalid) > 0) {
    stop("Invalid variable(s): ", paste(invalid, collapse = ", "),
         "\nValid options: ", paste(all_vars, collapse = ", "), " or 'all'.")
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
  base_url <- "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/bio"

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

    filename <- sprintf("CHELSA_%s_1981-2010_V.2.1.tif", v)
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
      warning("Failed to retrieve '", v, "': ", e$message)
      return(NULL)
    })
  })

  # -- Remove failed layers --------------------------------------------------
  failed  <- sapply(layers, is.null)
  if (all(failed)) {
    message("All variable downloads failed. Check your internet connection.")
    return(invisible(NULL))
  }
  if (any(failed)) {
    warning("Some variables could not be retrieved: ",
            paste(var[failed], collapse = ", "))
  }
  layers <- layers[!failed]

  # -- Stack and save --------------------------------------------------------
  result <- if (length(layers) == 1) layers[[1]] else terra::rast(layers)

  out_file <- file.path(destination_dir,
                        sprintf("chelsa_historic_1981-2010_%s.tif",
                                paste(var[!failed], collapse = "-")))
  terra::writeRaster(result, out_file, overwrite = TRUE)
  message("Raster saved at: ", out_file)

  return(result)
}
