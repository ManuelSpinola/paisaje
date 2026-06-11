#' @name get_cr_outline
#' @title Download Costa Rica Outline from GADM
#'
#' @description
#' Downloads the Costa Rica country boundary from GADM 4.1 via
#' \code{\link[geodata]{gadm}} and returns it as an \code{sf} object.
#' By default returns only the continental landmass, excluding the Isla del
#' Coco and other minor oceanic islands. The downloaded file is cached locally
#' to avoid repeated downloads.
#'
#' This function provides a reproducible, always up-to-date alternative to
#' the static \code{\link{cr_outline_c}} dataset included in the package, and
#' ensures consistency across packages that depend on a Costa Rica AOI
#' (e.g., \pkg{paisaje} and \pkg{h3sdm}).
#'
#' @param continental `logical`. If \code{TRUE} (default), returns only the
#'   continental landmass by selecting the polygon with the largest area.
#'   If \code{FALSE}, returns the full GADM boundary including all islands
#'   (Isla del Coco and minor Pacific/Caribbean islands).
#' @param path `character`. Directory where the GADM file will be cached.
#'   Defaults to \code{tempdir()}. For persistent caching across sessions,
#'   provide a permanent directory (e.g., \code{"data/"}).
#'
#' @return An \code{sf} object with one feature (POLYGON geometry) in
#'   WGS 84 (EPSG:4326), representing the Costa Rica outline.
#'
#' @details
#' ## Caching
#' GADM files are cached by \code{geodata::gadm()} in \code{path}. If the
#' file already exists, it is loaded from disk without downloading again.
#' For persistent caching, set \code{path} to a permanent directory.
#'
#' ## Consistency across packages
#' Both \pkg{paisaje} and \pkg{h3sdm} use Costa Rica as the default study
#' area in examples and vignettes. Using \code{get_cr_outline()} in both
#' packages ensures the same geometry is used, derived from the same GADM
#' version, rather than relying on static copies that may diverge over time.
#'
#' ## Island exclusion
#' When \code{continental = TRUE}, the Isla del Coco (~550 km offshore in
#' the Pacific Ocean) and any other minor islands are excluded by retaining
#' only the polygon with the largest area after casting to individual
#' \code{POLYGON} geometries.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{cr_outline_c}} — static continental outline (no islands).
#'   \item \code{\link{cr_outline}} — static full outline (with islands).
#'   \item \code{\link[geodata]{gadm}} — underlying download function.
#'   \item \code{\link{get_h3_grid}} — generate H3 grid over the returned AOI.
#'   \item GADM: \url{https://gadm.org}
#' }
#'
#' @references
#' Global Administrative Areas (GADM) version 4.1.
#' \url{https://gadm.org/license.html}
#'
#' @examples
#' \dontrun{
#' # Continental outline only (default)
#' cr <- get_cr_outline()
#' plot(sf::st_geometry(cr), main = "Costa Rica (continental)")
#'
#' # Full outline including islands
#' cr_full <- get_cr_outline(continental = FALSE)
#' plot(sf::st_geometry(cr_full), main = "Costa Rica (all territory)")
#'
#' # Use as AOI — consistent with h3sdm
#' h7  <- get_h3_grid(cr, res = 7)
#' bio <- get_chelsa_historic(var = "bio1", aoi = cr)
#'
#' # Persistent cache
#' cr <- get_cr_outline(path = "data/gadm")
#' }
#'
#' @importFrom geodata gadm
#' @importFrom sf st_as_sf st_cast st_area
#' @importFrom dplyr mutate slice_max select
#'
#' @export

get_cr_outline <- function(continental = TRUE,
                           path        = tempdir()) {

  # -- Download / load from cache --------------------------------------------
  cr <- tryCatch({
    geodata::gadm("CRI", level = 0, path = path) |>
      sf::st_as_sf()
  }, error = function(e) {
    stop("Failed to download GADM data for Costa Rica: ", e$message,
         "\nCheck your internet connection or try a different `path`.")
  })

  # -- Optionally keep only continental polygon ------------------------------
  if (continental) {
    cr <- cr |>
      sf::st_cast("POLYGON") |>
      dplyr::mutate(area = sf::st_area(geometry)) |>
      dplyr::slice_max(.data$area, n = 1) |>
      dplyr::select(geometry)
  }

  return(cr)
}
