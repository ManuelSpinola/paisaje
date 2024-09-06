#'
#' @name get_esa_10m
#'
#' @title Download land cover data
#'
#' @description
#' This function allows you to download land cover data from
#'  the \href{https://esa-worldcover.org/en}{ESA WorldCover}.
#'
#' @usage get_esa_10m(aoi_sf, year = 2021, output_folder = ".")
#'
#' @param aoi_sf (\code{character}) an sf_object which defines
#' the area of interest (AOI). an sf object can represent any
#' geographical area, such as a country, state, or custom boundary.
#'
#' @param year the Copernicus Global Land Service has different
#' versions of its land cover data for different years. As of now,
#' the available years are: 2020: This corresponds to the v100
#' product version, and 2021: This corresponds to the v200
#' product version.
#'
#' @param output_folder specifies the directory where data files
#' will be saved. The default value for output_folder is ".",
#' which represents the current working directory.
#'
#' @references
#'
#' WorldCover 2020 v100
#'
#' Zanaga, D., Van De Kerchove, R., De Keersmaecker, W.,
#' Souverijns, N., Brockmann, C., Quast, R., Wevers, J.,
#' Grosu, A., Paccini, A., Vergnaud, S., Cartus, O., Santoro,
#' M., Fritz, S., Georgieva, I., Lesiv, M., Carter, S., Herold,
#' M., Li, Linlin, Tsendbazar, N.E., Ramoino, F., Arino, O.,
#' 2021. ESA WorldCover 10 m 2020 v100.
#' https://doi.org/10.5281/zenodo.5571936
#'
#'
#' WorldCover 2021 v200
#'
#' Zanaga, D., Van De Kerchove, R., Daems, D., De Keersmaecker,
#'  W., Brockmann, C., Kirches, G., Wevers, J., Cartus, O.,
#'  Santoro, M., Fritz, S., Lesiv, M., Herold, M., Tsendbazar,
#'  N.E., Xu, P., Ramoino, F., Arino, O., 2022. ESA WorldCover
#'  10 m 2021 v200. https://doi.org/10.5281/zenodo.7254221
#'
#' ESA WorldCover project 2020 and 2021 / Contains modified
#' Copernicus Sentinel data (2020 and 2021) processed by ESA
#' WorldCover consortium
#' With year either 2020 or 2021 for the WorldCover 2020
#' and 2021 map, respectively.
#'
#' \href{https://esa-worldcover.org/en}{ESA WorldCover}
#'
#' @export
#'
#' @examples
#'
#' library(sf)
#'
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#'
#' get_esa_10m(nc, year = 2021, output_folder = ".")
#'
#'

get_esa_10m <- function(aoi_sf, year = NULL, output_folder = ".") {

  # Load the necessary packages
  library(sf)
  library(httr)
  library(progress)

  s3_url_prefix <- "https://esa-worldcover.s3.eu-central-1.amazonaws.com"

  # Load the WorldCover grid
  grid_url <- paste0(s3_url_prefix, "/esa_worldcover_grid.geojson")
  grid <- sf::st_read(grid_url)

  # Reproject the AOI to match the CRS of the grid
  grid_crs <- sf::st_crs(grid)
  aoi_sf <- sf::st_transform(aoi_sf, crs = grid_crs)

  # Get grid tiles intersecting the AOI
  intersecting_tiles <- st_intersects(grid, aoi_sf, sparse = FALSE)
  tiles <- grid[intersecting_tiles, ]

  if (nrow(tiles) == 0) {
    stop("No tiles found that intersect with the AOI.")
  }

  # Select the correct version tag based on the year
  version <- ifelse(year == 2020, "v100", "v200")

  # Prepare the progress bar
  pb <- progress::progress_bar$new(
    format = "  downloading [:bar] :percent eta: :eta",
    total = nrow(tiles),
    clear = FALSE,
    width = 60
  )

  # Download each tile
  for (tile in tiles$ll_tile) {
    url <- sprintf("%s/%s/%d/map/ESA_WorldCover_10m_%d_%s_%s_Map.tif",
                   s3_url_prefix, version, year, year, version, tile)

    destfile <- file.path(output_folder, basename(url))

    r <- httr::GET(url)

    if (r$status_code == 200) {
      writeBin(httr::content(r, "raw"), destfile)
    } else {
      message("Failed to download: ", url)
    }

    pb$tick()
  }

  message("Download completed.")
}
