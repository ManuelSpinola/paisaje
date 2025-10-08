#' @name get_esa_10m
#' @title Download ESA WorldCover land cover data
#' @description
#' Downloads ESA WorldCover land cover data at 10 m resolution for a specified
#' area of interest (AOI) and year. Useful for landscape ecology studies,
#' environmental analyses, and habitat mapping.
#'
#' @param aoi_sf `sf` An sf object defining the area of interest (AOI).
#'   This can be a country, state, or custom boundary.
#' @param year `numeric` Year of the land cover data. Available:
#'   - 2020: ESA WorldCover 10 m 2020 v100
#'   - 2021: ESA WorldCover 10 m 2021 v200
#' @param output_folder `character` Directory where data files will be saved.
#'   Default is "." (current working directory).
#'
#' @details
#' This function downloads global land cover raster data produced by the ESA WorldCover project.
#' The downloaded file can be large (hundreds of MB), and processing may take several minutes
#' depending on the AOI size and internet connection speed.
#'
#' @return `SpatRaster` A raster object containing land cover classification
#'   for the specified AOI and year. The raster values correspond to land cover classes
#'   as defined by the ESA WorldCover classification scheme.
#'
#' @references
#' Zanaga, D., Van De Kerchove, R., De Keersmaecker, W., et al. (2021).
#'   ESA WorldCover 10 m 2020 v100. https://doi.org/10.5281/zenodo.5571936
#' Zanaga, D., Van De Kerchove, R., Daems, D., et al. (2022).
#'   ESA WorldCover 10 m 2021 v200. https://doi.org/10.5281/zenodo.7254221
#'
#' @examples
#' \donttest{
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#' get_esa_10m(nc, year = 2021, output_folder = tempdir())
#' }
#'
#' @export

get_esa_10m <- function(aoi_sf,
                        year = 2020,
                        output_folder = NULL) {

  if (is.null(output_folder)) {
    output_folder <- tempdir()
    message("No output_folder provided. Using temporary directory: ", output_folder)
  }

  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }

  if (is.null(year) || !year %in% c(2020, 2021, 2022, 2023)) {
    stop("Please provide a valid year (e.g., 2020, 2021, 2022, 2023).")
  }

  s3_url_prefix <- "https://esa-worldcover.s3.eu-central-1.amazonaws.com"

  # Cargar la grilla de WorldCover con manejo de error
  grid_url <- paste0(s3_url_prefix, "/esa_worldcover_grid.geojson")
  grid <- tryCatch({
    sf::st_read(grid_url, quiet = TRUE)
  }, error = function(e) {
    stop("Failed to load WorldCover grid: ", e$message)
  })

  # Reproyectar AOI
  grid_crs <- sf::st_crs(grid)
  aoi_sf <- sf::st_transform(aoi_sf, crs = grid_crs)

  # Intersección de tiles
  intersecting_tiles <- sf::st_intersects(grid, aoi_sf, sparse = FALSE)
  tiles <- grid[intersecting_tiles, ]

  if (nrow(tiles) == 0) {
    stop("No tiles found that intersect with the AOI.")
  }

  version <- ifelse(year == 2020, "v100", "v200")

  pb <- progress::progress_bar$new(
    format = "  downloading [:bar] :percent eta: :eta",
    total = nrow(tiles),
    clear = FALSE,
    width = 60
  )

  for (tile in tiles$ll_tile) {
    url <- sprintf("%s/%s/%d/map/ESA_WorldCover_10m_%d_%s_%s_Map.tif",
                   s3_url_prefix, version, year, year, version, tile)

    destfile <- file.path(output_folder, basename(url))

    r <- tryCatch({
      httr::GET(url)
    }, error = function(e) {
      message("Error accessing URL: ", url)
      return(NULL)
    })

    if (!is.null(r) && r$status_code == 200) {
      writeBin(httr::content(r, "raw"), destfile)
    } else {
      message("Failed to download: ", url)
    }

    pb$tick()
  }

  message("Download completed. Files saved in: ", output_folder)
}
