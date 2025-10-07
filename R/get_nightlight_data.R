#' @name get_nightlight_data
#' @title Download and Retrieve Nightlight Data
#' @description
#' Downloads nightlight data from the Earth Observation Group's website.
#' It scrapes the website to locate and download the latest available
#' nightlight dataset for the specified year and month.
#'
#' @usage get_nightlight_data(year, month, version = "v10",
#'   destination_dir = ".", timeout = 1200)
#'
#' @param year (`numeric` or `character`) The year for which to download
#'   nightlight data (e.g., 2020).
#' @param month (`numeric` or `character`) Month of the year (1–12).
#'   Will be formatted as two digits (e.g., `"03"` for March).
#' @param version (`character`) Nightlight data version. Default is `"v10"`.
#' @param destination_dir (`character`) Directory where the downloaded
#'   `.tif` file will be saved. Default is the current working directory `"."`.
#' @param timeout (`numeric`) Timeout in seconds for the download. Default is `1200` seconds.
#'
#' @return (`character` or `NULL`) Path to the downloaded `.tif` file.
#'   Returns `NULL` if no file was found or if an error occurred.
#'
#' @details
#' The function constructs the appropriate URL for the specified year,
#' month, and data version, then scrapes the directory listing to locate
#' the latest available `.tif` file. It downloads and saves the file to
#' the `destination_dir`. This function is useful for retrieving
#' nightlight data for studies involving human activity, urbanization,
#' and environmental monitoring.
#'
#' @importFrom utils download.file unzip
#' @importFrom rvest html_attr
#'
#' @examples
#' \donttest{
#'   # Download nightlight data for March 2021
#'   file_path <- get_nightlight_data(2021, 3)
#'   print(file_path)
#' }
#'
#' @value
#' A string with the file path of the downloaded `.tif` file,
#' or `NULL` if no file was found or if there was an error.
#'
#' @export


get_nightlight_data <- function(year,
                                month,
                                version = "v10",
                                destination_dir = NULL,
                                timeout = 1200) {

  # Restaurar opción timeout al salir
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)

  # Definir carpeta por defecto segura
  if (is.null(destination_dir)) {
    destination_dir <- tempdir()
    message("No destination_dir provided. Using temporary directory: ", destination_dir)
  }

  # Formato año y mes
  year <- as.character(year)
  month <- sprintf("%02d", as.integer(month))

  base_url <- sprintf("https://eogdata.mines.edu/nighttime_light/monthly_notile/%s/%s/%s/vcmslcfg/",
                      version, year, paste0(year, month))

  message("Scraping URL: ", base_url)

  tryCatch({
    page <- rvest::read_html(base_url)
    file_links <- page |> rvest::html_nodes("a") |> rvest::html_attr("href")
    tif_files <- file_links[grepl("SVDNB_npp_.*avg_rade9h\\.tif$", file_links)]

    if (length(tif_files) == 0) {
      stop("No suitable .tif files found for the specified year and month.")
    }

    date_codes <- sub(".*_v10_(c[0-9]+)\\.avg_rade9h\\.tif", "\\1", tif_files)
    latest_date_code <- max(date_codes)
    latest_file <- tif_files[grep(latest_date_code, tif_files)]

    if (length(latest_file) == 0) {
      stop("Could not find a matching file for the latest date code.")
    }

    url <- paste0(base_url, latest_file)
    destfile <- file.path(destination_dir, latest_file)

    message("Downloading latest file from: ", url)
    options(timeout = timeout)  # Cambio temporal
    utils::download.file(url, destfile, mode = "wb")

    message("File downloaded successfully: ", destfile)
    return(destfile)

  }, error = function(e) {
    message("Error during web scraping or downloading: ", e$message)
    return(NULL)
  })
}
