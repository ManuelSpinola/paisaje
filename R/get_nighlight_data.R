#'
#' @name get_nighlight_data
#'
#' @title Download and Retrieve Nightlight Data
#'
#' @description
#' This function downloads nightlight data from the Earth
#' Observation Group's website. It scrapes the website to find
#' and download the latest available nightlight data for the
#' specified year and month.
#'
#' @usage get_nightlight_data(year, month, version = "v10",
#' destination_dir = ".", timeout = 1200)
#'
#' @param year A numeric or character year (e.g., 2020).
#' @param month A numeric or character month (1-12). This will be formatted as two digits.
#' @param version The version of the nightlight data (default is "v10").
#' @param destination_dir Directory to save the downloaded .tif file (default is current directory).
#' @param timeout Timeout in seconds for the download (default is 1200 seconds).
#'
#' @return The file path of the downloaded .tif file, or NULL if an error occurs.
#'
#' @details The function constructs the appropriate URL for the specified year,
#' month, and data version. It scrapes the directory listing on the website
#' to find the latest available .tif file for the requested time period. The
#' function then downloads this file and saves it to the specified directory.
#' If no suitable file is found, or an error occurs, it returns NULL.
#'
#' @importFrom utils download.file unzip
#' @importFrom rvest html_attr
#'
#' @examples
#' \dontrun{
#'   # Example: Download nightlight data for March 2021
#'   file_path <- get_nightlight_data(2021, 3)
#'   print(file_path)
#' }
#'
#' @export


get_nightlight_data <- function(year, month, version = "v10", destination_dir = ".", timeout = 1200) {

  # Ensure the year and month are properly formatted
  year <- as.character(year)
  month <- sprintf("%02d", as.integer(month))

  # Construct the URL to the directory listing
  base_url <- sprintf("https://eogdata.mines.edu/nighttime_light/monthly_notile/%s/%s/%s/vcmslcfg/",
                      version, year, paste0(year, month))

  # Scrape the directory to find available files
  cat("Scraping URL:", base_url, "\n")

  tryCatch({
    page <- rvest::read_html(base_url)
    # Extract the file names
    file_links <- page |> rvest::html_nodes("a") |> html_attr("href")
    # Filter for the relevant .tif file that contains 'avg_rade9h'
    tif_files <- file_links[grepl("SVDNB_npp_.*avg_rade9h\\.tif$", file_links)]

    if (length(tif_files) == 0) {
      stop("No suitable .tif files found for the specified year and month.")
    }

    # Extract the date codes from the file names
    date_codes <- sub(".*_v10_(c[0-9]+)\\.avg_rade9h\\.tif", "\\1", tif_files)

    # Find the latest date code (assumes lexicographical order is sufficient)
    latest_date_code <- max(date_codes)

    # Find the corresponding file with the latest date code
    latest_file <- tif_files[grep(latest_date_code, tif_files)]

    if (length(latest_file) == 0) {
      stop("Could not find a matching file for the latest date code.")
    }

    # Construct the full URL to the latest file
    url <- paste0(base_url, latest_file)
    destfile <- file.path(destination_dir, latest_file)

    # Download the file
    cat("Downloading latest file from URL:", url, "\n")
    options(timeout = timeout)
    download.file(url, destfile, mode = "wb")
    cat("File downloaded successfully:", destfile, "\n")

    # Return the file path
    return(destfile)
  }, error = function(e) {
    cat("Error during web scraping or downloading:", e$message, "\n")
    return(NULL)
  })
}
