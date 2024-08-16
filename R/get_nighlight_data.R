#'
#' @name get_nightlight_data
#'
#' @title Download nightlight data
#'
#' @description
#' This function allows you to download night lightdata from
#'  the \href{https://eogdata.mines.edu/}{The Earth Observation
#'   Group (EOG)}.
#'
#' @export
#'
#' @examples
#'
#' get_nightlight_data(2024, 3, "v10", ".", timeout = 1200)
#'
#'

library(rvest)

get_nightlight_data <- function(year, month, version = "v10", destination_dir = ".", timeout = 1200) {

  # Load the necessary packages
  library(rvest)

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
    file_links <- page %>% rvest::html_nodes("a") %>% html_attr("href")
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
