# Download and Retrieve Nightlight Data

Downloads nightlight data from the Earth Observation Group's website. It
scrapes the website to locate and download the latest available
nightlight dataset for the specified year and month.

## Usage

``` r
get_nightlight_data(
  year,
  month,
  version = "v10",
  destination_dir = NULL,
  timeout = 1200
)
```

## Arguments

- year:

  \`numeric\` o \`character\` The year for which to download nightlight
  data (e.g., 2020).

- month:

  \`numeric\` o \`character\` Month of the year (1–12). Will be
  formatted as two digits (e.g., \`"03"\` for March).

- version:

  \`character\` Nightlight data version. Default is \`"v10"\`.

- destination_dir:

  \`character\` Directory where the downloaded \`.tif\` file will be
  saved. Default is the current working directory \`"."\`.

- timeout:

  \`numeric\` Timeout in seconds for the download. Default is \`1200\`
  seconds.

## Value

\`character\` o \`NULL\` Path to the downloaded \`.tif\` file. Returns
\`NULL\` if no file was found or if an error occurred.

## Details

The function constructs the appropriate URL for the specified year,
month, and data version, then scrapes the directory listing to locate
the latest available \`.tif\` file. It downloads and saves the file to
the \`destination_dir\`. This function is useful for retrieving
nightlight data for studies involving human activity, urbanization, and
environmental monitoring.

## Examples

``` r
# \donttest{
  # Download nightlight data for March 2021
  file_path <- get_nightlight_data(2021, 3)
#> No destination_dir provided. Using temporary directory: /tmp/RtmpOQeLRy
#> Scraping URL: https://eogdata.mines.edu/nighttime_light/monthly_notile/v10/2021/202103/vcmslcfg/
#> Error during web scraping or downloading: No suitable .tif files found for the specified year and month.
  print(file_path)
#> NULL
# }
```
