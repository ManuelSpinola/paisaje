# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

library(sf)
library(h3jsr)
library(dplyr)

get_h3_grid <- function(sf_object, resolution = 7) {
  # Check if the input is an sf object
  if (!inherits(sf_object, "sf")) {
    stop("Input must be an sf object")
  }

  # Ensure the sf object is in the correct CRS (WGS84)
  sf_object <- st_transform(sf_object, 4326)

  # Get the bounding box of the sf object
  bbox <- st_bbox(sf_object)

  # Create a polygon from the bounding box
  bbox_poly <- st_as_sfc(st_bbox(sf_object))

  # Get H3 hexagons for the bounding box
  h3_hexagons <- polygon_to_cells(bbox_poly, res = resolution)

  # Convert H3 hexagons to sf object
  h3_sf <- cell_to_polygon(h3_hexagons)

  # Intersect H3 hexagons with the original sf object
  h3_intersect <- st_intersects(h3_sf, sf_object)

  # Keep only the hexagons that intersect with the sf object
  h3_grid <- h3_sf[lengths(h3_intersect) > 0, ]

  return(h3_grid)
}

# Example usage:
# Assuming you have an sf object called 'my_sf_object'
# h3_grid_sf <- get_h3_grid(my_sf_object, resolution = 7)
