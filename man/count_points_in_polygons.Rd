% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/count_points_in_polygons.R
\name{count_points_in_polygons}
\alias{count_points_in_polygons}
\title{Count Points within Polygons}
\usage{
count_points_in_polygons(points_sf, polygons_sf)
}
\arguments{
\item{points_sf}{points_sf An `sf` object representing point geometries. Must contain a `species` column.}

\item{polygons_sf}{An sf object representing polygon geometries.}
}
\value{
An `sf` object containing the original polygon geometries and additional columns, one for each species, indicating the count of points of that species within each polygon.
}
\description{
This function counts the number of points within each polygon and creates separate columns for each species if a `species` column is present in the points dataset. Spaces in species
names are replaced with underscores for column naming.
}
\examples{
\dontrun{
# Example usage assuming 'points' and 'polygons' are sf objects

library(sf)

# Create example points_sf with species column
points_sf <- st_as_sf(data.frame(
  id = 1:6,
  species = c("Panthera onca", "Panthera onca", "Felis catus",
              "Felis catus", "Felis catus", "Panthera leo"),
  x = c(1, 2, 3, 4, 5, 6),
  y = c(1, 2, 3, 4, 5, 6)
), coords = c("x", "y"), crs = 4326)

# Create example polygons_sf
polygons_sf <- st_as_sf(data.frame(
  id = 1:2,
  geometry = st_sfc(
    st_polygon(list(rbind(c(0, 0), c(3, 0), c(3, 3), c(0, 3), c(0, 0)))),
    st_polygon(list(rbind(c(3, 3), c(6, 3), c(6, 6), c(3, 6), c(3, 3))))
  )
), crs = 4326)

# Run the function
result <- count_points_in_polygons(points_sf, polygons_sf)

# View the result
print(result)
}

}
