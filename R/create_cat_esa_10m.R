#'
#' @name create_cat_esa_10m
#'
#' @title Create Categorical Land Cover Raster from Copernicus
#' Land Cover Data \href{https://esa-worldcover.org/en}{ESA
#' WorldCover}
#'
#' @description
#' This function takes a `SpatRaster` object representing
#' Copernicus land cover data, reclassifies it into categorical
#' values based on specified land cover classes, and returns
#' the resulting categorical raster.
#'
#' @usage
#' create_cat_esa_10m(land_cover)
#'
#' @param land_cover A SpatRaster object representing the input
#' land cover raster. The raster should contain land cover
#' classes defined by the Copernicus program.
#'
#' @return A SpatRaster object representing the reclassified
#' categorical land cover raster.
#'
#' @details The function defines land cover categories and
#' assigns corresponding colors. It reclassifies the input
#' raster based on predefined categories.
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
#' @examples
#' \dontrun{
#' # Assuming 'land_cover_raster' is a SpatRaster object
#' cat_raster <- create_cat_esa_10m(land_cover_raster)
#' }
#'
#' @export
#'



create_cat_esa_10m <- function(land_cover) {

  # Ensure input is a terra raster
  if (!inherits(land_cover, "SpatRaster")) {
    stop("The input must be a 'SpatRaster' object.")
  }

  # Define land cover categories and corresponding colors
  coltb <- data.frame(
    value = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 100),
    col = c("#006400", "#FFBB22", "#FFFF4C", "#F096FF", "#FA0000",
            "#B4B4B4", "#f0f0f0", "#0064C8", "#0096A0", "#00CF75",
            "#FAE6A0")
  )

  cls <- data.frame(
    value = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 100),
    lulc = c("Tree_cover", "Shrubland", "Grassland", "Cropland",
             "Built_up", "Bare_sparse_vegetation", "Snow_and_Ice",
             "Permanent_water_bodies", "Herbaceous_wetland",
             "Mangroves", "Moss_and_lichen")
  )

  # Reclassify the raster with the defined categories and colors
  terra::coltab(land_cover) <- coltb
  levels(land_cover) <- cls

  # Return the reclassified categorical raster
  return(land_cover)
}



