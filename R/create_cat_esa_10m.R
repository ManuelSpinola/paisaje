#'
#' @name create_cat_esa_10m
#'
#' @title Create Categorical Land Cover Raster from Copernicus
#' Land Cover Data \href{https://esa-worldcover.org/en}{ESA WorldCover}
#'
#' @description
#' This function loads a Copernicus land cover raster,
#' reclassifies it into specified land cover categories, and
#' optionally saves the reclassified raster to a file. The
#' reclassification is based on predefined original classes and
#' their corresponding new categorical classes.
#'
#' @usage
#' create_cat_esa_10m(input_raster, output_raster = NULL)
#'
#' @param input_raster A character string specifying the file
#' path to the input raster (Copernicus land cover raster) that
#' is to be reclassified.
#' @param output_raster A character string specifying the file
#' path where the output categorical raster will be saved.
#' If `NULL`, the raster will not be saved.
#'
#' @return A `SpatRaster` object containing the reclassified
#' categorical land cover raster.
#'
#' @details
#' The function defines a mapping of original land cover classes
#' to new categorical classes. The reclassification is performed
#' using the `levels` and `coltab` functions from the `terra`
#' package. The reclassified raster will have colors associated
#' with each class as defined in the `coltb` data frame.
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
#' # Create categorical land cover raster
#' categorical_raster <- create_cat_esa_10m("path_to_input_raster.tif",
#'                                            "path_to_output_raster.tif")
#' }
#'
#' @export
#'



create_cat_esa_10m <- function(input_raster, output_raster = NULL) {

  # Load the Copernicus land cover raster
  land_cover <- terra::rast(input_raster)

  # Define land cover categories (example: classes 1-10)
  # Modify this according to your specific land cover classes
  coltb <- data.frame(
    value = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 100), # original classes
    col = c("#006400", "#FFBB22", "#FFFF4C", "#F096FF", "#FA0000", "#B4B4B4", "#f0f0f0", "#0064C8", "#0096A0",  "#00CF75", "#FAE6A0") # colors
  )

  cls <- data.frame(
    value = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 100), # original classes
    lulc = c("Tree_cover", "Shrubland", "Grassland", "Cropland", "Built_up", "Bare_sparse_vegetation", "Snow_and_Ice", "Permanent_water_bodies", "Herbaceous_wetland", "Mangroves", "Moss_and_lichen") # new categorical classes
  )

  # Reclassify the raster to categorical values

  has.colors(land_cover)

  coltab(land_cover) <- coltb

  levels(land_cover) <- cls

  categorical_raster <- land_cover

  # If an output file path is provided, save the raster
  if (!is.null(output_raster)) {
    terra::writeRaster(categorical_raster, output_raster, datatype = "INT1U", overwrite = TRUE)
    message("Categorical land cover raster saved to: ", output_raster)
  }

  # Return the categorical raster
  return(categorical_raster)
}



