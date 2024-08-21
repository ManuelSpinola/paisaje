#'
#' @name create_cat_copernicus_10m
#'
#' @title Create a categorical land cover raster
#'
#' @description Create a categorical land cover raster from
#'  a land cover raster from copernicus 10m resolution
#'
#' @param input_raster Path to your input raster file, which
#'  should be the original Copernicus land cover raster.
#' @param output_raster (optional): Path where the categorized
#'  raster should be saved. If NULL, the raster is not saved to disk.
#'
#' @return A raster
#'
#' @references
#' Zanaga, D., Van De Kerchove, R., Daems, D., De Keersmaecker,
#'  W., Brockmann, C., Kirches, G., Wevers, J., Cartus, O.,
#'  Santoro, M., Fritz, S., Lesiv, M., Herold, M., Tsendbazar,
#'  N.E., Xu, P., Ramoino, F., Arino, O., 2022. ESA WorldCover
#'  10 m 2021 v200. https://doi.org/10.5281/zenodo.7254221
#'
#' ESA WorldCover project / Contains modified Copernicus
#' Sentinel data (2021) processed by ESA WorldCover consortium
#'
#' \href{https://worldcover2021.esa.int/}{ESA WorldCover 2021}
#'
#' @export
#'
#' @examples
#'
#' input_raster = "copernicus_10m_non_categorical_raster.tif"
#' output_raster = "copernicus_10m_categorical_raster.tif"
#'
#'
#' create_cat_copernicus_10m(input_raster, output_raster)
#'
#'


create_cat_copernicus_10m <- function(input_raster, output_raster = NULL) {

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
    cover = c("Tree_cover", "Shrubland", "Grassland", "Cropland", "Built_up", "Bare_sparse_vegetation", "Snow_and_Ice", "Permanent_water_bodies", "Herbaceous_wetland", "Mangroves", "Moss_and_lichen") # new categorical classes
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



