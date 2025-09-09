# Declare the pipe operator for R CMD check
#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

# Declare global variables used inside dplyr pipelines
# This avoids "no visible binding" notes in R CMD check
utils::globalVariables(c(
  "h3_address",
  "plot_id",
  "geometry",
  "metric",
  "value"
))
