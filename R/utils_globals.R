# Declare global variables used inside dplyr pipelines to avoid
# "no visible binding for global variable" notes in R CMD check
utils::globalVariables(c(
  ".data",
  "h3_address",
  "plot_id",
  "geometry",
  "metric",
  "value",
  "n",
  "sp_name",
  "sp_col",
  "hex_id"
))
