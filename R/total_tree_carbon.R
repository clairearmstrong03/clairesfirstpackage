#' Calculate Total Tree Carbon
#'
#' This function calculates total tree carbon using biomass estimates from the `allodb` package.
#'
#' @param data A data frame containing tree information.
#' @param dbh_col Optional. The name of the column containing DBH (diameter at breast height).
#' @param height_col Optional. The name of the column containing tree height.
#' @param genus_col The name of the column containing genus information.
#' @param species_col The name of the column containing species information.
#' @param lat_col The name of the column containing latitude.
#' @param lon_col The name of the column containing longitude.
#' @return A data frame with total estimated carbon content.
#' @import dplyr
#' @importFrom allodb get_biomass
#' @export()

total_tree_carbon <- function(data,
                              dbh_col,
                              genus_col,
                              species_col,
                              lat_col,
                              lon_col,
                              carbon, ...) {
  # Ensure required columns exist in the dataset
  required_cols <- c(dbh_col, species_col, lat_col, lon_col)
  if (!all(required_cols %in% names(data))) {
    stop("Dataset must contain tree DBH, species and location columns")
  }

  # Extract columns for checking
  dbh_values <- data[[dbh_col]]
  genus_values <- data[[genus_col]]
  species_values <- data[[species_col]]
  lat_values <- data[[lat_col]]
  lon_values <- data[[lon_col]]

  # Check that DBH values are positive and non-missing
  if (any(is.na(dbh_values)) || any(dbh_values <= 0)) {
    stop("dbh_col must contain only positive, non-missing values.")
  }

  # Check if column lengths match
  if (length(genus_values) != length(dbh_values) || length(species_values) != length(dbh_values)) {
    stop("Lengths of dbh_col, genus_col, and species_col must match.")
  }
  if (length(lat_values) != length(dbh_values) || length(lon_values) != length(dbh_values)) {
    stop("Lengths of dbh_col, lon_col, and lat_col must match.")
  }

  # Check if latitude and longitude are within valid ranges
  if (any(lat_values > 90 | lat_values < -90)) {
    stop("lat_col values must be between -90 and 90.")
  }
  if (any(lon_values > 180 | lon_values < -180)) {
    stop("lon_col values must be between -180 and 180.")
  }

  # Check for missing latitude or longitude values
  if (any(is.na(lat_values)) || any(is.na(lon_values))) {
    stop("lat_col or lon_col contains missing values.")
  }

  # Apply get_biomass function to each row
 data$biomass <- get_biomass(
   dbh = data[[dbh_col]],
   genus = data[[genus_col]],
   species = data[[species_col]],
   coords = data[,c(lon_col, lat_col)])

 data <- data %>%
   mutate(tree_carbon = biomass * carbon)

 total_carbon <- data %>%
   summarise(total_carbon = sum(tree_carbon))

  return(total_carbon)
}
