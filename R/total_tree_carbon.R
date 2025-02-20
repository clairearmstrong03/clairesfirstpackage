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
#' @export

total_tree_carbon <- function(csv_file,
                              dbh_col = NULL,
                              height_col = NULL,
                              genus_col,
                              species_col,
                              lat_col,
                              lon_col, ...) {
  data <- read.csv(csv_file)

  carbon_content <- 0.5

  # Ensure required columns exist in the dataset
  required_cols <- c(species_col, lat_col, lon_col)
  if (!all(required_cols %in% names(data))) {
    stop("Dataset must contain species and location columns")
  }

  # Check if DBH or height column is provided and exists in the dataset
  if (!is.null(dbh_col) && dbh_col %in% names(data)) {
    measurement_col <- dbh_col
    measurement_type <- "dbh"
  } else if (!is.null(height_col) && height_col %in% names(data)) {
    measurement_col <- height_col
    measurement_type <- "height"
  } else {
    stop("Dataset must contain either a DBH or height column")
  }

  # Extract columns for checking
  dbh_values <- data[[dbh_col]]
  height_values <- data[[height_col]]
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
  if (length(genus_values) != length(height_values) || length(species_values) != length(height_values)) {
    stop("Lengths of height_col, genus_col, and species_col must match.")
  }
  if (length(lat_values) != length(dbh_values) || length(lon_values) != length(dbh_values)) {
    stop("Lengths of dbh_col, lon_col, and lat_col must match.")
  }
  if (length(lat_values) != length(height_values) || length(lon_values) != length(height_values)) {
    stop("Lengths of height_col, lon_col, and lat_col must match.")
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
  total_carbon <- data %>%
    rowwise() %>%
    mutate(
      biomass = allodb::get_biomass(
        species = !!sym(species_col),
        genus = !!sym(genus_col),
        dbh = ifelse(measurement_type == "dbh", !!sym(measurement_col), NA),
        height = ifelse(measurement_type == "height", !!sym(measurement_col), NA),
        coords = c(!!sym(lon_col), !!sym(lat_col))
      ),
  # calculate carbon content of each tree
      tree_carbon = biomass*carbon_content
    ) %>%
    ungroup()%>%
  # calculate the sum of carbon content in the stand
    summarise(total_carbon = sum(tree_carbon))

  return(total_carbon)
}
