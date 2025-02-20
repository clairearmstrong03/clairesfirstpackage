% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/total_tree_carbon.R
\name{total_tree_carbon}
\alias{total_tree_carbon}
\title{Calculate Total Tree Carbon}
\usage{
total_tree_carbon(
  data,
  dbh_col,
  genus_col,
  species_col,
  lat_col,
  lon_col,
  carbon,
  ...
)
}
\arguments{
\item{data}{A data frame containing tree information.}

\item{dbh_col}{Optional. The name of the column containing DBH (diameter at breast height).}

\item{genus_col}{The name of the column containing genus information.}

\item{species_col}{The name of the column containing species information.}

\item{lat_col}{The name of the column containing latitude.}

\item{lon_col}{The name of the column containing longitude.}

\item{height_col}{Optional. The name of the column containing tree height.}
}
\value{
A data frame with total estimated carbon content.
}
\description{
This function calculates total tree carbon using biomass estimates from the \code{allodb} package.
}
