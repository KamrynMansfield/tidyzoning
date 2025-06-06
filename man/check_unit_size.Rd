% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_unit_size.R
\name{check_unit_size}
\alias{check_unit_size}
\title{Compare unit sizes and allowed unit sizes}
\usage{
check_unit_size(
  tidybuilding,
  tidydistrict,
  tidyparcel_dims,
  building_json,
  zoning_req = NULL
)
}
\arguments{
\item{tidybuilding}{A tidybuilding is a list of data frames used to represent a building.}

\item{tidydistrict}{The tidydistrict corresponding to the tidyparcel. A tidydistrict object is one row from a tidyzoning simple features object.}

\item{tidyparcel_dims}{A tidyparcel object is an simple features object depicting each side of a parcel and its label (front, Interior side, Exterior side, rear, centroid).}

\item{building_json}{The file path or json string of the json representing the building.}

\item{zoning_req}{The data frame result from the \code{get_zoning_req()} function.}
}
\value{
Returns TRUE, FALSE, or MAYBE stating whether or not the building would be allowed in the district based on the unit sizes.
}
\description{
\code{check_unit_size()} takes a tidybuilding and a tidydistrict to see if the district's zoning code allows the tidybuilding based on unit sizes.
}
