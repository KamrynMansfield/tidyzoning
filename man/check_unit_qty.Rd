% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_unit_qty.R
\name{check_unit_qty}
\alias{check_unit_qty}
\title{Do total building units meet zoning requirements}
\usage{
check_unit_qty(
  tidybuilding = NULL,
  tidydistrict = NULL,
  tidyparcel_dims = NULL,
  building_json,
  zoning_req = NULL
)
}
\arguments{
\item{tidybuilding}{A dataframe with fields to represent a building.
Created from the function \link{unify_tidybuilding}.}

\item{tidydistrict}{The tidydistrict corresponding to the tidyparcel.
A tidydistrict object is one row from a tidyzoning simple features object.}

\item{tidyparcel_dims}{A tidyparcel object is an simple features object
depicting each parcel and its its dimentions.}

\item{building_json}{The file path or json string of the json representing the building.}

\item{zoning_req}{The data frame result from the \code{get_zoning_req()} function.
If provided, the tidydistrict and parccel need not be provided.}
}
\description{
\code{check_unit_qty} compares the total units in a tidybuilding object
to the allowable number of units according to the tidyzoning data
and returns TRUE, FALSE, or MAYBE
}
