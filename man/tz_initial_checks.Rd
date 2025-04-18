% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tz_initial_checks.R
\name{tz_initial_checks}
\alias{tz_initial_checks}
\title{Run initial check functions}
\usage{
tz_initial_checks(
  tidybuilding,
  tidyzoning,
  tidyparcel_with_dimensions,
  func_names = c("check_land_use", "check_height", "check_height_eave", "check_floors",
    "check_unit_size", "check_far", "check_unit_density", "check_lot_coverage",
    "check_fl_area", "check_unit_qty"),
  run_parallel = FALSE,
  detailed_check = FALSE
)
}
\arguments{
\item{tidybuilding}{A tidybuilding is a list of data frames used to represent a building.}

\item{tidyzoning}{A tidyzoning simple features object representing the zoning districts and regulations for an area.}

\item{tidyparcel_with_dimensions}{A data frame with each tidyparcel and its information. The zoning_id column in the tidyparcel_with_dimensions must correspond to the row numbers of the tidyzoning object.}

\item{func_names}{A character list of the check functions you want to run.}

\item{run_parallel}{Logical. When `TRUE` the code runs on multiple cores (one less than your machine has available). Will save time if processing over 50,000 parcels. Default is `FALSE`}

\item{detailed_check}{Logical. When `TRUE`, it returns a data frame stating whether or not the building is allowed in the parcel. Otherwise, it will return a data frame with every chack function's results. Default is `FALSE`}
}
\value{
A data frame with initial check function results.
}
\description{
`tz_initial_checks()` runs the initial tidyzoning check functions to see if certain attributes of the building are allowed in the parcel.
}
