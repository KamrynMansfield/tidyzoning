% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_tidyparcel_dim.R
\name{get_tidyparcel_dim}
\alias{get_tidyparcel_dim}
\title{Isolate dimensional rows from tidyparcel data}
\usage{
get_tidyparcel_dim(parcels_data)
}
\arguments{
\item{parcels_data}{Either a file path to the a parcels geojson file or an sf object with the same data.}
}
\value{
an sf object with only dimensional and centroid data for each parcel
}
\description{
Isolate dimensional rows from tidyparcel data
}
