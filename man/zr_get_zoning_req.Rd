% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zr_get_zoning_req.R
\name{zr_get_zoning_req}
\alias{zr_get_zoning_req}
\title{List district zoning requirement values}
\usage{
zr_get_zoning_req(
  district_data,
  bldg_data = NULL,
  parcel_data = NULL,
  zoning_data = NULL,
  vars = NULL
)
}
\arguments{
\item{district_data}{one row (representing one district) of a
zoning data frame created from the OZFS *.zoning file}

\item{bldg_data}{either the file path to an OZFS *.bldg file or
a list created from the the *.bldg file using \code{rjson::fromJSON}}

\item{parcel_data}{one row of a parcel data frame created from the
OZFS *.parcel file}

\item{zoning_data}{either the path to a *.zoning file or
a list created from the the *.zoning file using \code{rjson::fromJSON}}

\item{vars}{the result from the \code{get_variables()} function.
If this data frame is supplied, bldg_data, parcel_data, and zoning_data
are not needed.}
}
\value{
Returns a data frame with the value of each zoning requirement for that specific building, parcel, and zoning district.
NA values indicate the requirement isn't recorded in that district.
If every value is NA, it could indicate that the building land use is not allowed in the zoning district.
}
\description{
Because many zoning requirements depend on the proposed building
or the parcel of that zoning district, the \code{zr_get_zoning_req()} function
takes a tidybuilding, a tidyparcel, and a district_data and outputs a data
frame listing the set zoning requirements that those three objects would create.
If every value is NA, it could indicate that the building
land use is not allowed in the zoning district.
}
