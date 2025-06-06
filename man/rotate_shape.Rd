% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rotate_shape.R
\name{rotate_shape}
\alias{rotate_shape}
\title{Rotate polygon by specified angle}
\usage{
rotate_shape(shape, angle_degrees, center = NULL)
}
\arguments{
\item{shape}{a polygon you want to rotate. Must be a special feature object with only one row.}

\item{angle_degrees}{The angle (in degrees) that you want to rotate the shape}

\item{center}{The center about which to rotate in the form of XY coordinates.
Default is null,  When NULL, rotation is about the centroid.}
}
\value{
Returns the simple feature shape object with rotated geometry
}
\description{
The \code{rotate_shape()} function takes a one-row special feature object and rotates the geometry about the centroid or a specified point.
}
