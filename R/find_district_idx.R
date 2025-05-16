#' Find corresponding tidydistrict
#'
#' @description
#' `find_district_idx()` takes in a tidyzoning object and a tidyparcel object.
#' Returns the row number of the district to which the parcel belongs.
#'
#' @inheritParams add_setbacks
#' @param tidyzoning The tidyzoning object is a simple features object with a row for each zoning district and columns holding the geojson formatted zoning requirements of each district.
#'
#' @return The row number of the tidyzoning object that contains the disrict to which the parcel belongs.
#' Returns NA if there is no district found.
#' @export
#'
find_district_idx <- function(tidyparcel_geo, tidyzoning){

# create a function that will get the index of the district the parcel is in

}
