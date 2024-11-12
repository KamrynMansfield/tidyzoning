#' Find District Index
#'
#' @description
#' Takes in an sf data frame with district geometry and a parcel.
#' Returns the row number of the district to which the parcel belongs.
#'
#' @param tidyparcel An sf data frame with a linestring for each side of the parcel.
#' @param tidyzoning The sf data frame with district geometry.
#'
#' @return The row number of the district to which the parcel belongs.
#' Returns NA if there is no district found.
#' @export
#'
#' @examples
find_district_idx <- function(tidyparcel, tidyzoning){
  # lists TRUE/FALSE to indicate which tidyzoning geometries cover the parcel centroid
  contains <- st_contains(st_make_valid(tidyzoning), tail(tidyparcel,1), sparse = F)
  # gives the index of the district that covers the centroid
  idx <- as.numeric(row.names(tidyzoning[contains, ]))

  # returns NA if there isn't a unique district covering the parcel centroid
  if (length(idx) == 1){
    print(idx)
  } else {
    print(NA)
  }
}
