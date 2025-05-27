#' Find corresponding tidydistrict
#'
#' @description
#' `find_district_idx()` adds a zoning_id column to the parcels data that
#' contains the tidyzoning row index of the district the parcel is in
#'
#' @param parcels_centroids_sf An sf object with the parcel centroids
#' @param tidyzoning A simple features object with a row for each zoning district and columns holding the geojson formatted zoning requirements of each district.
#'
#' @return The same parcels centroids object that was input but with an additional
#' column stating the row number of the corresponding tidyzoning district
#' @export
#'
find_district_idx <- function(parcels_centroids_sf, tidyzoning, idx_col_name = "zoning_id"){

  tidyzoning[[idx_col_name]] <- 1:nrow(tidyzoning)

  parcels_with_zoning_id <- sf::st_join(parcels_centroids_sf, tidyzoning[idx_col_name])

  # in the future, I may have to account for the rare case where
  # there is a parcel in two base districts


  return(parcels_with_zoning_id)

}
