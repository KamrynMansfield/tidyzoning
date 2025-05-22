#' Check Footprint Area
#'
#' @param tidyparcel_dims A data frame with the dimmensions of each parcel
#' @param tidybuilding A list of data frames describing a building
#'
#' @returns The tidyparcel data frame with an added column stating whether the area of the building is smaller than the area of the parcel
#' @export
#'
#' @examples
check_footprint_area <- function(tidybuilding, tidyparcel_dims){
  tidyparcel_dims$bldg_area <- tidybuilding$width * tidybuilding$depth

  tidyparcel_dims |>
    dplyr::mutate(check_footprint_area = bldg_area < (lot_area * 43560))

}
