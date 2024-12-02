#' Compare building land use and allowed land uses
#'
#' `check_land_use()` takes a tidybuilding and a tidydistrict to see if the district's zoning code allows the tidybuilding based on land use.
#'
#' @inheritParams add_setbacks
#'
#' @return
#' Returns TRUE or FALSE stating whether or not the building would be allowed in the district based on land use.
#' @export
#'
check_land_use <- function(tidybuilding, tidydistrict){
  dist_info_list <- fromJSON(tidydistrict$dist_info)

  bldg_type <- find_bldg_type(tidybuilding)

  if (bldg_type == "other"){
    print(FALSE)
    warning("Unable to calculate building type. Results may not be accurate")
  } else {
    bldg_type %in% dist_info_list$uses_permitted$uses_value
  }
}
