#' Compare building floor area and allowed floor area
#'
#' `check_fl_area()` takes a tidybuilding and a tidydistrict to see if the district's zoning code allows the tidybuilding based on floor area.
#'
#' @inheritParams add_setbacks
#'
#' @return
#' Returns TRUE or FALSE stating whether or not the building would be allowed in the district based on floor area.
#' @export
#'
check_fl_area <- function(tidybuilding, tidydistrict){
  structure_constraints <- fromJSON(tidydistrict$structure_constraints)

  if (length(tidybuilding$floor_area) == 1){
    fl_area <- tidybuilding$floor_area[[1]]
  } else if (length(tidybuilding$total_floors) == 1){
    floors <- tidybuilding$total_floors[[1]]
    fl_area <- st_area(tidybuilding) * floors
  } else{
    return(FALSE)
    warning("No floor area found in tidybuilding")
  }

  fl_area <- set_units(fl_area, "ft2")

  zoning_req <- get_zoning_req(tidybuilding, tidydistrict)

  if ("fl_area" %in% zoning_req$constraint_name){
    min_fl_area <- zoning_req[zoning_req$constraint_name == "fl_area", "min_value"]
    max_fl_area <- zoning_req[zoning_req$constraint_name == "fl_area", "max_value"]

    if (is.na(min_fl_area)){
      min_fl_area <- 0
    }

    if (is.na(max_fl_area)){
      max_fl_area <- 1000000
    }

    # change fl_area units to match the ones recorded in the code
    if (!is.na(zoning_req[zoning_req$constraint_name == "fl_area", "units"])){
      fl_area_units <- zoning_req[zoning_req$constraint_name == "fl_area", "units"]
      fl_area <- set_units(fl_area, fl_area_units)
    }

    return(fl_area >= min_fl_area & fl_area <= max_fl_area)

  } else{
    return(TRUE)
  }

}
