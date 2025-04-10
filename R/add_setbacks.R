#' Add setback column to tidyparcel object
#'
#' `add_setbacks()` returns the tidyparcel object with new columns describing the setback and units of each side.
#'
#'
#' @param tidyparcel A tidyparcel object is an simple features object depicting each side of a parcel and its label (front, Interior side, Exterior side, rear, centroid).
#' @param tidydistrict The tidydistrict corresponding to the tidyparcel. A tidydistrict object is one row from a tidyzoning simple features object.
#' @param tidybuilding A tidybuilding is a list of data frames used to represent a building.
#'
#' @return Returns the tidyparcel object with a "setbacks" and "units" column added to the end.
#' @export
#'


add_setbacks <- function(tidyparcel, tidydistrict, tidybuilding){
  tidyparcel <- tidyparcel[tidyparcel$side != "centroid",]
  tidyparcel <- tidyparcel[!is.na(tidyparcel$side),]
  zoning_req <- get_zoning_req(tidybuilding, tidydistrict, tidyparcel)

  if (class(zoning_req) == "character"){
    tidyparcel$setback <- NA
    tidyparcel$units <- NA
    return(tidyparcel)
  }

  name_key <- c(front = "setback_front",
                `Interior side` = "setback_side_int",
                `Exterior side` = "setback_side_ext",
                rear = "setback_rear")
  for (i in 1:nrow(tidyparcel)){
    side_type <- tidyparcel[[i,"side"]]
    filtered_constraints <- zoning_req |>
      filter(constraint_name == name_key[[side_type]])

    if (nrow(filtered_constraints) > 0){
      tidyparcel[i,"setback"] <- filtered_constraints[1,"min_value"]
      tidyparcel[i,"units"] <- filtered_constraints[1,"units"]
    } else {
      tidyparcel[i,"setback"] <- NA
      tidyparcel[i,"units"] <- NA
    }
  }

  tidyparcel
}
