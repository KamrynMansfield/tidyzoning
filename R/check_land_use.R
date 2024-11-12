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
