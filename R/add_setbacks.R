add_setbacks <- function(tidyparcel, tidydistrict, tidybuilding){
  tidyparcel <- tidyparcel[!is.na(tidyparcel$side),]
  zoning_req <- get_zoning_req(tidybuilding, tidyparcel, tidydistrict)
  name_key <- c(front = "setback_front",
                side.interior = "setback_side_int",
                side.exterior = "setback_side_ext",
                rear = "setback_rear")
  for (i in 1:nrow(tidyparcel)){
    side_type <- tidyparcel[[i,"side"]]
    filtered_constraints <- zoning_req |>
      filter(constraint_name == name_key[[side_type]])
    setback_value <- filtered_constraints[1,"min_value"]

    tidyparcel[i,"setback"] <- setback_value
    tidyparcel[i,"units"] <- filtered_constraints[1,"units"]
  }

  tidyparcel
}
