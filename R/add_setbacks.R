add_setbacks <- function(tidyparcel, tidydistrict, tidybuilding){
  tidyparcel <- tidyparcel[!is.na(tidyparcel$side),]
  zoning_req <- get_zoning_req(tidybuilding, tidyparcel, tidydistrict)

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
      setback_value <- filtered_constraints[1,"min_value"]

      tidyparcel[i,"setback"] <- setback_value
      tidyparcel[i,"units"] <- filtered_constraints[1,"units"]
    } else {
      tidyparcel[i,"setback"] <- NA
      tidyparcel[i,"units"] <- NA
    }
  }

  tidyparcel
}

#
# tidyparcel <- tidyparcel_list_haltom[[i]]
# tidydistrict <- tidyzoning_haltom[4,]
# tidybuilding <- tidybuilding_ex
# tidybuilding$units_3bed[1] <- 1


# tidyparcel <- tidyparcel_list_haltom[[2]]
# tidybuilding <- tidybuilding_12fam
# tidydistrict <- tidyzoning_haltom[4,]


#
# tidydistrict <- tidyzoning_haltom[17,]
# tidybuilding <- tidybuilding_ex
# tidyparcel <- tidyparcel_list_haltom[[1]]
