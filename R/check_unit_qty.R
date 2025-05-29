#' Do total building units meet zoning requirements
#'
#' `check_unit_qty` compares the total units in a tidybuilding object
#'  to the allowable number of units according to the tidyzoning data
#'  and returns TRUE, FALSE, or MAYBE
#'
#' @inheritParams check_height
#' @param building_json The file path or json string of the json representing the building.
#'
#' @returns
#' @export
#'
#' @examples
check_unit_qty <- function(tidybuilding = NULL, tidydistrict = NULL, tidyparcel_dims = NULL, building_json, zoning_req = NULL){
  # if zoning_req is not given, we need to run the get_zoning_req function
  if (is.null(zoning_req)){
    zoning_req <- get_zoning_req(tidybuilding, tidydistrict, tidyparcel_dims)
  }

  # if the zonning_req is "character" and not "data.frame", there were no zoning requirements recorded.
  # we return maybe with a warning
  if (class(zoning_req) == "character"){
    warning("No zoning requirements recorded for this district")
    return(TRUE)
  }



  # list of the fields that this function will check
  unit_qty_df <- zoning_req[zoning_req$constraint_name %in% c("unit_qty",
                                                              "pct_units_0bed",
                                                              "pct_units_1bed",
                                                              "pct_units_2bed",
                                                              "pct_units_3bed",
                                                              "pct_units_4bed",
                                                              "unit_0bed_qty",
                                                              "unit_1bed_qty",
                                                              "unit_2bed_qty",
                                                              "unit_3bed_qty",
                                                              "unit_4bed_qty"), ]


  unit_info <- get_unit_info(building_json)

  # getting values from the building's attributes
  if (nrow(unit_qty_df) == 0){
    return(TRUE)
  } else if (nrow(unit_qty_df) == 1 & "unit_qty" %in% unit_qty_df$constraint_name){
    values <- c(unit_qty = sum(unit_info$qty))
  } else if (length(unit_info$qty) > 0 & length(unit_info$bedrooms) > 0){
    units <- sum(unit_info$qty)

    bedrooms_df <- unit_info |>
      dplyr::group_by(bedrooms) |>
      dplyr::summarise(qty = sum(qty))

    units_0bed <- bedrooms_df[bedrooms_df$bedrooms == 0,"qty"][[1]]
    units_1bed <- bedrooms_df[bedrooms_df$bedrooms == 1,"qty"][[1]]
    units_2bed <- bedrooms_df[bedrooms_df$bedrooms == 2,"qty"][[1]]
    units_3bed <- bedrooms_df[bedrooms_df$bedrooms == 3,"qty"][[1]]
    units_4bed <- bedrooms_df[bedrooms_df$bedrooms == 4,"qty"][[1]]


    pct_0bed <- units_0bed / units
    pct_1bed <- units_1bed / units
    pct_2bed <- units_2bed / units
    pct_3bed <- units_3bed / units
    pct_4bed <- units_4bed / units

    values <- c(unit_qty = units,
                pct_units_0bed = ifelse(length(pct_0bed) > 0,pct_0bed,0),
                pct_units_1bed = ifelse(length(pct_1bed) > 0,pct_1bed,0),
                pct_units_2bed = ifelse(length(pct_2bed) > 0,pct_2bed,0),
                pct_units_3bed = ifelse(length(pct_3bed) > 0,pct_3bed,0),
                pct_units_4bed = ifelse(length(pct_4bed) > 0,pct_4bed,0),
                unit_0bed_qty = ifelse(length(units_0bed) > 0,units_0bed,0),
                unit_1bed_qty = ifelse(length(units_1bed) > 0,units_1bed,0),
                unit_2bed_qty = ifelse(length(units_2bed) > 0,units_2bed,0),
                unit_3bed_qty = ifelse(length(units_3bed) > 0,units_3bed,0),
                unit_4bed_qty = ifelse(length(units_4bed) > 0,units_4bed,0))

  } else if(length(unit_info$qty) > 1){
    units <- sum(unit_info$qty)

    values <- c(unit_qty = units,
                pct_units_0bed = 0,
                pct_units_1bed = 0,
                pct_units_2bed = 0,
                pct_units_3bed = 0,
                pct_units_4bed = 0,
                unit_0bed_qty = 0,
                unit_1bed_qty = 0,
                unit_2bed_qty = 0,
                unit_3bed_qty = 0,
                unit_4bed_qty = 0)
  } else{
    return("MAYBE")
    warning("Improper building data")
  }

  allowed <- c()
  warnings <- c()
  for (i in 1:nrow(unit_qty_df)){

    constraint <- unit_qty_df[i,1][[1]]
    value <- values[constraint][[1]]

    # assume min and max values if they are not recorded
    # if specific constraint we are looking for is not in zoning requirements, we assume any value is allowed
    if (constraint %in% zoning_req$constraint_name){
      min_requirement <- zoning_req[zoning_req$constraint_name == constraint, "min_value"][[1]]
      max_requirement <- zoning_req[zoning_req$constraint_name == constraint, "max_value"][[1]]

      if (is.na(min_requirement[[1]])){
        min_requirement <- 0
      }

      if (is.na(max_requirement[[1]])){
        max_requirement <- 100000
      }

    } else{
      allowed <- c(allowed, TRUE)
      next
    }

    # this tests for the usual case when each min and max requirements have one value
    if (length(min_requirement) & length(max_requirement) == 1){
      allowed <- c(allowed, value >= min_requirement & value <= max_requirement)
      next
    }

    # this is where multiple values are listed for a unique zoning requirement
    # assign 2 minimum values
    min_check_1 <- min(min_requirement) <= value
    min_check_2 <- max(min_requirement) <= value
    if (is.null(zoning_req[zoning_req$constraint_name == constraint, "min_val_note"][[1]])){
      min_val_either <- FALSE
    } else if(is.na(zoning_req[zoning_req$constraint_name == constraint, "min_val_note"][[1]])){
      min_val_either <- FALSE
    } else{
      min_val_either <- zoning_req[zoning_req$constraint_name == constraint, "min_val_note"][[1]] == "either"
    }

    # see if the minimum values are met
    if (min_val_either == TRUE){
      if (min_check_1 == FALSE & min_check_2 == FALSE){
        min_check <- FALSE
      } else{
        min_check <- TRUE
      }

    } else{
      if (min_check_1 == TRUE & min_check_2 == TRUE){
        min_check <- TRUE
      } else if (min_check_1 == FALSE & min_check_2 == FALSE){
        min_check <- FALSE
      } else{
        min_check <- "MAYBE"
      }
    }

    # assign 2 maximum values
    max_check_1 <- min(max_requirement) >= value
    max_check_2 <- max(max_requirement) >= value
    if (is.null(zoning_req[zoning_req$constraint_name == constraint, "max_val_note"][[1]])){
      max_val_either <- FALSE
    } else if(is.na(zoning_req[zoning_req$constraint_name == constraint, "max_val_note"][[1]])){
      max_val_either <- FALSE
    } else{
      max_val_either <- zoning_req[zoning_req$constraint_name == constraint, "max_val_note"][[1]] == "either"
    }

    # see if the maximum values are met
    if (max_val_either == TRUE){
      if (max_check_1 == FALSE & max_check_2 == FALSE){
        max_check <- FALSE
      } else{
        max_check <- TRUE
      }

    } else{
      if (max_check_1 == TRUE & max_check_2 == TRUE){
        max_check <- TRUE
      } else if (max_check_1 == FALSE & max_check_2 == FALSE){
        max_check <- FALSE
      } else{
        max_check <- "MAYBE"
      }
    }

    # use both min and max value check to see if it is allowed
    if (max_check == FALSE | min_check == FALSE){
      allowed <- c(allowed, FALSE)
      next
    } else if (max_check == TRUE & min_check == TRUE){
      allowed <- c(allowed, TRUE)
      next
    } else{
      explanation <- c()
      if (!is.na(zoning_req[zoning_req$constraint_name == constraint, "min_val_note"][[1]])){
        explanation <- c(explanation,zoning_req[zoning_req$constraint_name == constraint,"min_val_note"][[1]])
      }
      if (!is.na(zoning_req[zoning_req$constraint_name == constraint, "max_val_note"][[1]])){
        explanation <- c(explanation,zoning_req[zoning_req$constraint_name == constraint,"max_val_note"][[1]])
      }

      if (length(explanation) > 0){
        warnings <- c(warnings ,explanation)
      }

      allowed <- c(allowed, "MAYBE")

    }

  }


  if (length(allowed[allowed == FALSE]) == nrow(unit_qty_df)){
    return(FALSE)
  } else if (length(allowed[allowed == TRUE]) == nrow(unit_qty_df)){
    return(TRUE)
  } else if ("MAYBE" %in% allowed){
    warning(paste(warnings, collapse = ", "))
    return("MAYBE")
  } else{
    return(FALSE)
  }


}
