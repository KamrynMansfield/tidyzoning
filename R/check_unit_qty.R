#' Do total building units meet zoning requirements
#'
#' `check_unit_qty` compares the total units in a tidybuilding object
#'  to the allowable number of units according to the tidyzoning data
#'  and returns TRUE, FALSE, or MAYBE
#'
#' @param tidybuilding
#' @param tidydistrict
#' @param tidyparcel_dims
#' @param zoning_req
#'
#' @returns
#' @export
#'
#' @examples
check_unit_qty <- function(tidybuilding, tidydistrict = NULL, tidyparcel_dims = NULL, zoning_req = NULL){
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

  unit_qty_df <- zoning_req[zoning_req$constraint_name %in% c("unit_qty",
                                                              "pct_units_0bed",
                                                              "pct_units_1bed",
                                                              "pct_units_2bed",
                                                              "pct_units_3bed",
                                                              "pct_units_4bed"), ]

  if (nrow(unit_qty_df) == 0){
    return(TRUE)
  }



  # getting the value from the building's attributes
  if (length(tidybuilding$unit_info$qty) == 1 & length(tidybuilding$unit_info$bedrooms) == 1){
    units <- sum(tidybuilding$unit_info$qty)

    bedrooms_df <- tidybuilding$unit_info |>
      group_by(bedrooms) |>
      summarise(qty = sum(qty))

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
                pct_units_0bed = pct_0bed,
                pct_units_1bed = pct_1bed,
                pct_units_2bed = pct_2bed,
                pct_units_3bed = pct_3bed,
                pct_units_4bed = pct_4bed)

  } else if(length(tidybuilding$unit_info$qty) == 1){
    units <- sum(tidybuilding$unit_info$qty)

    values <- c(unit_qty = units,
                pct_units_0bed = 0,
                pct_units_1bed = 0,
                pct_units_2bed = 0,
                pct_units_3bed = 0,
                pct_units_4bed = 0)
  } else{
    return(FALSE)
    warning("No unit count found in tidybuilding")
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
  } else{
    warning(warnings)
    return("MAYBE")
  }


}
