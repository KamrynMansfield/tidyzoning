#' Compare building floor area ratio to zoning requirements
#'
#' `check_far()` takes a tidybuilding, tidydistrict, and tidyparcel to see if the district's zoning code allows the tidybuilding based on floor area ratio.
#'
#' @param tidybuilding A tidybuilding is a list of data frames used to represent a building.
#' @param tidydistrict The tidydistrict corresponding to the tidyparcel. A tidydistrict object is one row from a tidyzoning simple features object.
#' @param tidyparcel A tidyparcel object is an simple features object depicting each side of a parcel and its label (front, Interior side, Exterior side, rear, centroid).
#' @param zoning_req The data frame result from the `get_zoning_req()` function. If provided, the tidydistrict and parccel need not be provided.
#'
#'
#' @return
#' Returns TRUE or FALSE stating whether or not the building would be allowed in the district based on floor area ratio.
#' @export
#'
check_far <- function(tidybuilding, tidydistrict = NULL, tidyparcel, zoning_req = NULL){
  # if zoning_req is not given, we need to run the get_zoning_req function
  if (is.null(zoning_req)){
    zoning_req <- get_zoning_req(tidybuilding, tidydistrict, tidyparcel)
  }

  # if the zonning_req is "character" and not "data.frame", there were no zoning requirements recorded.
  # we return maybe with a warning
  if (class(zoning_req) == "character"){
    warning("No zoning requirements recorded for this district")
    return(TRUE)
  }

  # establish the constaint we are looking at
  constraint <- "far"

  # getting the value from the building's attributes
  # if the fl_area is not recorded
  if (length(tidybuilding$bldg_info$gross_fl_area) == 1){
    fl_area <- tidybuilding$bldg_info$gross_fl_area[[1]]
  } else if (length(tidybuilding$unit_info$fl_area) == 1){
    fl_area <- sum(tidybuilding$unit_info$fl_area * tidybuilding$unit_info$qty)
    warning("gross_fl_area calculated by summing unit fl_areas")
  } else{
    return(FALSE)
    warning("No floor area found in tidybuilding")
  }

  ######## need to double check to make sure I'm calling the lot_area correctly
  ########
  value <- fl_area / (tidyparcel$lot_area * 43560)

  # assume min and max values if they are not recorded
  # if specific constraint we are looking for is not in zoning requirements, we assume any value is allowed
  if (constraint %in% zoning_req$constraint_name){
    min_requirement <- zoning_req[zoning_req$constraint_name == constraint, "min_value"][[1]]
    max_requirement <- zoning_req[zoning_req$constraint_name == constraint, "max_value"][[1]]

    if (is.na(min_requirement[[1]])){
      min_requirement <- 0
    }

    if (is.na(max_requirement[[1]])){
      max_requirement <- 1000
    }

  } else{
    return(TRUE)
  }

  # this tests for the usual case when each min and max requirements have one value
  if (length(min_requirement) & length(max_requirement) == 1){
    return(value >= min_requirement & value <= max_requirement)
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
    return(FALSE)
  } else if (max_check == TRUE & min_check == TRUE){
    return(TRUE)
  } else{
    explanation <- c()
    if (!is.na(zoning_req[zoning_req$constraint_name == constraint, "min_val_note"][[1]])){
      explanation <- c(explanation,zoning_req[zoning_req$constraint_name == constraint,"min_val_note"][[1]])
    }
    if (!is.na(zoning_req[zoning_req$constraint_name == constraint, "max_val_note"][[1]])){
      explanation <- c(explanation,zoning_req[zoning_req$constraint_name == constraint,"max_val_note"][[1]])
    }

    if (length(explanation) > 0){
      warning(explanation)
    }

    return("MAYBE")

  }


}
