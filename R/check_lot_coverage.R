#' Compare building lot coverage to zoning requirements
#'
#' `check_lot_coverage()` takes a tidybuilding, tidydistrict, and tidyparcel to see if the district's zoning code allows the tidybuilding based on lot coverage.
#'
#' @inheritParams check_far
#'
#' @return
#' Returns TRUE, FALSE, or MAYBE stating whether or not the building would be allowed in the district based on lot coverage.
#' @export
#'
check_lot_coverage <- function(tidybuilding, tidydistrict = NULL, tidyparcel_dims, zoning_req = NULL){
  # if zoning_req is not given, we need to run the get_zoning_req function
  if (is.null(zoning_req)){
    zoning_req <- get_zoning_req(tidybuilding, tidydistrict, tidyparcel_dims)
  }

  # establish the constaint we are looking at
  constraint <- "lot_cov_bldg"

  # if the zonning_req is "character" and not "data.frame", there were no zoning requirements recorded.
  # we return maybe with a warning
  if (class(zoning_req) == "character"){
    warning("No zoning requirements recorded for this district")
    return(TRUE)
  } else if (!constraint %in% zoning_req$constraint_name){
    return(TRUE)
  }

  # getting the value from the building's attributes
  # if the fl_area is not recorded
  if (!is.null(tidybuilding$width) & !is.null(tidybuilding$depth)){
    footprint <- tidybuilding$width * tidybuilding$depth
  } else{
    warning("building data missing width or depth")
    return("MAYBE")
  }

  value <- (footprint / (tidyparcel_dims$lot_area * 43560)) * 100

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
