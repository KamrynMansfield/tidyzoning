#' Compare lot size to zonign requirements
#'
#' @param tidybuilding A dataframe with fields to represent a building.
#' Created from the function [unify_tiduybuilding].
#' @param tidydistrict The tidydistrict corresponding to the tidyparcel.
#' A tidydistrict object is one row from a tidyzoning simple features object.
#' @param tidyparcel_dims A tidyparcel object is an simple features object
#' depicting each parcel and its its dimentions.
#' @param zoning_req The data frame result from the `get_zoning_req()` function.
#' If provided, the tidybuilding and tidydistrict need not be provided.
#'
#' @returns
#' @export
#'
#' @examples
check_lot_size <- function(tidybuilding = NULL,
                           tidydistrict = NULL,
                           tidyparcel_dims,
                           zoning_req = NULL){

  if (is.null(zoning_req)){
    zoning_req <- get_zoning_req(tidybuilding, tidydistrict, tidyparcel_dims)
  }

  constraint <- "lot_size"

  if (class(zoning_req) == "character"){
    warning("No zoning requirements recorded for this district")
    return(TRUE)
  } else if (!constraint %in% zoning_req$constraint_name){
    return(TRUE)
  }


  if (is.na(tidyparcel_dims$lot_area)){
    warning("missing lot_area in parcel data")
    return("MAYBE")
  } else if (!is.null(tidyparcel_dims$lot_area)){
    value <- tidyparcel_dims$lot_area
  }else{
    warning("missing lot_area in parcel data")
    return("MAYBE")
  }


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
