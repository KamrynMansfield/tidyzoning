#' Compare building floor count and allowed floors
#'
#' `check_floors()` takes a tidybuilding and a tidydistrict to see if the district's zoning code allows the tidybuilding based on number of floors.
#'
#' @inheritParams add_setbacks
#'
#' @return
#' Returns TRUE or FALSE stating whether or not the building would be allowed in the district based on total floors.
#' @export
#'
check_floors <- function(tidybuilding, tidydistrict, tidyparcel = NULL){
  structure_constraints <- fromJSON(tidydistrict$structure_constraints)

  if (!is.null(tidybuilding$bldg_info$stories)){
    value <- tidybuilding$bldg_info$stories
  } else{
    return(TRUE)
  }

  zoning_req <- get_zoning_req(tidybuilding, tidydistrict, tidyparcel)

  if (class(zoning_req) == "character"){
    return(TRUE)
    warning("No zoning requirements recorded for this district")
  }

  if ("stories" %in% zoning_req$constraint_name){
    min_requirement <- zoning_req[zoning_req$constraint_name == "stories", "min_value"][[1]]
    max_requirement <- zoning_req[zoning_req$constraint_name == "stories", "max_value"][[1]]

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
  if (is.null(zoning_req[zoning_req$constraint_name == "stories", "min_val_note"][[1]])){
    min_val_either <- FALSE
  } else if(is.na(zoning_req[zoning_req$constraint_name == "stories", "min_val_note"][[1]])){
    min_val_either <- FALSE
  } else{
    min_val_either <- zoning_req[zoning_req$constraint_name == "stories", "min_val_note"][[1]] == "either"
  }

  # see if the minimum values are met
  if (min_val_either == TRUE){
    min_check <- min_check_1 + min_check_2 > 0
  } else{
    min_check <- min_check_1 + min_check_2 > 1
  }

  # assign 2 maximum values
  max_check_1 <- min(max_requirement) >= value
  max_check_2 <- max(max_requirement) >= value
  if (is.null(zoning_req[zoning_req$constraint_name == "stories", "max_val_note"][[1]])){
    max_val_either <- FALSE
  } else if(is.na(zoning_req[zoning_req$constraint_name == "stories", "max_val_note"][[1]])){
    max_val_either <- FALSE
  } else{
    max_val_either <- zoning_req[zoning_req$constraint_name == "stories", "max_val_note"][[1]] == "either"
  }

  # see if the maximum values are met
  if (max_val_either == TRUE){
    max_check <- max_check_1 + max_check_2 > 0
  } else{
    max_check <- max_check_1 + max_check_2 > 1
  }

  if (max_check == FALSE | min_check == FALSE){
    return(FALSE)
  } else if (max_check == TRUE & min_check == TRUE){
    return(TRUE)
  } else{
    explanation <- c()
    if (!is.na(zoning_req[zoning_req$constraint_name == "stories", "min_val_note"][[1]])){
      explanation <- c(explanation,zoning_req[zoning_req$constraint_name == "stories","min_val_note"][[1]])
    }
    if (!is.na(zoning_req[zoning_req$constraint_name == "stories", "max_val_note"][[1]])){
      explanation <- c(explanation,zoning_req[zoning_req$constraint_name == "stories","max_val_note"][[1]])
    }

    if (length(explanation) > 0){
      warning(explanation)
    }

    return("MAYBE")

  }


}
