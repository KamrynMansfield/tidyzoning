#' Compare building floor area and allowed floor area
#'
#' `check_fl_area()` states whether a building is allowed on a parcel based
#' on its floor area. It returns TRUE, FALSE, or "MAYBE"
#'
#' @inheritParams check_far
#'
#' @return
#' Returns "TRUE", "FALSE", or "MAYBE". "MAYBE" indicates a complex condition
#' the OZFS is currently not able to support.
#' @export
#'
check_fl_area <- function(vars, zoning_req){

  # if the zonning_req is "character" and not "data.frame", there were no zoning requirements recorded.
  # we return maybe with a warning
  if (class(zoning_req) == "character"){
    warning("No zoning requirements recorded for this district")
    return(TRUE)
  }


  # list of the fields that this function will check
  constraint_df <- zoning_req[zoning_req$constraint_name %in% c("fl_area",
                                                              "fl_area_first",
                                                              "fl_area_top"), ]

  if (nrow(constraint_df) == 0){
    return(TRUE)
  }

  values <- c(fl_area = vars$fl_area,
              fl_area_first = vars$fl_area_first,
              fl_area_top = vars$fl_area_top)


  allowed <- c()
  warnings <- c()
  for (i in 1:nrow(constraint_df)){

    constraint <- constraint_df[i,1][[1]]
    value <- values[constraint][[1]]

    if (is.na(value)){
      warning("improper building data")
      return("MAYBE")
    }


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


  if (length(allowed[allowed == FALSE]) == nrow(constraint_df)){
    return(FALSE)
  } else if (length(allowed[allowed == TRUE]) == nrow(constraint_df)){
    return(TRUE)
  } else if ("MAYBE" %in% allowed){
    warning(paste(warnings, collapse = ", "))
    return("MAYBE")
  } else{
    return(FALSE)
  }


}

