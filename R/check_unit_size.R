#' Compare unit sizes and allowed unit sizes
#'
#' `check_unit_size()` takes a tidybuilding and a tidydistrict to see if the district's zoning code allows the tidybuilding based on unit sizes.
#'
#' @inheritParams check_far
#' @param building_json The file path or json string of the json representing the building.
#' @param zoning_req The data frame result from the `get_zoning_req()` function.
#'
#' @return
#' Returns TRUE, FALSE, or MAYBE stating whether or not the building would be allowed in the district based on the unit sizes.
#' @export
#'
check_unit_size <- function(tidybuilding, tidydistrict, tidyparcel_dims, building_json, zoning_req = NULL){
  # get the tidydistrict into a list format
  ####### structure_constraints <- fromJSON(tidydistrict$structure_constraints)

  safe_parse <- purrr::possibly(parse, otherwise = NA)

  unit_info <- get_unit_info(building_json)

  if (is.null(zoning_req)){
    zoning_req <- get_zoning_req(tidybuilding, tidydistrict, tidyparcel_dims)
  }

  if (class(zoning_req) == "character"){
    warning("No zoning requirements recorded for this district")
    return(TRUE)
  }

  # if floor areas aren't in tidybuilding, we return TRUE and give a warning.
  if (is.null(unit_info$fl_area)){
    warning("No unit sizes recorded in the tidybuilding")
    return(TRUE)
  }

  # if no bedrooms area recorded, give a warning, and assume 2 beds per unit.
  if (is.null(unit_info$bedrooms)){
    warning("No bedroom qty recorded in the tidybuilding. Results may be innacurate")
    unit_info$bedrooms <- 2
  }



  ## CHECKING AVERAGE UNIT SIZE REQUIREMENTS
  if (!"unit_size_avg" %in% zoning_req$constraint_name){
    min_unit_size_avg <- 0
    max_unit_size_avg <- 100000
  } else{
    min_unit_size_avg <- zoning_req[zoning_req$constraint_name == "unit_size_avg","min_value"][[1]]
    max_unit_size_avg <- zoning_req[zoning_req$constraint_name == "unit_size_avg","max_value"][[1]]

    if (is.na(min_unit_size_avg)){
      min_unit_size_avg <- 0
    }

    if (is.na(max_unit_size_avg)){
      max_unit_size_avg <- 100000
    }
  }

  unit_size_avg <- mean(unit_info$fl_area)

  if (length(min_unit_size_avg) > 1){
    min_unit_avg_check_1 <- unit_size_avg >= min_unit_size_avg[[1]]
    min_unit_avg_check_2 <- unit_size_avg >= min_unit_size_avg[[2]]

    if (min_unit_avg_check_1 == FALSE & min_unit_avg_check_2 == FALSE){
      min_unit_avg_check <- FALSE
    } else if(min_unit_avg_check_1 == TRUE & min_unit_avg_check_2 == TRUE){
      min_unit_avg_check <- TRUE
    } else{
      min_unit_avg_check <- "MAYBE"
    }

  } else{
    min_unit_avg_check <- unit_size_avg >= min_unit_size_avg
  }

  if (length(max_unit_size_avg) > 1){
    min_unit_avg_check_1 <- unit_size_avg <= max_unit_size_avg[[1]]
    min_unit_avg_check_2 <- unit_size_avg <= max_unit_size_avg[[2]]

    if (min_unit_avg_check_1 == FALSE & min_unit_avg_check_2 == FALSE){
      max_unit_avg_check <- FALSE
    } else if(min_unit_avg_check_1 == TRUE & min_unit_avg_check_2 == TRUE){
      max_unit_avg_check <- TRUE
    } else{
      max_unit_avg_check <- "MAYBE"
    }

  } else{
    max_unit_avg_check <- unit_size_avg <= max_unit_size_avg
  }


  if (min_unit_avg_check == FALSE | max_unit_avg_check == FALSE){
    unit_size_avg_check <- FALSE
  } else if (min_unit_avg_check == TRUE & max_unit_avg_check == TRUE){
    unit_size_avg_check <- TRUE
  } else{
    unit_size_avg_check <- "MAYBE"
  }




  ## CHECKING UNIT SIZE REQUIREMENTS


  # Name the building type
  bldg_type <- tidybuilding$type

  # establish the parcel variables that might be used in the equations
  lot_width <- tidyparcel_dims$lot_width[[1]] # this should be in ft
  lot_depth <- tidyparcel_dims$lot_depth[[1]] # this should be in ft
  lot_area <- tidyparcel_dims$lot_area[[1]] # this should be in acres
  lot_type <- ifelse(!is.null(tidyparcel_dims$lot_type), tidyparcel_dims$lot_type, NA)


  # establish the building variables that might be used in the equations
  bed_list <- c(units_0bed = 0,
                units_1bed = 1,
                units_2bed = 2,
                units_3bed = 3,
                units_4bed = 4)


  total_bedrooms <- tidybuilding$total_bedrooms
  units_0bed <- ifelse("units_0bed" %in% names(tidybuilding), tidybuilding$units_0bed, 0)
  units_1bed <- ifelse("units_1bed" %in% names(tidybuilding), tidybuilding$units_1bed, 0)
  units_2bed <- ifelse("units_2bed" %in% names(tidybuilding), tidybuilding$units_2bed, 0)
  units_3bed <- ifelse("units_3bed" %in% names(tidybuilding), tidybuilding$units_3bed, 0)
  units_4bed <- ifelse("units_4bed" %in% names(tidybuilding), tidybuilding$units_4bed, 0)
  total_units <- ifelse(!is.null(tidybuilding$total_units), tidybuilding$total_units, NA)
  fl_area <- ifelse(length(tidybuilding$gross_fl_area) > 0,tidybuilding$gross_fl_area,NA)
  parking_enclosed <- sum(tidybuilding$parking_info$stalls[tidybuilding$parking_info$type == "enclosed"])
  height <- ifelse(length(tidybuilding$height) > 0, tidybuilding$height, NA)
  height_eave <- ifelse(length(tidybuilding$height_eave) > 0, tidybuilding$height_eave, NA)
  floors <- ifelse(length(tidybuilding$stories) > 0, tidybuilding$stories, NA)
  min_unit_size <- ifelse(!is.null(tidybuilding$min_unit_size), tidybuilding$min_unit_size, NA)
  max_unit_size <- ifelse(!is.null(tidybuilding$max_unit_size), tidybuilding$max_unit_size, NA)
  far <- tidybuilding$gross_fl_area / lot_area
  bldg_width <- ifelse(length(tidybuilding$width) > 0, tidybuilding$width, NA)
  bldg_depth <- ifelse(length(tidybuilding$depth) > 0, tidybuilding$depth, NA)



  # getting a df with just the min and max of each unit type
  unit_info_df <- unit_info |>
    dplyr::group_by(bedrooms) |>
    dplyr::summarise(min = min(fl_area),
                     max = max(fl_area))

  # for those values that don't have requirements in ozfs
  unit_info_df$min_val <- 0
  unit_info_df$max_val <- 100000

  # check_to see if there is even a value for unit_size
  if (is.null(tidydistrict[[1,"unit_size"]])){
    constraint_info <- list()
  } else if (is.na(tidydistrict[[1,"unit_size"]])){
    constraint_info <- list()
  } else{
    constraint_list <- rjson::fromJSON(tidydistrict[[1,"unit_size"]])

    for (j in 1:length(constraint_list)){
      use_name <- constraint_list[[j]]$use_name
      if (bldg_type %in% use_name){
        use_index <- j
        break
      }else {
        use_index <- NA
      }
    }

    if (is.na(use_index)){
      # something here
    }

    constraint_info <- constraint_list[[use_index]]
  }


  # loop through each row and find the zoning requirement for each unit type in the ozfs data
  max_vals <- list()
  min_vals <- list()
  for (i in 1:nrow(unit_info_df)){


    if (!is.null(unit_info$bedrooms)){
      bedrooms <- unit_info_df$bedrooms[[i]]
    }


    min_max_values <- c("min_val","max_val")
    for (k in 1:length(min_max_values)){
      min_max_val <- min_max_values[[k]]

      # Looking at possible min/max values
      if (length(constraint_info[[min_max_val]][[1]]) == 1){ # this is just a one-line expression for the constraint
        unit_info_df[[min_max_val]] <- safe_parse(text = constraint_info[[min_max_val]][[1]]) |>
          eval()

        constraint_val <- NA

      } else if (length(constraint_info[[min_max_val]]) == 0){
        constraint_val <- NA

      } else{ # this indicates a rule of some kind
        # loop through each rule
        for (j in 1:length(constraint_info[[min_max_val]])){
          rule_items <- names(constraint_info[[min_max_val]][[j]])
          if ("logical_operator" %in% rule_items){
            if (constraint_info[[min_max_val]][[j]]$logical_operator == "AND"){
              logical_operator <- "&"
            } else{
              logical_operator <- "|"
            }
            if ("select" %in% rule_items){ # there is logical_op and there is a select
              conditions_value <- safe_parse(text = paste(constraint_info[[min_max_val]][[j]]$conditions,
                                                          collapse = logical_operator)) |>
                eval()

              if (is.na(conditions_value)){
                constraint_min_note <- "OZFS Error"
                break
              }

              expressions <- c()
              if ("expressions" %in% rule_items){
                for (l in 1:length(constraint_info[[min_max_val]][[j]]$expressions)){
                  expressions <- c(expressions, eval(parse(text = constraint_info[[min_max_val]][[j]]$expressions[l])))
                }
              } else{
                constraint_val <- "OZFS Error"
                break
              }

              if (!is.na(conditions_value == TRUE) & conditions_value == TRUE){
                if (constraint_info[[min_max_val]][[j]]$select == "min"){
                  constraint_val <- min(expressions)
                  break
                } else if (constraint_info[[min_max_val]][[j]]$select == "max"){
                  constraint_val <- max(expressions)
                  break
                } else if (constraint_info[[min_max_val]][[j]]$select == "either"){
                  constraint_val <- expressions
                  constraint_min_note <- "either"
                } else{
                  constraint_val <- expressions
                  if (is.null(constraint_info[[min_max_val]][[j]]$select_info)){
                    constraint_min_note <- "unique requirements not specified"
                    break
                  } else{
                    constraint_min_note <- constraint_info[[min_max_val]][[j]]$select_info
                    break
                  }
                }
              }
            } else{ # there is logical_op and just one expression at end
              constraint_val <- safe_parse(text = constraint_info[[min_max_val]][[j]]$expression) |>
                eval()

              if (is.na(constraint_val)){
                constraint_min_note <- "OZFS Error"
                break
              }
              break
            }
          } else{
            if ("select" %in% rule_items){ # not logical_op, but there is a select
              if ("conditions" %in% rule_items){ # There is still a condition before selecting
                # there is a condition and there is a select
                conditions_value <- safe_parse(text = constraint_info[[min_max_val]][[j]]$conditions) |>
                  eval()

                if (is.na(conditions_value)){
                  constraint_min_note <- "OZFS Error"
                  break
                }

                expressions <- c()
                if ("expressions" %in% rule_items){
                  for (l in 1:length(constraint_info[[min_max_val]][[j]]$expressions)){
                    expressions <- c(expressions, eval(parse(text = constraint_info[[min_max_val]][[j]]$expressions[l])))
                  }
                } else{
                  constraint_val <- "OZFS Error"
                  break
                }


                if (!is.na(conditions_value == TRUE) & conditions_value == TRUE){
                  if (constraint_info[[min_max_val]][[j]]$select == "min"){
                    constraint_val <- min(expressions)
                    break
                  } else if (constraint_info[[min_max_val]][[j]]$select == "max"){
                    constraint_val <- max(expressions)
                    break
                  } else if (constraint_info[[min_max_val]][[j]]$select == "either"){
                    constraint_val <- expressions
                    constraint_min_note <- "either"
                  } else {
                    constraint_val <- expressions
                    if (is.null(constraint_info[[min_max_val]][[j]]$select_info)){
                      constraint_min_note <- "unique requirements not specified"
                      break
                    } else{
                      constraint_min_note <- constraint_info[[min_max_val]][[j]]$select_info
                      break
                    }
                  }
                }
              } else{ # it is just a select with the expressions
                expressions <- c()
                if ("expressions" %in% rule_items){
                  for (l in 1:length(constraint_info[[min_max_val]][[j]]$expressions)){
                    expressions <- c(expressions, eval(parse(text = constraint_info[[min_max_val]][[j]]$expressions[l])))
                  }
                } else{
                  constraint_val <- "OZFS Error"
                  break
                }

                if (constraint_info[[min_max_val]][[j]]$select == "min"){
                  constraint_val <- min(expressions)
                  break
                } else if (constraint_info[[min_max_val]][[j]]$select == "max"){
                  constraint_val <- max(expressions)
                  break
                } else if (constraint_info[[min_max_val]][[j]]$select == "either"){
                  constraint_val <- expressions
                  constraint_min_note <- "either"
                } else {
                  constraint_val <- expressions
                  if (is.null(constraint_info[[min_max_val]][[j]]$select_info)){
                    constraint_min_note <- "unique requirements not specified"
                    break
                  } else{
                    constraint_min_note <- constraint_info[[min_max_val]][[j]]$select_info
                    break
                  }
                }
              }
            } else{ # no logical_op and no select
              conditions_value <- safe_parse(text = constraint_info[[min_max_val]][[j]]$conditions) |>
                eval()

              if (is.na(conditions_value)){
                constraint_min_note <- "OZFS Error"
                break
              }

              if (!is.na(conditions_value == TRUE) & conditions_value == TRUE){
                constraint_val <- safe_parse(text = constraint_info[[min_max_val]][[j]]$expression) |>
                  eval()

                if (is.na(constraint_val)){
                  constraint_min_note <- "OZFS Error"
                  break
                }

                break
              } else{
                if (j == length(constraint_info[[min_max_val]])){
                  constraint_val <- NA
                }
              }
            }
          }
        }
      }

      if (!is.na(constraint_val[[1]])){
        if (min_max_val == "min_val"){
          min_vals[[i]] <- constraint_val
        }
        if (min_max_val == "max_val"){
          max_vals[[i]] <- constraint_val
        }
      } else{
        if (min_max_val == "min_val"){
          min_vals[[i]] <- unit_info_df$min_val[[i]]
        }
        if (min_max_val == "max_val"){
          max_vals[[i]] <- unit_info_df$max_val[[i]]
        }
      }

    }

  }

  unit_info_df$min_val <- min_vals
  unit_info_df$max_val <- max_vals
  unit_info_df$min_ok <- NA
  unit_info_df$max_ok <- NA
  unit_info_df$permitted <- NA


  for (i in 1:nrow(unit_info_df)){
    min_val <- unit_info_df[i,"min"][[1]][[1]]
    max_val <- unit_info_df[i,"max"][[1]][[1]]
    min_req <- unit_info_df[i,"min_val"][[1]][[1]]
    max_req <- unit_info_df[i,"max_val"][[1]][[1]]

    if (length(min_req) > 1){
      min_min_check1 <- min_val >= min_req[[1]]
      min_min_check2 <- min_val >= min_req[[2]]
      max_min_check1 <- max_val >= min_req[[1]]
      max_min_check2 <- max_val >= min_req[[2]]

      sum_min_min_check <- min_min_check1 + min_min_check2
      sum_max_min_check <- max_min_check1 + max_min_check2

      sum_min_checks <- sum_min_min_check + sum_max_min_check

      if (sum_min_min_check == 0 | sum_max_min_check == 0){
        min_check <- FALSE
      } else if(sum_min_min_check == 2 | sum_max_min_check == 2){
        min_check <- TRUE
      } else{
        min_check <- "MAYBE"
      }

    } else{
      min_check <- min_val >= min_req & max_val >= min_req
    }

    if (length(max_req) > 1){
      min_min_check1 <- min_val <= max_req[[1]]
      min_min_check2 <- min_val <= max_req[[2]]
      max_min_check1 <- max_val <= max_req[[1]]
      max_min_check2 <- max_val <= max_req[[2]]

      sum_min_min_check <- min_min_check1 + min_min_check2
      sum_max_min_check <- max_min_check1 + max_min_check2

      sum_min_checks <- sum_min_min_check + sum_max_min_check

      if (sum_min_min_check == 0 | sum_max_min_check == 0){
        max_check <- FALSE
      } else if(sum_min_min_check == 2 | sum_max_min_check == 2){
        max_check <- TRUE
      } else{
        max_check <- "MAYBE"
      }

    } else{
      max_check <- min_val <= max_req & max_val <= max_req
    }


    if (min_check == FALSE | max_check == FALSE){
      permitted <- FALSE
    } else if(min_check == TRUE & max_check == TRUE){
      permitted <- TRUE
    } else{
      permitted <- "MAYBE"
    }


    unit_info_df$min_ok[[i]] <- min_check
    unit_info_df$max_ok[[i]] <- max_check
    unit_info_df$permitted[[i]] <- permitted

  }

  permitted_col <- unique(unit_info_df$permitted)

  if (FALSE %in% permitted_col){
    unit_size_check <- FALSE
  } else if ("MAYBE" %in% permitted_col){
    unit_size_check <- "MAYBE"
  } else{
    unit_size_check <- TRUE
  }


  ## COMBINE CHECKS TOGETHER
  if (unit_size_avg_check == FALSE | unit_size_check == FALSE){
    return(FALSE)
  } else if (unit_size_avg_check == TRUE & unit_size_check == TRUE){
    return(TRUE)
  } else{
    return("MAYBE")
  }



}
