#' List district zoning requirement values
#'
#' Because many zoning requirements depend on the proposed building
#' or the parcel of that zoning district, `the get_zoning_req()` function
#' takes a tidybuilding, a tidyparcel, and a tidydistrict and outputs a data
#' frame listing the set zoning requirements that those three objects would create.
#' If every value is NA, it could indicate that the building
#' land use is not allowed in the zoning district.
#'
#' @inheritParams add_setbacks
#'
#' @return
#' Returns a data frame with the value each zoning requirement for that specific building, parcel, and zoning district.
#' NA values indicate the requirement isn't recorded in that district.
#' If every value is NA, it could indicate that the building land use is not allowed in the zoning district.
#' @export
#'
get_zoning_req <- function(tidybuilding, tidydistrict, tidyparcel_dims){

  # this will be used later to make sure the expressions are written correctly
  safe_parse <- purrr::possibly(parse, otherwise = NA)

  tidydistrict_con <- tidydistrict |>
    sf::st_drop_geometry() |>
    dplyr::select(!c("dist_name","dist_abbr","planned_dev","overlay","uses_permitted"))

  constraints <- data.frame(constraint_name = names(tidydistrict_con))

  if (nrow(constraints) == 0){
    return("No zoning requirements recorded for this district")
  }

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

  bedrooms <- NA
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


  # starting empty lists that we will later add to the data frame
  min_vals <- list()
  max_vals <- list()
  min_val_notes <- list()
  max_val_notes <- list()
  length(min_vals) <- nrow(constraints)
  length(max_vals) <- nrow(constraints)
  length(min_val_notes) <- nrow(constraints)
  length(max_val_notes) <- nrow(constraints)

  # loop through each zoning regulation in the district
  for (i in 1:nrow(constraints)){
    name <- constraints$constraint_name[[i]]
    if (is.na(tidydistrict[[1,name]])){
      min_vals[[i]] <- NA
      max_vals[[i]] <- NA
      min_val_notes[[i]] <- NA
      max_val_notes[[i]] <- NA
      next
    }
    constraint_list <- rjson::fromJSON(tidydistrict[[1,name]])
    constraint_min_note <- NA
    constraint_max_note <- NA
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
      min_vals[[i]] <- NA
      max_vals[[i]] <- NA
      min_val_notes[[i]] <- NA
      max_val_notes[[i]] <- NA
      next
    }
    constraint_info <- constraint_list[[use_index]]

    # Looking at possible min values
    if (length(constraint_info$min_val[[1]]) == 1){ # this is just a one-line expression for the constraint
      constraint_min_val <- safe_parse(text = constraint_info$min_val[[1]]) |>
        eval()

      if (is.na(constraint_min_val)){
        min_vals[[i]] <- NA
        max_vals[[i]] <- NA
        min_val_notes[[i]] <- "OZFS error"
        max_val_notes[[i]] <- NA
        next
      }

    } else if (length(constraint_info$min_val) == 0){
      constraint_min_val <- NA

    } else{ # this indicates a rule of some kind
      # loop through each rule
      for (j in 1:length(constraint_info$min_val)){
        rule_items <- names(constraint_info$min_val[[j]])
        if ("logical_operator" %in% rule_items){
          if (constraint_info$min_val[[j]]$logical_operator == "AND"){
            logical_operator <- "&"
          } else{
            logical_operator <- "|"
          }
          if ("select" %in% rule_items){ # there is logical_op and there is a select
            conditions_value <- safe_parse(text = paste(constraint_info$min_val[[j]]$conditions,
                                                        collapse = logical_operator)) |>
              eval()

            if (is.na(conditions_value)){
              constraint_min_note <- "OZFS Error"
              break
            }

            expressions <- c()
            if ("expressions" %in% rule_items){
              for (l in 1:length(constraint_info$min_val[[j]]$expressions)){
                expressions <- c(expressions, eval(parse(text = constraint_info$min_val[[j]]$expressions[l])))
              }
              expressions <- c(min(expressions), max(expressions))
            } else{
              constraint_min_val <- NA
              constraint_min_note <- "OZFS Error"
              break
            }

            if (!is.na(conditions_value == TRUE) & conditions_value == TRUE){
              if (constraint_info$min_val[[j]]$select == "min"){
                constraint_min_val <- min(expressions)
                break
              } else if (constraint_info$min_val[[j]]$select == "max"){
                constraint_min_val <- max(expressions)
                break
              } else if (constraint_info$min_val[[j]]$select == "either"){
                constraint_min_val <- expressions
                constraint_min_note <- "either"
              } else{
                constraint_min_val <- expressions
                if (is.null(constraint_info$min_val[[j]]$select_info)){
                  constraint_min_note <- "unique requirements not specified"
                  break
                } else{
                  constraint_min_note <- constraint_info$min_val[[j]]$select_info
                  break
                }
              }
            }
          } else{ # there is logical_op and just one expression at end
            constraint_min_val <- safe_parse(text = constraint_info$min_val[[j]]$expression) |>
              eval()

            if (is.na(constraint_min_val)){
              constraint_min_note <- "OZFS Error"
              break
            }
            break
          }
        } else{
          if ("select" %in% rule_items){ # not logical_op, but there is a select
            if ("conditions" %in% rule_items){ # There is still a condition before selecting
              # there is a condition and there is a select
              conditions_value <- safe_parse(text = constraint_info$min_val[[j]]$conditions) |>
                eval()

              if (is.na(conditions_value)){
                constraint_min_note <- "OZFS Error"
                break
              }

              expressions <- c()
              if ("expressions" %in% rule_items){
                for (l in 1:length(constraint_info$min_val[[j]]$expressions)){
                  expressions <- c(expressions, eval(parse(text = constraint_info$min_val[[j]]$expressions[l])))
                }
                expressions <- c(min(expressions), max(expressions))
              } else{
                constraint_min_val <- NA
                constraint_min_note <- "OZFS Error"
                break
              }


              if (!is.na(conditions_value == TRUE) & conditions_value == TRUE){
                if (constraint_info$min_val[[j]]$select == "min"){
                  constraint_min_val <- min(expressions)
                  break
                } else if (constraint_info$min_val[[j]]$select == "max"){
                  constraint_min_val <- max(expressions)
                  break
                } else if (constraint_info$min_val[[j]]$select == "either"){
                  constraint_min_val <- expressions
                  constraint_min_note <- "either"
                } else {
                  constraint_min_val <- expressions
                  if (is.null(constraint_info$min_val[[j]]$select_info)){
                    constraint_min_note <- "unique requirements not specified"
                    break
                  } else{
                    constraint_min_note <- constraint_info$min_val[[j]]$select_info
                    break
                  }
                }
              }
            } else{ # it is just a select with the expressions
              expressions <- c()
              if ("expressions" %in% rule_items){
                for (l in 1:length(constraint_info$min_val[[j]]$expressions)){
                  expressions <- c(expressions, eval(parse(text = constraint_info$min_val[[j]]$expressions[l])))
                }
                expressions <- c(min(expressions), max(expressions))
              } else{
                constraint_min_val <- NA
                constraint_min_note <- "OZFS Error"
                break
              }

              if (constraint_info$min_val[[j]]$select == "min"){
                constraint_min_val <- min(expressions)
                break
              } else if (constraint_info$min_val[[j]]$select == "max"){
                constraint_min_val <- max(expressions)
                break
              } else if (constraint_info$min_val[[j]]$select == "either"){
                constraint_min_val <- expressions
                constraint_min_note <- "either"
              } else {
                constraint_min_val <- expressions
                if (is.null(constraint_info$min_val[[j]]$select_info)){
                  constraint_min_note <- "unique requirements not specified"
                  break
                } else{
                  constraint_min_note <- constraint_info$min_val[[j]]$select_info
                  break
                }
              }
            }
          } else{ # no logical_op and no select
            conditions_value <- safe_parse(text = constraint_info$min_val[[j]]$conditions) |>
              eval()

            if (is.na(conditions_value)){
              constraint_min_note <- "OZFS Error"
              break
            }

            if (!is.na(conditions_value == TRUE) & conditions_value == TRUE){
              constraint_min_val <- safe_parse(text = constraint_info$min_val[[j]]$expression) |>
                eval()

              if (is.na(constraint_min_val)){
                constraint_min_note <- "OZFS Error"
                break
              }

              break
            } else{
              if (j == length(tidydistrict[[name]][[i]])){
                constraint_min_val <- NA
              }
            }
          }
        }

      }
    }

    # Looking at possible max values
    if (length(constraint_info$max_val[[1]]) == 1){ # this is just a one-line expression for the constraint
      constraint_max_val <- safe_parse(text = constraint_info$max_val[[1]]) |>
        eval()

      if (is.na(constraint_max_val)){
        min_vals[[i]] <- NA
        max_vals[[i]] <- NA
        min_val_notes[[i]] <- NA
        max_val_notes[[i]] <- "OZFS error"
        next
      }

    } else if (length(constraint_info$max_val) == 0){
      constraint_max_val <- NA
    } else{ # this indicates a rule of some kind
      # loop through each rule
      for (j in 1:length(constraint_info$max_val)){
        rule_items <- names(constraint_info$max_val[[j]])
        if ("logical_operator" %in% rule_items){
          if (constraint_info$max_val[[j]]$logical_operator == "AND"){
            logical_operator <- "&"
          } else{
            logical_operator <- "|"
          }
          if ("select" %in% rule_items){ # there is logical_op and there is a select
            conditions_value <- safe_parse(text = paste(constraint_info$max_val[[j]]$conditions,
                                                        collapse = logical_operator)) |>
              eval()

            if (is.na(conditions_value)){
              constraint_max_note <- "OZFS Error"
              break
            }

            expressions <- c()
            if ("expressions" %in% rule_items){
              for (l in 1:length(constraint_info$max_val[[j]]$expressions)){
                expressions <- c(expressions, eval(parse(text = constraint_info$max_val[[j]]$expressions[l])))
              }
              expressions <- c(min(expressions), max(expressions))
            } else{
              constraint_max_val <- NA
              constraint_max_note <- "OZFS Error"
              break
            }

            if (!is.na(conditions_value == TRUE) & conditions_value == TRUE){
              if (constraint_info$max_val[[j]]$select == "min"){
                constraint_max_val <- min(expressions)
                break
              } else if (constraint_info$max_val[[j]]$select == "max"){
                constraint_max_val <- max(expressions)
                break
              } else if (constraint_info$max_val[[j]]$select == "either"){
                constraint_max_val <- expressions
                constraint_max_note <- "either"
              } else {
                constraint_max_val <- expressions
                if (is.null(constraint_info$max_val[[j]]$select_info)){
                  constraint_max_note <- "unique requirements not specified"
                  break
                } else{
                  constraint_max_note <- constraint_info$max_val[[j]]$select_info
                  break
                }
              }
            }
          } else{ # there is logical_op and just one expression at end
            constraint_max_val <- safe_parse(text = constraint_info$max_val[[j]]$expression) |>
              eval()

            if (is.na(constraint_max_val)){
              constraint_max_note <- "OZFS Error"
              break
            }
            break
          }
        } else{
          if ("select" %in% rule_items){ # not logical_op, but there is a select
            if ("conditions" %in% rule_items){ # There is still a condition before selecting
              # there is logical_op and there is a select
              conditions_value <- safe_parse(text = constraint_info$max_val[[j]]$conditions) |>
                eval()

              if (is.na(conditions_value)){
                constraint_max_note <- "OZFS Error"
                break
              }

              expressions <- c()
              if ("expressions" %in% rule_items){
                for (l in 1:length(constraint_info$max_val[[j]]$expressions)){
                  expressions <- c(expressions, eval(parse(text = constraint_info$max_val[[j]]$expressions[l])))
                }
                expressions <- c(min(expressions), max(expressions))
              } else{
                constraint_max_val <- NA
                constraint_max_note <- "OZFS Error"
                break
              }

              if (!is.na(conditions_value == TRUE) & conditions_value == TRUE){
                if (constraint_info$max_val[[j]]$select == "min"){
                  constraint_max_val <- min(expressions)
                  break
                } else if (constraint_info$max_val[[j]]$select == "max"){
                  constraint_max_val <- max(expressions)
                  break
                } else if (constraint_info$max_val[[j]]$select == "either"){
                  constraint_max_val <- expressions
                  constraint_max_note <- "either"
                } else {
                  constraint_max_val <- expressions
                  if (is.null(constraint_info$max_val[[j]]$select_info)){
                    constraint_max_note <- "unique requirements not specified"
                    break
                  } else{
                    constraint_max_note <- constraint_info$max_val[[j]]$select_info
                    break
                  }
                }
              }
            } else{ # it is just a select with the expressions
              expressions <- c()
              if ("expressions" %in% rule_items){
                for (l in 1:length(constraint_info$max_val[[j]]$expressions)){
                  expressions <- c(expressions, eval(parse(text = constraint_info$max_val[[j]]$expressions[l])))
                }
                expressions <- c(min(expressions), max(expressions))
              } else{
                constraint_max_val <- NA
                constraint_max_note <- "OZFS Error"
                break
              }

              if (constraint_info$max_val[[j]]$select == "min"){
                constraint_max_val <- min(expressions)
                break
              } else if (constraint_info$max_val[[j]]$select == "max"){
                constraint_max_val <- max(expressions)
                break
              } else if (constraint_info$max_val[[j]]$select == "either"){
                constraint_max_val <- expressions
                constraint_max_note <- "either"
              } else {
                constraint_max_val <- expressions
                if (is.null(constraint_info$max_val[[j]]$select_info)){
                  constraint_max_note <- "unique requirements not specified"
                  break
                } else{
                  constraint_max_note <- constraint_info$max_val[[j]]$select_info
                  break
                }
              }
            }
          } else{ # no logical_op and no select
            conditions_value <- safe_parse(text = constraint_info$max_val[[j]]$conditions) |>
              eval()

            if (is.na(conditions_value)){
              constraint_max_note <- "OZFS Error"
              break
            }

            if (!is.na(conditions_value == TRUE) & conditions_value == TRUE){
              constraint_max_val <- safe_parse(text = constraint_info$max_val[[j]]$expression) |>
                eval()

              if (is.na(constraint_max_val)){
                constraint_max_note <- "OZFS Error"
                break
              }
              break
            }
          }
        }

      }
    }

    min_vals[[i]] <- constraint_min_val
    max_vals[[i]] <- constraint_max_val
    min_val_notes[[i]] <- constraint_min_note
    max_val_notes[[i]] <- constraint_max_note

  }

  constraints$min_value <- min_vals
  constraints$max_value <- max_vals
  constraints$min_val_note <- min_val_notes
  constraints$max_val_note <- max_val_notes

  return(constraints)
}
