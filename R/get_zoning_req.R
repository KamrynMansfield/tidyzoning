#' Get district zoning requirements
#'
#' Because many zoning requirements depend on the proposed building or the parcel of that zoning district, `the get_zoning_req()` function takes a tidybuilding, a tidyparcel, and a tidydistrict and outputs a data frame listing the set zoning requirements that those three objects would create.
#' If every value is NA, it could indicate that the building land use is not allowed in the zoning district.
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

  if (is.na(tidydistrict$lot_constraints)){
    lot_cons <- "[]"
  } else{
    lot_cons <- tidydistrict$lot_constraints
  }

  if (is.na(tidydistrict$structure_constraints)){
    structure_cons <- "[]"
  } else{
    structure_cons <- tidydistrict$structure_constraints
  }

  if (is.na(tidydistrict$other_constraints)){
    other_cons <- "[]"
  } else{
    other_cons <- tidydistrict$other_constraints
  }

  # make tidydistrit a nested list instead of sf object
  tidydistrict <- list(lot_constraints = fromJSON(lot_cons),
                       structure_constraints = fromJSON(structure_cons),
                       other_constraints = fromJSON(other_cons))

  # this will be used later to make sure the expressions are written correctly
  safe_parse <- purrr::possibly(parse, otherwise = NA)

  # What are the constraints that are recorded?
  # These three dfs will be appended and combined in the end

  zoning_constraints <- list()
  for (i in 1:length(tidydistrict)){
    if (length(tidydistrict[[i]]) > 0){
      constraints <- data.frame(constraint_name = names(tidydistrict[[i]]))

      constraint_name <- names(tidydistrict)[[i]]
      zoning_constraints[[constraint_name]] <- constraints
    }
  }

  if (nrow(bind_rows(zoning_constraints)) == 0){
    return("No zoning requirements recorded for this district")
  }

  # Name the building type
  bldg_type <- tidybuilding$bldg_info$type

  # establish the parcel variables that might be used in the equations
  lot_width <- tidyparcel_dims$lot_width[[1]] # this should be in ft
  lot_depth <- tidyparcel_dims$lot_depth[[1]] # this should be in ft
  lot_area <- tidyparcel_dims$lot_area[[1]] # this should be in acres
  lot_type <- ifelse(tidyparcel_dims$Parcel_label[[1]] == "regular corner parcel", "corner","regular")

  # establish the building variables that might be used in the equations
  bed_list <- c(units_0bed = 0,
                units_1bed = 1,
                units_2bed = 2,
                units_3bed = 3,
                units_4bed = 4)

  bedrooms <- NA
  added_tot_bed <- tidybuilding$unit_info |> mutate(tot_bed = bedrooms * qty)
  total_bedrooms <- sum(added_tot_bed$tot_bed)

  units_bed_type <- tidybuilding$unit_info |> mutate(bed_type = ifelse(bedrooms < 4,paste0("units_",bedrooms,"bed"), "units_4bed"))
  bed_type_totals <- units_bed_type |> group_by(bed_type) |> summarise(total = sum(qty))

  units_0bed <- ifelse("units_0bed" %in% bed_type_totals$bed_type, bed_type_totals$total[bed_type_totals$bed_type == "units_0bed"], 0)
  units_1bed <- ifelse("units_1bed" %in% bed_type_totals$bed_type, bed_type_totals$total[bed_type_totals$bed_type == "units_1bed"], 0)
  units_2bed <- ifelse("units_2bed" %in% bed_type_totals$bed_type, bed_type_totals$total[bed_type_totals$bed_type == "units_2bed"], 0)
  units_3bed <- ifelse("units_3bed" %in% bed_type_totals$bed_type, bed_type_totals$total[bed_type_totals$bed_type == "units_3bed"], 0)
  units_4bed <- ifelse("units_4bed" %in% bed_type_totals$bed_type, bed_type_totals$total[bed_type_totals$bed_type == "units_4bed"], 0)
  total_units <- ifelse(length(tidybuilding$unit_info$qty) > 0,sum(tidybuilding$unit_info$qty),NA)
  fl_area <- ifelse(length(tidybuilding$bldg_info$gross_fl_area) > 0,tidybuilding$bldg_info$gross_fl_area,NA)
  parking_covered <- sum(tidybuilding$parking_info$stalls[tidybuilding$parking_info$type == "covered"])
  parking_uncovered <- sum(tidybuilding$parking_info$stalls[tidybuilding$parking_info$type == "uncovered"])
  parking_enclosed <- sum(tidybuilding$parking_info$stalls[tidybuilding$parking_info$type == "enclosed"])
  parking_floors <- ifelse(length(tidybuilding$parking_info$level) > 0 ,length(unique(tidybuilding$parking_info$level)), 0)
  parking_bel_grade <- ifelse(nrow(tidybuilding$parking_info[tidybuilding$parking_info$level < 0,]) > 0, "yes","no")
  garage_entry <- ifelse(unique(tidybuilding$parking_info$entry[!is.na(tidybuilding$parking_info$entry)]) > 0,unique(tidybuilding$parking_info$entry[!is.na(tidybuilding$parking_info$entry)]),NA)
  height <- ifelse(length(tidybuilding$bldg_info$height) > 0, tidybuilding$bldg_info$height, NA)
  height_eave <- ifelse(length(tidybuilding$bldg_info$height_eave) > 0, tidybuilding$bldg_info$height_eave, NA)
  floors <- ifelse(length(tidybuilding$bldg_info$stories) > 0, tidybuilding$bldg_info$stories, NA)
  min_unit_size <- ifelse(length(tidybuilding$unit_info$fl_area) > 0, min(tidybuilding$unit_info$fl_area), NA)
  max_unit_size <- ifelse(length(tidybuilding$unit_info$fl_area) > 0, max(tidybuilding$unit_info$fl_area), NA)
  far <- tidybuilding$bldg_info$gross_fl_area / lot_area
  bldg_width <- ifelse(length(tidybuilding$bldg_info$width) > 0, tidybuilding$bldg_info$width, NA)
  bldg_depth <- ifelse(length(tidybuilding$bldg_info$depth) > 0, tidybuilding$bldg_info$depth, NA)
  level_units_table <- tidybuilding$unit_info |> group_by(level) |> summarise(units = sum(qty))
  units_floor1 <- ifelse(length(level_units_table$units[level_units_table$level == 1]) > 0,level_units_table$units[level_units_table$level == 1],0)
  units_floor2 <- ifelse(length(level_units_table$units[level_units_table$level == 2]) > 0,level_units_table$units[level_units_table$level == 2],0)
  units_floor3 <- ifelse(length(level_units_table$units[level_units_table$level == 3]) > 0,level_units_table$units[level_units_table$level == 3],0)

  # loop through each zoning regulation in the district
  warnings <- 0 # if tidyparcel == NULL, this will keep track of potential incorrect calculations
  for (k in 1:length(zoning_constraints)){
    constraints <- zoning_constraints[[k]]

    # starting empty lists that we will later add to the data frame
    min_vals <- list()
    max_vals <- list()
    units <- list()
    min_val_notes <- list()
    max_val_notes <- list()
    length(min_vals) <- nrow(constraints)
    length(max_vals) <- nrow(constraints)
    length(units) <- nrow(constraints)
    length(min_val_notes) <- nrow(constraints)
    length(max_val_notes) <- nrow(constraints)

    # listing constraints
    for (i in 1:nrow(constraints)){
      name <- names(zoning_constraints)[[k]]
      constraint_min_note <- NA
      constraint_max_note <- NA
      for (j in 1:length(tidydistrict[[name]][[i]])){
        use_name <- tidydistrict[[name]][[i]][[j]]$use_name
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
        units[[i]] <- NA
        min_val_notes[[i]] <- NA
        max_val_notes[[i]] <- NA
        next
      }
      constraint_info <- tidydistrict[[name]][[i]][[use_index]]

      # Looking at possible min values
      if (length(constraint_info$min_val[[1]]) == 1){ # this is just a one-line expression for the constraint
        constraint_min_val <- safe_parse(text = constraint_info$min_val[[1]]) |>
          eval()

        if (is.na(constraint_min_val)){
          min_vals[[i]] <- NA
          max_vals[[i]] <- NA
          units[[i]] <- NA
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
          units[[i]] <- NA
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
      units[[i]] <- constraint_info$unit
      min_val_notes[[i]] <- constraint_min_note
      max_val_notes[[i]] <- constraint_max_note

      if (is.na(constraint_min_val[[1]]) & is.na(constraint_max_val[[1]]) & !is.na(constraint_info$unit) & is.null(tidyparcel_dims)){
        warnings <- warnings + 1
      }

    }

    zoning_constraints[[k]]$min_value <- min_vals
    zoning_constraints[[k]]$max_value <- max_vals
    zoning_constraints[[k]]$unit <- units
    zoning_constraints[[k]]$min_val_note <- min_val_notes
    zoning_constraints[[k]]$max_val_note <- max_val_notes
  }


  if (warnings > 0){
    warning("Some values my be inaccurate because they rely on parcel dimmensions")
    return(bind_rows(zoning_constraints))
  } else{
    return(bind_rows(zoning_constraints))
  }

}
