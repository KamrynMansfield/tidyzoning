get_zoning_req <- function(tidybuilding, tidyparcel, tidydistrict){
  # make tidydistrit a nested list instead of sf object
  tidydistrict <- list(lot_constraints = fromJSON(tidydistrict$lot_constraints),
                       structure_constraints = fromJSON(tidydistrict$structure_constraints),
                       other_constraints = fromJSON(tidydistrict$other_constraints))

  # What are the constraints that are recorded?
  # These three dfs will be appended and combined in the end
  if (length(tidydistrict$lot_constraints) > 0){
    lot_constraints <- data.frame(constraint_name = names(tidydistrict$lot_constraints),
                                  min_value = NA,
                                  max_value = NA)
  } else{
    lot_constraints <- data.frame()
  }
  if (length(tidydistrict$structure_constraints) > 0){
    structure_constraints <- data.frame(constraint_name = names(tidydistrict$structure_constraints),
                                        min_value = NA,
                                        max_value = NA)
  } else{
    structure_constraints <- data.frame()
  }
  if (length(tidydistrict$other_constraints) > 0){
    other_constraints <- data.frame(constraint_name =names(tidydistrict$other_constraints),
                                    min_value = NA,
                                    max_value = NA)
  } else{
    other_constraints <- data.frame()
  }

  if (nrow(rbind(lot_constraints, structure_constraints, other_constraints)) == 0){
    return("No zoning requirements recorded for this district")
  }

  # Name the building type
  bldg_type <- find_bldg_type(tidybuilding)

  # establish the parcel variables that might be used in the equations
  front_of_parcel <- tidyparcel |>
    filter(side == "front")
  parcel_without_centroid <- tidyparcel[!is.na(tidyparcel$side),]

  lot_width <- st_length(front_of_parcel) * 3.28084 # converting to ft
  units(lot_width) <- "ft"
  lot_area <- st_polygonize(st_union(parcel_without_centroid)) |> st_area() * 10.7639 # converting to ft
  units(lot_area) <- "ft^2"
  # establish the building variables that might be used in the equations
  bed_list <- c(units_0bed = 0,
                units_1bed = 1,
                units_2bed = 2,
                units_3bed = 3,
                units_4bed = 4)

  bedrooms <- min(bed_list[names(tidybuilding)], na.rm = T)
  units_0bed <- ifelse(length(tidybuilding$units_0bed) > 0, tidybuilding$units_0bed, 0)
  units_1bed <- ifelse(length(tidybuilding$units_1bed) > 0, tidybuilding$units_0bed, 0)
  units_2bed <- ifelse(length(tidybuilding$units_2bed) > 0, tidybuilding$units_0bed, 0)
  units_3bed <- ifelse(length(tidybuilding$units_3bed) > 0, tidybuilding$units_3bed, 0)
  units_4bed <- ifelse(length(tidybuilding$units_4bed) > 0, tidybuilding$units_0bed, 0)
  total_units <- units_0bed + units_1bed + units_2bed + units_3bed + units_4bed
  fl_area <- ifelse(length(tidybuilding$floor_area) > 0, tidybuilding$floor_area, NA)
  parking <- ifelse(length(tidybuilding$parking) > 0, tidybuilding$parking, NA)
  height <- ifelse(length(tidybuilding$height) > 0, tidybuilding$height, NA)
  floors <- ifelse(length(tidybuilding$floors) > 0, tidybuilding$floors, NA)
  min_unit_size <- ifelse(length(tidybuilding$min_unit_size) > 0, tidybuilding$min_unit_size, NA)
  max_unit_size <- ifelse(length(tidybuilding$max_unit_size) > 0, tidybuilding$max_unit_size, NA)

  # loop through each zoning regulation in the district

  # listing lot constraints
  for (i in 1:nrow(lot_constraints)){
    for (j in 1:length(tidydistrict$lot_constraints[[i]])){
      use_name <- tidydistrict$lot_constraints[[i]][[j]]$use_name
      if (bldg_type %in% use_name){
        use_index <- j
      }else {
        use_index <- NA
      }
    }
    if (is.na(use_index)){
      break
    }
    constraint_info <- tidydistrict$lot_constraints[[i]][[use_index]]

    # Looking at possible min values
    if (length(constraint_info$min_val[[1]]) == 1){ # this is just a one-line expression for the constraint
      constraint_min_val <- parse(text = constraint_info$min_val[[1]]) |>
        eval()

    } else if (length(constraint_info$min_val) == 0){
      constraint_min_val <- NA

    } else{ # this indicates a rule of some kind
      # loop through each rule
      for (j in 1:length(constraint_info$min_val)){
        rule_items <- names(constraint_info$min_val[[j]])
        if ("logical_operator" %in% rule_items ){
          if (constraint_info$min_val[[j]]$logical_operator == "AND"){
            logical_operator <- "&"
          } else{
            logical_operator <- "|"
          }
          if ("select" %in% rule_items){ # there is logical_op and there is a select
            conditions_value <- parse(text = paste(constraint_info$min_val[[j]]$conditions,
                                                   collapse = logical_operator)) |>
              eval()

            expressions <- c()
            for (i in 1:length(constraint_info$min_val[[j]]$expressions)){
              expressions <- c(expressions, eval(parse(text = constraint_info$min_val[[j]]$expressions[i])))
            }
            if (conditions_value == TRUE){
              if (constraint_info$min_val[[j]]$select == "min"){
                constraint_min_val <- min(expressions)
                break
              } else{
                constraint_min_val <- max(expressions)
                break
              }
            }
          } else{ # there is logical_op and just one expression at end
            constraint_min_val <- parse(text = constraint_info$min_val[[j]]$expression) |>
              eval()
            break
          }
        } else{
          if ("select" %in% rule_items){ # not logical_op, but there is a select
            if ("conditions" %in% rule_items){ # There is still a condition before selecting
              # there is logical_op and there is a select
              conditions_value <- parse(text = constraint_info$min_val[[j]]$conditions) |>
                eval()

              expressions <- c()
              for (i in 1:length(constraint_info$min_val[[j]]$expressions)){
                expressions <- c(expressions, eval(parse(text = constraint_info$min_val[[j]]$expressions[i])))
              }

              if (conditions_value == TRUE){
                if (constraint_info$min_val[[j]]$select == "min"){
                  constraint_min_val <- min(expressions)
                  break
                } else{
                  constraint_min_val <- max(expressions)
                  break
                }
              }
            } else{ # it is just a select with the expressions
              expressions <- c()
              for (i in 1:length(constraint_info$min_val[[j]]$expressions)){
                expressions <- c(expressions, eval(parse(text = constraint_info$min_val[[j]]$expressions[i])))
              }
              if (constraint_info$min_val[[j]]$select == "min"){
                constraint_min_val <- min(expressions)
                break
              } else{
                constraint_min_val <- max(expressions)
                break
              }
            }
          } else{ # no logical_op and no select
            conditions_value <- parse(text = constraint_info$min_val[[j]]$conditions) |>
              eval()
            if (conditions_value == TRUE){
              constraint_min_val <- parse(text = constraint_info$min_val[[j]]$expression) |>
                eval()
              break
            }
          }
        }

      }
    }

    # Looking at possible max values
    if (length(constraint_info$max_val[[1]]) == 1){ # this is just a one-line expression for the constraint
      constraint_max_val <- parse(text = constraint_info$max_val[[1]]) |>
        eval()
    } else if (length(constraint_info$max_val) == 0){
      constraint_max_val <- NA
    } else{ # this indicates a rule of some kind
      # loop through each rule
      for (j in 1:length(constraint_info$max_val)){
        rule_items <- names(constraint_info$max_val[[j]])
        if ("logical_operator" %in% rule_items ){
          if (constraint_info$max_val[[j]]$logical_operator == "AND"){
            logical_operator <- "&"
          } else{
            logical_operator <- "|"
          }
          if ("select" %in% rule_items){ # there is logical_op and there is a select
            conditions_value <- parse(text = paste(constraint_info$max_val[[j]]$conditions,
                                                   collapse = logical_operator)) |>
              eval()

            expressions <- c()
            for (i in 1:length(constraint_info$max_val[[j]]$expressions)){
              expressions <- c(expressions, eval(parse(text = constraint_info$max_val[[j]]$expressions[i])))
            }
            if (conditions_value == TRUE){
              if (constraint_info$max_val[[j]]$select == "min"){
                constraint_max_val <- min(expressions)
                break
              } else{
                constraint_max_val <- max(expressions)
                break
              }
            }
          } else{ # there is logical_op and just one expression at end
            constraint_max_val <- parse(text = constraint_info$max_val[[j]]$expression) |>
              eval()
            break
          }
        } else{
          if ("select" %in% rule_items){ # not logical_op, but there is a select
            if ("conditions" %in% rule_items){ # There is still a condition before selecting
              # there is logical_op and there is a select
              conditions_value <- parse(text = constraint_info$max_val[[j]]$conditions) |>
                eval()

              expressions <- c()
              for (i in 1:length(constraint_info$max_val[[j]]$expressions)){
                expressions <- c(expressions, eval(parse(text = constraint_info$max_val[[j]]$expressions[i])))
              }

              if (conditions_value == TRUE){
                if (constraint_info$max_val[[j]]$select == "min"){
                  constraint_max_val <- min(expressions)
                  break
                } else{
                  constraint_max_val <- max(expressions)
                  break
                }
              }
            } else{ # it is just a select with the expressions
              expressions <- c()
              for (i in 1:length(constraint_info$max_val[[j]]$expressions)){
                expressions <- c(expressions, eval(parse(text = constraint_info$max_val[[j]]$expressions[i])))
              }
              if (constraint_info$max_val[[j]]$select == "min"){
                constraint_max_val <- min(expressions)
                break
              } else{
                constraint_max_val <- max(expressions)
                break
              }
            }
          } else{ # no logical_op and no select
            conditions_value <- parse(text = constraint_info$max_val[[j]]$conditions) |>
              eval()
            if (conditions_value == TRUE){
              constraint_max_val <- parse(text = constraint_info$max_val[[j]]$expression) |>
                eval()
              break
            }
          }
        }

      }
    }

    lot_constraints[i,"min_value"] <- constraint_min_val
    lot_constraints[i,"max_value"] <- constraint_max_val
    lot_constraints[i, "units"] <- constraint_info$unit
  }



  # listing structure constraints
  for (i in 1:nrow(structure_constraints)){
    for (j in 1:length(tidydistrict$structure_constraints[[i]])){
      use_name <- tidydistrict$structure_constraints[[i]][[j]]$use_name
      if (bldg_type %in% use_name){
        use_index <- j
      } else  {
        use_index <- NA
      }
    }
    if (is.na(use_index)){
      break
    }

    constraint_info <- tidydistrict$structure_constraints[[i]][[use_index]]

    # Looking at possible min values
    if (length(constraint_info$min_val[[1]]) == 1){ # this is just a one-line expression for the constraint
      constraint_min_val <- parse(text = constraint_info$min_val[[1]]) |>
        eval()

    } else if (length(constraint_info$min_val) == 0){
      constraint_min_val <- NA

    } else{ # this indicates a rule of some kind
      # loop through each rule
      for (j in 1:length(constraint_info$min_val)){
        rule_items <- names(constraint_info$min_val[[j]])
        if ("logical_operator" %in% rule_items ){
          if (constraint_info$min_val[[j]]$logical_operator == "AND"){
            logical_operator <- "&"
          } else{
            logical_operator <- "|"
          }
          if ("select" %in% rule_items){ # there is logical_op and there is a select
            conditions_value <- parse(text = paste(constraint_info$min_val[[j]]$conditions,
                                                   collapse = logical_operator)) |>
              eval()

            expressions <- c()
            for (i in 1:length(constraint_info$min_val[[j]]$expressions)){
              expressions <- c(expressions, eval(parse(text = constraint_info$min_val[[j]]$expressions[i])))
            }
            if (conditions_value == TRUE){
              if (constraint_info$min_val[[j]]$select == "min"){
                constraint_min_val <- min(expressions)
                break
              } else{
                constraint_min_val <- max(expressions)
                break
              }
            }
          } else{ # there is logical_op and just one expression at end
            constraint_min_val <- parse(text = constraint_info$min_val[[j]]$expression) |>
              eval()
            break
          }
        } else{
          if ("select" %in% rule_items){ # not logical_op, but there is a select
            if ("conditions" %in% rule_items){ # There is still a condition before selecting
              # there is logical_op and there is a select
              conditions_value <- parse(text = constraint_info$min_val[[j]]$conditions) |>
                eval()

              expressions <- c()
              for (i in 1:length(constraint_info$min_val[[j]]$expressions)){
                expressions <- c(expressions, eval(parse(text = constraint_info$min_val[[j]]$expressions[i])))
              }

              if (conditions_value == TRUE){
                if (constraint_info$min_val[[j]]$select == "min"){
                  constraint_min_val <- min(expressions)
                  break
                } else{
                  constraint_min_val <- max(expressions)
                  break
                }
              }
            } else{ # it is just a select with the expressions
              expressions <- c()
              for (i in 1:length(constraint_info$min_val[[j]]$expressions)){
                expressions <- c(expressions, eval(parse(text = constraint_info$min_val[[j]]$expressions[i])))
              }
              if (constraint_info$min_val[[j]]$select == "min"){
                constraint_min_val <- min(expressions)
                break
              } else{
                constraint_min_val <- max(expressions)
                break
              }
            }
          } else{ # no logical_op and no select
            conditions_value <- parse(text = constraint_info$min_val[[j]]$conditions) |>
              eval()
            if (conditions_value == TRUE){
              constraint_min_val <- parse(text = constraint_info$min_val[[j]]$expression) |>
                eval()
              break
            }
          }
        }

      }
    }

    # Looking at possible max values
    if (length(constraint_info$max_val[[1]]) == 1){ # this is just a one-line expression for the constraint
      constraint_max_val <- parse(text = constraint_info$max_val[[1]]) |>
        eval()

    } else if (length(constraint_info$max_val) == 0){
      constraint_max_val <- NA

    } else{ # this indicates a rule of some kind
      # loop through each rule
      for (j in 1:length(constraint_info$max_val)){
        rule_items <- names(constraint_info$max_val[[j]])
        if ("logical_operator" %in% rule_items ){
          if (constraint_info$max_val[[j]]$logical_operator == "AND"){
            logical_operator <- "&"
          } else{
            logical_operator <- "|"
          }
          if ("select" %in% rule_items){ # there is logical_op and there is a select
            conditions_value <- parse(text = paste(constraint_info$max_val[[j]]$conditions,
                                                   collapse = logical_operator)) |>
              eval()

            expressions <- c()
            for (i in 1:length(constraint_info$max_val[[j]]$expressions)){
              expressions <- c(expressions, eval(parse(text = constraint_info$max_val[[j]]$expressions[i])))
            }
            if (conditions_value == TRUE){
              if (constraint_info$max_val[[j]]$select == "min"){
                constraint_max_val <- min(expressions)
                break
              } else{
                constraint_max_val <- max(expressions)
                break
              }
            }
          } else{ # there is logical_op and just one expression at end
            constraint_max_val <- parse(text = constraint_info$max_val[[j]]$expression) |>
              eval()
            break
          }
        } else{
          if ("select" %in% rule_items){ # not logical_op, but there is a select
            if ("conditions" %in% rule_items){ # There is still a condition before selecting
              # there is logical_op and there is a select
              conditions_value <- parse(text = constraint_info$max_val[[j]]$conditions) |>
                eval()

              expressions <- c()
              for (i in 1:length(constraint_info$max_val[[j]]$expressions)){
                expressions <- c(expressions, eval(parse(text = constraint_info$max_val[[j]]$expressions[i])))
              }

              if (conditions_value == TRUE){
                if (constraint_info$max_val[[j]]$select == "min"){
                  constraint_max_val <- min(expressions)
                  break
                } else{
                  constraint_max_val <- max(expressions)
                  break
                }
              }
            } else{ # it is just a select with the expressions
              expressions <- c()
              for (i in 1:length(constraint_info$max_val[[j]]$expressions)){
                expressions <- c(expressions, eval(parse(text = constraint_info$max_val[[j]]$expressions[i])))
              }
              if (constraint_info$max_val[[j]]$select == "min"){
                constraint_max_val <- min(expressions)
                break
              } else{
                constraint_max_val <- max(expressions)
                break
              }
            }
          } else{ # no logical_op and no select
            conditions_value <- parse(text = constraint_info$max_val[[j]]$conditions) |>
              eval()
            if (conditions_value == TRUE){
              constraint_max_val <- parse(text = constraint_info$max_val[[j]]$expression) |>
                eval()
              break
            }
          }
        }

      }
    }

    structure_constraints[i,"min_value"] <- constraint_min_val
    structure_constraints[i,"max_value"] <- constraint_max_val
    structure_constraints[i, "units"] <- constraint_info$unit
  }

  # listing other constraints
  for (i in 1:nrow(other_constraints)){
    for (j in 1:length(tidydistrict$other_constraints[[i]])){
      use_name <- tidydistrict$other_constraints[[i]][[j]]$use_name
      if (bldg_type %in% use_name){
        use_index <- j
      } else  {
        use_index <- NA
      }
    }
    if (is.na(use_index)){
      break
    }

    constraint_info <- tidydistrict$other_constraints[[i]][[use_index]]

    # Looking at possible min values
    if (length(constraint_info$min_val[[1]]) == 1){ # this is just a one-line expression for the constraint
      constraint_min_val <- parse(text = constraint_info$min_val[[1]]) |>
        eval()

    } else if (length(constraint_info$min_val) == 0){
      constraint_min_val <- NA

    } else{ # this indicates a rule of some kind
      # loop through each rule
      for (j in 1:length(constraint_info$min_val)){
        rule_items <- names(constraint_info$min_val[[j]])
        if ("logical_operator" %in% rule_items ){
          if (constraint_info$min_val[[j]]$logical_operator == "AND"){
            logical_operator <- "&"
          } else{
            logical_operator <- "|"
          }
          if ("select" %in% rule_items){ # there is logical_op and there is a select
            conditions_value <- parse(text = paste(constraint_info$min_val[[j]]$conditions,
                                                   collapse = logical_operator)) |>
              eval()

            expressions <- c()
            for (i in 1:length(constraint_info$min_val[[j]]$expressions)){
              expressions <- c(expressions, eval(parse(text = constraint_info$min_val[[j]]$expressions[i])))
            }
            if (conditions_value == TRUE){
              if (constraint_info$min_val[[j]]$select == "min"){
                constraint_min_val <- min(expressions)
                break
              } else{
                constraint_min_val <- max(expressions)
                break
              }
            }
          } else{ # there is logical_op and just one expression at end
            constraint_min_val <- parse(text = constraint_info$min_val[[j]]$expression) |>
              eval()
            break
          }
        } else{
          if ("select" %in% rule_items){ # not logical_op, but there is a select
            if ("conditions" %in% rule_items){ # There is still a condition before selecting
              # there is logical_op and there is a select
              conditions_value <- parse(text = constraint_info$min_val[[j]]$conditions) |>
                eval()

              expressions <- c()
              for (i in 1:length(constraint_info$min_val[[j]]$expressions)){
                expressions <- c(expressions, eval(parse(text = constraint_info$min_val[[j]]$expressions[i])))
              }

              if (conditions_value == TRUE){
                if (constraint_info$min_val[[j]]$select == "min"){
                  constraint_min_val <- min(expressions)
                  break
                } else{
                  constraint_min_val <- max(expressions)
                  break
                }
              }
            } else{ # it is just a select with the expressions
              expressions <- c()
              for (i in 1:length(constraint_info$min_val[[j]]$expressions)){
                expressions <- c(expressions, eval(parse(text = constraint_info$min_val[[j]]$expressions[i])))
              }
              if (constraint_info$min_val[[j]]$select == "min"){
                constraint_min_val <- min(expressions)
                break
              } else{
                constraint_min_val <- max(expressions)
                break
              }
            }
          } else{ # no logical_op and no select
            conditions_value <- parse(text = constraint_info$min_val[[j]]$conditions) |>
              eval()
            if (conditions_value == TRUE){
              constraint_min_val <- parse(text = constraint_info$min_val[[j]]$expression) |>
                eval()
              break
            }
          }
        }

      }
    }

    # Looking at possible max values
    if (length(constraint_info$max_val[[1]]) == 1){ # this is just a one-line expression for the constraint
      constraint_max_val <- parse(text = constraint_info$max_val[[1]]) |>
        eval()

    } else if (length(constraint_info$max_val) == 0){
      constraint_max_val <- NA

    } else{ # this indicates a rule of some kind
      # loop through each rule
      for (j in 1:length(constraint_info$max_val)){
        rule_items <- names(constraint_info$max_val[[j]])
        if ("logical_operator" %in% rule_items ){
          if (constraint_info$max_val[[j]]$logical_operator == "AND"){
            logical_operator <- "&"
          } else{
            logical_operator <- "|"
          }
          if ("select" %in% rule_items){ # there is logical_op and there is a select
            conditions_value <- parse(text = paste(constraint_info$max_val[[j]]$conditions,
                                                   collapse = logical_operator)) |>
              eval()

            expressions <- c()
            for (i in 1:length(constraint_info$max_val[[j]]$expressions)){
              expressions <- c(expressions, eval(parse(text = constraint_info$max_val[[j]]$expressions[i])))
            }
            if (conditions_value == TRUE){
              if (constraint_info$max_val[[j]]$select == "min"){
                constraint_max_val <- min(expressions)
                break
              } else{
                constraint_max_val <- max(expressions)
                break
              }
            }
          } else{ # there is logical_op and just one expression at end
            constraint_max_val <- parse(text = constraint_info$max_val[[j]]$expression) |>
              eval()
            break
          }
        } else{
          if ("select" %in% rule_items){ # not logical_op, but there is a select
            if ("conditions" %in% rule_items){ # There is still a condition before selecting
              # there is logical_op and there is a select
              conditions_value <- parse(text = constraint_info$max_val[[j]]$conditions) |>
                eval()

              expressions <- c()
              for (i in 1:length(constraint_info$max_val[[j]]$expressions)){
                expressions <- c(expressions, eval(parse(text = constraint_info$max_val[[j]]$expressions[i])))
              }

              if (conditions_value == TRUE){
                if (constraint_info$max_val[[j]]$select == "min"){
                  constraint_max_val <- min(expressions)
                  break
                } else{
                  constraint_max_val <- max(expressions)
                  break
                }
              }
            } else{ # it is just a select with the expressions
              expressions <- c()
              for (i in 1:length(constraint_info$max_val[[j]]$expressions)){
                expressions <- c(expressions, eval(parse(text = constraint_info$max_val[[j]]$expressions[i])))
              }
              if (constraint_info$max_val[[j]]$select == "min"){a
                constraint_max_val <- min(expressions)
                break
              } else{
                constraint_max_val <- max(expressions)
                break
              }
            }
          } else{ # no logical_op and no select
            conditions_value <- parse(text = constraint_info$max_val[[j]]$conditions) |>
              eval()
            if (conditions_value == TRUE){
              constraint_max_val <- parse(text = constraint_info$max_val[[j]]$expression) |>
                eval()
              break
            }
          }
        }

      }
    }

    other_constraints[i,"min_value"] <- constraint_min_val
    other_constraints[i,"max_value"] <- constraint_max_val
    other_constraints[i, "units"] <- constraint_info$unit
  }

  rbind(lot_constraints, structure_constraints, other_constraints)
}
