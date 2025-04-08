#' Compare unit sizes and allowed unit sizes
#'
#' `check_unit_size()` takes a tidybuilding and a tidydistrict to see if the district's zoning code allows the tidybuilding based on unit sizes.
#'
#' @inheritParams add_setbacks
#'
#' @return
#' Returns TRUE, FALSE, or MAYBE stating whether or not the building would be allowed in the district based on the unit sizes.
#' @export
#'
check_unit_size <- function(tidybuilding, tidydistrict, tidyparcel, zoning_req = NULL){
  # get the tidydistrict into a list format
  structure_constraints <- fromJSON(tidydistrict$structure_constraints)

  safe_parse <- purrr::possibly(parse, otherwise = NA)

  if (is.null(zoning_req)){
    zoning_req <- get_zoning_req(tidybuilding, tidydistrict, tidyparcel)
  }

  if (class(zoning_req) == "character"){
    return(TRUE)
    warning("No zoning requirements recorded for this district")
  }

  # if floor areas aren't in tidybuilding, we return TRUE and give a warning.
  if (is.null(tidybuilding$unit_info$fl_area)){
    warning("No unit sizes recorded in the tidybuilding")
    return(TRUE)
  }

  # if no bedrooms area recorded, we return TRUE, give a warning, and assume 2 beds per unit.
  if (is.null(tidybuilding$unit_info$bedrooms)){
    warning("No bedroom qty recorded in the tidybuilding. Results may be innacurate")
    tidybuilding$unit_info$bedrooms <- 2
  }



  ## CHECKING AVERAGE UNIT SIZE REQUIREMENTS
  if (is.null(structure_constraints$unit_size_avg)){
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

  unit_size_avg <- mean(tidybuilding$unit_info$fl_area)

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


  # getting same variables as get_zoning_req
  bldg_type <- tidybuilding$bldg_info$type

  # establish the parcel variables that might be used in the equations
  lot_width <- tidyparcel$lot_width[[1]] # this should be in ft
  lot_depth <- tidyparcel$lot_depth[[1]] # this should be in ft
  lot_area <- tidyparcel$lot_area[[1]] # this should be in acres
  lot_type <- ifelse(tidyparcel$Parcel_label[[1]] == "regular corner parcel", "corner","regular")

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
  bldg_dpth <- ifelse(length(tidybuilding$bldg_info$depth) > 0, tidybuilding$bldg_info$depth, NA)
  level_units_table <- tidybuilding$unit_info |> group_by(level) |> summarise(units = sum(qty))
  units_floor1 <- ifelse(length(level_units_table$units[level_units_table$level == 1]) > 0,level_units_table$units[level_units_table$level == 1],0)
  units_floor2 <- ifelse(length(level_units_table$units[level_units_table$level == 2]) > 0,level_units_table$units[level_units_table$level == 2],0)
  units_floor3 <- ifelse(length(level_units_table$units[level_units_table$level == 3]) > 0,level_units_table$units[level_units_table$level == 3],0)



  # getting a df with just the min and max of each unit type
  unit_info_df <- tidybuilding$unit_info |>
    group_by(bedrooms) |>
    summarise(min = min(fl_area),
              max = max(fl_area))

  # for those values that don't have requirements in ozfs
  unit_info_df$min_val <- 0
  unit_info_df$max_val <- 100000


  # loop through each row and find the zoning requirement for each unit type in the ozfs data
  max_vals <- list()
  min_vals <- list()
  for (i in 1:nrow(unit_info_df)){

    if (!is.null(tidybuilding$unit_info$bedrooms)){
      bedrooms <- unit_info_df$bedrooms[[i]]
    }

    for (j in 1:length(structure_constraints$unit_size)){
      use_name <- structure_constraints$unit_size[[j]]$use_name
      if (tidybuilding$bldg_info$type %in% use_name){
        use_index <- j
        break
      }else {
        use_index <- NA
      }
    }

    if (is.na(use_index)){
      return(TRUE)
    }

    constraint_info <- structure_constraints$unit_size[[use_index]]

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
