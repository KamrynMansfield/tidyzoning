#' List district zoning requirement values
#'
#' Because many zoning requirements depend on the proposed building
#' or the parcel of that zoning district, `the get_zoning_req()` function
#' takes a tidybuilding, a tidyparcel, and a district_data and outputs a data
#' frame listing the set zoning requirements that those three objects would create.
#' If every value is NA, it could indicate that the building
#' land use is not allowed in the zoning district.
#'
#' @param bldg_file the file path to an OZFS *.bldg file
#' @param parcel_data one row of a parcel data frame created from the
#' OZFS *.parcel file
#' @param district_data one row (representing one district) of a
#' zoning data frame created from the OZFS *.zoning file
#' @param zoning_data either the path to a *.zoning file or
#' a list created from the the *.zoning file using `rjson::fromJSON`
#' @param vars the result from the `get_variables()` function.
#' If this data frame is supplied, bldg_file, parcel_data, and zoning_data
#' are not needed.
#'
#' @return
#' Returns a data frame with the value each zoning requirement for that specific building, parcel, and zoning district.
#' NA values indicate the requirement isn't recorded in that district.
#' If every value is NA, it could indicate that the building land use is not allowed in the zoning district.
#' @export
#'
get_zoning_req <- function(district_data, bldg_file = NULL, parcel_data = NULL, zoning_data = NULL, vars = NULL){

  # if there are no constraints under the constraints section,
  # it will output a string stating that
  if (is.null(district_data$constraints) | is.na(district_data$constraints)){
    return("No zoning requirements recorded for this district")
  }

  listed_constraints <- rjson::fromJSON(district_data$constraints)
  constraints <- names(listed_constraints)

  if (length(constraints) == 0){
    return("No zoning requirements recorded for this district")
  }


  if (is.null(vars)){
    vars <- get_variables(bldg_data, parcel_data, district_data, zoning_data)
  }

  for (var_idx in 1:ncol(vars)){
    vname <- names(vars)[[var_idx]]
    assign(vname, vars[[1,vname]])
  }


  # loop through each zoning regulation in the district
  for (i in 1:length(constraints)){
    name <- constraints[[i]]
    if (is.null(listed_constraints[[name]])){
      next
    }
    constraint_list <- listed_constraints[[name]]

    # Loop through the each uses available
    # to select the appropriate constraint info
    for (j in 1:length(constraint_list)){
      use_name <- constraint_list[[j]]$uses
      if (bldg_type %in% use_name){
        use_index <- j
        break
      }else {
        use_index <- NA
      }
    }
    if (is.na(use_index)){ # if there are no values for the specific land use it gives a warning
      min_val_notes[[i]] <- "no constraints for land use"
      max_val_notes[[i]] <- "no constraints for land use"
      next
    }
    # this is the info for the constraint that matches the building's land use
    constraint_info <- constraint_list[[use_index]]

    # see what values are recorded: "min_val", "max_val", or "min_val" and "max_val"
    if ("min_val" %in% names(constraint_info) & "max_val" %in% names(constraint_info)){
      minmax_options <- c("min_val","max_val")
    } else if ("min_val" %in% names(constraint_info)){
      minmax_options <- "min_val"
    } else if ("max_val" %in% names(constraint_info)){
      minmax_options <- "max_val"
    } else{
      min_val_notes[[i]] <- "no min or max value recorded"
      max_val_notes[[i]] <- "no min or max value recorded"
      warning("There is constraint info for the building's land use, but no associated values")
      next
    }

    # loop through the min_val and max_val lists
    # it is often just one or the other
    for (minmax in minmax_options){
      constraint_note <- NA

      # Assign the pertinent min/max_val item to a variable val_list
      # val_list is the list that contains, condition, expression, criterion, more_restrictive
      if (length(constraint_info[[minmax]]) == 1){ # there are no conditions, just one item
        val_list <- constraint_info[[minmax]][[1]]

      } else if (length(constraint_info[[minmax]]) > 1){ # there are conditions to loop through
        for (item in constraint_info[[minmax]]){

          condition <- gsub("and","&",item$condition)
          condition <- gsub("or","|", condition)

          parsed_condition <- tryCatch({
            # Try to calculate the square root
            parse(text = condition)
          }, error = function(e) {
            warning("Unable to properly parse constraint condition")
            FALSE
          })

          condition_met <- eval(parsed_condition)

          if (is.na(condition_met)){
            condition_met <- FALSE
          }

          if (condition_met){
            val_list <- item
            break
          }
        }

      } else{
        warning("Improper ozfs formatting")
        constraint_note <- paste0(minmax, " recorded but no info attached")
        next
      }

      # if there is data under the val_list,
      # then find the constraint value
      if (exists("val_list")){

        if (is.null(val_list$criterion)){ # then it will be just an expression
          expression <- val_list$expression
          parsed <- tryCatch({
            # Try to calculate the square root
            parse(text = expression)
          }, error = function(e) {
            warning("Unable to properly parse constraint expression")
            NA
          })

          value <- eval(parsed)

          if(is.na(value)){
            constraint_note <- paste0("parsing error (improper expression for ", name,")")
          }

        } else{ # then it will be expression, criterion (and maybe more_restrictive)

          # loop through each expression and parse and evaluate it
          values <- c() # will become a list of values of for each expression
          for (expression in val_list$expression){
            parsed <- tryCatch({
              # Try to calculate the square root
              parse(text = expression)
            }, error = function(e) {
              warning("Unable to properly parse constraint expression")
              NA
            })

            val <- eval(parsed)

            values <- c(values, val)

            if(is.na(val)){
              constraint_note <- paste0("parsing error (improper expression for ", name,")")
            }

          }

          # once we have the values list, we pick the value according to the criteron
          # if criterion is "dependent" then we out both values
          if (val_list$criterion == "min"){
            value <- min(values)
          } else if (val_list$criterion == "max"){
            value <- max(values)
          } else if (val_list$criterion == "dependent"){
            value <- c(min(values), max(values))
            if (!is.null(val_list$more_restrictive)){
              constraint_note <- val_list$more_restrictive
            } else{
              constraint_note <- "no reason given for dependent criterion"
            }
          } else{
            constraint_note <- "invalid criteron"
          }

        }

      } else{
        constraint_note <- paste0("no conditions met or imporper ozfs for ", name)
      }

      # Now we should have assigned the correct value or values.
      # Determine if it is a min_val or a max_val and update the lists accordingly
      if (minmax == "min_val"){
        min_vals[[i]] <- value
        min_val_notes[[i]] <- constraint_note
      } else{
        max_vals[[i]] <- value
        max_val_notes[[i]] <- constraint_note
      }

    }

  }

  # put everything into a data frame
  constraints_df <- data.frame(constraint_name = constraints)
  constraints_df$min_value <- min_vals
  constraints_df$max_value <- max_vals
  constraints_df$min_val_note <- min_val_notes
  constraints_df$max_val_note <- max_val_notes

  return(constraints_df)
}
