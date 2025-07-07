#' Add setback column to tidyparcel object
#'
#' `zr_add_setbacks()` returns the tidyparcel object with a new setback column
#'
#'
#' @param tidyparcel_geo A tidyparcel object is a simple features object depicting each side of a parcel and its label (front, interior side, exterior side, rear, centroid).
#' @param tidydistrict The tidydistrict corresponding to the tidyparcel. A tidydistrict object is one row from a tidyzoning simple features object.
#' @param tidybuilding A tidybuilding is a list of data frames used to represent a building.
#' @param tidyparcel_dims The simple features object with each parcel centroid and the parcel dimensions
#' @param zoning_req The results of the get_zoning_req funcion. If provided, tidyparcel_dims need not be provided.
#'
#' @return Returns the tidyparcel object with a "setbacks" column added to the end.
#' @export
#'

zr_add_setbacks <- function(tidyparcel_geo, tidydistrict, tidybuilding, tidyparcel_dims = NULL, zoning_req = NULL){
  tidyparcel <- tidyparcel_geo[tidyparcel_geo$side != "centroid",]
  tidyparcel <- tidyparcel[!is.na(tidyparcel$side),]

  # if zoning_req is not given, we need to run the get_zoning_req function
  if (is.null(zoning_req)){
    zoning_req <- zr_get_zoning_req(tidybuilding, tidydistrict, tidyparcel_dims)
  }

  if (class(zoning_req) == "character"){
    tidyparcel$setback <- NA
    return(tidyparcel)
  }

  name_key <- c(front = "setback_front",
                `interior side` = "setback_side_int",
                `exterior side` = "setback_side_ext",
                rear = "setback_rear")

  # loop through each side
  setback_value <- list()
  for (i in 1:nrow(tidyparcel)){
    side_type <- tidyparcel[[i,"side"]]
    filtered_constraints <- zoning_req |>
      dplyr::filter(constraint_name == name_key[[side_type]])

    if (nrow(filtered_constraints) > 0){
      setback_value[i] <- filtered_constraints[1,"min_value"]
    } else {
      setback_value[i] <- NA
    }
  }

  tidyparcel$setback <- I(setback_value)


  ## EVERYTHING BELOW HAS BEEN ADDED TO ACCOMODATE EXTRA SETBACK RULES ##

  # Now check to see if there are extra setback rules we need to worry about
  extra_setback_info <- c()
  if ("setback_side_sum" %in% zoning_req$constraint_name){
    # If there is a setback_side_sum constraint
    # Then we record it in extra_setback_info and
    # we assign it's value to side_sum variable
    extra_setback_info <- c(extra_setback_info, "setback_side_sum")
    side_sum <- zoning_req[zoning_req$constraint_name == "setback_side_sum", "min_value"][[1]]
  }

  if ("setback_front_sum" %in% zoning_req$constraint_name){
    # If there is a setback_front_sum constraint
    # Then we record it in extra_setback_info and
    # we assign it's value to front_sum variable
    extra_setback_info <- c(extra_setback_info, "setback_front_sum")
    front_sum <- zoning_req[zoning_req$constraint_name == "setback_front_sum", "min_value"][[1]]
  }

  if ("setback_dist_boundary" %in% zoning_req$constraint_name){
    # If there is a setback_dist_boundary constraint
    # Then we record it in extra_setback_info and assign it's value to dist_boundary variable
    # We then add a new column to mark the sides that are touching the boundary
    extra_setback_info <- c(extra_setback_info, "setback_dist_boundary")
    dist_boundary <- zoning_req[zoning_req$constraint_name == "setback_dist_boundary", "min_value"][[1]]

    # Turn the district polygon into a multilinestring
    # Give it a 5 meter buffer
    # Mark the sides that are completely inside the buffer
    district_lines <- tidydistrict |>
      sf::st_cast("MULTILINESTRING")
    buffered_district <- sf::st_buffer(district_lines, 5)
    close_sides_idx <- sf::st_covered_by(tidyparcel, buffered_district)
    border_sides_logical <- lengths(close_sides_idx) > 0

    # Adding the column to
    tidyparcel$on_boundary <- border_sides_logical
  }

  # If no extra setback rules, we return the previously created data frame with setbacks added
  # If there are extra rules, we see if we need to update the data frame
  if (length(extra_setback_info) == 0){
    return(tidyparcel)
  } else{

    # If there is a setback_dist_boundary constraint, make those updates first
    if ("setback_dist_boundary" %in% extra_setback_info){
      # make sure the sides with on_boundary == TRUE have a setback greater than dist_boundary
      # if not, change the setback to the value of the setback_dist_boundary
      for (j in 1:nrow(tidyparcel)){
        if (tidyparcel$on_boundary[[j]]){
          setback_value <- tidyparcel$setback[[j]]
          if (length(setback_value) == 1 & length(dist_boundary) == 1){
            tidyparcel$setback[[j]] <- pmax(dist_boundary,setback_value)
          } else{
            value <- pmax(dist_boundary,setback_value)
            if (value[[1]] == value[[2]]){
              value <- value[[1]]
            }
            tidyparcel$setback[[j]] <- value
          }
        }
      }
    }

    # Now look to see if setback_side_sum is a constraint and make updates
    if ("setback_side_sum" %in% extra_setback_info){
      # get idx of just the rows with side edges
      just_sides <- which(tidyparcel$side %in% c("interior side","exterior side"))

      # if there are less than 2, we can't calculate the sum of the sides
      if (length(just_sides) < 2){
        warning("setback_side_sum cannot be calculated due to lack of parcel edges")
      } else{
        # get idx of just interior side edges
        # get idx of just exterior side edges
        int_idxs <- which(tidyparcel$side == "interior side")
        ext_idxs <- which(tidyparcel$side == "exterior side")

        # Assign a side_1 and side_2 making sure the exterior side is side_1 when applicable
        if (length(int_idxs) > 0 & length(ext_idxs) > 0){
          side_1_idx <- ext_idxs[[1]]
          side_2_idx <- int_idxs[[1]]
        } else if (length(int_idxs) > 0){
          side_1_idx <- int_idxs[[1]]
          side_2_idx <- int_idxs[[2]]
        }else if (length(ext_idxs) > 0){
          side_1_idx <- ext_idxs[[1]]
          side_2_idx <- ext_idxs[[2]]
        }
        # get the setback values for those sidse
        side_1_value <- tidyparcel$setback[[side_1_idx]]
        side_2_value <- tidyparcel$setback[[side_2_idx]]

        # subract the summed_sides from the side_sum constratin value
        summed_sides_check <- side_sum - (side_1_value + side_2_value)

        # if the difference is negative or zero, we don't need to change the setback
        side_setback_increase <- ifelse(summed_sides_check < 0, 0 , summed_sides_check)

        # adding the extra setback needed to the side_2 setback
        tidyparcel$setback[[side_2_idx]] <- side_2_value + side_setback_increase
      }

    }

    # Now look to see if setback_front_sum is a constraint and make updates
    if ("setback_front_sum" %in% extra_setback_info){
      # get idx of just the rows with fornt and rear sides
      front_idxs <- which(tidyparcel$side == "front")
      rear_idxs <- which(tidyparcel$side == "rear")

      # if there is a front side and a rear side, we can check it
      if (length(front_idxs) > 0 & length(rear_idxs) > 0){
        # get the idx of the front side
        # get the ind of the rear side
        front_idx <- front_idxs[[1]]
        rear_idx <- rear_idxs[[1]]

        # get the values of the front and rear setbacks
        front_value <- tidyparcel$setback[[front_idx]]
        rear_value <- tidyparcel$setback[[rear_idx]]

        # subract the front/rear sum from the front_sum constratin value
        summed_sides_check <- front_sum - (front_value + rear_value)

        # if the difference is negative or zero, we don't need to change the setback
        rear_setback_increase <- ifelse(summed_sides_check < 0, 0 , summed_sides_check)

        # adding the extra setback needed to the rear setback
        tidyparcel$setback[[rear_idx]] <- rear_value + rear_setback_increase

      } else{
        warning("setback_front_sum cannot be calculated due to missing rear or front edge")
      }

    }

    return(tidyparcel)
  }
}
