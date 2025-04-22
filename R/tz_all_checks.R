#' Run initial check functions
#'
#' `tz_initial_checks()` runs the initial tidyzoning check functions to see if certain attributes of the building are allowed in the parcel.
#'
#' @param tidybuilding A tidybuilding is a list of data frames used to represent a building.
#' @param tidyzoning A tidyzoning simple features object representing the zoning districts and regulations for an area.
#' @param tidyparcel_with_dimensions A data frame with each tidyparcel and its information. The zoning_id column in the tidyparcel_with_dimensions must correspond to the row numbers of the tidyzoning object.
#' @param tidyparcel_with_side_geom A data frame with each parcel side and its geometry
#' @param func_names A character list of the check functions you want to run.
#' @param run_parallel Logical. When `TRUE` the code runs on multiple cores (one less than your machine has available). Will save time if processing over 50,000 parcels. Default is `FALSE`
#'
#' @returns A data frame with initial check function results.
#' @export
#'
#' @examples
tz_all_checks <- function(tidybuilding,
                          tidyzoning,
                          tidyparcel_with_dimensions,
                          tidyparcel_with_side_geom,
                          func_names = c("check_land_use",
                                         "check_height",
                                         "check_height_eave",
                                         "check_floors",
                                         "check_unit_size",
                                         "check_far",
                                         "check_unit_density",
                                         "check_lot_coverage",
                                         "check_fl_area",
                                         "check_unit_qty"),
                          run_parallel = FALSE){
  start_time <- proc.time()

  perform_checks <- function(tidybuilding, tidyzoning, tidyparcel_with_dimensions, tidyparcel_with_side_geom, func_names){

    tidyparcel_df <- tidyparcel_with_dimensions

    # Loop through each parcel and perform the different checks
    errors <- c()
    allowed <- c()
    reason <- list()
    for (i in 1:nrow(tidyparcel_df)){
      tidyparcel <- tidyparcel_df[i,]
      tidydistrict <- tidyzoning[tidyparcel$zoning_id,]
      zoning_req <- get_zoning_req(tidybuilding, tidydistrict, tidyparcel)

      maybe_reasons <- c()

      # loop through each function
      for (j in 1:length(func_names)){
        func_name <- func_names[[j]]
        func <- get(func_name)

        check <- tryCatch({
          suppressWarnings(func(tidybuilding = tidybuilding,
                                tidydistrict = tidydistrict,
                                tidyparcel = tidyparcel,
                                zoning_req = zoning_req))
        }, error = function(e) {
          # code to execute for errors
          paste("Error in zoning_id",tidyparcel$zoning_id)
        })
        if (!check %in% c(TRUE, FALSE, "MAYBE")){
          errors <- c(errors,check)
          check <- "MAYBE"
        }
        if(check == FALSE){
          allowed <- c(allowed, FALSE)
          reason[[i]] <- func_name
          break
        } else if(check == "MAYBE"){
          maybe_reasons <- c(maybe_reasons, func_name)
        }

        # If the parcel makes it through all the checks, it will be a MAYBE or TRUE
        if (j == length(func_names)){
          if (length(maybe_reasons) > 0){ #if it is a maybe
            reason[[i]] <- paste(maybe_reasons, collapse = ",")
            allowed <- c(allowed, "MAYBE")
          } else{ # it is allowed and we need to do the footprint check

            if (check_footprint_area(tidybuilding, tidyparcel)$check_footprint_area[[1]] == TRUE){
              tidyparcel_sides <- tidyparcel_with_side_geom |>
                filter(parcel_id == tidyparcel$parcel_id)
              parcel_with_setbacks <- add_setbacks(tidyparcel_with_side_geom, zoning_req = zoning_req)
              buildable_area <- get_buildable_area(parcel_with_setbacks)

              if (length(buildable_area) > 1){
                check_1 <- check_footprint(tidybuilding, buildable_area[[1]])

                if (check_1){
                  check <- check_1
                } else{
                  check_2 <- check_footprint(tidybuilding, buildable_area[[2]])
                  if (check_2){
                    check <- "MAYBE"
                  } else{
                    check <- FALSE
                  }
                }

              } else{
                check <- check_footprint(tidybuilding, buildable_area[[1]])
              }

              if (check == FALSE){
                allowed <- c(allowed, FALSE)
                reason[[i]] <- "check_footprint"
              } else if (check == "MAYBE"){
                allowed <- c(allowed, "MAYBE")
                reason[[i]] <- "check_footprint"
              } else{
                allowed <- c(allowed, TRUE)
                reason[[i]] <- "The building is allowed and fits in the parcel"
              }
            }
          }
        }


      }




    }

    tidyparcel_df$allowed <- allowed
    tidyparcel_df$reason <- unlist(reason)


    if (length(errors) > 0){
      warning(paste("There were",length(errors),"errors in check functions"))
    }

    return(tidyparcel_df)

  }



  go_through_checks_function <- function(tidyparcel){
    library(dplyr)
    library(rjson)
    library(sf)
    library(terra)
    library(units)
    library(devtools)
    library(tidyzoning)
    library(ggspatial)

    perform_checks(tidybuilding, tidyzoning, tidyparcel_with_dimensions = tidyparcel, tidyparcel_with_side_geom, func_names)
  }

  # Parallel processing #
  check_in_parallel <- function(tidybuilding, tidyzoning, tidyparcel_with_dimensions, tidyparcel_with_side_geom, func_names){


    # Number of cores to use
    num_cores <- detectCores() - 1

    chunks <- suppressWarnings(split(tidyparcel_with_dimensions, rep(1:num_cores, each = ceiling(nrow(tidyparcel_with_dimensions) / num_cores))))

    # Set up parallel computing
    cl <- makeCluster(num_cores)
    clusterExport(cl, varlist = c("go_through_checks_function",
                                  "perform_checks",
                                  "tidyzoning",
                                  "tidybuilding",
                                  "tidyparcel_with_side_geom",
                                  "func_names"),
                  envir = environment(tz_all_checks)) # Export your processing function

    errors <- c()
    # Execute the processing function in parallel
    results <- parLapply(cl, chunks,fun = go_through_checks_function)

    # Stop the cluster
    stopCluster(cl)

    # View the final result
    return(do.call(rbind, results))
  }

  if (run_parallel == TRUE){
    result <- check_in_parallel(tidybuilding, tidyzoning, tidyparcel_with_dimensions, func_names)
    end_time <- proc.time()
    print(end_time - start_time)
    return(result)
  } else{
    result <- perform_checks(tidybuilding, tidyzoning, tidyparcel_with_dimensions, func_names)
    end_time <- proc.time()
    print(end_time - start_time)
    return(result)
  }

}
