#' Run initial check functions
#'
#' `tz_initial_checks()` runs the initial tidyzoning check functions to see if certain attributes of the building are allowed in the parcel.
#'
#' @param tidybuilding A tidybuilding is a list of data frames used to represent a building.
#' @param tidyzoning A tidyzoning simple features object representing the zoning districts and regulations for an area.
#' @param tidyparcel_with_dimensions A data frame with each tidyparcel and its information. The zoning_id column in the tidyparcel_with_dimensions must correspond to the row numbers of the tidyzoning object.
#' @param func_names A character list of the check functions you want to run.
#' @param run_parallel Logical. When `TRUE` the code runs on multiple cores (one less than your machine has available). Will save time if processing over 50,000 parcels. Default is `FALSE`
#' @param detailed_check Logical. When `TRUE`, it returns a data frame stating whether or not the building is allowed in the parcel. Otherwise, it will return a data frame with every chack function's results. Default is `FALSE`
#'
#' @returns A data frame with initial check function results.
#' @export
#'
#' @examples
tz_initial_checks <- function(tidybuilding,
                              tidyzoning,
                              tidyparcel_with_dimensions,
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
                              run_parallel = FALSE,
                              detailed_check = FALSE){
  start_time <- proc.time()

  perform_checks <- function(tidybuilding, tidyzoning, tidyparcel_with_dimensions, func_names){

    tidyparcel_df <- tidyparcel_with_dimensions

    if (detailed_check == TRUE){
      # Loop through each parcel and perform the different checks
      errors <- c()
      for (i in 1:nrow(tidyparcel_df)){
        tidyparcel <- tidyparcel_df[i,]
        tidydistrict <- tidyzoning[tidyparcel$zoning_id,]
        zoning_req <- get_zoning_req(tidybuilding, tidydistrict, tidyparcel)

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
          tidyparcel_df[i, func_name] <- check
        }

      }
    } else{
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
            if (length(maybe_reasons) > 0){
              reason[[i]] <- paste(maybe_reasons, collapse = ",")
              allowed <- c(allowed, "MAYBE")
            } else{
              reason[[i]] <- "The building is allowed in the parcel"
              allowed <- c(allowed, TRUE)
            }
          }


        }

      }

      tidyparcel_df$allowed <- allowed
      tidyparcel_df$reason <- unlist(reason)
    }


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

    perform_checks(tidybuilding, tidyzoning, tidyparcel_with_dimensions = tidyparcel, func_names)
  }

  # Parallel processing #
  check_in_parallel <- function(tidybuilding, tidyzoning, tidyparcel_with_dimensions, func_names){


    # Number of cores to use
    num_cores <- detectCores() - 1

    chunks <- suppressWarnings(split(tidyparcel_with_dimensions, rep(1:num_cores, each = ceiling(nrow(tidyparcel_with_dimensions) / num_cores))))

    # Set up parallel computing
    cl <- makeCluster(num_cores)
    clusterExport(cl, varlist = c("zoning_analysis_all_checks_pipeline",
                                  "go_through_pipeline_all_checks",
                                  "tidyzoning",
                                  "tidybuilding",
                                  "func_names")) # Export your processing function

    errors <- c()
    # Execute the processing function in parallel
    results <- parLapply(cl, chunks,fun = go_through_checks_function)

    # Stop the cluster
    stopCluster(cl)

    # View the final result
    return(do.call(rbind, results))
  }

  end_time <- proc.time()
  print(end_time - start_time)

  if (run_parallel == TRUE){
    return(check_in_parallel(tidybuilding, tidyzoning, tidyparcel_with_dimensions, func_names))
  } else{
    return(perform_checks(tidybuilding, tidyzoning, tidyparcel_with_dimensions, func_names))
  }

}
