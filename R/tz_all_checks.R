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
                          tidyparcel_dims,
                          tidyparcel_geo = NULL,
                          detailed_check = FALSE,
                          run_check_land_use = TRUE,
                          run_check_height = TRUE,
                          run_check_height_eave = TRUE,
                          run_check_floors = TRUE,
                          run_check_unit_size = TRUE,
                          run_check_far = TRUE,
                          run_check_unit_density = TRUE,
                          run_check_lot_coverage = TRUE,
                          run_check_fl_area = TRUE,
                          run_check_unit_qty = TRUE,
                          run_check_footprint = FALSE){

  total_start_time <- proc.time()[[3]]

  if (is.null(tidyparcel_geo) & run_check_footprint == TRUE){
    warning("No parcel side geometry given. Skipping check_fooprint function")
  }

  tidyparcel_df <- tidyparcel_dims |>
    mutate(false_reasons = as.character(NA),
           maybe_reasons = as.character(NA))


  false_df <- list() # start a list to store the false data frames of the check functions
  false_df_idx <- 1

  if (run_check_land_use == TRUE){
    lu_start_time <- proc.time()[[3]]
    # land use for each district
    lu_check <- c()
    for (k in 1:nrow(tidyzoning)){
      tidydistrict <- tidyzoning[k, ]
      lu_check <- c(lu_check,check_land_use(tidybuilding, tidydistrict))
    }

    # add a column stating the land_use_check results
    tidyparcel_df <- tidyparcel_df |>
      mutate(check_land_use = ifelse(zoning_id %in% which(lu_check == TRUE), TRUE, FALSE))

    if (detailed_check == FALSE){
      # filter the tidyparcels that aren't in those districts and give them a reason
      tidyparcel_false <- tidyparcel_df |>
        filter(!zoning_id %in% which(lu_check == TRUE))
      tidyparcel_false$false_reasons <- "check_land_use"

      false_df[[false_df_idx]] <- tidyparcel_false
      false_df_idx <- false_df_idx + 1

      tidyparcel_df <- tidyparcel_df |>
        filter(zoning_id %in% which(lu_check == TRUE))
    }

    time_lapsed <- proc.time()[[3]] - lu_start_time
    print(paste0("_____","check_land_use","_____"))
    print(paste0("runtime: ", round(time_lapsed,1), " sec (",round(time_lapsed / 60,2)," min)"))
    print(paste(length(which(tidyparcel_df[,"check_land_use"][[1]] %in% c(TRUE, 'MAYBE'))),"parcels are TRUE or MAYBE"))

  }

  func_names <- c("check_height",
                  "check_height_eave",
                  "check_floors",
                  "check_unit_size",
                  "check_far",
                  "check_unit_density",
                  "check_lot_coverage",
                  "check_fl_area",
                  "check_unit_qty")

  check_functions <- c(run_check_height,
                       run_check_height_eave,
                       run_check_floors,
                       run_check_unit_size,
                       run_check_far,
                       run_check_unit_density,
                       run_check_lot_coverage,
                       run_check_fl_area,
                       run_check_unit_qty)


  errors <- c()
  for (j in 1:length(check_functions)){
    if (check_functions[[j]]){ #if the function is marked true, then it will run the function
      func_name <- func_names[[j]]
      func <- get(func_name)

      func_start_time <- proc.time()[[3]]

      if (nrow(tidyparcel_df) == 0){
        break
      }

      for (i in 1:nrow(tidyparcel_df)){
        tidyparcel <- tidyparcel_df[i,]
        tidydistrict <- tidyzoning[tidyparcel$zoning_id,]
        zoning_req <- get_zoning_req(tidybuilding, tidydistrict, tidyparcel)

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

        tidyparcel_df[i, func_name] <- as.character(check)

        if (check == "MAYBE"){
          tidyparcel_df[i,"maybe_reasons"] <- ifelse(is.na(tidyparcel_df[i,"maybe_reasons"]), func_name, paste(tidyparcel_df[i,"maybe_reasons"], func_name, sep = ", "))
        }

        if (check == FALSE){
          tidyparcel_df[i,"false_reasons"] <- ifelse(is.na(tidyparcel_df[i,"false_reasons"]), func_name, paste(tidyparcel_df[i,"false_reasons"], func_name, sep = ", "))
        }
      }

      if (detailed_check == FALSE){
        tidyparcel_false <- tidyparcel_df[tidyparcel_df[,func_name][[1]] == FALSE,]
        tidyparcel_df <- tidyparcel_df[tidyparcel_df[,func_name][[1]] %in% c(TRUE, "MAYBE"),]
        # Add allowed and reasons to the tidyparcel_false
        false_df[[false_df_idx]] <- tidyparcel_false
        false_df_idx <- false_df_idx + 1
      }

      time_lapsed <- proc.time()[[3]] - func_start_time
      print(paste0("_____",func_name,"_____"))
      print(paste0("runtime: ", round(time_lapsed,1), " sec (",round(time_lapsed / 60,2)," min)"))
      print(paste(length(which(tidyparcel_df[,func_name][[1]] %in% c(TRUE, 'MAYBE'))),"parcels are TRUE or MAYBE"))
    }
  }

  # run the check footprint function last
  if (run_check_footprint & nrow(tidyparcel_df) > 0){
    foot_start_time <- proc.time()[[3]]
    for (z in 1:nrow(tidyparcel_df)){
      tidyparcel <- tidyparcel_df[z,]
      tidydistrict <- tidyzoning[tidyparcel$zoning_id,]
      zoning_req <- get_zoning_req(tidybuilding, tidydistrict, tidyparcel)
      if (check_footprint_area(tidybuilding, tidyparcel)$check_footprint_area[[1]] == TRUE){
        tidyparcel_sides <- tidyparcel_geo |>
          filter(parcel_id == tidyparcel$parcel_id)
        parcel_with_setbacks <- add_setbacks(tidyparcel_sides, zoning_req = zoning_req)
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

      } else{
        check <- FALSE
      }

      tidyparcel_df[z, "check_footprint"] <- as.character(check)

      if (check == "MAYBE"){
        tidyparcel_df[z,"maybe_reasons"] <- ifelse(is.na(tidyparcel_df[z,"maybe_reasons"]), "check_footprint", paste(tidyparcel_df[z,"maybe_reasons"], "check_footprint", sep = ", "))
      }

      if (check == FALSE){
        tidyparcel_df[z,"false_reasons"] <- ifelse(is.na(tidyparcel_df[z,"false_reasons"]), "check_footprint", paste(tidyparcel_df[z,"false_reasons"], "check_footprint", sep = ", "))
      }
    }

    time_lapsed <- proc.time()[[3]] - foot_start_time
    print(paste0("_____","check_footprint","_____"))
    print(paste0("runtime: ", round(time_lapsed,1), " sec (",round(time_lapsed / 60,2)," min)"))
    print(paste(length(which(tidyparcel_df[,"check_footprint"][[1]] %in% c(TRUE, 'MAYBE'))),"parcels are TRUE or MAYBE"))
  }


  final_df <- bind_rows(false_df, tidyparcel_df)
  final_df$has_false <- rowSums(final_df == FALSE, na.rm = T)
  final_df$has_maybe <- rowSums(final_df == "MAYBE", na.rm = T)
  final_df <- final_df |>
    mutate(allowed = ifelse(has_false > 0, FALSE, ifelse(has_maybe > 0, "MAYBE",TRUE)),
           reason = ifelse(!is.na(maybe_reasons) | !is.na(false_reasons),
                           paste("FALSE encountered:", false_reasons, "- MAYBE encountered:", maybe_reasons),
                           "The building is allowed in the parcel")) |>
    select(!c("has_false","has_maybe"))

  if (detailed_check == FALSE){
    final_df <- final_df |>
      select(!any_of(c("check_land_use",
                       "check_height",
                       "check_height_eave",
                       "check_floors",
                       "check_unit_size",
                       "check_far",
                       "check_unit_density",
                       "check_lot_coverage",
                       "check_fl_area",
                       "check_unit_qty",
                       "check_footprint",
                       "maybe_reasons",
                       "false_reasons")))
  } else{
    final_df <- final_df |>
      select(!any_of(c("maybe_reasons",
                       "false_reasons")))
  }

  total_time <- proc.time()[[3]] - total_start_time
  print("_____summary_____")
  print(paste0("total runtime: ", round(total_time,1), " sec (",round(total_time / 60,2)," min)"))
  print(paste(length(which(final_df$allowed == TRUE)), "/", nrow(final_df), "parcels allow the building"))
  print(paste(length(which(final_df$allowed == "MAYBE")), "/", nrow(final_df), "parcels might allow the building"))

  return(final_df)

}
