#' Find where a building is allowed to be built
#'
#' `zr_run_zoning_checks()` checks the building information against all the
#' zoning constraints to see which parcels will allow the building.
#'
#' @param bldg_file The path to the OZFS *.bldg file
#' @param parcels_file The path to the OZFS *.parcel file
#' @param zoning_file The path to the OZFS *.zoning
#' @param detailed_check When TRUE, every parcel passes through each check no matter the result,
#' and it take more time. When FALSE, subsequent checks are skipped as soon as one check reads FALSE
#' @param checks A list of all the checks that should take place. The default is
#' every check possible. Note, if a zoning file doesn't have zoning info for one
#' of the constraints listed in the checks variable, then we assume it is allowed
#' based on that condition.
#'
#' @returns a simple features data frame with the centroid of each parcel with a column
#' stating building allowance on the parcel and a column stating the reason
#' why certain parcels don't allow the building.
#' @export
#'
#' @examples
zr_run_zoning_checks <- function(bldg_file,
                                    parcels_file,
                                    zoning_file,
                                    detailed_check = TRUE,
                                    checks = c("land_use",
                                               "far",
                                               "fl_area",
                                               "fl_area_first",
                                               "fl_area_top",
                                               "footprint",
                                               "height",
                                               "height_eave",
                                               "lot_cov_bldg",
                                               "lot_size",
                                               "parking_enclosed",
                                               "stories",
                                               "unit_0bed",
                                               "unit_1bed",
                                               "unit_2bed",
                                               "unit_3bed",
                                               "unit_4bed",
                                               "unit_density",
                                               "unit_pct_0bed",
                                               "unit_pct_1bed",
                                               "unit_pct_2bed",
                                               "unit_pct_3bed",
                                               "unit_pct_4bed",
                                               "total_units",
                                               "unit_size_avg",
                                               "unit_size",
                                               "bldg_fit",
                                               "overlay")){
  # track the start time to give a time stamp at end
  total_start_time <- proc.time()[[3]]

  initial_checks <- checks[checks != c("land_use",
                                       "unit_size",
                                       "bldg_fit",
                                       "overlay")]

  ########---- START DATA PREP----########
  ## the buildign json list ##
  bldg_data <- rjson::fromJSON(file = bldg_file)

  ## zoning data ##
  # get the full ozfs data as an sf data frame
  zoning_all_sf <- sf::st_read(zoning_file, quiet = TRUE)
  # get just the overlay districts with geometry
  overlays <- zoning_all_sf |>
    dplyr::filter(!sf::st_is_empty(geometry)) |>
    dplyr::filter(overlay == TRUE)
  # get just the pd_districts with geometry
  pd_districts <- zoning_all_sf |>
    dplyr::filter(!sf::st_is_empty(geometry)) |>
    dplyr::filter(planned_dev == TRUE)
  # get just the base districts with geometry
  # this is the one I will use for most of the checks
  zoning_sf <- zoning_all_sf |>
    dplyr::filter(!sf::st_is_empty(geometry)) |>
    dplyr::filter(overlay == FALSE) |>
    dplyr::filter(planned_dev == FALSE)

  #zoning_data is the json list form of the zoning file
  zoning_data <- rjson::fromJSON(file = zoning_file)

  # get appropriate crs in meters to use in the check footprint function
  crs <- zr_get_crs(zoning_sf)

  ## TIDYPARCELS ##
  # separate the parcel data into two special feature data frames
  parcel_geo <- zr_get_parcel_geo(parcels_file) # parcels with side labels
  parcel_dims <- zr_get_parcel_dims(parcels_file) # parcels with centroid and dimensions


  ## GET DISTRICT INDICES ##
  # use the base zoning districts to add zoning_id to parcel_dims
  parcel_dims <- zr_find_district_idx(parcel_dims, zoning_sf, "zoning_id")

  # add false_reasons and maybe_reasons columns to parcel_dims (for tracking maybs and falses)
  # this parcel_df is what we will use for most of the calculations
  parcel_df <- parcel_dims |>
    dplyr::mutate(false_reasons = as.character(NA),
                  maybe_reasons = as.character(NA))



  # start a list that will store the false data frames of the check functions
  false_df <- list()
  false_df_idx <- 1
  ########----END DATA PREP----########

  ########----START CHECKS----########
  # PLANNED DEVELOPMENT CHECK
  # if parcels are in a planned development, the building is automatically not allowed
  if (nrow(pd_districts) > 0){ # if there are pd_districts
    # make a new df with the pd district indexes
    tidyparcel_pd <- zr_find_district_idx(parcel_dims, pd_districts, "pd_id") |>
      dplyr::filter(!is.na(pd_id))

    pd_parcels <- unique(tidyparcel_pd$parcel_id)

    parcel_df <- parcel_df |>
      dplyr::mutate(check_pd = ifelse(parcel_id %in% pd_parcels, FALSE, TRUE),
                    false_reasons = ifelse(parcel_id %in% pd_parcels, ifelse(!is.na(false_reasons),paste(false_reasons, "in planned development district", sep = ", "),"in planned development district"), false_reasons))

    # if detailed_check == FALSE, then we store the FALSE parcels in a list to be combined later
    # we filter the parcel_df to have just the TRUEs and MAYBEs
    # so it will be small for the next checks
    if (detailed_check == FALSE){
      tidyparcel_false <- parcel_df |>
        dplyr::filter(check_pd == FALSE)
      # Add the tidyparcel_false to the false_df list
      false_df[[false_df_idx]] <- tidyparcel_false
      false_df_idx <- false_df_idx + 1

      parcel_df <- parcel_df |>
        dplyr::filter(check_pd == TRUE)
    }
  }

  # LAND USE CHECK
  # perform land use check
  if (run_check_land_use == TRUE){
    lu_start_time <- proc.time()[[3]]
    # land use for each district
    lu_check <- c()

    # for this one, we just need to loop through each district to check the land use.
    # then any parcels in those unacceptable districts will be marked FALSE
    for (k in 1:nrow(zoning_sf)){
      tidydistrict <- zoning_sf[k, ]
      lu_check <- c(lu_check,check_land_use(tidybuilding, tidydistrict))
    }

    # add a column stating the land_use_check results and any false reasons
    parcel_df <- parcel_df |>
      dplyr::mutate(check_land_use = ifelse(zoning_id %in% which(lu_check == TRUE), TRUE, FALSE),
                    false_reasons = ifelse(zoning_id %in% which(lu_check == TRUE), false_reasons, ifelse(!is.na(false_reasons),paste(false_reasons, "check_land_use", sep = ", "),"check_land_use")))


    # if detailed_check == FALSE, then we store the FALSE parcels in a list to be combined later
    # we filter the parcel_df to have just the TRUEs and MAYBEs
    # so it will be smaller for the next checks
    if (detailed_check == FALSE){
      # filter the tidyparcels that don't allow the land use
      tidyparcel_false <- parcel_df |>
        dplyr::filter(check_land_use == FALSE)
      # Add the tidyparcel_false to the false_df list
      false_df[[false_df_idx]] <- tidyparcel_false
      false_df_idx <- false_df_idx + 1

      parcel_df <- parcel_df |>
        dplyr::filter(check_land_use == TRUE)
    }

    time_lapsed <- proc.time()[[3]] - lu_start_time
    cat(paste0("_____","check_land_use","_____\n"))
    cat(paste0("runtime: ", round(time_lapsed,1), " sec (",round(time_lapsed / 60,2)," min)\n"))
    cat(paste(length(which(parcel_df[,"check_land_use"][[1]] %in% c(TRUE, 'MAYBE'))),"parcels are TRUE or MAYBE\n"))

  }

  # INITIAL CHECKS
  # perform all the initial checks

  # start empty variables to store potential errors and warnings
  errors <- c()
  warnings <- c()
  for (i in 1:nrow(parcel_df)){
    parcel_data <- parcel_df[i,]
    district_data <- zoning_sf[parcel_data$zoning_id,]
    vars <- zr_get_variables(bldg_data, parcel_data, district_data, zoning_data)
    zoning_req <- zr_get_zoning_req(district_data, vars = vars)



    checks_df <- tryCatch({
      zr_check_constraints(vars, zoning_req, initial_checks)
    }, warning = function(w) {
      # code to execute for errors
      paste("Warning in zoning_id",tidyparcel$zoning_id)
    }, error = function(e) {
      # code to execute for errors
      paste("Error in zoning_id",tidyparcel$zoning_id)
    })


    # need to update this so it works with the data frame output.
    if (class(checks_df)[[1]] == "data.frame"){
      if (length(grep("Warning",check)) > 0){
        warnings <- c(warnings, check)
        check <- "MAYBE"
      } else{
        errors <- c(errors,check)
        check <- "MAYBE"
      }
    }

    parcel_df[i, func_name] <- as.character(check)

    # if the check returns FALSE or MAYBE,
    # then write the function name in the reasons column
    if (check == "MAYBE"){
      parcel_df[i,"maybe_reasons"] <- ifelse(is.na(parcel_df[[i,"maybe_reasons"]]), func_name, paste(parcel_df[[i,"maybe_reasons"]], func_name, sep = ", "))
    }

    if (check == FALSE){
      parcel_df[i,"false_reasons"] <- ifelse(is.na(parcel_df[[i,"false_reasons"]]), func_name, paste(parcel_df[[i,"false_reasons"]], func_name, sep = ", "))
    }
  }

  # if detailed_check == FALSE, then we store the FALSE parcels in a list to be combined later
  # we filter the parcel_df to have just the TRUEs and MAYBEs
  # so it will be smalle for the next checks
  if (detailed_check == FALSE){
    tidyparcel_false <- parcel_df[parcel_df[,func_name][[1]] == FALSE,]
    parcel_df <- parcel_df[parcel_df[,func_name][[1]] %in% c(TRUE, "MAYBE"),]
    # Add the tidyparcel_false to the false_df list
    false_df[[false_df_idx]] <- tidyparcel_false
    false_df_idx <- false_df_idx + 1
  }

  for (j in 1:length(check_functions)){ # loop through each check function

    if (check_functions[[j]][[1]]){ #if the function is marked true, then it will run the function

      func_start_time <- proc.time()[[3]] #start time for function time stamps

      # if there are no more parcels to check, it will exit the loop
      if (nrow(parcel_df) == 0){
        break
      }


      # print out info about the function run
      time_lapsed <- proc.time()[[3]] - func_start_time
      cat(paste0("_____",func_name,"_____\n"))
      cat(paste0("runtime: ", round(time_lapsed,1), " sec (",round(time_lapsed / 60,2)," min)\n"))
      cat(paste(length(which(parcel_df[,func_name][[1]] %in% c(TRUE, 'MAYBE'))),"parcels are TRUE or MAYBE\n"))
    }
  }



  # SIDE LABEL CHECK
  # if parcels have labeled sides, we can move on to the footprint check
  if (run_check_fit & nrow(parcel_df) > 0){
    parcels_with_sides <- unique(parcel_geo$parcel_id)

    parcel_df <- parcel_df |>
      dplyr::mutate(check_side_lbl = ifelse(parcel_id %in% parcels_with_sides,TRUE, "MAYBE"),
                    maybe_reasons = ifelse(parcel_id %in% parcels_with_sides, maybe_reasons, ifelse(!is.na(maybe_reasons),paste(maybe_reasons, "no side labels", sep = ", "),"no side labels")))

    tidyparcel_no_sides <- parcel_df |>
      dplyr::filter(!parcel_id %in% parcels_with_sides)

    false_df[[false_df_idx]] <- tidyparcel_no_sides
    false_df_idx <- false_df_idx + 1

    parcel_df <- parcel_df |>
      dplyr::filter(parcel_id %in% parcels_with_sides)
  }


  # FOOTPRINT CHECK
  # see if the building footprint fits in the parcel's buildable area

  if (run_check_fit & nrow(parcel_df) > 0 & !is.null(parcel_geo)){
    foot_start_time <- proc.time()[[3]]
    for (z in 1:nrow(parcel_df)){
      tidyparcel <- parcel_df[z,]
      tidydistrict <- zoning_sf[tidyparcel$zoning_id,]
      zoning_req <- get_zoning_req(tidybuilding, tidydistrict, tidyparcel)

      # if the footprint area is smaller than the parcel area,
      # then run the check_fit function
      if (check_fit_area(tidybuilding, tidyparcel)$check_fit_area[[1]] == TRUE){
        tidyparcel_sides <- parcel_geo |>
          dplyr::filter(parcel_id == tidyparcel$parcel_id)
        parcel_with_setbacks <- add_setbacks(tidyparcel_sides, zoning_req = zoning_req)
        buildable_area <- get_buildable_area(parcel_with_setbacks)

        # if two buildable areas were recorded, we need to test for both
        if (length(buildable_area) > 1){
          check_1 <- check_fit(tidybuilding, sf::st_make_valid(buildable_area[[1]]), crs = crs)

          if (check_1){
            check <- check_1
          } else{
            check_2 <- check_fit(tidybuilding, sf::st_make_valid(buildable_area[[2]]), crs = crs)
            if (check_2){
              check <- "MAYBE"
            } else{
              check <- FALSE
            }
          }

        } else{
          check <- check_fit(tidybuilding, sf::st_make_valid(buildable_area[[1]]), crs = crs)
        }

      } else{
        check <- FALSE
      }

      parcel_df[z, "check_fit"] <- as.character(check)

      # if the check returns FALSE or MAYBE,
      # then write the function name in the reasons column
      if (check == "MAYBE"){
        parcel_df[z,"maybe_reasons"] <- ifelse(is.na(parcel_df[[z,"maybe_reasons"]]), "check_fit", paste(parcel_df[[z,"maybe_reasons"]], "check_fit", sep = ", "))
      }

      if (check == FALSE){
        parcel_df[z,"false_reasons"] <- ifelse(is.na(parcel_df[[z,"false_reasons"]]), "check_fit", paste(parcel_df[[z,"false_reasons"]], "check_fit", sep = ", "))
      }
    }

    # if detailed_check == FALSE, then we store the FALSE parcels in a list to be combined later
    # we filter the parcel_df to have just the TRUEs and MAYBEs
    # so it will be smalle for the next checks
    if (detailed_check == FALSE){
      tidyparcel_false <- parcel_df[parcel_df[,"check_fit"][[1]] == FALSE,]
      parcel_df <- parcel_df[parcel_df[,"check_fit"][[1]] %in% c(TRUE, "MAYBE"),]
      # Add the tidyparcel_false to the false_df list
      false_df[[false_df_idx]] <- tidyparcel_false
      false_df_idx <- false_df_idx + 1
    }

    # print out info about the function run
    time_lapsed <- proc.time()[[3]] - foot_start_time
    cat(paste0("_____","check_fit","_____\n"))
    cat(paste0("runtime: ", round(time_lapsed,1), " sec (",round(time_lapsed / 60,2)," min)\n"))
    cat(paste(length(which(parcel_df[,"check_fit"][[1]] %in% c(TRUE, 'MAYBE'))),"parcels are TRUE or MAYBE\n"))
  }

  # OVERLAY CHECK
  # of the parcels that pass all the checks,
  # the ones in an overlay district will be marked as "MAYBE"
  if (nrow(overlays) > 0){ # if there are pd_districts
    # make a new df with the overlay district indexes
    tidyparcel_overlays <- find_district_idx(parcel_dims, overlays, "overlay_id") |>
      dplyr::filter(!is.na(overlay_id))

    overlay_parcels <- unique(tidyparcel_overlays$parcel_id)

    parcel_df <- parcel_df |>
      dplyr::mutate(check_overlay = ifelse(parcel_id %in% overlay_parcels,"MAYBE", TRUE),
                    maybe_reasons = ifelse(parcel_id %in% overlay_parcels, ifelse(!is.na(maybe_reasons),paste(maybe_reasons, "parcel in overlay district", sep = ", "),"parcel in overlay district"), maybe_reasons))
  }
  ########----END CHECKS----########


  ########----FINALIZING THINGS----########
  # combind all the false_df and the parcel_df
  class(parcel_df$false_reasons) <- "character"
  class(parcel_df$maybe_reasons) <- "character"
  final_df <- dplyr::bind_rows(false_df, parcel_df)
  final_without_geom <- sf::st_drop_geometry(final_df)
  final_df$has_false <- rowSums(final_without_geom == FALSE, na.rm = T)
  final_df$has_maybe <- rowSums(final_without_geom == "MAYBE", na.rm = T)
  # add the "allowed" and "reason" columns
  final_df <- final_df |>
    dplyr::mutate(allowed = ifelse(has_false > 0, FALSE, ifelse(has_maybe > 0, "MAYBE",TRUE)),
                  reason = ifelse(!is.na(maybe_reasons) | !is.na(false_reasons),
                                  paste("FALSE encountered:", false_reasons, "- MAYBE encountered:", maybe_reasons),
                                  "The building is allowed in the parcel")) |>
    dplyr::select(!c("has_false","has_maybe"))

  # select only the columns needed depending on whether detailed check is TRUE or FALSE
  if (detailed_check == FALSE){
    final_df <- final_df |>
      dplyr::select(any_of(c("parcel_id",
                              "allowed",
                              "reason",
                              "geometry")))
  } else{
    final_df <- final_df |>
      dplyr::select(!any_of(c("maybe_reasons",
                              "false_reasons",
                              "lot_width",
                              "lot_depth",
                              "lot_area",
                              "lot_type",
                              "zoning_id",
                              "pd_id",
                              "overlay_id")))
  }


  ## DEALING WITH DUPLICATE PARCEL_IDs ##
  # these are the few parcels that had two districts overlapping

  # get duplicate parcel_id names
  duplicates <- unique(final_df$parcel_id[duplicated(final_df$parcel_id)])

  if (length(duplicates) > 0){
    # loop through each duplicated parcel_id
    new_dfs <- list()
    length(new_dfs) <- length(duplicates)
    for (i in 1:length(duplicates)){
      id <- duplicates[[i]]

      # filter to just the first duplicate ids
      new_df <- final_df |>
        dplyr::filter(parcel_id == id)

      # make a vector of all the allowed values
      allowed_vals <- new_df$allowed

      # if all duplicates are TRUE, then it is still TRUE
      # if all duplicates are FALSE, then it is still FALSE
      # if any other combination, it is MABYE
      if (sum(allowed_vals == TRUE) == length(allowed_vals)){
        val <- TRUE
      } else if (sum(allowed_vals == FALSE) == length(allowed_vals)){
        val <- FALSE
      } else{
        val <- "MAYBE"
      }

      # this just groups the rows so I can combine the reason
      updated <- new_df |>
        dplyr::group_by(parcel_id) |>
        dplyr::summarise(allowed = val,
                         reason = paste(reason,collapse = " ---||--- "))

      new_reason <- updated$reason

      # make a new df with just one row for the parcel_id
      updated_df <- new_df[1,]
      updated_df[1,"allowed"] <- as.character(val)
      updated_df[1,"reason"] <- new_reason
      # add that df to a list of the combined parcel_id dfs
      new_dfs[[i]] <- updated_df

    }

    # make one df out of all the combined parcel_id dfs
    combined_duplicates <- dplyr::bind_rows(new_dfs)

    # take out the old duplicated parcel_id rows
    final_df <- final_df |>
      dplyr::filter(!parcel_id %in% duplicates)

    # add the new combined parcel_id rows
    final_df <- rbind(final_df, combined_duplicates)
  }


  ## RUN STATISTICS ##
  # report total runtime and other statistics
  total_time <- proc.time()[[3]] - total_start_time
  cat("_____summary_____\n")
  cat(paste0("total runtime: ", round(total_time,1), " sec (",round(total_time / 60,2)," min)\n"))
  cat(paste(length(which(final_df$allowed == TRUE)), "/", nrow(final_df), "parcels allow the building\n"))
  cat(paste(length(which(final_df$allowed == "MAYBE")), "/", nrow(final_df), "parcels might allow the building\n"))

  # Return the final data frame
  # It will contain every parcel with an "allowed" column and a "reason" column
  return(final_df)

}

# bldg_file <- "../personal_rpoj/tidyzoning2.0/tidybuildings/2_fam.bldg"
# parcels_file <- "../personal_rpoj/tidyzoning2.0/tidyparcels/Cockrell Hill.parcel"
# zoning_file <- "../personal_rpoj/tidyzoning2.0/tidyzonings/Cockrell Hill.zoning"
# detailed_check <- TRUE
# run_check_land_use <- TRUE
# run_check_height <- TRUE
# run_check_height_eave <- TRUE
# run_check_floors <- TRUE
# run_check_unit_size <- TRUE
# run_check_far <- TRUE
# run_check_unit_density <- TRUE
# run_check_lot_coverage <- TRUE
# run_check_fl_area <- TRUE
# run_check_unit_qty <- TRUE
# run_check_lot_size <- TRUE
# run_check_parking_enclosed <- TRUE
# run_check_fit <- FALSE
# crs <- 3081
#
#
#
# ggplot2::ggplot(final_df) +
#   ggplot2::geom_sf(ggplot2::aes(color = allowed)) +
#   ggplot2::geom_sf(data = parcel_geo)
#
# ggplot2::ggplot(tidyzoning) +
#   ggplot2::geom_sf(ggplot2::aes(fill = dist_abbr),alpha = .6) +
#   ggplot2::geom_sf(data = parcel_geo) +
#   ggplot2::geom_sf(data = final_df, ggplot2::aes(color = allowed))
#
# df <- zoning_analysis_pipline(bldg_file,
#                                parcels_file,
#                                zoning_file,
#                                detailed_check = TRUE,
#                                run_check_land_use = TRUE,
#                                run_check_height = TRUE,
#                                run_check_height_eave = TRUE,
#                                run_check_floors = TRUE,
#                                run_check_unit_size = TRUE,
#                                run_check_far = TRUE,
#                                run_check_unit_density = TRUE,
#                                run_check_lot_coverage = TRUE,
#                                run_check_fl_area = TRUE,
#                                run_check_unit_qty = TRUE,
#                                run_check_fit = FALSE)
#
# ggplot2::ggplot(df) +
#   ggplot2::geom_sf(ggplot2::aes(color = allowed))
#
#
# type_list <- list()
# length(type_list) <- nrow(parcel_df)
# for (z in 1:nrow(parcel_df)){
#   tidyparcel <- parcel_df[z,]
#   tidydistrict <- tidyzoning[tidyparcel$zoning_id,]
#   zoning_req <- get_zoning_req(tidybuilding, tidydistrict, tidyparcel)
#   if (check_fit_area(tidybuilding, tidyparcel)$check_fit_area[[1]] == TRUE){
#     tidyparcel_sides <- parcel_geo |>
#       dplyr::filter(parcel_id == tidyparcel$parcel_id)
#     parcel_with_setbacks <- add_setbacks(tidyparcel_sides, zoning_req = zoning_req)
#     buildable_area <- get_buildable_area(parcel_with_setbacks)
#
#     type_list[[z]] <- class(buildable_area)
#
#
#   }
# }
# unique(type_list)
