#' Complete analysis to find where the building fits
#'
#' `zoning_analysis_pipline()` runs through all of the zoning checks to see which parcels
#' allow a certain building.
#'
#' @param bldg_file The path to the json file representing a building
#' @param parcels_file The path to the geojson file representing the parcels
#' @param ozfs_zoning_file The path to the geojson file with the ozfs zoning codes.
#' @param detailed_check When TRUE, every parcel passes through each check no matter the result,
#' and it take more time. When FALSE, subsequent checks are skipped as soon as one check reads FALSE
#' @param run_check_land_use Should the analysis run the check_land_use function? (logical)
#' @param run_check_height Should the analysis run the check_height function? (logical)
#' @param run_check_height_eave Should the analysis run the check_height_eave function? (logical)
#' @param run_check_floors Should the analysis run the check_floors function? (logical)
#' @param run_check_unit_size Should the analysis run the check_unit_size function? (logical)
#' @param run_check_far Should the analysis run the check_far function? (logical)
#' @param run_check_unit_density Should the analysis run the check_unit_density function? (logical)
#' @param run_check_lot_coverage Should the analysis run the check_lot_coverage function? (logical)
#' @param run_check_fl_area Should the analysis run the check_fl_area function? (logical)
#' @param run_check_unit_qty Should the analysis run the check_unit_qty function? (logical)
#' @param run_check_footprint Should the analysis run the check_footprint function? (logical)
#' @param crs_m Projected Coordinate Reference System (in meters) for your study area.
#'
#' @returns a simple features data frame with the centroid of each parcel with a column
#' stating building allowance on the parcel and a column stating the reason
#' why certain parcels don't allow the building.
#' @export
#'
#' @examples
zoning_analysis_pipline <- function(bldg_file,
                                    parcels_file,
                                    ozfs_zoning_file,
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
                                    run_check_footprint = FALSE,
                                    crs_m = 3081){
  # track the start time to give a time stamp at end
  total_start_time <- proc.time()[[3]]

  ########---- START DATA PREP----########
  ## TIDYBUILDING ##
  # get building summary data frame with unify_tidybuilding
  tidybuilding <- unify_tidybuilding(bldg_file, ozfs_zoning_file)

  ## TIDYZONING ##
  # get the full ozfs data as an sf data frame
  tidyzoning_full <- sf::st_read(ozfs_zoning_file, quiet = TRUE)
  # get just the overlay districts with geometry
  overlays <- tidyzoning_full |>
    dplyr::filter(!sf::st_is_empty(geometry)) |>
    dplyr::filter(overlay == TRUE)
  # get just the pd_districts with geometry
  pd_districts <- tidyzoning_full |>
    dplyr::filter(!sf::st_is_empty(geometry)) |>
    dplyr::filter(planned_dev == TRUE)
  # get just the base districts with geometry
  # this is the one I will use for most of the checks
  tidyzoning <- tidyzoning_full |>
    dplyr::filter(!sf::st_is_empty(geometry)) |>
    dplyr::filter(overlay == FALSE) |>
    dplyr::filter(planned_dev == FALSE)


  ## TIDYPARCELS ##
  # separate the parcel data into two special feature data frames
  tidyparcel_geo <- get_tidyparcel_geo(parcels_file) # parcels with side labels
  tidyparcel_dims <- get_tidyparcel_dim(parcels_file) # parcels with centroid and dimensions


  ## GET DISTRICT INDICES ##
  # use the base zoning districts to add zoning_id to tidyparcel_dims
  tidyparcel_dims <- find_district_idx(tidyparcel_dims, tidyzoning, "zoning_id")

  # if there are pd_districts, add their zoning id in a pd_id column
  if (nrow(pd_districts) > 0){
    tidyparcel_dims <- find_district_idx(tidyparcel_dims, pd_districts, "pd_id")
  }

  # if there are overlay districts, add their zoning id in an overlay_id column
  if (nrow(overlays) > 0){
    tidyparcel_dims <- find_district_idx(tidyparcel_dims, overlays, "overlay_id")
  }

  # add false_reasons and maybe_reasons columns to tidyparcel_dims (for tracking maybs and falses)
  # this tidyparcel_df is what we will use for most of the calculations
  tidyparcel_df <- tidyparcel_dims |>
    dplyr::mutate(false_reasons = as.character(NA),
                  maybe_reasons = as.character(NA))


  ## VARIABLES WE HAVE ##
  # - tidyparcel_df: this is used in all the check functions
  # - tidybuilding: this is used in all the check functions
  # - tidyzoning: this is used in all the check functions
  # - tidyparcel_geo: this is used for the buildable area calculation later


  # start a list that will store the false data frames of the check functions
  false_df <- list()
  false_df_idx <- 1
  ########----END DATA PREP----########

  ########----START CHECKS----########
  # PLANNED DEVELOPMENT CHECK
  # if parcels are in a planned development, the building is automatically not allowed
  if (!is.null(tidyparcel_df$pd_id)){ #find the parcels that have an index for a planned development district
    tidyparcel_df <- tidyparcel_df |>
      dplyr::mutate(check_pd = ifelse(is.na(pd_id), TRUE, FALSE),
                    false_reasons = ifelse(is.na(pd_id), false_reasons, ifelse(!is.na(false_reasons),paste(false_reasons, "in planned development district", sep = ", "),"in planned development district")))

    # if detailed_check == FALSE, then we store the FALSE parcels in a list to be combined later
    # we filter the tidyparcel_df to have just the TRUEs and MAYBEs
    # so it will be smalle for the next checks
    if (detailed_check == FALSE){
      tidyparcel_false <- tidyparcel_df |>
        dplyr::filter(check_pd == FALSE)
      # Add the tidyparcel_false to the false_df list
      false_df[[false_df_idx]] <- tidyparcel_false
      false_df_idx <- false_df_idx + 1

      tidyparcel_df <- tidyparcel_df |>
        dplyr::filter(is.na(pd_id))
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
    for (k in 1:nrow(tidyzoning)){
      tidydistrict <- tidyzoning[k, ]
      lu_check <- c(lu_check,check_land_use(tidybuilding, tidydistrict))
    }

    # add a column stating the land_use_check results and any false reasons
    tidyparcel_df <- tidyparcel_df |>
      dplyr::mutate(check_land_use = ifelse(zoning_id %in% which(lu_check == TRUE), TRUE, FALSE),
                    false_reasons = ifelse(zoning_id %in% which(lu_check == TRUE), false_reasons, ifelse(!is.na(false_reasons),paste(false_reasons, "check_land_use", sep = ", "),"check_land_use")))


    # if detailed_check == FALSE, then we store the FALSE parcels in a list to be combined later
    # we filter the tidyparcel_df to have just the TRUEs and MAYBEs
    # so it will be smaller for the next checks
    if (detailed_check == FALSE){
      # filter the tidyparcels that don't allow the land use
      tidyparcel_false <- tidyparcel_df |>
        dplyr::filter(check_land_use == FALSE)
      # Add the tidyparcel_false to the false_df list
      false_df[[false_df_idx]] <- tidyparcel_false
      false_df_idx <- false_df_idx + 1

      tidyparcel_df <- tidyparcel_df |>
        dplyr::filter(check_land_use == TRUE)
    }

    time_lapsed <- proc.time()[[3]] - lu_start_time
    cat(paste0("_____","check_land_use","_____\n"))
    cat(paste0("runtime: ", round(time_lapsed,1), " sec (",round(time_lapsed / 60,2)," min)\n"))
    cat(paste(length(which(tidyparcel_df[,"check_land_use"][[1]] %in% c(TRUE, 'MAYBE'))),"parcels are TRUE or MAYBE\n"))

  }

  # INITIAL CHECKS
  # perform all the initial checks

  # We will use this variable to loop through each check function that is marked TRUE
  check_functions <- c(run_check_height,
                       run_check_height_eave,
                       run_check_floors,
                       run_check_unit_size,
                       run_check_far,
                       run_check_unit_density,
                       run_check_lot_coverage,
                       run_check_fl_area,
                       run_check_unit_qty)

  # start empty variables to store potential errors and warnings
  errors <- c()
  warnings <- c()
  for (j in 1:length(check_functions)){ # loop through each check function
    if (check_functions[[j]]){ #if the function is marked true, then it will run the function

      func_start_time <- proc.time()[[3]] #start time for function time stamps

      # if there are no more parcels to check, it will exit the loop
      if (nrow(tidyparcel_df) == 0){
        break
      }

      for (i in 1:nrow(tidyparcel_df)){
        tidyparcel <- tidyparcel_df[i,]
        tidydistrict <- tidyzoning[tidyparcel$zoning_id,]
        zoning_req <- get_zoning_req(tidybuilding, tidydistrict, tidyparcel)

        # this variable stores the function and its inputs
        # that allows me to use do.call() function to run the correct check function
        funcs <- list(list("check_height",list(tidybuilding = tidybuilding, zoning_req = zoning_req)),
                           list("check_height_eave",list(tidybuilding = tidybuilding, zoning_req = zoning_req)),
                           list("check_floors",list(tidybuilding = tidybuilding, zoning_req = zoning_req)),
                           list("check_unit_size",list(tidybuilding = tidybuilding, tidydistrict = tidydistrict, tidyparcel_dims = tidyparcel, building_json = bldg_file, zoning_req = zoning_req)),
                           list("check_far",list(tidybuilding = tidybuilding, tidyparcel_dims = tidyparcel, zoning_req = zoning_req)),
                           list("check_unit_density",list(tidybuilding = tidybuilding, tidyparcel_dims = tidyparcel, zoning_req = zoning_req)),
                           list("check_lot_coverage",list(tidybuilding = tidybuilding, tidyparcel_dims = tidyparcel, zoning_req = zoning_req)),
                           list("check_fl_area",list(tidybuilding = tidybuilding, zoning_req = zoning_req)),
                           list("check_unit_qty",list(building_json = bldg_file, zoning_req = zoning_req)))

        func_name <- funcs[[j]][[1]]

        check <- tryCatch({
          do.call(funcs[[j]][[1]], funcs[[j]][[2]])
        }, warning = function(w) {
          # code to execute for errors
          paste("Warning in zoning_id",tidyparcel$zoning_id)
        }, error = function(e) {
          # code to execute for errors
          paste("Error in zoning_id",tidyparcel$zoning_id)
        })

        if (!check %in% c(TRUE, FALSE, "MAYBE")){
          if (length(grep("Warning",check)) > 0){
            warnings <- c(warnings, check)
            check <- "MAYBE"
          } else{
            errors <- c(errors,check)
            check <- "MAYBE"
          }
        }

        tidyparcel_df[i, func_name] <- as.character(check)

        # if the check returns FALSE or MAYBE,
        # then write the function name in the reasons column
        if (check == "MAYBE"){
          tidyparcel_df[i,"maybe_reasons"] <- ifelse(is.na(tidyparcel_df[[i,"maybe_reasons"]]), func_name, paste(tidyparcel_df[[i,"maybe_reasons"]], func_name, sep = ", "))
        }

        if (check == FALSE){
          tidyparcel_df[i,"false_reasons"] <- ifelse(is.na(tidyparcel_df[[i,"false_reasons"]]), func_name, paste(tidyparcel_df[[i,"false_reasons"]], func_name, sep = ", "))
        }
      }


      # if detailed_check == FALSE, then we store the FALSE parcels in a list to be combined later
      # we filter the tidyparcel_df to have just the TRUEs and MAYBEs
      # so it will be smalle for the next checks
      if (detailed_check == FALSE){
        tidyparcel_false <- tidyparcel_df[tidyparcel_df[,func_name][[1]] == FALSE,]
        tidyparcel_df <- tidyparcel_df[tidyparcel_df[,func_name][[1]] %in% c(TRUE, "MAYBE"),]
        # Add the tidyparcel_false to the false_df list
        false_df[[false_df_idx]] <- tidyparcel_false
        false_df_idx <- false_df_idx + 1
      }

      # print out info about the function run
      time_lapsed <- proc.time()[[3]] - func_start_time
      cat(paste0("_____",func_name,"_____\n"))
      cat(paste0("runtime: ", round(time_lapsed,1), " sec (",round(time_lapsed / 60,2)," min)\n"))
      cat(paste(length(which(tidyparcel_df[,func_name][[1]] %in% c(TRUE, 'MAYBE'))),"parcels are TRUE or MAYBE\n"))
    }
  }



  # SIDE LABEL CHECK
  # if parcels have labeled sides, we can move on to the footprint check
  if (run_check_footprint & nrow(tidyparcel_df) > 0){
    parcels_with_sides <- unique(tidyparcel_geo$parcel_id)

    tidyparcel_df <- tidyparcel_df |>
      dplyr::mutate(check_side_lbl = ifelse(parcel_id %in% parcels_with_sides,TRUE, "MAYBE"),
                    maybe_reasons = ifelse(parcel_id %in% parcels_with_sides, maybe_reasons, ifelse(!is.na(maybe_reasons),paste(maybe_reasons, "no side labels", sep = ", "),"no side labels")))

    tidyparcel_no_sides <- tidyparcel_df |>
      dplyr::filter(!parcel_id %in% parcels_with_sides)

    false_df[[false_df_idx]] <- tidyparcel_no_sides
    false_df_idx <- false_df_idx + 1

    tidyparcel_df <- tidyparcel_df |>
      dplyr::filter(parcel_id %in% parcels_with_sides)
  }


  # FOOTPRINT CHECK
  # see if the building footprint fits in the parcel's buildable area

  if (run_check_footprint & nrow(tidyparcel_df) > 0 & !is.null(tidyparcel_geo)){
    foot_start_time <- proc.time()[[3]]
    for (z in 1:nrow(tidyparcel_df)){
      tidyparcel <- tidyparcel_df[z,]
      tidydistrict <- tidyzoning[tidyparcel$zoning_id,]
      zoning_req <- get_zoning_req(tidybuilding, tidydistrict, tidyparcel)

      # if the footprint area is smaller than the parcel area,
      # then run the check_footprint function
      if (check_footprint_area(tidybuilding, tidyparcel)$check_footprint_area[[1]] == TRUE){
        tidyparcel_sides <- tidyparcel_geo |>
          dplyr::filter(parcel_id == tidyparcel$parcel_id)
        parcel_with_setbacks <- add_setbacks(tidyparcel_sides, zoning_req = zoning_req)
        buildable_area <- get_buildable_area(parcel_with_setbacks)

        # if two buildable areas were recorded, we need to test for both
        if (length(buildable_area) > 1){
          check_1 <- check_footprint(tidybuilding, buildable_area[[1]], crs = crs_m)

          if (check_1){
            check <- check_1
          } else{
            check_2 <- check_footprint(tidybuilding, buildable_area[[2]], crs = crs_m)
            if (check_2){
              check <- "MAYBE"
            } else{
              check <- FALSE
            }
          }

        } else{
          check <- check_footprint(tidybuilding, buildable_area[[1]], crs = crs_m)
        }

      } else{
        check <- FALSE
      }

      tidyparcel_df[z, "check_footprint"] <- as.character(check)

      # if the check returns FALSE or MAYBE,
      # then write the function name in the reasons column
      if (check == "MAYBE"){
        tidyparcel_df[z,"maybe_reasons"] <- ifelse(is.na(tidyparcel_df[[z,"maybe_reasons"]]), "check_footprint", paste(tidyparcel_df[[z,"maybe_reasons"]], "check_footprint", sep = ", "))
      }

      if (check == FALSE){
        tidyparcel_df[z,"false_reasons"] <- ifelse(is.na(tidyparcel_df[[z,"false_reasons"]]), "check_footprint", paste(tidyparcel_df[[z,"false_reasons"]], "check_footprint", sep = ", "))
      }
    }

    # if detailed_check == FALSE, then we store the FALSE parcels in a list to be combined later
    # we filter the tidyparcel_df to have just the TRUEs and MAYBEs
    # so it will be smalle for the next checks
    if (detailed_check == FALSE){
      tidyparcel_false <- tidyparcel_df[tidyparcel_df[,"check_footprint"][[1]] == FALSE,]
      tidyparcel_df <- tidyparcel_df[tidyparcel_df[,"check_footprint"][[1]] %in% c(TRUE, "MAYBE"),]
      # Add the tidyparcel_false to the false_df list
      false_df[[false_df_idx]] <- tidyparcel_false
      false_df_idx <- false_df_idx + 1
    }

    # print out info about the function run
    time_lapsed <- proc.time()[[3]] - foot_start_time
    print(paste0("_____","check_footprint","_____"))
    print(paste0("runtime: ", round(time_lapsed,1), " sec (",round(time_lapsed / 60,2)," min)"))
    print(paste(length(which(tidyparcel_df[,"check_footprint"][[1]] %in% c(TRUE, 'MAYBE'))),"parcels are TRUE or MAYBE"))
  }

  # OVERLAY CHECK
  # of the parcels that pass all the checks,
  # the ones in an overlay district will be marked as "MAYBE"

  if (!is.null(tidyparcel_df$overlay_id)){ # make sure there are overlays
    tidyparcel_df <- tidyparcel_df |>
      dplyr::mutate(check_overlay = ifelse(is.na(overlay_id),TRUE, "MAYBE"),
                    maybe_reasons = ifelse(is.na(overlay_id), maybe_reasons, ifelse(!is.na(maybe_reasons),paste(maybe_reasons, "parcel in overlay district", sep = ", "),"parcel in overlay district")))
  }
  ########----END CHECKS----########


  ########----FINALIZING THINGS----########
  # combind all the false_df and the tidyparcel_df
  final_df <- dplyr::bind_rows(false_df, tidyparcel_df)
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

# bldg_file <- "../personal_rpoj/tidyzoning2.0/tidybuildings/bldg_2_fam.json"
# parcels_file <- "../personal_rpoj/tidyzoning2.0/tidyparcels/Garland_parcels.geojson"
# ozfs_zoning_file <- "../personal_rpoj/tidyzoning2.0/tidyzonings/Garland.geojson"
#
# ggplot2::ggplot(final_df) +
#   ggplot2::geom_sf(ggplot2::aes(color = allowed)) +
#   ggplot2::geom_sf(data = tidyparcel_geo)
#
# ggplot2::ggplot(tidyzoning) +
#   ggplot2::geom_sf(ggplot2::aes(fill = dist_abbr),alpha = .6) +
#   ggplot2::geom_sf(data = tidyparcel_geo) +
#   ggplot2::geom_sf(data = final_df, ggplot2::aes(color = allowed))
#
# df <- zoning_analysis_pipline(bldg_file,
#                                parcels_file,
#                                ozfs_zoning_file,
#                                detailed_check = FALSE,
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
#                                run_check_footprint = FALSE)
#
#
# type_list <- list()
# length(type_list) <- nrow(tidyparcel_df)
# for (z in 1:nrow(tidyparcel_df)){
#   tidyparcel <- tidyparcel_df[z,]
#   tidydistrict <- tidyzoning[tidyparcel$zoning_id,]
#   zoning_req <- get_zoning_req(tidybuilding, tidydistrict, tidyparcel)
#   if (check_footprint_area(tidybuilding, tidyparcel)$check_footprint_area[[1]] == TRUE){
#     tidyparcel_sides <- tidyparcel_geo |>
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
