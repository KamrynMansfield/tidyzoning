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
                                    run_check_footprint = FALSE){

  # separate the parcel data into two sf data frames
  tidyparcel_geo <- get_tidyparcel_geo(parcels_file)
  tidyparcel_dims <- get_tidyparcel_dim(parcels_file)

  # add zoning_id to tidyparcel_dims
  tidyparcel_dims <- find_district_idx(tidyparcel_dims, zoning_sf)

  # get building summary data frame with unify_tidybuilding
  tidybuilding <- unify_tidybuilding(bldg_file)

  # get the ozfs data as an sf data frame
  zoning_sf <- sf::st_read(ozfs_zoning_file, quiet = TRUE)

  # Now we have
  # - tidyparcel_geo: this is used for the buildable area calculation later
  # - tidyparcel_dims: this is used in all the check functions
  # - tidybuilding: this is used in all the check functions
  # - zoning_sf: this is used in all the check functions


  # PLANNED DEVELOPMENT CHECK
  # if parcels are in a planned development, the building is automatically not allowed


  # INITIAL CHECKS
  # perform all the initial checks


  # SIDE LABEL CHECK
  # if parcels have labeled sides, we can move on to the footprint check


  # FOOTPRINT CHECK
  # see if the building footprint fits in the parcel's buildable area


  # OVERLAY CHECK
  # of the parcels that pass all the checks,
  # the ones in an overlay district will be marked as "MAYBE"


}
