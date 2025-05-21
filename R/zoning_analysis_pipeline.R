#' Complete analysis to find where the building fits
#'
#' `zoning_analysis_pipline()` runs through all of the zoning checks to see which parcels
#' allow a certain building.
#'
#' @param bldg_file The path to the json file representing a building
#' @param parcels_file The path to the geojson file representing the parcels
#' @param ozfs_zoning_file The path to the geojson file with the ozfs zoning codes.
#' @param detailed_check When TRUE, every parcel passes through each check no matter the result, and it take more time. When FALSE, subsequent checks are skipped as soon as one check reads FALSE
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
