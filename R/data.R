#' A 4-family residence as a tidyzoning object
#'
#' This was created based on observations of a typical 4-family residence
#' with a square-like footprint.
#'
#' @format
#' A special features object with 1 row and 7 columns:
#' \describe{
#'   \item{units_3bed}{Number of 3-bedroom units in the building}
#'   \item{total_floors}{Floor count of the building}
#'   \item{max_unit_size}{Square footage of the smallest unit in the building}
#'   \item{min_unit_size}{Square footage of the biggest unit in the building}
#'   \item{floor_area}{Total floor area of the building}
#'   \item{building_height}{Building height}
#'   \item{geometry}{Building footprint geometry}
#'
#' }
# "tidybuilding_4fam_wide"

#' A 4-family residence as a tidyzoning object
#'
#' This was created based on observations of a typical 4-family residence
#' with a rectangular footprint.
#'
#' @format
#' A special features object with 1 row and 7 columns:
#' \describe{
#'   \item{units_2bed}{Number of 2-bedroom units in the building}
#'   \item{total_floors}{Floor count of the building}
#'   \item{max_unit_size}{Square footage of the smallest unit in the building}
#'   \item{min_unit_size}{Square footage of the biggest unit in the building}
#'   \item{floor_area}{Total floor area of the building}
#'   \item{building_height}{Building height}
#'   \item{geometry}{Building footprint geometry}
#'
#' }
# "tidybuilding_4fam_thin"

#' State Planes with Projected CRS Data
#'
#' This data set contains the geometry for each US State Plane Zone
#' along with the EPSG code for its projected coordinate system.
#' The geometry was gathered from an arcgis hub data set, and
#' the epsg codes were gathered from epsg.io
#'
#' @format ## `state_planes_crs`
#' Simple feature collection with 121 features and 4 fields
#' \describe{
#'   \item{OBJECTID}{Row Index}
#'   \item{ZONE}{Zone Abbreviation}
#'   \item{ZONENAME}{Zone Name}
#'   \item{EPSG_NAD83}{Projected CRS EPSG Code}
#'   \item{geometry}{Zone Geometry}
#'
#' }
#' @source <https://hub.arcgis.com/datasets/esri::usa-state-plane-zones-nad83/explore>
#' @source <https://epsg.io/>
"state_planes_crs"
