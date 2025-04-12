#' Check if footprint fits in buildable area
#'
#' `check_footprint()` takes a building footprint from tidybuilding object and checks to see if it will fit in the provided buildable area. The buildable area can be any shape.
#'
#' @inheritParams add_setbacks
#' @inheritParams check_footprint_simple
#' @param num_rotations The number of rotations you wish to spin the building footprint. More rotations will be more accurate, but will cost significantly more time.
#' Ex: if `num_rotations = 4` then the building footprint will be turned 90 degrees each time until it finds where it fits.
#' Ex: if `num_rotations = 24` then the building footprint will be turned 15 degrees each time until it finds where it fits.
#'
#' @return
#' Returns TRUE of FALSE stating whether or not the building footprint would fit in the buildable area.
#' @export
#'
check_footprint <- function(tidybuilding, buildable_area, num_rotations = 24){
  # change crs so it is working in feet
  # (we may want to change this someday)
  if (is.na(st_crs(buildable_area)) & is.na(st_crs(tidybuilding))){
    tidybuilding <- tidybuilding
    buildable_area <- buildable_area
  } else if (is.na(st_crs(buildable_area))){
    tidybuilding <- st_transform(tidybuilding, 2277)
    buildable_area <- st_set_crs(buildable_area, 2277)
  } else if (is.na(st_crs(tidybuilding))){
    tidybuilding <- st_set_crs(tidybuilding, 2277)
    buildable_area <- st_transform(buildable_area, 2277)
  } else{
    tidybuilding <- st_transform(tidybuilding, 2277)
    buildable_area <- st_transform(buildable_area, 2277)
  }

  area_tb <- as.numeric(st_area(tidybuilding)) # area of the tidybuilding footprint
  area_ba <- as.numeric(st_area(buildable_area)) # area of the buildable area

  # make sure the building isn't bigger than the parcel
  if (area_tb > area_ba){
    return(FALSE)
  }

  # rasterize the buildable_area
  # (resolution 3 if it is a big parcel, but 1 if it is smaller)
  if (area_ba > 3000){
    res <- 3
  } else{
    res <- 1
  }

  raster_template <- rast(ext(st_as_sf(buildable_area)), resolution = res)
  build_area_raster <- rasterize(st_as_sf(buildable_area), raster_template,
                                 touches = TRUE) |>
    as.data.frame(xy = TRUE)

  rot_degrees <- seq(0,360,360 / num_rotations)
  # do the process and then rotate the footprint if it doesn't work
  for (j in 1:(length(rot_degrees) - 1)){
    tidybuilding <- rotate_shape(tidybuilding, rot_degrees[j])

    # get a point from tidybuilding outline
    building_points <- tidybuilding[,"geometry"] |>
      st_cast("LINESTRING") |>
      st_cast("POINT")

    one_bldg_point <- building_points[3,]

    # loop through each pixel until it is inside
    for (i in 1:nrow(build_area_raster)){
      footprint <- tidybuilding[,"geometry"]
      pixel_loc <- c(build_area_raster[[i,"x"]], build_area_raster[[i,"y"]])

      # find difference between point and pixel
      diff <- st_geometry(one_bldg_point) - pixel_loc

      # move the tidybuilding
      translated_geometry <- st_geometry(footprint) - diff
      st_geometry(footprint) <- translated_geometry

      # see if the buildable area covers the tidybuilding
      covers <- st_covers(buildable_area, st_set_crs(footprint,st_crs(buildable_area)), sparse = F)

      # change output to TRUE once it is inside the buildable area
      if (covers[[1]] == TRUE){
        return(TRUE)
      }
    }
  }
  return(FALSE)
}


# fits <- function(mask, width, depth) {
#   # Function to check if a rectangle of width x depth can fit inside the rasterized polygon described by 'mask'
#
#   for (x in seq_len(nrow(mask))) {  # Iterate over rows
#     for (y in seq_len(ncol(mask))) {  # Iterate over columns
#       if (mask[x, y]) {  # Check if the position is True
#         # Check original orientation
#         if ((x + width - 1) <= nrow(mask) && (y + depth - 1) <= ncol(mask)
#             && all(mask[x:(x + width - 1), y:(y + depth - 1)])) {
#           return(TRUE)
#         }
#
#         # Check rotated orientation
#         if ((x + depth - 1) <= nrow(mask) && (y + width - 1) <= ncol(mask)
#             && all(mask[x:(x + depth - 1), y:(y + width - 1)])) {
#           return(TRUE)
#         }
#       }
#     }
#   }
#
#   return(FALSE)
# }
