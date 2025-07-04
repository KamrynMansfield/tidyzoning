#' Check if footprint fits in buildable area
#'
#' `check_footprint()` takes a building footprint from tidybuilding object and checks to see if it will fit in the provided buildable area. The buildable area can be any shape.
#'
#' @param tidybuilding A list of data frames with attributes representing a building.
#' @param buildable_area A geometry. Usually of the parcels buildable area calculated from the setback requirements.
#' @param crs The projected Coordinate Reference System for the study area. Must be in meters.
#' @return
#' Returns TRUE of FALSE stating whether or not the building footprint would fit in the buildable area.
#' @export
#'
check_fit <- function(tidybuilding, buildable_area, crs = 3081){
  width <- tidybuilding$width * 0.3048
  depth <- tidybuilding$depth * 0.3048

  rot_degrees <- seq(0,75, 15)
  # do the process and then rotate the footprint if it doesn't work

  # put it into the correct crs
  buildable_area <- sf::st_transform(buildable_area, crs)

  if (length(buildable_area) < 1){
    return(FALSE)
  }

  for (j in 1:length(rot_degrees)){


    buildable_area_vect <- terra::vect(buildable_area)

    raster_template <- terra::rast(buildable_area_vect, resolution = 1, crs = terra::crs(buildable_area_vect))
    build_area_raster <- terra::rasterize(buildable_area_vect,
                                   raster_template,
                                   field = 1,
                                   touches = TRUE)

    mask <- !is.na(build_area_raster)


    for (x in 1:nrow(mask)) {  # Iterate over rows
      for (y in 1:ncol(mask)) {  # Iterate over columns
        if (mask[x, y][[1]]) {  # Check if the position is True
          # Check original orientation
          if ((x + width - 1) <= nrow(mask) && (y + depth - 1) <= ncol(mask) && all(mask[x:(x + width - 1), y:(y + depth - 1)])){
            return(TRUE)
          }

          # Check rotated orientation
          if ((x + depth - 1) <= nrow(mask) && (y + width - 1) <= ncol(mask) && all(mask[x:(x + depth - 1), y:(y + width - 1)])){
            return(TRUE)
          }
        }
      }
    }

    if (j != length(rot_degrees)){
      buildable_area <- rotate_shape(buildable_area, 15)
    }

  }
  return(FALSE)
}
