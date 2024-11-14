check_footprint <- function(tidybuilding, buildable_area){
  # change crs?
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

  if (st_area(tidybuilding) > st_area(buildable_area)){
    return(FALSE)
  }

  # rasterize the buildable_area (resolution 3 so it is about every yard)
  # I MIGHT WANT TO CHANGE THE RESOLUTION DEPENDING ON HOW BIG THE PARCEL IS????
  raster_template <- rast(ext(st_as_sf(buildable_area)), resolution = 1)
  build_area_raster <- rasterize(st_as_sf(buildable_area), raster_template,
                                 touches = TRUE) |>
    as.data.frame(xy = TRUE)
  # get a point from tidybuilding outline
  building_points <- tidybuilding[,"geometry"] |>
    st_cast("LINESTRING") |>
    st_cast("POINT")

  one_bldg_point <- building_points[3,]

  # make output == FALSE
  output <- FALSE

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
      output <- TRUE
      break
    }
  }

  # return the output (NEED TO ROTATE IT IF OUTPUT IS FALSE)
  output

}
