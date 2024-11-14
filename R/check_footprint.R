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



#
# ggplot(build_area_raster) +
#   geom_tile(aes(x = x, y = y)) +
#   geom_sf(data = footprint, color = "red4", fill = "red", alpha = .5)
#
#
#
#
#
# tidybuilding <- tidybuilding_ex
# tidyparcel <- tidyparcel_ex[tidyparcel_ex$OBJECTID == 1,]
# tidydistrict <- tidyzoning_ex[11,]
#
# show_shapes(tidybuilding)
#
# parcel_with_setbacks <- add_setbacks(tidyparcel,tidydistrict, tidybuilding)
# buildable_area <- get_buildable_area(parcel_with_setbacks)
#
# show_shapes(buildable_area,parcel_with_setbacks)
#
# show_shapes(tidybuilding)
# show_shapes(buildable_area)
#
# check_footprint(tidybuilding, buildable_area)
#
# show_shapes(shape1, shape2)
#
# system.time(check_footprint(shape2,shape1,4))
# system.time(check_footprint(shape2,shape1))
#

#
# # This is the outside shape
# shape_1 <- st_polygon(list(matrix(c(0,0,
#                                     25,0,
#                                     25,50,
#                                     0,50,
#                                     0,0), byrow = T, ncol = 2)))
#
# # A little smaller than shape_1 and rotatad 90 degrees
# shape_2 <- st_polygon(list(matrix(c(0,0,
#                                     49,0,
#                                     49,24,
#                                     0,24,
#                                     0,0), byrow = T, ncol = 2)))
#
# shape_sf <- st_sf(geometry = st_sfc(shape_1, shape_2))
#
# shape1 <- shape_sf[1,"geometry"]
# shape2 <- shape_sf[2,"geometry"]
#
# show_shapes(shape1,rotate_shape(shape1,120))
#
# st_area(shape2) / st_area(shape1)
#
# st_area(st_transform(tidybuilding,2277))
#







# haltom_ozfs_df <- st_read("../files/atlas_to_ozfs/haltom_ozfs_output.geojson")
# testparcels <- st_read("../files/testparcels2.geojson")
#
# tidybuilding_4fam_thin <- st_read("../../zoning-data/qmd/files/fourplex_tall.geojson") # ready
# tidybuilding_4fam_wide <- st_read("../../zoning-data/qmd/files/fourplex_square.geojson") # ready
# tidybuilding_12fam <- st_read("../../zoning-data/qmd/files/twelveplex_tall.geojson") # ready
# testparcels <- st_read("../../zoning-data/qmd/files/testparcels2.geojson") |> st_transform(4326) # in progress
#
# use_data(tidybuilding_4fam_thin)# ready
# use_data(tidybuilding_4fam_wide)# ready
# use_data(tidybuilding_12fam)# ready
# use_data(tidyparcel_list_haltom)# ready?????
#
#
# list_parcels <- function(testparcels){
#   list_of_parcels <- list()
#   for (i in 1:length(unique(testparcels$OBJECTID))){
#     parcel <- filter(testparcels, OBJECTID == unique(testparcels$OBJECTID)[[i]])
#
#     list_of_parcels[[i]] <- parcel
#   }
#   list_of_parcels
# }
#
# add_centroid <- function(list_parcels){
#   for (i in 1:length(list_parcels)){
#     df <- list_parcels[[i]]
#     new_row <- data.frame(OBJECTID = NA,
#                           side = NA,
#                           geometry = st_centroid(st_union(df))) |>
#       st_sf()
#
#     st_crs(new_row) <- st_crs(df)
#
#     list_parcels[[i]] <- rbind(df, new_row)
#   }
#
#   list_parcels
# }
#
# list_of_parcels <- list_parcels(testparcels)
# tidyparcel_list_haltom <- add_centroid(list_of_parcels)
#
#
# run_func_haltom <- function(tidyparcel){
#   find_district_idx(tidyparcel, tidyzoning_haltom)
# }
#
# idx_list <- lapply(tidyparcel_list_haltom, run_func_haltom)
#
# tidyparcel_list_haltom <- tidyparcel_list_haltom[!is.na(idx_list)]
# length(tidyparcel_list_haltom)

