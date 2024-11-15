get_buildable_area <- function(tidyparcel_with_setbacks){
  # make tidyparcel a polygon
  polygon <- st_polygonize(st_union(tidyparcel_with_setbacks))

  # convert the setback units to meters
  units(tidyparcel_with_setbacks$setback) <- tidyparcel_with_setbacks$units[[1]]
  tidyparcel_with_setbacks <- tidyparcel_with_setbacks |>
    mutate(setback_m = set_units(setback,"m"))

  # put a buffer on each side (need to convert to meters)
  buffered_sides <- tidyparcel_with_setbacks |>
    mutate(geometry = st_buffer(geometry,setback_m,1))

  # make the buffered sides all one polygon
  buffered_polygon <- st_union(buffered_sides)

  # make a new shape the parts not overlapping
  not_overlapping <- st_sym_difference(buffered_polygon, polygon)

  # separate the polygons from the multipolygon
  not_overlapping <- st_cast(not_overlapping, "POLYGON")

  # select the non-overlapping shape that is the buildable area
  buildable_area <- not_overlapping[2]

  # get only the nodes from buildable area that will creat a smooth, accurate line
  parcel_geometries <- parcel_with_setbacks[,"geometry"]
  parcel_nodes <- st_cast(parcel_geometries, "POINT")
  build_area_nodes <- st_cast(buildable_area, "POINT")
  important_nodes <- st_nearest_feature(parcel_nodes, build_area_nodes)

  build_area_nodes[important_nodes] |>
    st_union() |>
    st_cast("MULTILINESTRING") |>
    st_cast("POLYGON")

}
