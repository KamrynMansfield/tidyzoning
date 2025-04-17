#' Create a parcel's buildable area
#'
#' get_buildable_area() takes a tidyparcel object with setback information and produces a polygon representing the builablel area of the parcel.
#'
#' @param tidyparcel_with_setbacks A tidyparcel object that has setback infomration added. tidyparcel_with_setbacks is the output of the [add_setbacks()] function.
#'
#' @return
#' Returns a polygon representing the buildable area of the parcel.
#' @export
#'
get_buildable_area <- function(tidyparcel_with_setbacks){
  # make tidyparcel a polygon
  polygon <- tidyparcel_with_setbacks |>
    st_union() |>
    st_polygonize() |>
    st_collection_extract()

  if (nrow(tidyparcel_with_setbacks[!is.na(tidyparcel_with_setbacks$setback),]) == 0){
    return(polygon)
  }

  # seprate the min and max setbacks
  tidyparcel_with_setbacks$min_setback <- unlist(lapply(tidyparcel_with_setbacks$setback, min))
  tidyparcel_with_setbacks$max_setback <- unlist(lapply(tidyparcel_with_setbacks$setback, max))

  # convert feet to meters
  tidyparcel_with_setbacks <- tidyparcel_with_setbacks |>
    mutate(min_setback = min_setback * 0.3048,
           max_setback = max_setback * 0.3048)

  if (identical(tidyparcel_with_setbacks$min_setback,tidyparcel_with_setbacks$max_setback)){ # just one setback for each side

    # put a buffer on each side (need to convert to meters)
    buffered_sides <- tidyparcel_with_setbacks |>
      mutate(geometry = st_buffer(geometry,min_setback))

    # make the buffered sides all one polygon
    buffered_polygon <- st_union(buffered_sides)
    buildable_area <- st_difference(st_make_valid(polygon),st_make_valid(buffered_polygon)) |>
      list()


  } else{ #multiple setback possibilities

    # put a buffer on each side (need to convert to meters)
    buffered_sides_relaxed <- tidyparcel_with_setbacks |>
      mutate(geometry = st_buffer(geometry,min_setback))

    # make the buffered sides all one polygon
    buffered_polygon_relaxed <- st_union(buffered_sides_relaxed)
    buildable_area_relaxed <- st_difference(st_make_valid(polygon),st_make_valid(buffered_polygon_relaxed))

    # put a buffer on each side (need to convert to meters)
    buffered_sides_strict <- tidyparcel_with_setbacks |>
      mutate(geometry = st_buffer(geometry,max_setback))

    # make the buffered sides all one polygon
    buffered_polygon_strict <- st_union(buffered_sides_strict)
    buildable_area_strict <- st_difference(st_make_valid(polygon),st_make_valid(buffered_polygon_strict))

    buildable_area <- list(buildable_area_strict, buildable_area_relaxed)

  }

  return(buildable_area)

}


