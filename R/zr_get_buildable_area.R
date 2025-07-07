#' Create a parcel's buildable area
#'
#' `zr_get_buildable_area()` takes a tidyparcel object with setback information and produces a polygon representing the builablel area of the parcel.
#'
#' @param tidyparcel_with_setbacks A tidyparcel object that has setback infomration added. tidyparcel_with_setbacks is the output of the [add_setbacks()] function.
#'
#' @return
#' Returns a polygon representing the buildable area of the parcel.
#' @export
#'
zr_get_buildable_area <- function(tidyparcel_with_setbacks){
  # make tidyparcel a polygon
  polygon <- tidyparcel_with_setbacks |>
    sf::st_union() |>
    sf::st_polygonize() |>
    sf::st_collection_extract()

  if (nrow(tidyparcel_with_setbacks[!is.na(tidyparcel_with_setbacks$setback),]) == 0){
    return(list(polygon))
  }

  # seprate the min and max setbacks
  tidyparcel_with_setbacks$min_setback <- unlist(lapply(tidyparcel_with_setbacks$setback, min))
  tidyparcel_with_setbacks$max_setback <- unlist(lapply(tidyparcel_with_setbacks$setback, max))

  tidyparcel_with_setbacks <- tidyparcel_with_setbacks  |>
    dplyr::mutate(min_setback = ifelse(is.na(min_setback), 0.1, min_setback),
           max_setback = ifelse(is.na(max_setback), 0.1, max_setback))

  # convert feet to meters
  tidyparcel_with_setbacks <- tidyparcel_with_setbacks |>
    dplyr::mutate(min_setback = min_setback * 0.3048,
           max_setback = max_setback * 0.3048)

  if (identical(tidyparcel_with_setbacks$min_setback,tidyparcel_with_setbacks$max_setback)){ # just one setback for each side

    # put a buffer on each side (need to convert to meters)
    buffered_sides <- tidyparcel_with_setbacks |>
      dplyr::mutate(geometry = sf::st_buffer(geometry,min_setback))

    # make the buffered sides all one polygon
    buffered_polygon <- sf::st_union(buffered_sides)
    buildable_area <- sf::st_difference(sf::st_make_valid(polygon),sf::st_make_valid(buffered_polygon)) |>
      list()


  } else{ #multiple setback possibilities

    # put a buffer on each side (need to convert to meters)
    buffered_sides_relaxed <- tidyparcel_with_setbacks |>
      dplyr::mutate(geometry = sf::st_buffer(geometry,min_setback))

    # make the buffered sides all one polygon
    buffered_polygon_relaxed <- sf::st_union(buffered_sides_relaxed)
    buildable_area_relaxed <- sf::st_difference(sf::st_make_valid(polygon),sf::st_make_valid(buffered_polygon_relaxed))

    # put a buffer on each side (need to convert to meters)
    buffered_sides_strict <- tidyparcel_with_setbacks |>
      dplyr::mutate(geometry = sf::st_buffer(geometry,max_setback))

    # make the buffered sides all one polygon
    buffered_polygon_strict <- sf::st_union(buffered_sides_strict)
    buildable_area_strict <- sf::st_difference(sf::st_make_valid(polygon),sf::st_make_valid(buffered_polygon_strict))

    buildable_area <- list(buildable_area_strict, buildable_area_relaxed)

  }

  return(buildable_area)

}
