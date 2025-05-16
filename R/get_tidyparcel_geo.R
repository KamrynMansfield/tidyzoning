#' Isolate known side geometry from tidyparcel data
#'
#' @param parcels_data Either a file path to the a parcels geojson file or an sf object with the same data.
#'
#' @returns an sf object with only the side geometries of the parcels with identified sides
#' @export
#'
#' @examples
get_tidyparcel_geo <- function(parcels_data){
  if (class(parcels_data)[[1]] == "character"){ # then it is a file path
    parcels_sf <- tryCatch({
      sf::st_read(parcels_data, quiet = TRUE)
    }, error = function(e) {
      stop("Unable to open file. Check to make sure it is a proper geojson")
    })
  } else if (class(parcels_data)[[1]] == "sf"){ # then it is an sf object
    parcels_sf <- parcels_data
  } else{ # it isn't going to work
    stop("improper input")
  }

  # filter to just have the side geometry if they are labeled
  parcels_geo <- parcels_sf |>
    filter(side != "centroid") |>
    filter(side != "unknown") |>
    select(parcel_id, side)

  return(parcels_geo)
}
