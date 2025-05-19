#' Rotate polygon by specified angle
#'
#' The `rotate_shape()` function takes a one-row special feature object and rotates the geometry about the centroid or a specified point.
#'
#' @param shape a polygon you want to rotate. Must be a special feature object with only one row.
#' @param angle_degrees The angle (in degrees) that you want to rotate the shape
#' @param center The center about which to rotate in the form of XY coordinates.
#' Default is null,  When NULL, rotation is about the centroid.
#'
#' @return
#' Returns the simple feature shape object with rotated geometry
#' @export
#'
rotate_shape <- function(shape, angle_degrees, center = NULL) {
  # Convert angle to radians
  rad <- angle_degrees * pi / 180

  # Create the rotation matrix
  rotation_matrix <- matrix(c(cos(rad), -sin(rad), sin(rad), cos(rad)), ncol = 2)

  # If no center is provided, use the centroid
  if (is.null(center)) {
    center <- sf::st_centroid(shape)
    center <- sf::st_coordinates(center)[1, ]
  }

  coords <- shape |>
    sf::st_coordinates()

  coords <- coords[1:nrow(coords),1:2]

  new_coords <- (sweep(coords, 2, as.vector(center), "-") %*% rotation_matrix) |>
    sweep(2,as.vector(center),"+")

  # Convert back to the same geometry type
  new_geom <- sf::st_set_crs(sf::st_sfc(sf::st_polygon(list(new_coords))), sf::st_crs(shape))

  return(new_geom)
}
