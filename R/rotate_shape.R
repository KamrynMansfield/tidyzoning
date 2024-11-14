rotate_shape <- function(shape, angle_degrees, center = NULL) {
  # Convert angle to radians
  rad <- angle_degrees * pi / 180

  # Create the rotation matrix
  rotation_matrix <- matrix(c(cos(rad), -sin(rad), sin(rad), cos(rad)), ncol = 2)

  # If no center is provided, use the centroid
  if (is.null(center)) {
    center <- st_centroid(shape[,"geometry"])
    center <- st_coordinates(center)[1, ]
  }

  # Translate shape to origin, apply rotation, then translate back
  coords <- shape[,"geometry"] |>
    st_cast("POINT") |>
    st_coordinates()
  new_coords <- (sweep(coords, 2, as.vector(center), "-") %*% rotation_matrix) |>
    sweep(2,as.vector(center),"+")

  # Convert back to the same geometry type
  new_geom <- st_set_geometry(st_as_sf(shape), st_sfc(st_polygon(list(new_coords))))
  new_geom <- st_set_crs(new_geom, st_crs(shape))

  return(new_geom)
}
