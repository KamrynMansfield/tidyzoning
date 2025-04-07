add_parcel_dims <- function(parcels){
  parcels_with_length <- parcels |>
    mutate(length = st_length(geometry))

  parcel_areas <- parcels |>
    filter(side != "centroid") |>
    group_by(parcel_id) |>
    summarise(area = st_area(st_cast(st_union(geometry),"POLYGON"))) |>
    mutate(area_acres = as.numeric(set_units(area, acres))) |>
    select(parcel_id, area_acres) |>
    st_drop_geometry()

  parcel_widths <- parcels_with_length |>
    filter(side %in% c("front", "rear")) |>
    group_by(parcel_id) |>
    summarise(width = max(length)) |>
    mutate(width_ft = as.numeric(set_units(width, ft))) |>
    select(parcel_id, width_ft) |>
    st_drop_geometry()

  parcel_depths <- parcels_with_length |>
    filter(side %in% c("Interior side", "Exterior side")) |>
    group_by(parcel_id) |>
    summarise(depth = max(length)) |>
    mutate(depth_ft = as.numeric(set_units(depth, ft))) |>
    select(parcel_id, depth_ft) |>
    st_drop_geometry()

  all_parcels <- left_join(parcel_areas, parcel_widths) |>
    left_join(parcel_depths)

  return(all_parcels)
}
