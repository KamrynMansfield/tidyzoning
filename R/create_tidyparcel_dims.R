create_tidyparcel_dims <- function(parcels, tidyzoning){
  start_time <- proc.time()

  if (class(parcels)[[1]] == "character"){
    parcels <- st_read(parcels)
    parcels <- parcels |>
      select(Prop_ID, parcel_id, side, parcel_label, geometry) |>
      st_transform(4326)
  } else{
    parcels <- parcels |>
      select(Prop_ID, parcel_id, side, parcel_label, geometry) |>
      st_transform(4326)
  }

  if (class(tidyzoning)[[1]] == "character"){
    tidyzoning <- st_read(tidyzoning)
  }

  zoning_with_idx <- tidyzoning |>
    st_transform(4326) |>
    mutate(idx = c(1:nrow(tidyzoning))) # |>
  # filter(idx != 2247) # This district didn't work when I ran it once, so I'm just leaving this note here for now

  parcels_with_centroid <- parcels |>
    filter(side == "centroid")

  joined_parcels <- st_join(parcels_with_centroid, st_make_valid(zoning_with_idx), join = st_covered_by) |>
    select(Prop_ID, parcel_id, side, parcel_label, zoning_id = idx)

  parcels_filtered <- parcels[parcels$parcel_id %in% joined_parcels$parcel_id,]

  add_parcel_dims <- function(parcels){
    library(sf)
    library(dplyr)
    library(units)

    parcels_with_length <- parcels |>
      mutate(length = st_length(geometry))

    parcel_areas <- parcels |>
      filter(side != "centroid") |>
      group_by(parcel_id) |>
      summarise(area = st_area(st_cast(st_union(geometry),"POLYGON"))) |>
      mutate(lot_area = as.numeric(set_units(area, acres))) |>
      select(parcel_id, lot_area) |>
      st_drop_geometry()

    parcel_widths <- parcels_with_length |>
      filter(side %in% c("front", "rear")) |>
      group_by(parcel_id) |>
      summarise(width = max(length)) |>
      mutate(lot_width = as.numeric(set_units(width, ft))) |>
      select(parcel_id, lot_width) |>
      st_drop_geometry()

    parcel_depths <- parcels_with_length |>
      filter(side %in% c("Interior side", "Exterior side")) |>
      group_by(parcel_id) |>
      summarise(depth = max(length)) |>
      mutate(lot_depth = as.numeric(set_units(depth, ft))) |>
      select(parcel_id, lot_depth) |>
      st_drop_geometry()

    all_parcels <- left_join(parcel_areas, parcel_widths) |>
      left_join(parcel_depths)

    return(all_parcels)
  }

  # Parallel processing #
  with_parallel <- function(big_df){

    # Number of cores to use
    num_cores <- detectCores() - 1

    grouped <- big_df |> group_by(parcel_id) |> group_split()

    # Assign each group to a chunk
    chunk_assignment <- sort(rep(1:num_cores, length.out = length(grouped)))

    # Split groups into chunks
    chunks <- split(grouped, chunk_assignment)

    # Combine each list of groups back into a data frame
    chunks <- lapply(chunks, bind_rows)

    # Set up parallel computing
    cl <- makeCluster(num_cores)
    clusterExport(cl, varlist = c("add_parcel_dims"),
                  envir = environment(create_tidyparcel_dims)) # Export your processing function

    # Execute the processing function in parallel
    results <- parLapply(cl, chunks,fun =  add_parcel_dims)

    # Stop the cluster
    stopCluster(cl)

    # Combine the results into a single data frame
    final_result <- do.call(rbind, results)

    end_time <- proc.time()

    print(end_time - start_time)
    # View the final result
    return(final_result)
  }

  parcel_dims <- with_parallel(parcels_filtered)

  parcels_with_dims <- left_join(parcel_dims, st_drop_geometry(joined_parcels), by = "parcel_id") |>
    select(Prop_ID,
           parcel_id,
           parcel_label,
           lot_width,
           lot_depth,
           lot_area,
           zoning_id)

  return(parcels_with_dims)
}
