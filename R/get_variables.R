#' Get summarized building info in one table
#'
#' @param bldg_data the file path to an OZFS *.bldg file
#' @param parcel_data one row of a parcel data frame created from the
#' OZFS *.parcel file
#' @param district_data one row (representing one district) of a
#' zoning data frame created from the OZFS *.zoning file
#'
#' @returns a one-row data frame with a column for each variable
#' @export
#'
#' @examples
#'
get_variables <- function(bldg_data, parcel_data, district_data){

  bldg_json <- tryCatch({
    rjson::fromJSON(file = bldg_data)
  }, error = function(e) {
    stop("bldg_data must be a file path to a *.bldg file and it must be in json format")
  })

  if (is.null(bldg_json$bldg_info) | is.null(bldg_json$unit_info) | is.null(bldg_json$level_info)){
    stop("Improper format: json must contain bldg_info, unit_info, and level_info sections")
  }

  # creating a data frame for the unit info
  unit_info_df <- do.call(rbind.data.frame, bldg_json$unit_info)

  # creating a data frame for the level info
  level_info_df <- do.call(rbind.data.frame, bldg_json$level_info)

  # assigning the values to variables
  bldg_depth <- bldg_json$bldg_info$depth
  bldg_width <- bldg_json$bldg_info$width
  dist_abbr <- district_data$dist_abbr
  fl_area <- sum(level_info_df$gross_fl_area)
  fl_area_first <- ifelse(length(level_info_df$gross_fl_area[level_info_df$level == 1]) == 1,
                          level_info_df$gross_fl_area[level_info_df$level == 1],
                          0)
  fl_area_top <- ifelse(sum(level_info_df$level > 1) > 0,
                        level_info_df$gross_fl_area[level_info_df$level == max(level_info_df$level)],
                        0)
  floors <- max(level_info_df$level)
  height_deck <- ifelse(!is.null(bldg_json$bldg_info$height_deck),bldg_json$bldg_info$height_deck,bldg_json$bldg_info$height_top)
  height_eave <- ifelse(!is.null(bldg_json$bldg_info$height_eave),bldg_json$bldg_info$height_eave,bldg_json$bldg_info$height_top)
  height_plate <- bldg_json$bldg_info$height_plate
  height_top <- bldg_json$bldg_info$height_top
  height_tower <- ifelse(!is.null(bldg_json$bldg_info$height_tower),bldg_json$bldg_info$height_tower,0)
  lot_area <- parcel_data$lot_area
  lot_depth <- parcel_data$lot_depth
  lot_type <- parcel_data$lot_area
  lot_width <- parcel_data$lot_width
  max_unit_size <- max(unit_info_df$fl_area)
  min_unit_size <- min(unit_info_df$fl_area)
  n_ground_entry <- sum(unit_info_df$qty[unit_info_df$entry_level == 1])
  n_outside_entry <- sum(unit_info_df$qty[unit_info_df$outside_entry == TRUE])
  parking_enclosed <- ifelse(!is.null(bldg_json$bldg_info$parking),bldg_json$bldg_info$parking,0)
  roof_type <- ifelse(!is.null(bldg_json$bldg_info$roof_type),bldg_json$bldg_info$roof_type,"flat")
  sep_platting <- ifelse(!is.null(bldg_json$bldg_info$sep_platting),bldg_json$bldg_info$sep_platting, FALSE)
  total_bedrooms <- sum(unit_info_df$bedrooms * unit_info_df$qty)
  total_units <- sum(unit_info_df$qty)
  units_0bed <- sum(unit_info_df$qty[unit_info_df$bedrooms == 0])
  units_1bed <- sum(unit_info_df$qty[unit_info_df$bedrooms == 1])
  units_2bed <- sum(unit_info_df$qty[unit_info_df$bedrooms == 2])
  units_3bed <- sum(unit_info_df$qty[unit_info_df$bedrooms == 3])
  units_4bed <- sum(unit_info_df$qty[unit_info_df$bedrooms > 3])
  far <- fl_area / (lot_area * 43560)
  # height: a variable that will be created using the zoning definitions
  # res_type: a variable that will be created using the zoning definitions
  # bedrooms: a variable that we will need when we check unit size


  # making it a data frame to return
  vars_df <- data.frame(bldg_depth = bldg_depth,
                             bldg_width = bldg_width,
                             dist_abbr = dist_abbr,
                             fl_area = fl_area,
                             fl_area_first = fl_area_first,
                             fl_area_top = fl_area_top,
                             floors = floors,
                             height_deck = height_deck,
                             height_eave = height_eave,
                             height_plate = height_plate,
                             height_top = height_top,
                             height_tower = height_tower,
                             lot_area = lot_area,
                             lot_depth = lot_depth,
                             lot_type = lot_type,
                             lot_width = lot_width,
                             max_unit_size = max_unit_size,
                             min_unit_size = min_unit_size,
                             n_ground_entry = n_ground_entry,
                             n_outside_entry = n_outside_entry,
                             parking_enclosed = parking_enclosed,
                             roof_type = roof_type,
                             sep_platting = sep_platting,
                             total_bedrooms = total_bedrooms,
                             total_units = total_units,
                             units_0bed = units_0bed,
                             units_1bed = units_1bed,
                             units_2bed = units_2bed,
                             units_3bed = units_3bed,
                             units_4bed = units_4bed,
                             far = far)

  return(vars_df)
}
