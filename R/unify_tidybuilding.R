#' Get summarized building info in one table
#'
#' @param file_path a json file with ozfs building attributes see "insert hyperlink here"
#' @param string a json string with ozfs building attributes see "insert hyperlink here"
#'
#' @returns a one-row data frame with values describing the building.
#' @export
#'
#' @examples
#'
unify_tidybuilding <- function(file_path = NULL, string = NULL){

  if (!is.null(string)){
    listed_json <- tryCatch({
      fromJSON(string)
    }, error = function(e) {
      stop("The string must be a json")
    })
  }

  if (!is.null(file_path)){
    split_name <- strsplit(basename(file_path),".", fixed = TRUE)[[1]]
    ext <- split_name[[length(split_name)]]

    if (ext == "json" | ext == "JSON"){
      listed_json <- fromJSON(file = file_path)
    } else{
      stop("The file must be a json")
    }
  }


  if (is.null(listed_json$bldg_info) | is.null(listed_json$unit_info) | is.null(listed_json$level_info)){
    stop("Improper format: json must contain bldg_info, unit_info, and level_info sections")
  }

  fl_area_val <- c()
  bedrooms_val <- c()
  qty_val <- c()
  for (unit in listed_json$unit_info){
    fl_area_val <- c(fl_area_val, unit$fl_area)
    bedrooms_val <- c(bedrooms_val, unit$bedrooms)
    qty_val <- c(qty_val, unit$qty)
  }
  unit_info_df <- data.frame(fl_area = fl_area_val,
                             bedrooms = bedrooms_val,
                             qty = qty_val)

  level_val <- c()
  gross_fl_area_val <- c()
  for (level in listed_json$level_info){
    level_val <- c(level_val, level$level)
    gross_fl_area_val <- c(gross_fl_area_val, level$gross_fl_area)
  }
  level_info_df <- data.frame(level = level_val,
                              gross_fl_area = gross_fl_area_val)

  height_val <- listed_json$bldg_info$height
  width_val <- listed_json$bldg_info$width
  depth_val <- listed_json$bldg_info$depth
  roof_type_val <- listed_json$bldg_info$roof_type
  parking_val <- ifelse(!is.null(listed_json$bldg_info$parking),listed_json$bldg_info$parking,0)
  height_eave_val <- ifelse(!is.null(listed_json$bldg_info$height_eave),listed_json$bldg_info$height_eave,listed_json$bldg_info$height)
  stories_val <- max(level_info_df$level)
  total_units_val <- sum(unit_info_df$qty)
  type_val <- ifelse(total_units_val > 3, "4_family", paste0(total_units_val,"_family"))
  gross_fl_area_val <- sum(level_info_df$gross_fl_area)
  total_bedrooms_val <- sum(unit_info_df$bedrooms * unit_info_df$qty)
  fl_area_first_val <- level_info_df$gross_fl_area[level_info_df$level == min(level_info_df$level)]
  fl_area_top_val <- level_info_df$gross_fl_area[level_info_df$level == max(level_info_df$level)]
  units_0bed_val <- sum(unit_info_df$qty[unit_info_df$bedrooms == 0])
  units_1bed_val <- sum(unit_info_df$qty[unit_info_df$bedrooms == 1])
  units_2bed_val <- sum(unit_info_df$qty[unit_info_df$bedrooms == 2])
  units_3bed_val <- sum(unit_info_df$qty[unit_info_df$bedrooms == 3])
  units_4bed_val <- sum(unit_info_df$qty[unit_info_df$bedrooms > 3])

  bldg_info_df <- data.frame(height = height_val,
                             width = width_val,
                             depth = depth_val,
                             roof_type = roof_type_val,
                             parking = parking_val,
                             height_eave = height_eave_val,
                             stories = stories_val,
                             total_units = total_units_val,
                             type = type_val,
                             gross_fl_area = gross_fl_area_val,
                             total_bedrooms = total_bedrooms_val,
                             fl_area_first = fl_area_first_val,
                             fl_area_top = fl_area_top_val,
                             units_0bed = units_0bed_val,
                             units_1bed = units_1bed_val,
                             units_2bed = units_2bed_val,
                             units_3bed = units_3bed_val,
                             units_4bed = units_4bed_val)

  return(bldg_info_df)
}
