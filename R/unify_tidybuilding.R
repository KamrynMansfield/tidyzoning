#' Get summarized building info in one table
#'
#' @param bldg_data_file a json file with ozfs building attributes see "insert hyperlink here"
#' @param ozfs_data_file the path to the
#' @param bldg_data_string a json bldg_data_string with ozfs building attributes see "insert hyperlink here"
#'
#' @returns a one-row data frame with values describing the building.
#' @export
#'
#' @examples
#'
unify_tidybuilding <- function(bldg_data_file = NULL, ozfs_data_file = NULL, bldg_data_string = NULL){

  if (is.null(bldg_data_file) & is.null(ozfs_data_file) & is.null(bldg_data_string)){
    return(cat("No input given \nPlease provide either a json file path or json bldg_data_string"))
  }


  if (!is.null(bldg_data_string)){
    listed_json <- tryCatch({
      rjson::fromJSON(bldg_data_string)
    }, error = function(e) {
      stop("The bldg_data_string must be a json")
    })
  }

  if (!is.null(bldg_data_file) & length(grep(".",bldg_data_file,fixed = TRUE)) == 0){
    stop("Improper file path")
  }

  if (!is.null(bldg_data_file)){
    split_name <- strsplit(basename(bldg_data_file),".", fixed = TRUE)[[1]]
    ext <- split_name[[length(split_name)]]

    if (ext == "json" | ext == "JSON"){
      listed_json <- rjson::fromJSON(file = bldg_data_file)
    } else{
      stop("The file must be a json")
    }
  }

  if (is.null(listed_json$bldg_info) | is.null(listed_json$unit_info) | is.null(listed_json$level_info)){
    stop("Improper format: json must contain bldg_info, unit_info, and level_info sections")
  }

  fl_area <- c()
  bedrooms <- c()
  qty <- c()
  for (unit in listed_json$unit_info){
    fl_area <- c(fl_area, unit$fl_area)
    bedrooms <- c(bedrooms, unit$bedrooms)
    qty <- c(qty, unit$qty)
  }
  unit_info_df <- data.frame(fl_area = fl_area,
                             bedrooms = bedrooms,
                             qty = qty)

  level <- c()
  gross_fl_area <- c()
  for (lev in listed_json$level_info){
    level <- c(level, lev$level)
    gross_fl_area <- c(gross_fl_area, lev$gross_fl_area)
  }
  level_info_df <- data.frame(level = level,
                              gross_fl_area = gross_fl_area)

  height <- listed_json$bldg_info$height
  width <- listed_json$bldg_info$width
  depth <- listed_json$bldg_info$depth
  roof_type <- listed_json$bldg_info$roof_type
  parking <- ifelse(!is.null(listed_json$bldg_info$parking),listed_json$bldg_info$parking,0)
  height_eave <- ifelse(!is.null(listed_json$bldg_info$height_eave),listed_json$bldg_info$height_eave,listed_json$bldg_info$height)
  stories <- max(level_info_df$level)
  total_units <- sum(unit_info_df$qty)
  type <- ifelse(total_units > 3, "4_family", paste0(total_units,"_family"))
  gross_fl_area <- sum(level_info_df$gross_fl_area)
  total_bedrooms <- sum(unit_info_df$bedrooms * unit_info_df$qty)
  fl_area_first <- level_info_df$gross_fl_area[level_info_df$level == min(level_info_df$level)]
  fl_area_top <- level_info_df$gross_fl_area[level_info_df$level == max(level_info_df$level)]
  units_0bed <- sum(unit_info_df$qty[unit_info_df$bedrooms == 0])
  units_1bed <- sum(unit_info_df$qty[unit_info_df$bedrooms == 1])
  units_2bed <- sum(unit_info_df$qty[unit_info_df$bedrooms == 2])
  units_3bed <- sum(unit_info_df$qty[unit_info_df$bedrooms == 3])
  units_4bed <- sum(unit_info_df$qty[unit_info_df$bedrooms > 3])
  min_unit_size <- min(unit_info_df$fl_area)
  max_unit_size <- max(unit_info_df$fl_area)

  bldg_info_df <- data.frame(height = height,
                             width = width,
                             depth = depth,
                             roof_type = roof_type,
                             parking = parking,
                             height_eave = height_eave,
                             stories = stories,
                             total_units = total_units,
                             type = type,
                             gross_fl_area = gross_fl_area,
                             total_bedrooms = total_bedrooms,
                             fl_area_first = fl_area_first,
                             fl_area_top = fl_area_top,
                             units_0bed = units_0bed,
                             units_1bed = units_1bed,
                             units_2bed = units_2bed,
                             units_3bed = units_3bed,
                             units_4bed = units_4bed,
                             min_unit_size = min_unit_size,
                             max_unit_size = max_unit_size)

  if (!is.null(ozfs_data_file)){
    listed_ozfs <- rjson::fromJSON(file = ozfs_data_file)

    # Loop through each hight definition
    for (definition in listed_ozfs$definitions[["height"]]){
      if (definition$roof == bldg_info_df$roof_type){
        new_height <- eval(parse(text = definition$def))
        break
      }
    }

    bldg_info_df$height <- new_height
  }


  return(bldg_info_df)
}
