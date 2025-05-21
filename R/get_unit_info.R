#' Create data frame with unit info
#'
#' `get_unit_info()` takes a json representing a building and
#' returns a data frame with data for each unit in the building.
#'
#' @inheritParams unify_tidybuilding
#'
#' @returns a data frame with information for each unit in the building
#' @export
#'
#' @examples
get_unit_info <- function(file_path = NULL, string = NULL){

  if (!is.null(string)){
    listed_json <- tryCatch({
      rjson::fromJSON(string)
    }, error = function(e) {
      stop("The string must be a json")
    })
  }

  if (!is.null(file_path) & length(grep(".",file_path,fixed = TRUE)) == 0){
    stop("Improper file path")
  }

  if (!is.null(file_path)){
    split_name <- strsplit(basename(file_path),".", fixed = TRUE)[[1]]
    ext <- split_name[[length(split_name)]]

    if (ext == "json" | ext == "JSON"){
      listed_json <- rjson::fromJSON(file = file_path)
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

  return(unit_info_df)

}
