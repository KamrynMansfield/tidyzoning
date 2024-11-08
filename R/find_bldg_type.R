#' Find Building Type
#'
#' @param tidybuilding Tidybuilding data formatted in a data fram
#'
#' @return A string stating the building land use (1_family, 2_family, etc.).
#' This will coincide with the way it is written in the tidyzining object.
#' @export
#'
#' @examples
find_bldg_type <- function(tidybuilding){
  possible_columns <- c("units_0bed",
                        "units_1bed",
                        "units_2bed",
                        "units_3bed",
                        "units_4bed")

  tidybuilding <- tidybuilding[,names(tidybuilding) %in% possible_columns] |>
    st_drop_geometry()

  row_sum <- rowSums(tidybuilding)

  if (row_sum %in% c(1,2,3)){
    paste0(row_sum[[1]], "_family")
  } else if (row_sum > 3){
    paste("4-family")
  } else {
    paste("other")
  }
}
