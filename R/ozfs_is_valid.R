#' Check OZFS validity
#'
#'`ozfs_is_valid()` State the validity of the ozfs data
#'
#' @param ... A file path to an OZFS geojson file or list of file paths, an sf object created from and OZFS geojson data, or a combination of those.
#'
#' @returns TRUE or FALSE stating whether data is in valid OZFS format or not
#' @export
#'
#' @examples
ozfs_is_valid <- function(...){
  result <- suppressWarnings(try(ozfs_validate(...), silent = TRUE))
  if (inherits(result, "try-error")) {
    return(FALSE)
  }

  if (nrow(result) > 0){
    return(FALSE)
  } else{
    return(TRUE)
  }
}
