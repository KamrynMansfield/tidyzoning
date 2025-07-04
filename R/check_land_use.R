#' Compare building land use and allowed land uses
#'
#' `check_land_use()` checks to see if the residential type is listed in the
#' districts permitted uses. It returns TRUE or FALSE.
#'
#' @param vars A data frame with a column for each OZFS variable. The result
#' of the [get_variables] function.
#' @param district_data one row (representing one district) of a
#' zoning data frame created from the OZFS *.zoning file
#'
#' @return
#' Returns TRUE or FALSE stating whether or not the building would be allowed in the district based on land use.
#' Note: If there is no recorded land use requirement in zoning code, it returns FALSE
#' @export
#'
check_land_use <- function(vars, district_data){
  res_type <- vars$res_type

  if (is.null(res_type)){
    return(FALSE)
    warning("Building data lacking res_type")
  }

  if (length(district_data$res_types_allowed[[1]]) == 0){
    warning("Can't find permitted land uses. Assumed FALSE")
    return(FALSE)
  } else{
    return(res_type %in% district_data$res_types_allowed[[1]])
  }
}

