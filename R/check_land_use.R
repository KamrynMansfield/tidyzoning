#' Compare building land use and allowed land uses
#'
#' `check_land_use()` takes a tidybuilding and a tidydistrict to see if the district's zoning code allows the tidybuilding based on land use.
#'
#' @inheritParams add_setbacks
#'
#' @return
#' Returns TRUE or FALSE stating whether or not the building would be allowed in the district based on land use.
#' Note: If there is no recorded land use requirement in zoning code, it returns FALSE
#' @export
#'
check_land_use <- function(tidybuilding, tidydistrict){
  bldg_type <- tidybuilding$type

  if (is.null(bldg_type)){
    return(FALSE)
    warning("Building data lacking bldg_type")
  }

  if (length(tidydistrict$res_uses[[1]]) == 0){
    warning("Can't find permitted land uses. Assumed FALSE")
    return(FALSE)
  } else{
    return(bldg_type %in% tidydistrict$res_uses[[1]])
  }
}
