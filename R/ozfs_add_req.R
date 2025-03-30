#' Add Zoning Requirements
#'
#' `ozfs_add_req()` takes an existing geojson file in ozfs format, and adds a simple constraint.
#'
#' @param ozfs_file The file path for the ozfs file that needs editing
#' @param district The district that needs editing
#' @param constraint_type The constraint type. "structure_constraints", "lot_constraints", or "other_constraints"
#' @param constraint The constraint that you are either editing or adding.
#' @param units The units of the constraint. This is only necessary when you are adding a new constraint.
#' @param land_uses The land use that the constraint applies to. If it is an existing constraint, the land_uses has to match the existing land use possibilities.
#' @param min_or_max Whether the expression will be "min" or "max"
#' @param expression The expression used to describe the new zoning constraint.
#' @param overwrite If TRUE, will overwrite the existing file.
#'
#' @returns A statement saying which file the updates were added to.
#' @export
#'
#' @examples
ozfs_add_req <- function(ozfs_file,
                         district,
                         constraint_type = "structure_constraints",
                         constraint,
                         units = NULL,
                         land_uses,
                         min_or_max,
                         expression,
                         overwrite = FALSE){

  ozfs_list <- fromJSON(file = ozfs_file)

  for (i in 1:length(ozfs_list$features)){
    dist_abbr <- ozfs_list$features[[i]]$properties$dist_info$dist_abbr

    if (district == dist_abbr){
      dist_idx <- i
      break
    }
  }

  if (ozfs_list$features[[dist_idx]]$properties$dist_info$dist_abbr != district){
    stop("District not found. Couldn't add constraint")
  }

  constraint_info <- ozfs_list$features[[dist_idx]]$properties$structure_constraints[[constraint]]

  if (is.null(ozfs_list$features[[dist_idx]]$properties$structure_constraints[[constraint]]) & is.null(units)){
    warning("New constraint added, but missing units value")
    land_use_idx <- 1
    ozfs_list$features[[dist_idx]]$properties$structure_constraints[[constraint]][[land_use_idx]]$use_name <- land_uses

  } else if (!is.null(ozfs_list$features[[dist_idx]]$properties$structure_constraints[[constraint]])){
    for (j in 1:length(constraint_info)){
      if (identical(constraint_info[[j]]$use_nam,land_uses)){
        land_use_idx <- j
      }
    }

    if (!exists("land_use_idx")){
      stop("Could not find land use")
    }

  } else{
    land_use_idx <- 1
    ozfs_list$features[[dist_idx]]$properties$structure_constraints[[constraint]][[land_use_idx]]$use_name <- land_uses
    land_use_idx <- 1
    ozfs_list$features[[dist_idx]]$properties$structure_constraints[[constraint]][[land_use_idx]]$unit <- units
  }


  if (min_or_max == "min"){
    ozfs_list$features[[dist_idx]]$properties$structure_constraints[[constraint]][[land_use_idx]]$min_val$expression <- expression
  } else if (min_or_max == "max"){
    ozfs_list$features[[dist_idx]]$properties$structure_constraints[[constraint]][[land_use_idx]]$max_val$expression <- expression
  } else{
    stop("Specified `min` or `max` value needed")
  }

  if (overwrite == TRUE){
    save_file_path <- ozfs_file
  } else{
    save_file_path <- paste0(dirname(ozfs_file),"/","new_",basename(ozfs_file))
  }

  write(toJSON(ozfs_list), save_file_path)
  return(paste("updated ozfs file save to",save_file_path))
}
