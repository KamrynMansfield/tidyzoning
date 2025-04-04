#' Validate ozfs
#'
#' `ozfs_validate()` lists any errors in the formatting of the OZFS data
#'
#' @param ... A file path to an OZFS geojson file or list of file paths, an sf object created from and OZFS geojson data, or a combination of those.
#'
#' @returns A data frame with info on where the errors were found.
#' @export
#'
ozfs_validate <- function(...){

  ozfs_list <- list(...)

  list_of_ozfs <- list()
  for (i in 1:length(ozfs_list)){
    object <- ozfs_list[[i]]

    if (length(class(object)) > 1){
      if (class(object)[[1]] == "sf"){ # it is ozfs in the sf format

        list_of_features <- list()
        length(list_of_features) <- nrow(object)
        for (j in 1:nrow(object)){
          list_of_features[[j]]$properties$dist_info <- fromJSON(object$dist_info[[j]])
          list_of_features[[j]]$properties$lot_constraints <- fromJSON(object$lot_constraints[[j]])
          list_of_features[[j]]$properties$structure_constraints <- fromJSON(object$structure_constraints[[j]])
          list_of_features[[j]]$properties$other_constraints <- fromJSON(object$other_constraints[[j]])

        }

        new_ozfs <- list(features = list_of_features)

        list_of_ozfs <- append(list_of_ozfs, new_ozfs$features)
        next

      } else{ # it is the wrong format
        stop("Input must be a geojson ozfs file path, a list of file paths, or an ozfs file converted to an sf object using st_read()")
      }
    }

    if (class(object) == "character"){ # it is a file or vector of files
      if (length(object) > 1){
        for (j in 1:length(object)){
          list_of_ozfs <- append(list_of_ozfs, fromJSON(file = object[[j]])$features)
        }

      } else{
        list_of_ozfs <- append(list_of_ozfs, fromJSON(file = object)$features)
      }
    }

    if (class(object) == "list"){ # what we do for lists
      if (class(object[[1]]) == "character"){ # is it a list of characters?

        for (j in 1:length(object)){
          list_of_ozfs <- append(list_of_ozfs, fromJSON(file = object[[j]])$features)
        }

      } else{
        stop("Input must be a geojson ozfs file path, a list of file paths, or an ozfs file converted to an sf object using st_read()")
      }
    }

  }


  ozfs <- list(features = list_of_ozfs)

  safe_parse <- purrr::possibly(parse, otherwise = NA)
  safe_eval <- purrr::possibly(eval, otherwise = NA)

  possible_names <- c("muni_name",
                      "muni_gnis",
                      "county_name",
                      "dist_name",
                      "dist_abbr",
                      "dist_type",
                      "created_by",
                      "created_date",
                      "updated_by",
                      "updated_date",
                      "uses_permitted",
                      "lot_width",
                      "lot_depth",
                      "lot_size",
                      "lot_size_avg",
                      "project_size",
                      "height",
                      "height_eave",
                      "width",
                      "elevation",
                      "stories",
                      "setback_front",
                      "setback_side_ext",
                      "setback_side_int",
                      "setback_rear",
                      "setback_front_sum",
                      "setback_side_sum",
                      "setback_side_garage",
                      "setback_rear_garage",
                      "setback_front_garage",
                      "setback_side_bldg",
                      "setback_rear_bldg",
                      "setback_dist_boundary",
                      "lot_cov_bldg",
                      "lot_cov_imp",
                      "pct_primary_area",
                      "footprint",
                      "fl_area",
                      "far",
                      "unit_qty",
                      "unit_0bed_qty",
                      "unit_1bed_qty",
                      "unit_2bed_qty",
                      "unit_3bed_qty",
                      "unit_4bed_qty",
                      "unit_density",
                      "unit_density_avg",
                      "unit_size",
                      "unit_size_avg",
                      "unit_pct_0bed",
                      "unit_pct_1bed",
                      "unit_pct_2bed",
                      "unit_pct_3bed",
                      "unit_pct_4bed",
                      "bedrooms",
                      "roof_pitch",
                      "fl_area_first",
                      "fl_area_top",
                      "parking_enclosed",
                      "parking_covered",
                      "parking_uncovered",
                      "parking_on_street",
                      "open_space",
                      "swr_connect",
                      "wtr_connect",
                      "acc_struct_permitted")

  lot_width <- 1
  lot_depth <- 1
  lot_area <- 1
  lot_type <- "corner"
  bedrooms <- 1
  total_bedrooms <- 1
  units_0bed <- 1
  units_1bed <- 1
  units_2bed <- 1
  units_3bed <- 1
  units_4bed <- 1
  total_units <- 1
  fl_area <- 1
  fl_area_bottom <- 1
  parking_covered <- 1
  parking_uncovered <- 1
  parking_enclosed <- 1
  parking_floors <- 1
  parking_bel_grade <- "yes"
  garage_entry <- "front"
  height <- 1
  height_eave <- 1
  floors <- 1
  min_unit_size <- 1
  max_unit_size <- 1
  far <- 1
  bldg_width <- 1
  bldg_dpth <- 1
  level_units_table <- 1
  units_floor1 <- 1
  units_floor2 <- 1
  units_floor3 <- 1



  cities <- c()
  counties <- c()
  districts <- c()
  errors <- c()
  land_uses <- c()
  for (i in 1:length(ozfs$features)){ # loop through each feature
    district <- ozfs$features[[i]]




    ## CHECK FIELDS ##
    fields <- c()
    for (j in 1:length(district$properties)){
      fields <- c(fields, names(district$properties[[j]]))
    }

    dist_df <- data.frame(name = fields)
    dist_df$valid_name <- fields %in% possible_names

    if (nrow(dist_df[dist_df$valid_name == FALSE,]) > 0){
      errors <- c(errors, paste("Unknown field(s):",paste(dist_df[dist_df$valid_name == FALSE,]$name, collapse = ", ")))
      cities <- c(cities, ifelse(!is.null(district$properties$dist_info$muni_name),district$properties$dist_info$muni_name, NA))
      counties <- c(counties, ifelse(!is.null(district$properties$dist_info$county_name),district$properties$dist_info$county_name, NA))
      districts <- c(districts, ifelse(!is.null(district$properties$dist_info$dist_abbr),district$properties$dist_info$dist_abbr, NA))
      land_uses <- c(land_uses, paste(fields[!fields %in% possible_names], collapse = ", "))
    }

    ## CHECK EXPRESSIONS AND RULES##

    expressions_rule_names1 <- c("select", "expressions")
    expressions_rule_names2 <- c("select", "expressions", "select_info")
    expressions_rule_names3 <- c("select", "expressions", "conditions")
    expressions_rule_names4 <- c("select", "expressions", "conditions", "logical_operator")
    expressions_rule_names5 <- c("select", "expressions", "conditions", "select_info")
    expressions_rule_names6 <- c("select", "expressions", "conditions", "select_info", "logical_operator")

    expression_rule_names1 <- c("conditions", "expression")
    expression_rule_names2 <- c("conditions", "expression","logical_operator")

    expressions <- list()
    location <- list()
    rules_good <- list()
    uses <- list()

    if (length(district$properties) < 2){ # If there are no constraint sections
      next
    }

    for (k in 2:length(district$properties)){ # looping through the sections
      if (length(district$properties[[k]]) == 0){ # If there is not data conected to the constratint section
        next
      }
      for (l in 1:length(district$properties[[k]])){ # looping through the constraints
        for (m in 1:length(district$properties[[k]][[l]])){ # looping through each land use in the constraints
          constraint <- district$properties[[k]][[l]][[m]]
          use_name <- paste(district$properties[[k]][[l]][[m]]$use_name,collapse = ", ")

          if ("min_val" %in% names(constraint)){
            if ("expression" %in% names(constraint$min_val)){
              expressions <- append(expressions, constraint$min_val$expression)
              location <- append(location, names(district$properties[[k]])[[l]])
              rules_good <- append(rules_good, TRUE)
              uses <- append(uses, use_name)
            } else{
              for (n in 1:length(constraint$min_val)){ # loop through the rules

                rule_names <- names(constraint$min_val[[n]])
                if (sum(rule_names %in% expressions_rule_names1) == length(expressions_rule_names1)){
                  rule_check <- TRUE
                } else if (sum(rule_names %in% expressions_rule_names2) == length(expressions_rule_names2)){
                  rule_check <- TRUE
                } else if (sum(rule_names %in% expressions_rule_names3) == length(expressions_rule_names3)){
                  rule_check <- TRUE
                } else if (sum(rule_names %in% expressions_rule_names4) == length(expressions_rule_names4)){
                  rule_check <- TRUE
                } else if (sum(rule_names %in% expressions_rule_names5) == length(expressions_rule_names5)){
                  rule_check <- TRUE
                } else if (sum(rule_names %in% expressions_rule_names6) == length(expressions_rule_names6)){
                  rule_check <- TRUE
                } else if (sum(rule_names %in% expression_rule_names1) == length(expression_rule_names1)){
                  rule_check <- TRUE
                } else if (sum(rule_names %in% expression_rule_names2) == length(expression_rule_names2)){
                  rule_check <- TRUE
                } else{
                  rule_check <- FALSE
                  errors <- c(errors, paste("Incorrect rules:", names(district$properties[[k]])[[l]]))
                  cities <- c(cities, ifelse(!is.null(district$properties$dist_info$muni_name),district$properties$dist_info$muni_name, NA))
                  counties <- c(counties, ifelse(!is.null(district$properties$dist_info$county_name),district$properties$dist_info$county_name, NA))
                  districts <- c(districts, ifelse(!is.null(district$properties$dist_info$dist_abbr),district$properties$dist_info$dist_abbr, NA))
                  land_uses <- c(land_uses, use_name)

                  rules_good <- append(rules_good, rule_check)
                  expressions <- append(expressions, "1 + 1")
                  location <- append(location, names(district$properties[[k]])[[l]])
                  uses <- append(uses, use_name)
                  next

                }

                if ("expression" %in% rule_names){
                  expressions <- append(expressions, constraint$min_val[[n]]$expression)
                  location <- append(location, names(district$properties[[k]])[[l]])
                  uses <- append(uses, use_name)
                  rules_good <- append(rules_good, TRUE)

                  if ("conditions" %in% rule_names){
                    expressions <- append(expressions, constraint$min_val[[n]]$conditions)
                    location <- append(location, rep(names(district$properties[[k]])[[l]],length(constraint$min_val[[n]]$conditions)))
                    rules_good <- append(rules_good, rep(TRUE,length(constraint$min_val[[n]]$conditions)))
                    uses <- append(uses, rep(use_name,length(constraint$min_val[[n]]$conditions)))

                  }

                } else if ("expressions" %in% rule_names){
                  expressions <- append(expressions, constraint$min_val[[n]]$expressions)
                  location <- append(location, rep(names(district$properties[[k]])[[l]],length(constraint$min_val[[n]]$expressions)))
                  rules_good <- append(rules_good, rep(TRUE,length(constraint$min_val[[n]]$expressions)))
                  uses <- append(uses, rep(use_name,length(constraint$min_val[[n]]$expressions)))

                  if ("conditions" %in% rule_names){
                    expressions <- append(expressions, constraint$min_val[[n]]$conditions)
                    location <- append(location, rep(names(district$properties[[k]])[[l]],length(constraint$min_val[[n]]$conditions)))
                    rules_good <- append(rules_good, rep(TRUE,length(constraint$min_val[[n]]$conditions)))
                    uses <- append(uses, rep(use_name,length(constraint$min_val[[n]]$conditions)))

                  }
                }
              }
            }
          }

          if("max_val" %in% names(constraint)){
            if ("expression" %in% names(constraint$max_val)){
              expressions <- append(expressions, constraint$max_val$expression)
              location <- append(location, names(district$properties[[k]])[[l]])
              rules_good <- append(rules_good, TRUE)
              uses <- append(uses, use_name)
            } else{
              for (n in 1:length(constraint$max_val)){ # loop through the rules

                rule_names <- names(constraint$max_val[[n]])
                if (sum(rule_names %in% expressions_rule_names1) == length(expressions_rule_names1)){
                  rule_check <- TRUE
                } else if (sum(rule_names %in% expressions_rule_names2) == length(expressions_rule_names2)){
                  rule_check <- TRUE
                } else if (sum(rule_names %in% expressions_rule_names3) == length(expressions_rule_names3)){
                  rule_check <- TRUE
                } else if (sum(rule_names %in% expressions_rule_names4) == length(expressions_rule_names4)){
                  rule_check <- TRUE
                } else if (sum(rule_names %in% expressions_rule_names5) == length(expressions_rule_names5)){
                  rule_check <- TRUE
                } else if (sum(rule_names %in% expressions_rule_names6) == length(expressions_rule_names6)){
                  rule_check <- TRUE
                } else if (sum(rule_names %in% expression_rule_names1) == length(expression_rule_names1)){
                  rule_check <- TRUE
                } else if (sum(rule_names %in% expression_rule_names2) == length(expression_rule_names2)){
                  rule_check <- TRUE
                } else{
                  rule_check <- FALSE
                  errors <- c(errors, paste("Incorrect rules:", names(district$properties[[k]])[[l]]))
                  cities <- c(cities, ifelse(!is.null(district$properties$dist_info$muni_name),district$properties$dist_info$muni_name, NA))
                  counties <- c(counties, ifelse(!is.null(district$properties$dist_info$county_name),district$properties$dist_info$county_name, NA))
                  districts <- c(districts, ifelse(!is.null(district$properties$dist_info$dist_abbr),district$properties$dist_info$dist_abbr, NA))
                  land_uses <- c(land_uses, use_name)

                  rules_good <- append(rules_good, rule_check)
                  expressions <- append(expressions, "1 + 1")
                  location <- append(location, names(district$properties[[k]])[[l]])
                  uses <- append(uses, use_name)
                  next

                }

                if ("expression" %in% rule_names){
                  expressions <- append(expressions, constraint$max_val[[n]]$expression)
                  location <- append(location, names(district$properties[[k]])[[l]])
                  uses <- append(uses, use_name)
                  rules_good <- append(rules_good, TRUE)

                  if ("conditions" %in% rule_names){
                    expressions <- append(expressions, constraint$max_val[[n]]$conditions)
                    location <- append(location, rep(names(district$properties[[k]])[[l]],length(constraint$max_val[[n]]$conditions)))
                    rules_good <- append(rules_good, rep(TRUE,length(constraint$max_val[[n]]$conditions)))
                    uses <- append(uses, rep(use_name,length(constraint$max_val[[n]]$conditions)))

                  }

                } else if ("expressions" %in% rule_names){
                  expressions <- append(expressions, constraint$max_val[[n]]$expressions)
                  location <- append(location, rep(names(district$properties[[k]])[[l]],length(constraint$max_val[[n]]$expressions)))
                  rules_good <- append(rules_good, rep(TRUE,length(constraint$max_val[[n]]$expressions)))
                  uses <- append(uses, rep(use_name,length(constraint$max_val[[n]]$expressions)))

                  if ("conditions" %in% rule_names){
                    expressions <- append(expressions, constraint$max_val[[n]]$conditions)
                    location <- append(location, rep(names(district$properties[[k]])[[l]],length(constraint$max_val[[n]]$conditions)))
                    rules_good <- append(rules_good, rep(TRUE,length(constraint$max_val[[n]]$conditions)))
                    uses <- append(uses, rep(use_name,length(constraint$max_val[[n]]$conditions)))

                  }

                }
              }
            }
          }

        }
      }
    }

    expression_df <- data.frame(expression = unlist(expressions),
                                constraint = unlist(location),
                                rule_check = unlist(rules_good),
                                land_use = unlist(uses))

    if (nrow(expression_df) == 0){
      next
    }

    for (j in 1:nrow(expression_df)){
      expression_df$parsed[[j]] <- safe_parse(text = expression_df$expression[[j]])
    }

    for (j in 1:nrow(expression_df)){
      expression_df$evaluated[[j]] <- safe_eval(expression_df$parsed[[j]])
    }

    for (j in 1:nrow(expression_df)){
      parsed <- expression_df$parsed[[j]]
      evaluated <- expression_df$evaluated[[j]]

      if (length(suppressWarnings(is.na(parsed))) > 1){
        errors <- c(errors, paste("Parse error:",expression_df$constraint[[j]]))
        cities <- c(cities, ifelse(!is.null(district$properties$dist_info$muni_name),district$properties$dist_info$muni_name, NA))
        counties <- c(counties, ifelse(!is.null(district$properties$dist_info$county_name),district$properties$dist_info$county_name, NA))
        districts <- c(districts, ifelse(!is.null(district$properties$dist_info$dist_abbr),district$properties$dist_info$dist_abbr, NA))
        land_uses <- c(land_uses, expression_df$land_use[[j]])
        next
      }

      if (suppressWarnings(is.na(parsed))){
        expression_df$expression_check[[j]] <- FALSE
        expression_df$note[[j]] <- paste("Parse error:",expression_df$constraint[[j]])
      } else if (suppressWarnings(is.na(evaluated))){
        expression_df$expression_check[[j]] <- FALSE
        expression_df$note[[j]] <- paste("Unknown variable:",expression_df$constraint[[j]])
      } else if(class(evaluated) != "numeric" & class(evaluated) != "logical"){
        expression_df$expression_check[[j]] <- FALSE
        expression_df$note[[j]] <- paste("Parse error:",expression_df$constraint[[j]])
      } else{
        expression_df$expression_check[[j]] <- TRUE
        expression_df$note[[j]] <- NA
      }
    }

    if (nrow(expression_df[!is.na(expression_df$note),]) > 0){
      for (j in 1:nrow(expression_df[!is.na(expression_df$note),])){
        errors <- c(errors, expression_df$note[!is.na(expression_df$note)][[j]])
        cities <- c(cities, ifelse(!is.null(district$properties$dist_info$muni_name),district$properties$dist_info$muni_name, NA))
        counties <- c(counties, ifelse(!is.null(district$properties$dist_info$county_name),district$properties$dist_info$county_name, NA))
        districts <- c(districts, ifelse(!is.null(district$properties$dist_info$dist_abbr),district$properties$dist_info$dist_abbr, NA))
        land_uses <- c(land_uses, expression_df$land_use[!is.na(expression_df$note)][[j]])
      }
    }



  }

  data.frame(city = cities,
             county = counties,
             district = districts,
             error = errors,
             land_use = land_uses)

}
