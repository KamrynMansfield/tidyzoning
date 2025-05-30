test_that("ouptut is a data frame", {
  json_file <- system.file("extdata/12_fam.bldg", package = "tidyzoning")
  result <- unify_tidybuilding(json_file)
  expect_s3_class(result, "data.frame")
})

test_that("ouptut has correct columns", {
  json_file <- system.file("extdata/12_fam.bldg", package = "tidyzoning")
  result <- unify_tidybuilding(json_file)
  expected_names <- c("height_top",
                      "width"     ,
                      "depth",
                      "roof_type",
                      "parking"       ,
                      "height_eave"  ,
                      "stories"     ,
                      "total_units",
                      "type"           ,
                      "gross_fl_area"  ,
                      "total_bedrooms" ,
                      "fl_area_first" ,
                      "fl_area_top"    ,
                      "units_0bed"     ,
                      "units_1bed"    ,
                      "units_2bed"   ,
                      "units_3bed"     ,
                      "units_4bed"     ,
                      "min_unit_size" ,
                      "max_unit_size",
                      "height")
  expect_identical(names(result), expected_names)
})

test_that("string is not json error",{
  expect_error(unify_tidybuilding(bldg_data_string = ""), regexp = "The bldg_data_string must be in json format")
})

test_that("json but incorrect format error",{
  expect_error(unify_tidybuilding(bldg_data_string = "{}"), regexp = "Improper format: json must contain bldg_info, unit_info, and level_info sections")
})


