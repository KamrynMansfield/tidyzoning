test_that("ouptut is a data frame", {
  json_file <- system.file("extdata/bldg_12_fam.json", package = "tidyzoning")
  result <- unify_tidybuilding(json_file)
  expect_s3_class(result, "data.frame")
})

test_that("ouptut has correct columns", {
  json_file <- system.file("extdata/bldg_12_fam.json", package = "tidyzoning")
  result <- unify_tidybuilding(json_file)
  expected_names <- c("height",
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
                      "max_unit_size")
  expect_identical(names(result), expected_names)
})

test_that("improper file error",{
  expect_error(unify_tidybuilding(file_path = ""), regexp = "Improper file path")
})

test_that("file is not json error",{
  expect_error(unify_tidybuilding(file_path = "fakefile.geojson"), regexp = "The file must be a json")
})

test_that("string is not json error",{
  expect_error(unify_tidybuilding(string = ""), regexp = "The string must be a json")
})

test_that("json but incorrect format error",{
  expect_error(unify_tidybuilding(string = "{}"), regexp = "Improper format: json must contain bldg_info, unit_info, and level_info sections")
})


