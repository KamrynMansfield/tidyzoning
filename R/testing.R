#
# parcels_path <- "inst/extdata/Cockrell_Hill_parcels.geojson"
# zoning_path <- "inst/extdata/Cockrell_Hill_zoning.geojson"
# building_path <- "inst/extdata/bldg_12_fam.json"
# zoning_old <- sf::st_read("../personal_rpoj/FortWorthData/nza_to_ozfs_all_counties/ozfs/Cockrell Hill.geojson")
# zoning <- sf::st_read(zoning_path)
#
#
# tbldg <- unify_tidybuilding(building_path)
#
# tidyparcel_dims <- get_tidyparcel_dim(parcels_path)
# tidyparcel_geo <- get_tidyparcel_geo(parcels_path)
#
# check_land_use(tbldg,zoning_old[1,])
# get_zoning_req(tbldg, zoning_old[3,],tidyparcel_dims)
#
