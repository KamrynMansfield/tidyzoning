
parcels_path <- "../personal_rpoj/tidyzoning2.0/tidyparcels/Cockrell Hill_parcels.geojson"
zoning_path <- "tidyzoning2.0/tidyzonings/Cockrell Hill.geojson"
building_path <- "tidyzoning2.0/tidybuildings/bldg_2_fam.json"
zoning_old <- sf::st_read("FortWorthData/nza_to_ozfs_all_counties/ozfs/Cockrell Hill.geojson")
zoning <- sf::st_read(zoning_path)

tbldg <- unify_tidybuilding(building_path)

tidyparcel_dims <- get_tidyparcel_dim(parcels_path)
tidyparcel_geo <- get_tidyparcel_geo(parcels_path)

check_land_use(tbldg,zoning_old[1,])
get_zoning_req(tbldg, zoning_old[1,],tidyparcel_dims)
check_far()
