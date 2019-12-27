

# set up layers -----------------------------------------------------------
countyLayer <- tigris::counties(c("17", "18"), cb = TRUE, class = "sf") %>%
  dplyr::mutate(area_sq_mi = ALAND * 3.861e-7) %>%
  dplyr::select(
    -c(
      COUNTYNS,
      AFFGEOID,
      LSAD,
      ALAND,
      AWATER
    )
  )

region6CountyLayer <- subset(
  countyLayer,
  STATEFP == "17" & COUNTYFP %in% region6CountyList
) %>%
  dplyr::left_join(
    select(illinoisCountyData, -NAME),
    by = c("GEOID")
  )

tractLayer <- tigris::tracts(
  state = "17",
  county = region6CountyList,
  cb = TRUE,
  class = "sf"
) %>%
  dplyr::mutate(area_sq_mi = ALAND * 3.861e-7) %>%
  dplyr::select(-c(AFFGEOID, LSAD, ALAND, AWATER)) %>%
  dplyr::left_join(illinoisTractData, by = "GEOID") %>%
  dplyr::mutate(den_pop = est_pop / area_sq_mi)

blockGroupLayer <- tigris::block_groups(
  state = "17",
  county = region6CountyList,
  cb = TRUE,
  class = "sf"
) %>%
  dplyr::mutate(area_sq_mi = ALAND * 3.861e-7) %>%
  dplyr::select(-c(AFFGEOID, LSAD, ALAND, AWATER)) %>%
  dplyr::left_join(region6BlockGroupData, by = "GEOID") %>%
  dplyr::mutate(den_pop = est_pop / area_sq_mi)

crs <- sf::st_crs("+init=esri:102008 +lon_0=-89")
# bgLyr <- block_groups("17", counties, cb = TRUE, class = "sf") %>%
#   mutate(area_sq_mi = ALAND * 3.861e-7) %>%
#   select(-c(AFFGEOID, LSAD, ALAND, AWATER)) %>%
#   left_join(r6bgTbl, by = "GEOID")
