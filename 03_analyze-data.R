crs <- sf::st_crs("+init=esri:102008 +lon_0=-89")

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
  ) %>%
  sf::st_transform(crs = crs)

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
  dplyr::left_join(illinoisTractData, by = "GEOID") %>%
  dplyr::select(-c(AFFGEOID, LSAD, ALAND, AWATER, contains("dnm"))) %>%
  dplyr::mutate(den_pop = est_pop / area_sq_mi) %>%
  subset(per_urban <= 0.5) %>%
  sf::st_transform(crs = crs)

blockGroupLayer <- tigris::block_groups(
  state = "17",
  county = region6CountyList,
  cb = TRUE,
  class = "sf"
) %>%
  dplyr::mutate(area_sq_mi = ALAND * 3.861e-7) %>%
  dplyr::left_join(region6BlockGroupData, by = "GEOID") %>%
  dplyr::select(-c(AFFGEOID, LSAD, ALAND, AWATER, contains("dnm"))) %>%
  dplyr::mutate(den_pop = est_pop / area_sq_mi) %>%
  sf::st_transform(crs = crs) %>%
  .[tractLayer, op = st_covered_by]

cityLayer <- tigris::places(state = 17, cb = TRUE, class = "sf") %>%
  st_transform(crs) %>%
  select(GEOID, NAME)

tractData <- tractLayer[drop = TRUE] %>%
  dplyr::select(-c(geometry)) %>%
  as_tibble

blockGroupData <- blockGroupLayer[drop = TRUE] %>%
  dplyr::select(-c(geometry)) %>%
  as_tibble

illinoisTractSummary <- drop_na(illinoisTractData) %>%
  select(c(gini, contains("inc"), contains("per"))) %>%
  subset(per_urban <= 0.5) %>%
  summarize_all(mean)

