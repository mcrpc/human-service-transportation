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
  STATEFP == "17" & COUNTYFP %in% region6CountyFIPS3
) %>%
  dplyr::left_join(
    select(illinoisCountyData, -NAME),
    by = c("GEOID")
  )

tractLayer <- tigris::tracts(
  state = "17",
  cb = TRUE,
  class = "sf"
) %>%
  dplyr::mutate(area_sq_mi = ALAND * 3.861e-7) %>%
  dplyr::left_join(illinoisTractData, by = "GEOID") %>%
  dplyr::select(-c(AFFGEOID, LSAD, ALAND, AWATER, contains("dnm"))) %>%
  dplyr::mutate(den_pop = est_pop / area_sq_mi) %>%
  subset(per_urban <= 0.5) %>%
  sf::st_transform(crs = crs) %>%
  dplyr::left_join(select(illinoisTractPercentChangeData, c(GEOID, growth_pop))) %>%
  dplyr::select(-NAME.x) %>%
  dplyr::rename(NAME = NAME.y)

ruralTractVector <- tractLayer$GEOID

blockGroupLayer <- tigris::block_groups(
  state = "17",
  county = region6CountyFIPS3,
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

selection <- paste0("17", region6CountyFIPS3)
subset(illinoisCountyPercentChangeData, GEOID %in% selection | NAME == "mean")
# nothing of note, really. population of iroquois and livingston counties declined by 1%

subset(
  illinoisTractPercentChangeData,
  str_trunc(GEOID, 5, "right", ellipsis = "") %in% selection
) %>%
  select(GEOID, growth_pop) %>%
  summarize(mean(growth_pop))
