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
region6CountyLayer <- dplyr::subset(
  countyLayer,
  STATEFP == "17" & COUNTYFP %in% region6CountyList
) %>%
  dplyr::left_join(illinoisCountyData, by = c("GEOID"))
tractLayer <- tigris::tracts(
  state = "17",
  county = region6CountyList,
  cb = TRUE,
  class = "sf"
) %>%
  dplyr::mutate(area_sq_mi = ALAND * 3.861e-7) %>%
  dplyr::select(-c(AFFGEOID, LSAD, ALAND, AWATER)) %>%
  dplyr::left_join(illinoisTractData, by = "GEOID")
crs <- sf::st_crs("+init=esri:102008 +lon_0=-89")
# bgLyr <- block_groups("17", counties, cb = TRUE, class = "sf") %>%
#   mutate(area_sq_mi = ALAND * 3.861e-7) %>%
#   select(-c(AFFGEOID, LSAD, ALAND, AWATER)) %>%
#   left_join(r6bgTbl, by = "GEOID")

# HSTP map function -------------------------------------------------------

getHSTPMap <- function(
  sf,
  backgroundLayer = countyLayer,   # "background layer"
  foregroundLayer = region6CountyLayer, # "foreground layer"
  variable,
  title,
  proj = crs,
  n = 7,
  vals = "whole",
  classificationStyle = "jenks",
  palette = "seq"
) {
  require(tidycensus, tidyverse, tmap)
  formatDollarAmount <- function(x, digits = 0) {
    paste0("$", formatC(x, digits = digits, format = "f", big.mark = ","))
  } # helper function to format dollar amounts
  tmap::tmap_mode("plot")
  if (stringr::str_length(sf$GEOID[1]) == 11) {
    geography = "Census Tracts"
  } else if (stringr::str_length(sf$GEOID[1]) == 12) {
    geography = "Census Block Groups"
  } else {
    geography = "ACS Estimates"
  }
  if (vals == "dollars") {
    legendFormat = list(fun = formatDollarAmount)
    legendTitle = paste("US Dollars,", acsYear)
  } else if (vals == "percent") {
    legendFormat = list(fun = percent, format = "f", suffix = "%", digits = 2)
    legendTitle = paste(geography, acsYear, sep = ", ")
  } else if (vals == "decimal") {
    legendFormat = list(format = "f", digits = 2)
    legendTitle = paste(geography, acsYear, sep = ", ")
  } else {
    legendFormat = list(format = "f", digits = 0)
    legendTitle = paste(geography, acsYear, sep = ", ")
  }
  map <- tmap::tm_shape(
    backgroundLayer,
    bbox = sf,
    projection = proj,
    unit = "mi"
  ) +
    tmap::tm_fill(col = "grey85") +
    tmap::tm_borders(col = "white", lwd = 2) +
    tmap::tm_shape(sf) +
    tmap::tm_fill(
      col = variable,
      n = n,
      style = classificationStyle,
      title = legendTitle,
      palette = palette
    ) +
    tmap::tm_borders(col = "grey50", lwd = .5) +
    tmap::tm_shape(foregroundLayer) +
    tmap::tm_borders(col = "black", lwd = 2) +
    tmap::tm_text(
      text = "NAME",
      size = 1,
      shadow = T
    ) +
    tmap::tm_layout(
      legend.position = c("left", "top"),
      legend.title.size = 1.1,
      legend.title.fontface = "bold",
      title = title,
      title.size = 1.2,
      title.fontface = "bold",
      fontfamily = "sans",
      legend.format = legendFormat,
      frame.lwd = 2,
      outer.bg.color = "#00000000",
    ) +
    tmap::tm_scale_bar(
      width = 0.25,
      text.size = .5
    ) +
    tmap::tm_credits(
      text = paste(
        "Data Source: Census ACS 5-year estimates,",
        acsYear - 5,
        "-",
        acsYear
      ),
      size = .5,
      bg.color = "white",
      bg.alpha = .5
    )
  map
}

inc_percapTractMap <- getHSTPMap(
  sf = tractLayer,
  variable = "inc_percap",
  title = "Income Per Capita",
  vals = "dollars"
) %T>%
  tmap::tmap_save(filename = paste(outputMapDirectory, "/", acsYear, "_inc-per-cap-Tract-Map.pdf", sep = ""), width = 7, height = 7)

