# set up layers -----------------------------------------------------------
ctyLyr <- counties(c("17", "18"), cb = TRUE, class = "sf") %>%
  mutate(area_sq_mi = ALAND * 3.861e-7) %>%
  select(-c(COUNTYNS, AFFGEOID, LSAD, ALAND, AWATER))
r6ctyLyr <- subset(ctyLyr, STATEFP == "17" & COUNTYFP %in% counties) %>%
  left_join(r6ctyTbl)
trLyr <- tracts("17", counties, cb = TRUE, class = "sf") %>%
  mutate(area_sq_mi = ALAND * 3.861e-7) %>%
  select(-c(AFFGEOID, LSAD, ALAND, AWATER)) %>%
  semi_join(illinoisRuralTractTable) %>%
  left_join(r6trTbl, by = "GEOID")
crs <- st_crs("+init=esri:102008 +lon_0=-89")
# bgLyr <- block_groups("17", counties, cb = TRUE, class = "sf") %>%
#   mutate(area_sq_mi = ALAND * 3.861e-7) %>%
#   select(-c(AFFGEOID, LSAD, ALAND, AWATER)) %>%
#   left_join(r6bgTbl, by = "GEOID")

# HSTP map function -------------------------------------------------------

hstpmap <- function(sf,
                    backLyr = ctyLyr,   # "background layer"
                    frontLyr = r6ctyLyr, # "foreground layer"
                    var,
                    title,
                    proj = crs,
                    n = 7,
                    vals = "whole",
                    style = "jenks",
                    palette = "seq"
) {
  require(tidycensus, tidyverse, tmap)
  make_dollar <- function(x, digits = 0) {
    paste0("$", formatC(x, digits = digits, format = "f", big.mark = ","))
  } # helper function to format dollar amounts
  tmap_mode("plot")
  if (str_length(sf$GEOID[1]) == 11) {
    geoName = "Rural Census Tracts"
  } else if (str_length(sf$GEOID[1]) == 12) {
    geoName = "Rural Census Block Groups"
  } else {
    geoName = "ACS Estimates"
  }
  if (vals == "dollars") {
    lformat = list(fun = make_dollar)
    ltitle = paste("US Dollars,", year)
  } else if (vals == "percent") {
    lformat = list(fun = percent, format = "f", suffix = "%", digits = 2)
    ltitle = paste(geoName, year, sep = ", ")
  } else if (vals == "decimal") {
    lformat = list(format = "f", digits = 2)
    ltitle = paste(geoName, year, sep = ", ")
  } else {
    lformat = list(format = "f", digits = 0)
    ltitle = paste(geoName, year, sep = ", ")
  }
  map <- tm_shape(backLyr,
                  bbox = sf,
                  projection = proj,
                  unit = "mi") +
    tm_fill(col = "grey85") +
    tm_borders(col = "white", lwd = 2) +
    tm_shape(sf) +
    tm_fill(
      col = var,
      n = n,
      style = style,
      title = ltitle,
      palette = palette
    ) +
    tm_borders(col = "grey50", lwd = .5) +
    tm_shape(frontLyr) +
    tm_borders(col = "black", lwd = 2) +
    tm_text(
      text = "NAME",
      size = 1,
      shadow = T
    ) +
    tm_layout(
      legend.position = c("left", "top"),
      legend.title.size = 1.1,
      legend.title.fontface = "bold",
      title = title,
      title.size = 1.2,
      title.fontface = "bold",
      fontfamily = "sans",
      legend.format = lformat,
      frame.lwd = 2,
      outer.bg.color = "#00000000",
    ) +
    tm_scale_bar(
      width = 0.25,
      text.size = .5
    ) +
    tm_credits(
      text = paste("Data Source: Census ACS 5-year estimates,", year-5, "-", year),
      size = .5,
      bg.color = "white",
      bg.alpha = .5
    )
  map
}

map_popVets <- hstpmap(sf = mutate(trLyr, per = popVets/popTot), var = "per", title = "popVets", vals = "percent") %T>%
  tmap_save(filename = paste(mapDirectory, "/popVets", year, ".pdf", sep = ""), width = 7, height = 7)
map_popOvr65 <- hstpmap(sf = mutate(trLyr, per = popOvr65/popTot), var = "per", title = "popOvr65", vals = "percent") %T>%
  tmap_save(filename = paste(mapDirectory, "/popOvr65", year, ".pdf", sep = ""), width = 7, height = 7)
map_medHHInc <- hstpmap(sf = mutate(trLyr), var = "medHHInc", title = "medHHInc", vals = "dollars") %T>%
  tmap_save(filename = paste(mapDirectory, "/medHHInc", year, ".pdf", sep = ""), width = 7, height = 7)
map_perCapInc <- hstpmap(sf = mutate(trLyr), var = "perCapInc", title = "perCapInc", vals = "dollars") %T>%
  tmap_save(filename = paste(mapDirectory, "/perCapInc", year, ".pdf", sep = ""), width = 7, height = 7)
map_popGQ <- hstpmap(sf = mutate(trLyr, per = popGQ/popTot), var = "per", title = "popGQ", vals = "percent") %T>%
  tmap_save(filename = paste(mapDirectory, "/popGQ", year, ".pdf", sep = ""), width = 7, height = 7)
map_popDis <- hstpmap(sf = mutate(trLyr, per = popDis/popTot), var = "per", title = "popDis", vals = "percent") %T>%
  tmap_save(filename = paste(mapDirectory, "/popDis", year, ".pdf", sep = ""), width = 7, height = 7)
map_popSub18 <- hstpmap(sf = mutate(trLyr, per = popSub18/popTot), var = "per", title = "Percent of Population Under 18", vals = "percent") %T>%
  tmap_save(filename = paste(mapDirectory, "/popSub18", year, ".pdf", sep = ""), width = 7, height = 7)
map_popPov <- hstpmap(sf = mutate(trLyr, per = popPov/popTot), var = "per", title = "Percent of Population under Poverty Level", vals = "percent") %T>%
  tmap_save(filename = paste(mapDirectory, "/popPov", year, ".pdf", sep = ""), width = 7, height = 7)
map_popNoCar <- hstpmap(sf = mutate(trLyr, per = popNoCar/popTot), var = "per", title = "popNoCar", vals = "percent") %T>%
  tmap_save(filename = paste(mapDirectory, "/popNoCar", year, ".pdf", sep = ""), width = 7, height = 7)
map_gini <- hstpmap(sf = mutate(trLyr), var = "gini", title = "gini", vals = "decimal") %T>%
  tmap_save(filename = paste(mapDirectory, "/gini", year, ".pdf", sep = ""), width = 7, height = 7)
map_popNoDip <- hstpmap(sf = mutate(trLyr, per = popNoDip/popTot), var = "per", title = "popNoDip", vals = "percent") %T>%
  tmap_save(filename = paste(mapDirectory, "/popNoDip", year, ".pdf", sep = ""), width = 7, height = 7)

trShp <- tracts("17", counties, cb = TRUE, class = "sf") %>%
  mutate(area_sq_mi = ALAND * 3.861e-7) %>%
  select(-c(AFFGEOID, LSAD, ALAND, AWATER)) %>%
  left_join(ilTractTbl) %>%
  select(c(COUNTYFP, GEOID, area_sq_mi, urbanPer, geometry)) %>%
  left_join(r6trTbl) %T>%
  st_write(paste(outputDataDirectory, "trLyr.shp", sep = "/"), delete_dsn = TRUE)

