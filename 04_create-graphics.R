# define local functions --------------------------------------------------

getACSYearsLabel <- function(acsYear) {
  paste("Data Source: Census ACS 5-year estimates,", acsYear - 4, "-", acsYear)
}

# time series graphs ------------------------------------------------------

drawTimeSeriesGraph <- function(
  dataset,
  var1,
  var2,
  var2Scale = "numbers",
  category,
  graphTitle,
  graphSubTitle = "American Community Survey 5-year Estimates"
) {
  if (var2Scale == "numbers") {
    yLabel = "Estimate"
    yLabelFormat = scales::comma
  } else if (var2Scale == "dollars") {
    yLabel = "Dollars"
    yLabelFormat = scales::dollar
  } else if (var2Scale == "percentage") {
    yLabel = "Percent"
    yLabelFormat = scales::percent
  } else {
    yLabel = "Estimate"
    yLabelFormat = scales::comma
  }
  plot <- ggplot(data = dataset, aes_string(x = var1, y = var2, color = category)) +
    geom_point() +
    geom_line() +
    scale_y_continuous(
      yLabel,
      labels = yLabelFormat
    ) +
    labs(
      x = "Year",
      title = graphTitle,
      subtitle = graphSubTitle,
      color = "County"
    )
  return(plot)
}

Region6TotalPopulationTimeSeriesGraph <- drawTimeSeriesGraph(
  dataset = srs_pop,
  var1 = "year",
  var2 = "estimate",
  category = "county_name",
  graphTitle = "Total Population"
)

Region6IncomePerCapitaTimeSeriesGraph <- drawTimeSeriesGraph(
  dataset = srs_percap,
  var1 = "year",
  var2 = "estimate",
  var2Scale = "dollars",
  category = "county_name",
  graphTitle = "Income Per Capita"
)

# drawTimeSeriesGraph(srs_medhh, "year", "estimate", "county_name")

Region6TotalVeteranTimeSeriesGraph <- drawTimeSeriesGraph(
  dataset = srs_vet,
  var1 = "year",
  var2 = "estimate",
  category = "county_name",
  graphTitle = "Total Veterans"
)

Region6PercentVeteransOver55TimeSeriesGraph <- drawTimeSeriesGraph(
  dataset = srs_veto55,
  var1 = "year",
  var2 = "percentage",
  var2Scale = "percentage",
  category = "county_name",
  graphTitle = "Percent Veterans Over 55"
)

Region6PercentNoCarTimeSeriesGraph <- drawTimeSeriesGraph(
  dataset = srs_nocars,
  var1 = "year",
  var2 = "percentage",
  var2Scale = "percentage",
  category = "county_name",
  graphTitle = "Percent Population Without Household Vehicles"
)

# for testing
# 
# srs_test <- filter(illinoisTractTimeSeriesData, variable == "B01001_001")
# 
# mdl_test <- lm(estimate ~ year, srs_test)
# 
# ggplot(data = srs_test, aes(x = year, y = estimate, color = county_name)) +
#   geom_point() +
#   geom_line()


# other graphs ------------------------------------------------------------
# drawGraph <- function(dataset, var1, var2) {
#   linearModel <- lm(
#     paste(var1, "~", var2),
#     data = dataset
#   )
#   plot <- ggplot(data = dataset, aes_string(x = var1, y = var2)) +
#     geom_point() +
#     stat_smooth(method = "lm", col = "red")
#     # geom_line(mapping = aes(linearModel))
#   print(summary(linearModel))
#   return(plot)
# }
# 
# region6TractData <- filter(tractData, COUNTYFP %in% region6CountyFIPS3)
# 
# drawGraph(dataset = tractData, var1 = "per_dsblty", var2 = "per_blwpov") # sig
# drawGraph(dataset = tractData, var1 = "gini", var2 = "per_blwpov") # sig
# drawGraph(dataset = tractData, var1 = "growth_pop", var2 = "per_dsblty")
# drawGraph(dataset = tractData, var1 = "per_blwpov", var2 = "per_dsblty")
# drawGraph(dataset = tractData, var1 = "den_pop", var2 = "per_ovr65")
# drawGraph(dataset = tractData, var1 = "per_blwpov", var2 = "den_pop")

# HSTP map function -------------------------------------------------------
getHSTPMap <- function(
  sf,
  backgroundLayer = countyLayer,
  foregroundLayer = region6CountyLayer,
  variable,
  title,
  proj = crs,
  n = 7,
  vals = "whole",
  classificationStyle = "jenks",
  palette = "seq"
) {
  # 2019-12-19 TRRILEY - seems to break tmap plotting
  # require(tidycensus, tidyverse, tmap)
  formatDollarAmount <- function(x, digits = 0) {
    # helper function to format dollar amounts
    paste0("$", formatC(x, digits = digits, format = "f", big.mark = ","))
  }
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
    legendTitle = paste(geography, acsYear, sep = ", ")
  } else if (vals == "percent") {
    legendFormat = list(fun = percent, suffix = "%", digits = 2)
    legendTitle = paste(geography, acsYear, sep = ", ")
  } else if (vals == "decimal") {
    legendFormat = list(digits = 2)
    legendTitle = paste(geography, acsYear, sep = ", ")
  } else {
    legendFormat = list(digits = 0)
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
      size = .9,
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
      asp = 4/3
    ) +
    tmap::tm_scale_bar(
      width = 0.2,
      text.size = .5
    ) +
    tmap::tm_credits(
      text = getACSYearsLabel(acsYear),
      size = .5,
      bg.color = "white",
      bg.alpha = .5
    )
  map
}

# variables for map legends -----------------------------------------------

# mapVariables <- c(
#   IncomePerCapita,
#   GINICoefficient,
#   MedianHouseholdIncome,
#   PercentBelow18,
#   Percent18to65,
#   PercentOver65,
#   PercentVeteran,
#   PercentDisability,
#   PercentBelowPoverty,
#   PercentNoCar,
#   PercentNoDiploma,
#   PercentHouseholdSNAPOver60,
#   PopulationDensity
# )

mapTitles <- c(
  IncomePerCapitaMapTitle <- "Income Per Capita",
  GINICoefficientMapTitle <- "GINI Coefficient",
  MedianHouseholdIncomeMapTitle <- "Median Household Income",
  PercentBelow18MapTitle <- "Percent of Population under 18",
  Percent18to65MapTitle <- "Percent of Population between 18 and 65",
  PercentOver65MapTitle <- "Percent of Population Over 65",
  PercentVeteranMapTitle <- "Veterans as Percent of Adults",
  PercentDisabilityMapTitle <- "Percent of Population with a Disability",
  PercentBelowPovertyMapTitle <- "Percent of Population with Household Income below Poverty", # too long
  PercentNoCarMapTitle <- "Percent of Population with Zero Household Vehicles", # too long
  PercentNoDiplomaMapTitle <- "Percent of Adults without High School Diploma", # too long
  PercentHouseholdSNAPOver60MapTitle <- "Percent of Households receiving SNAP with someone over 60",# too long
  PopulationDensityMapTitle <- "Population per Square Mile",
  PopulationGrowthMapTitle <- "Population Change, 2017-2018"
)
mapVariablePalettes <- c(
  IncomePerCapitaPalette <- "Blues",
  GINICoefficientPalette <- "Reds",
  MedianHouseholdIncomePalette <- "Greens",
  PercentBelow18Palette <- "YlGn",
  Percent18to65Palette <- "YlGn",
  PercentOver65Palette <- "YlGn",
  PercentVeteranPalette <- "YlGn",
  PercentDisabilityPalette <- "YlGn",
  PercentBelowPovertyPalette <- "YlOrRd",
  PercentNoCarPalette <- "YlOrRd",
  PercentNoDiplomaPalette <- "YlOrRd",
  PercentHouseholdSNAPOver60Palette <- "YlOrRd",
  PopulationDensityPalette <- "YlGn",
  PopulationGrowthPalette <- "RdBu"
)

# county maps -------------------------------------------------------------
# region 6 tract maps -----------------------------------------------------
# income per capita by tract
suppressWarnings(
  Region6TractIncomePerCapitaMap <- getHSTPMap(
    sf = region6TractLayer,
    variable = "inc_percap",
    title = IncomePerCapitaMapTitle,
    vals = "dollars",
    palette = IncomePerCapitaPalette
  ) %T>%
    tmap::tmap_save(
      filename = paste(
        outputMapDirectory,
        addACSYearsToFilename("Region-6-Tract_Income-per-Capita.pdf", acsYear),
        sep = "/"
      )
    )
)
# gini coefficient by tract
suppressWarnings(
  Region6TractGINICoefficientMap <- getHSTPMap(
    sf = region6TractLayer,
    variable = "gini",
    title = GINICoefficientMapTitle,
    vals = "decimal",
    palette = GINICoefficientPalette
  ) %T>%
    tmap::tmap_save(
      filename = paste(
        outputMapDirectory,
        addACSYearsToFilename("Region-6-Tract_GINI-Coefficient.pdf", acsYear),
        sep = "/"
      )
    )
)
# median household income by tract
suppressWarnings(
  Region6TractMedianHouseholdIncomeMap <- getHSTPMap(
    sf = region6TractLayer,
    variable = "inc_medhh",
    title = MedianHouseholdIncomeMapTitle,
    vals = "dollars",
    palette = MedianHouseholdIncomePalette
  ) %T>%
    tmap::tmap_save(
      filename = paste(
        outputMapDirectory,
        addACSYearsToFilename("Region-6-Tract_Median-Household-Income.pdf", acsYear),
        sep = "/"
      )
    )
)
# percent below 18 by tract
suppressWarnings(
  Region6TractPercentBelow18Map <- getHSTPMap(
    sf = region6TractLayer,
    variable = "per_blw18",
    title = PercentBelow18MapTitle,
    vals = "percent",
    palette = PercentBelow18Palette
  ) %T>%
    tmap::tmap_save(
      filename = paste(
        outputMapDirectory,
        addACSYearsToFilename("Region-6-Tract_Percent-Below-18.pdf", acsYear),
        sep = "/"
      )
    )
)
# percent 18 to 65 by tract
suppressWarnings(
  Region6TractPercent18to65Map <- getHSTPMap(
    sf = region6TractLayer,
    variable = "per_18to65",
    title = Percent18to65MapTitle,
    vals = "percent",
    palette = Percent18to65Palette
  ) %T>%
    tmap::tmap_save(
      filename = paste(
        outputMapDirectory,
        addACSYearsToFilename("Region-6-Tract_Percent-18-to-65.pdf", acsYear),
        sep = "/"
      )
    )
)
# percent over 65 by tract
suppressWarnings(
  Region6TractPercentOver65Map <- getHSTPMap(
    sf = region6TractLayer,
    variable = "per_ovr65",
    title = PercentOver65MapTitle,
    vals = "percent",
    palette = PercentOver65Palette
  ) %T>%
    tmap::tmap_save(
      filename = paste(
        outputMapDirectory,
        addACSYearsToFilename("Region-6-Tract_Percent-Over-65.pdf", acsYear),
        sep = "/"
      )
    )
)
# percent veteran by tract
suppressWarnings(
  Region6TractPercentVeteranMap <- getHSTPMap(
    sf = region6TractLayer,
    variable = "per_vet",
    title = PercentVeteranMapTitle,
    vals = "percent",
    palette = PercentVeteranPalette
  ) %T>%
    tmap::tmap_save(
      filename = paste(
        outputMapDirectory,
        addACSYearsToFilename("Region-6-Tract_Percent-Veteran.pdf", acsYear),
        sep = "/"
      )
    )
)
# percent with disability by tract
suppressWarnings(
  Region6TractPercentDisabilityMap <- getHSTPMap(
    sf = region6TractLayer,
    variable = "per_dsblty",
    title = PercentDisabilityMapTitle,
    vals = "percent",
    palette = PercentDisabilityPalette
  ) %T>%
    tmap::tmap_save(
      filename = paste(
        outputMapDirectory,
        addACSYearsToFilename("Region-6-Tract_Percent-Disability.pdf", acsYear),
        sep = "/"
      )
    )
)
# percent below poverty by tract
suppressWarnings(
  Region6TractPercentBelowPovertyMap <- getHSTPMap(
    sf = region6TractLayer,
    variable = "per_blwpov",
    title = PercentBelowPovertyMapTitle,
    vals = "percent",
    palette = PercentBelowPovertyPalette
  ) %T>%
    tmap::tmap_save(
      filename = paste(
        outputMapDirectory,
        addACSYearsToFilename("Region-6-Tract_Percent-Below-Poverty.pdf", acsYear),
        sep = "/"
      )
    )
)
# percent without car by tract
suppressWarnings(
  Region6TractPercentNoCarMap <- getHSTPMap(
    sf = region6TractLayer,
    variable = "per_nocars",
    title = PercentNoCarMapTitle,
    vals = "percent",
    palette = PercentNoCarPalette
  ) %T>%
    tmap::tmap_save(
      filename = paste(
        outputMapDirectory,
        addACSYearsToFilename("Region-6-Tract_Percent-No-Car.pdf", acsYear),
        sep = "/"
      )
    )
)
# percent without diploma by tract
suppressWarnings(
  Region6TractPercentNoDiplomaMap <- getHSTPMap(
    sf = region6TractLayer,
    variable = "per_nodipl",
    title = PercentNoDiplomaMapTitle,
    vals = "percent",
    palette = PercentNoDiplomaPalette
  ) %T>%
    tmap::tmap_save(
      filename = paste(
        outputMapDirectory,
        addACSYearsToFilename("Region-6-Tract_Percent-No-Diploma.pdf", acsYear),
        sep = "/"
      )
    )
)
# percent in household on SNAP with person over 60 by tract
suppressWarnings(
  Region6TractPercentHouseholdSNAPOver60Map <- getHSTPMap(
    sf = region6TractLayer,
    variable = "per_hhSo60",
    title = PercentHouseholdSNAPOver60MapTitle,
    vals = "percent",
    palette = PercentHouseholdSNAPOver60Palette
  ) %T>%
    tmap::tmap_save(
      filename = paste(
        outputMapDirectory,
        addACSYearsToFilename("Region-6-Tract_Percent-Household-SNAP-Over-60.pdf", acsYear),
        sep = "/"
      )
    )
)
# population density of tracts
suppressWarnings(
  Region6TractPopulationDensityMap <- getHSTPMap(
    sf = region6TractLayer,
    variable = "den_pop",
    title = PopulationDensityMapTitle,
    vals = "decimal",
    palette = PopulationDensityPalette,
    n = 5,
    classificationStyle = "quantile"
  ) %T>%
    tmap::tmap_save(
      filename = paste(
        outputMapDirectory,
        addACSYearsToFilename("Region-6-Tract_Population-Density.pdf", acsYear),
        sep = "/"
      )
    )
)
# population growth of tracts
suppressWarnings(
  Region6TractPopulationGrowthMap <- getHSTPMap(
    sf = region6TractLayer,
    variable = "growth_pop",
    title = PopulationGrowthMapTitle,
    vals = "percent",
    palette = PopulationGrowthPalette,
    n = 5,
    classificationStyle = "jenks"
  ) %T>%
    tmap::tmap_save(
      filename = paste(
        outputMapDirectory,
        addACSYearsToFilename("Region-6-Tract_Population-Growth.pdf", acsYear),
        sep = "/"
      )
    )
)
# region 6 block group maps -----------------------------------------------
# income per capita by block group
suppressWarnings(
  Region6BlockGroupIncomePerCapitaMap <- getHSTPMap(
    sf = blockGroupLayer,
    variable = "inc_percap",
    title = IncomePerCapitaMapTitle,
    vals = "dollars",
    palette = IncomePerCapitaPalette
  ) %T>%
    tmap::tmap_save(
      filename = paste(
        outputMapDirectory,
        addACSYearsToFilename("Region-6-Block-Group_Income-per-Capita.pdf", acsYear),
        sep = "/"
      )
    )
)
# percent below 18 by block group
suppressWarnings(
  Region6BlockGroupPercentBelow18Map <- getHSTPMap(
    sf = blockGroupLayer,
    variable = "per_blw18",
    title = PercentBelow18MapTitle,
    vals = "percent",
    palette = PercentBelow18Palette
  ) %T>%
    tmap::tmap_save(
      filename = paste(
        outputMapDirectory,
        addACSYearsToFilename("Region-6-Block-Group_Percent-Below-18.pdf", acsYear),
        sep = "/"
      )
    )
)
# percent 18 to 65 by block group
suppressWarnings(
  Region6BlockGroupPercent18to65Map <- getHSTPMap(
    sf = blockGroupLayer,
    variable = "per_18to65",
    title = Percent18to65MapTitle,
    vals = "percent",
    palette = Percent18to65Palette
  ) %T>%
    tmap::tmap_save(
      filename = paste(
        outputMapDirectory,
        addACSYearsToFilename("Region-6-Block-Group_Percent-18-to-65.pdf", acsYear),
        sep = "/"
      )
    )
)
# percent over 65 by block group
suppressWarnings(
  Region6BlockGroupPercentOver65Map <- getHSTPMap(
    sf = blockGroupLayer,
    variable = "per_ovr65",
    title = PercentOver65MapTitle,
    vals = "percent",
    palette = PercentOver65Palette
  ) %T>%
    tmap::tmap_save(
      filename = paste(
        outputMapDirectory,
        addACSYearsToFilename("Region-6-Block-Group_Percent-Over-65.pdf", acsYear),
        sep = "/"
      )
    )
)
# percent veteran by block group
suppressWarnings(
  Region6BlockGroupPercentVeteranMap <- getHSTPMap(
    sf = blockGroupLayer,
    variable = "per_vet",
    title = PercentVeteranMapTitle,
    vals = "percent",
    palette = PercentVeteranPalette
  ) %T>%
    tmap::tmap_save(
      filename = paste(
        outputMapDirectory,
        addACSYearsToFilename("Region-6-Block-Group_Percent-Veteran.pdf", acsYear),
        sep = "/"
      )
    )
)
# veteran dot density block group map
veteransHealthAdministrationFacilities <- sf::read_sf(
  HSTPGeoPackage,
  "facilities_VHA"
) %>%
  sf::st_transform(crs = crs)

Region6BlockGroupVeteranDotDensityMap <- tmap::tm_shape(
  countyLayer,
  projection = crs,
  bbox = veteranDotDensityLayer,
  unit = "mi"
) +
  tmap::tm_fill(col = "grey85") +
  tmap::tm_borders(col = "white", lwd = 2) +
  tmap::tm_shape(blockGroupLayer) +
  tmap::tm_fill(col = "white") +
  tmap::tm_borders(col = "grey50", lwd = .5) +
  tmap::tm_shape(veteranDotDensityLayer) +
  tmap::tm_dots(col = "darkgreen") +
  tmap::tm_shape(veteransHealthAdministrationFacilities) +
  tmap::tm_symbols(
    col = "gold",
    shape = 24,
    size = 0.7
  ) +
  tmap::tm_shape(region6CountyLayer) +
  tmap::tm_borders(col = "black", lwd = 2) +
  tmap::tm_layout(
    legend.position = c("left", "top"),
    legend.title.size = 1.1,
    legend.title.fontface = "bold",
    title = "Veterans per Block Group",
    title.size = 1.2,
    title.fontface = "bold",
    fontfamily = "sans",
    frame.lwd = 2,
    outer.bg.color = "#00000000",
    asp = 4/3
  ) +
  tmap::tm_add_legend(
    type = "symbol",
    size = c(.3, .7),
    col = c("darkgreen", "gold"),
    shape = c(16, 24),
    labels = c(" ~ 10 veterans", " VHA Outpatient Clinic")
  ) +
  tmap::tm_scale_bar(
    width = 0.2,
    text.size = .5
  ) +
  tmap::tm_credits(
    text = getACSYearsLabel(acsYear),
    size = .5,
    bg.color = "white",
    bg.alpha = .5
  )

# population density of block groups
suppressWarnings(
  Region6BlockGroupPopulationDensityMap <- getHSTPMap(
    sf = blockGroupLayer,
    variable = "den_pop",
    title = PopulationDensityMapTitle,
    vals = "decimal",
    palette = PopulationDensityPalette,
    n = 5,
    classificationStyle = "quantile"
  ) %T>%
    tmap::tmap_save(
      filename = paste(
        outputMapDirectory,
        addACSYearsToFilename("Region-6-Block-Group_Population-Density.pdf", acsYear),
        sep = "/"
      )
    )
)


# SHOWBUS mapping ---------------------------------------------------------
# lots of lines, intricate labeling > better to do in QGIS
# getShowbusMap <- function(
#   extentLayer = showbusRoutes,
#   routesLayer = showbusRoutes,
#   stopsLayer = showbusStops,
#   foregroundLayer = region6CountyLayer,
#   backgroundLayer = countyLayer,
#   title,
#   proj = crs
# ) {
#   require(tmap)
#   map <- tm_shape(
#     backgroundLayer,
#     bbox = extentLayer,
#     projection = proj,
#     unit = "mi"
#   ) +
#     tm_fill(col = "grey85") +
#     tm_borders(col = "white", lwd = 2) +
#     tm_shape(foregroundLayer) +
#     tm_fill(col = "white", lwd = 2) +
#     tm_borders(col = "black", lwd = 2) +
#     tm_shape(routesLayer) +
#     tm_lines(
#       lty = 2
#     ) +
#     tm_shape(stopsLayer) +
#     tm_dots() +
#     tm_layout(
#       title = title,
#       title.size = 1.2,
#       title.fontface = "bold",
#       fontfamily = "sans",
#       frame.lwd = 2,
#       outer.bg.color = "#00000000",
#       asp = 4/3
#     )
#   map
# }
# getShowbusMap(
#   title = "SHOWBUS Service Map"
# )
# 
# 
# getShowbusChoroplethMap <- function(
#   extentLayer = showbusRoutes,
#   foregroundLayer = region6CountyLayer,
#   backgroundLayer = countyLayer,
#   title,
#   proj = crs
# ) {
#   
# }


# make pretty tables for report -------------------------------------------
# for reasons unknown, the ± sometimes causes problems with the tables,
# typically by inserting a strange character. re-running this block
# seems to solve the problem when it occurs

Region6TotalPopulationTimeSeriesTable <- srs_pop %>%
  ungroup() %>%
  transmute(
    County = county_name,
    year = year,
    value = paste(
      comma(estimate, 1),
      comma(moe, 1),
      sep = " ±"
    )
  ) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  knitr::kable(align = c('lccccc'))

Region6TotalVeteranTimeSeriesTable <- srs_vet %>%
  ungroup() %>%
  transmute(
    County = county_name,
    year = year,
    value = paste(
      comma(estimate, 1),
      comma(moe, 1),
      sep = " ±"
    )
  ) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  knitr::kable(align = c('lccccc'))

Region6PercentVeteransOver55TimeSeriesTable <- srs_veto55 %>%
  ungroup() %>%
  transmute(
    County = county_name,
    year = year,
    value = paste(
      percent(percentage, accuracy = .1),
      percent(moe, accuracy = .1),
      sep = ", ±"
    )
  ) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  knitr::kable(align = c('lccccc'))

Region6IncomePerCapitaTimeSeriesTable <- srs_percap %>%
  ungroup() %>%
  transmute(
    County = county_name,
    year = year,
    value = dollar(estimate, accuracy = 1)
  ) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  knitr::kable(align = c('lccccc'))

Region6PercentNoCarTimeSeriesTable <- srs_nocars %>%
  ungroup %>%
  transmute(
    County = county_name,
    year = year,
    value = paste(
      percent(percentage, accuracy = .1),
      percent(moe, accuracy = .1),
      sep = ", ±"
    )
  ) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  knitr::kable(align = c('lccccc'))


