

# define local functions --------------------------------------------------
getACSYearsLabel <- function(acsYear) {
  paste("Data Source: Census ACS 5-year estimates,", acsYear - 4, "-", acsYear)
}

# make some graphs --------------------------------------------------------
ggplot(data = blockGroupData) +
  geom_jitter(mapping = aes(per_vet, den_pop))

ggplot(data = tractData, mapping = aes(per_blwpov)) +
  geom_histogram()

# poverty
ggplot(data = tractData, mapping = aes(per_blwpov)) +
  geom_histogram()

poverty <- tractLayer$per_blwpov
povertyOutliers <- subset(tractLayer, per_blwpov > (mean(poverty) + sd(poverty)))

tmap::tmap_mode("view")
tm_shape(tractLayer) +
  tm_borders("grey") +
  tm_shape(povertyOutliers) +
  tm_fill("den_pop", alpha = 0.8) +
  tm_shape(cityLayer[tractLayer,]) +
  tm_text("NAME", size = 0.6)
  

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
  PercentBelowPovertyMapTitle <- "Percent of Population with Household Income below Poverty",
  PercentNoCarMapTitle <- "Percent of Population with Zero Household Vehicles",
  PercentNoDiplomaMapTitle <- "Percent of Adults without High School Diploma",
  PercentHouseholdSNAPOver60MapTitle <- "Percent of Households receiving SNAP with someone over 60",
  PopulationDensityMapTitle <- "Population per Square Mile"
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
  PopulationDensityPalette <- "YlGn"
)

# county maps -------------------------------------------------------------
# region 6 tract maps -----------------------------------------------------
# income per capita by tract
suppressWarnings(
  Region6TractIncomePerCapitaMap <- getHSTPMap(
    sf = tractLayer,
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
    sf = tractLayer,
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
    sf = tractLayer,
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
    sf = tractLayer,
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
    sf = tractLayer,
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
    sf = tractLayer,
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
    sf = tractLayer,
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
    sf = tractLayer,
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
    sf = tractLayer,
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
    sf = tractLayer,
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
    sf = tractLayer,
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
    sf = tractLayer,
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
    sf = tractLayer,
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
