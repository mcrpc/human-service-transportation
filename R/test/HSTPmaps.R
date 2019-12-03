library(tidyverse)  # used because it makes data processing in R 1000% more bearable
library(tidycensus) # used to make dealing with the census API a manageable task for humans
library(tmap)       # used to quickly and intuitively make maps
options(tigris_use_cache = TRUE) # causes geometries to be stored in cache instead of re-loaded every time

setwd("G:/_Projects/HSTP/ACS")
load(".RData")
api_key <- "2f44a09c684e6b031d3e76f3655c169c167aeba8"

v17 <- load_variables(2017, "acs5", cache = TRUE)         # !!CHANGE THIS WHEN UPDATED DATA IS RELEASED!!


# define parameters for census API calls ----------------------------------
counties <- c("113",
              "053",
              "105",
              "075",
              "091")
year <- 2017                                              # !!CHANGE THIS WHEN UPDATED DATA IS RELEASED!!
survey <- "acs5"
vars <- c(
  median_income = "B19013_001",
  gini = "B19083_001",
  popOver65 = ""
  # hhnoCar = "",
  # hhPov = ""
)

subjVars <- c(
  popVetPer = "S2101_C04_001",
  popDisPer = "S1810_C03_001",
  popOver65Per = "S0101_C02_030",
  popPovPer = "S1701_C03_001"
)

sumVars <- c(
  total_pop = "B01003_001"
  # total_hh = ""
)

getarea <- function(sf) {
  require(tidyverse, units)
  mutate(sf, area_sq_mi = set_units(st_area(geometry), mi^2))
} # function to return column with area of geographies

# set up layers -----------------------------------------------------------
countyLyr <- get_acs(  # this will only be used as a background to the tracts or block groups which will have the actual data
  geography = "county",
  variables = "B01001_001",  # we won't use these estimates. There doesn't seem to be a way to pull only geometry via tidycensus, and I don't want to load another library or dataset just for the background
  state = c("17", "18"),  # include Indiana because it borders Kankakee on the east and white space is bad
  geometry = TRUE,
  output = "wide"
)

r6countyLyr <- get_acs(  # this will be used to highlight region 6 county boundaries in the foreground and for labeling said counties
  geography = "county",
  variables = "B01001_001",  # we won't use these estimates. There doesn't seem to be a way to pull only geometry via tidycensus, and I don't want to load another library or dataset just for the background
  state = c("17"),
  county = counties,
  geometry = TRUE,
  output = "wide"
) %>%
  mutate(NAME = word(NAME, 1)) # replaces the entries in the NAME column with only the first word before a space

subjTractTable <- get_acs(
  geography = "tract",
  variables = subjVars,
  state = "17",
  county = counties,
  year = year,
  survey = "acs5",
  output = "wide"  # wide format is better for mapping
)

tractLyr <- get_acs(
  geography = "tract",
  variables = vars,
  state = "17",
  county = counties,
  year = year,
  survey = survey,
  summary_var = sumVars[1],
  geometry = TRUE,
  output = "wide"  # wide format is better for mapping
) %>%
  getarea() %>%
  mutate(pop_den = summary_est / area_sq_mi) %>%
  left_join(subjTractTable)

blockGroupLyr <- get_acs(
  geography = "block group",
  variables = vars,
  state = "17",
  county = counties,
  year = year,
  survey = survey,
  geometry = TRUE,
  output = "wide"
)

# HSTP map function -------------------------------------------------------

hstpmap <- function(sf,
                    var,
                    title,
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
  if (vals == "dollars") {
    lformat = list(fun = make_dollar)
    ltitle = paste("US Dollars, ", year)
  } else if (vals == "percent") {
    lformat = list(format = "f", suffix = "%", digits = 1)
    ltitle = paste("Estimate, ", year)
  } else if (vals == "decimal") {
    lformat = list(format = "f", digits = 2)
    ltitle = paste("Estimate, ", year)
  } else {
    lformat = list(format = "f", digits = 0)
    ltitle = paste("Estimate, ", year)
  }
  map <- tm_shape(countyLyr,
                  bbox = sf,
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
    tm_shape(r6countyLyr) +
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

map_popVets <- hstpmap(tractLyr, var = "popVetPerE", title = "Veterans as Percent of Total Population", vals = "percent") %T>%
  tmap_save(filename = paste("popVets_", year, ".pdf", sep = ""))
map_popDis <- hstpmap(tractLyr, var = "popDisPerE", title = "Persons with Disabilities as Percent of Total Population", vals = "percent") %T>%
  tmap_save(filename = paste("popDis_", year, ".pdf", sep = ""))
map_popOver65 <- hstpmap(tractLyr, var = "popOver65PerE", title = "Population 65 or Over as Percent of Total Population", vals = "percent") %T>%
  tmap_save(filename = paste("popOver65_", year, ".pdf", sep = ""))


map_medinc <- hstpmap(tractLyr, var = "median_incomeE", title = "Median Household Income by Tract", vals = "dollars", palette = "div")
map_popden <- hstpmap(tractLyr, var = "pop_den", title = "Population Density by Tract", style = "kmeans", palette = "Greens")
