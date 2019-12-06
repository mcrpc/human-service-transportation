
# load packages and set options -------------------------------------------
packages <- c(
  "tidyverse",
  "tidycensus",
  "tmap",
  "censusapi",
  "sf",
  "magrittr",
  "tigris",
  "units",
  "scales",
  "cartography",
  "lehdr",
  "stplanr"
  )

lapply(packages, library, character.only = TRUE)
options(tigris_use_cache = TRUE) # causes geometries to be stored in cache instead of re-loaded every time

# initialize global variables and set environments -------------------------------
inputDataDirectory <- "G:/_Projects/HSTP/data_in"
outputDataDirectory <- "G:/_Projects/HSTP/data_out"
chartDataDirectory <- "G:/_Projects/HSTP/charts/data"
mapDirectory <- "G:/_Projects/HSTP/maps"
acsTableInventory <- readr::read_csv("G:/_Projects/HSTP/R/inventory.csv")
censusAPIKey <- "2f44a09c684e6b031d3e76f3655c169c167aeba8"
Sys.setenv(CENSUS_KEY = apiKey)
acsVariableTable2017 <- tidycensus::load_variables(2017, "acs5", cache = TRUE)         # !!CHANGE THIS WHEN UPDATED DATA IS RELEASED!!

# initialize global functions ---------------------------------------------
getArea <- function(sf) {
  dplyr::mutate(sf, area_sq_mi = units::set_units(sf::st_area(geometry), mi^2))
} # function to return column with area of geographies
coalesceByColumn <- function(df) {
  return(dplyr::coalesce(!!! as.list(df)))
} # useful function for coalescing a table by columns -- https://stackoverflow.com/a/45515491

# source other scripts to begin processing --------------------------------



