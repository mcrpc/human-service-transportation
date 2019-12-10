
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
  "stplanr",
  "here"
)

invisible(lapply(packages, library, character.only = TRUE))
options(tigris_use_cache = TRUE) # causes geometries to be stored in cache instead of re-loaded every time

# initialize global variables and set environments -------------------------------
inputDataDirectory <- here::here("data", "input")
outputDataDirectory <- here::here("data", "output")
outputChartDirectory <- here::here("chart")
outputMapDirectory <- here::here("map")
censusDataInventoryFile <- paste(
  inputDataDirectory,
  "2019-10-25_TRRILEY_census-data-inventory.csv",
  sep = "/"
)
censusDataInventory <- readr::read_csv(censusDataInventoryFile)
censusAPIKey <- "2f44a09c684e6b031d3e76f3655c169c167aeba8"
Sys.setenv(CENSUS_KEY = censusAPIKey)
acs2017VariableTable <- tidycensus::load_variables(2017, "acs5", cache = TRUE)         # !!CHANGE THIS WHEN UPDATED DATA IS RELEASED!!

# initialize global functions ---------------------------------------------
getArea <- function(sf) {
  dplyr::mutate(sf, area_sq_mi = units::set_units(sf::st_area(geometry), mi^2))
} # function to return column with area of geographies
coalesceByColumn <- function(df) {
  return(dplyr::coalesce(!!! as.list(df)))
} # useful function for coalescing a table by columns -- https://stackoverflow.com/a/45515491

# source other scripts to begin processing --------------------------------
source(here::here("01_retrieve-data.R"))
source(here::here("02_analyze-data.R"))
source(here::here("03_create-graphics.R"))

