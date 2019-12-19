
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
tmap_options(
  show.messages = FALSE,
  output.dpi = 300
  )

# initialize global variables and set environments -------------------------------
inputDataDirectory <- here::here("data", "input")
outputDataDirectory <- here::here("data", "output") %T>%
  if(!dir.exists(.)) {
    dir.create(.)
  }
outputChartDirectory <- here::here("chart") %T>%
  if(!dir.exists(.)) {
    dir.create(.)
  }
outputMapDirectory <- here::here("map") %T>%
  if(!dir.exists(.)) {
    dir.create(.)
  }
censusDataInventoryFile <- paste(
  inputDataDirectory,
  "census-data-inventory.csv",
  sep = "/"
)
censusDataInventory <- readr::read_csv(censusDataInventoryFile)
censusAPIKey <- "2f44a09c684e6b031d3e76f3655c169c167aeba8"
Sys.setenv(CENSUS_KEY = censusAPIKey)
acs2017VariableTable <- tidycensus::load_variables(2017, "acs5", cache = TRUE)
acs2018VariableTable <- tidycensus::load_variables(2018, "acs5", cache = TRUE)
region6CountyList <- c("113", "053", "105", "075", "091")
acsYear <- 2018                                              # !!CHANGE THIS WHEN UPDATED DATA IS RELEASED!!


# define global functions -------------------------------------------------
getArea <- function(sf) {
  # create new column with calculated area in square miles
  dplyr::mutate(sf, area_sq_mi = units::set_units(sf::st_area(geometry), mi^2))
}
coalesceByColumn <- function(df) {
  # coalesce a table by columns -- https://stackoverflow.com/a/45515491
  # this is used to clean the ACS data up after it is retrieved from the
  # API
  return(dplyr::coalesce(!!! as.list(df)))
}
addACSYearsToFilename <- function(filename, acsYear) {
  require(stringr)
  firstPart <- str_split(filename, "[.]", simplify = TRUE)[1]
  secondPart <- paste("_ACS-", (acsYear - 4), "-", acsYear, ".", sep = "")
  thirdPart <- str_split(filename, "[.]", simplify = TRUE)[2]
  paste0(firstPart, secondPart, thirdPart, sep = "")
}

# source other scripts to begin processing --------------------------------
source(here::here("01_retrieve-data.R"))
source(here::here("02_process-data.R"))
source(here::here("03_analyze-data.R"))
source(here::here("04_create-graphics.R"))

