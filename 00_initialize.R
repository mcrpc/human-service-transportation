
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
  "here"
)

invisible(lapply(packages, library, character.only = TRUE))
options(tigris_use_cache = TRUE) # causes geometries to be stored in cache instead of re-loaded every time
# tmap_options(
#   show.messages = FALSE,
#   output.dpi = 300
# )

# initialize global variables and set environments -------------------------------
inputDataDirectory <- here::here("data", "input")

if(!dir.exists(outputDataDirectory <- here::here("data", "output"))) {
  dir.create(outputDataDirectory)
}

if(!dir.exists(outputChartDirectory <- here::here("chart"))) {
  dir.create(outputChartDirectory)
}

if(!dir.exists(outputMapDirectory <- here::here("map"))) {
  dir.create(outputMapDirectory)
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

# create tibble to store relationship between desired variable's name,
# its description, and the ACS variables from which it is derived
acsVariableTibble <- tibble::tribble(
  ~short_name, ~long_name, ~acs_variables_2017, ~denominator,
  "est_pop", "Total Population", est_pop <- c("B01001_001E"), NULL,
  "est_blw18", "Population Under 18", est_blw18 <- paste0("B01001_0", stringr::str_pad(c(3:6,27:30), 2, "left", pad = "0"), "E"), dnm_blw18 <- "B01001_001E",
  "est_18to65", "Population Between 18 and 65", est_18to65 <- paste0("B01001_0", stringr::str_pad(c(7:9,10:19,31:43), 2, "left", pad = "0"), "E"), dnm_18to65 <- "B01001_001E",
  "est_ovr65", "Population Over 65", est_ovr65 <- paste0("B01001_0", c(20:25, 44:49), "E"), dnm_ovr65 <- "B01001_001E",
  "est_vet", "Population of Veterans", est_vet <- "B21001_002E", dnm_vet <- "B21001_001E",
  "est_dsblty", "Population with a Disability", est_dsblty <- subset(acs2017VariableTable, stringr::str_detect(name, "B18101_") & stringr::str_detect(label, "With a disability")) %>% .$name %>% paste(.,"E",sep=""), dnm_dsblty <- "B18101_001E",
  "est_blwpov", "Population with household income below poverty", est_blwpov <- "B17020_002E", dnm_blwpov <- "B17020_001E",
  "est_nocars", "Population with no vehicles available", est_nocars <- "B08141_002E", dnm_nocars <- "B08141_001E",
  "gini", "GINI Coefficient", gini <- "B19083_001E", NULL,
  "est_nodipl", "Population without High School Diploma", est_nodipl <- paste0("B15003_0", stringr::str_pad(c(2:16), 2, "left", pad = "0"), "E"), dnm_nodipl <- "B15003_001E",
  "inc_percap", "Per Capita Income", inc_percap <- "B19301_001E", NULL,
  "inc_medhh", "Median Household Income", inc_medhh <- "B22008_001E", NULL,
  "est_hhSo60", "Population in households receiving SNAP with at least one person over 60", est_hhSo60 <- "B22001_003E", dnm_hhSo60 <- "B22001_001E"
)


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


# run the scripts! --------------------------------------------------------
source(here::here("01_retrieve-data.R"))
source(here::here("02_process-data.R"))
source(here::here("03_analyze-data.R"))
source(here::here("04_create-graphics.R"))

