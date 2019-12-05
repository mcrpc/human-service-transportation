library(tidyverse)  # used because it makes data processing in R 1000% more bearable
library(tidycensus) # used to make dealing with the census API a manageable task for humans
library(tmap)       # used to quickly and intuitively make maps
library(censusapi)
library(sf)
library(magrittr)
library(tigris)
library(units)
library(scales)
library(cartography)
library(lehdr)
library(stplanr)
options(tigris_use_cache = TRUE) # causes geometries to be stored in cache instead of re-loaded every time

# define global parameters
setwd("G:/_Projects/HSTP/R")
inputDataDirectory <- "G:/_Projects/HSTP/data_in"
outputDataDirectory <- "G:/_Projects/HSTP/data_out"
chartDataDirectory <- "G:/_Projects/HSTP/charts/data"
mapDirectory <- "G:/_Projects/HSTP/maps"
acsTableInventory <- readr::read_csv("G:/_Projects/HSTP/R/inventory.csv")
censusAPIKey <- "2f44a09c684e6b031d3e76f3655c169c167aeba8"
Sys.setenv(CENSUS_KEY = apiKey)
acsVariableTable2017 <- tidycensus::load_variables(2017, "acs5", cache = TRUE)         # !!CHANGE THIS WHEN UPDATED DATA IS RELEASED!!

# define global functions
getArea <- function(sf) {
  dplyr::mutate(sf, area_sq_mi = units::set_units(sf::st_area(geometry), mi^2))
} # function to return column with area of geographies
coalesceByColumn <- function(df) {
  return(dplyr::coalesce(!!! as.list(df)))
} # useful function for coalescing a table by columns -- https://stackoverflow.com/a/45515491
# checkBlockGroupDataAvailability <- function(var) {
#   test <- censusapi::getCensus("acs/acs5",
#                                vintage = 2017,
#                                region = "block group:*",
#                                regionin = "state:17+county:113+tract:000102",
#                                vars = paste(var, "E", sep = ""))
#   print(sum(is.na(test)))
# }

source('G:/_Projects/HSTP/R/process.R')
source('G:/_Projects/HSTP/R/analyze.R')
