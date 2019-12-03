library(tidyverse)
library(tidycensus)


setwd("G:/_Projects/HSTP/ACS")
api_key <- "2f44a09c684e6b031d3e76f3655c169c167aeba8"

coalesce_by_column <- function(df) {
  return(dplyr::coalesce(!!! as.list(df)))
} # useful function for coalescing a table by columns -- https://stackoverflow.com/a/45515491

# retrieve ACS data via API -----------------------------------------------
#census_api_key(api_key)
# run these lines to get a searchable list of variables
v17 <- load_variables(2017, "acs5", cache = TRUE)

View(v17)

# change these objects to modify the geography, area, year, and scope of the estimates retrieved
counties <- c("113",
              "053",
              "105",
              "075",
              "091")
tables <- read_csv("inventory.csv")
year <- 2017
survey <- "acs5"

# retrieve tables in tidy format ------------------------------------------
output <- "tidy"
ACStracts <- map_dfr(
  filter(tables, geo_level != "block")[[1]], 
  ~ get_acs(
    geography = "tract",
    table = .,
    state = "17",
    county = counties,
    year = year,
    survey = survey,
    output = output
  )
)

ACSblockGroups <- map_dfr(
  filter(tables, geo_level == "block group")[[1]], 
  ~ get_acs(
    geography = "block group",
    table = .,
    state = "17",
    county = counties,
    year = year,
    survey = survey,
    output = output
  )
)

# write tidy tables to csv for storage
write_csv(ACStracts, "tracts.csv")
write_csv(ACSblockGroups, "blockGroups.csv")

# retrieve tables in wide format ------------------------------------------
output <- "wide"
ACStracts_wide <- map_dfr(
  filter(tables, geo_level != "block")[[1]], 
    ~ get_acs(
        geography = "tract",
        table = .,
        state = "17",
        county = counties,
        year = year,
        survey = survey,
        output = output
      )
  ) %>%
    group_by(GEOID) %>%
    summarize_all(coalesce_by_column)


ACSblockGroups_wide <- map_dfr(
  filter(tables, geo_level == "block group")[[1]], 
  ~ get_acs(
    geography = "block group",
    table = .,
    state = "17",
    county = counties,
    year = year,
    survey = survey,
    output = output
  )
) %>%
  group_by(GEOID) %>%
  summarize_all(coalesce_by_column) %>%
  map(~.x) %>%  #https://community.rstudio.com/t/drop-all-na-columns-from-a-dataframe/5844/3
  discard(~all(is.na(.x))) %>%
  map_df(~.x)

# write wide tables to CSV for storage
write_csv(ACStracts_wide, "tracts_wide.csv")
write_csv(ACSblockGroups_wide, "blockGroups_wide.csv")


# pull table descriptions from census API ------------------------------------------
tableNames <- v17[-2]
tableNames$name <- with(tableNames, str_sub(name, start = 1, end = -5))
tableNames <- distinct(tableNames)
tables <- left_join(tables, tableNames, by = c("table_id" = "name"))
write_csv(tables, "inventory.csv")

# attempt to write meaningful column names to wide tables ----------------------------
ACStracts_wide <- read_csv("tracts_wide.csv")
ACSblockGroups_wide <- read_csv("blockGroups_wide.csv")

ACStracts_colnames <- enframe(colnames(ACStracts)) %>%
  mutate(name = value) %>%
  mutate(name = str_sub(value, start = 1, end = -2)) %>%
  left_join(v17) %>%
  mutate(label = ifelse(str_length(value) < 6, value, label)) %>%
  mutate(desc = ifelse(is.na(concept), label, str_c(concept, label,  sep = " - "))) %>%
  mutate(desc = ifelse(str_sub(value, start = -1L) == "M", str_c(concept, label, "Margin of Error", sep = " - "), desc)) %>%
  select(value, desc) %>%
  print()




# build table with meaningful variable names from tidy tables -------------
varNames <- load_variables(2017, "acs5", cache = TRUE) %>%
  bind_rows(load_variables(2017, "acs5/subject", cache = TRUE))
ACStracts <- read_csv("tracts.csv") %>%
  left_join(varNames, by = c("variable" = "name"))

tractData <- drop_na(ACStracts, estimate) %>%
  group_by(GEOID)

# begin massive list of selected variables

popTotal <- filter(tractData, str_detect(variable, "B01001_001")) %>%
  summarize(
    popTotal = sum(estimate),
    popTotal_moe = moe_sum(moe, estimate)
  )

popWithDis <- filter(tractData, str_detect(variable, "B18101") & str_detect(label, "With")) %>%
  summarize(
    popWithDis = sum(estimate),
    popWithDis_moe = moe_sum(moe, estimate)
  )

popInPov <- filter(tractData, str_detect(variable, "B17001_002")) %>%
  summarize(
    popInPov = sum(estimate),
    popInPov_moe = moe_sum(moe, estimate)
  )

popOver65 <- filter(tractData, str_detect(variable, "B08101_008")) %>%
  summarize(
    popOver65 = sum(estimate),
    popOver65_moe = moe_sum(moe, estimate)
  )

popVets <- filter(tractData, str_detect(variable, "S2101_C03_001")) %>%
  summarize(
    popVets = sum(estimate),
    popVets_moe = moe_sum(moe, estimate)
  )

hhNoCar <- filter(tractData, str_detect(variable, "B08141_002")) %>%
  summarize(
    hhNoCar = sum(estimate),
    hhNoCar_moe = moe_sum(moe, estimate)
  )

gini <- filter(tractData, str_detect(variable, "B19083_001")) %>%
  summarize(
    gini = sum(estimate),
    gini_moe = moe_sum(moe, estimate)
  )

VAcare <- filter(tractData, str_detect(variable, "C27009") & str_detect(label, "With VA")) %>% # first str_detect is redundant, may become necessary if more tables are added to inventory.csv
  summarize(
    VAcare = sum(estimate),
    VAcare_moe = moe_sum(moe, estimate)
  )

# join massive list of selected variables
tractData <- left_join(popTotal, popWithDis) %>%
  left_join(popInPov) %>%
  left_join(popOver65) %>%
  left_join(popVets) %>%
  left_join(hhNoCar) %>%
  left_join(gini) %>%
  left_join(VAcare) %>%
  print()

# write to csv for storage, this can be used for joins in GIS
write_csv(tractData, "tractsFiltered.csv")

